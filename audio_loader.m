%   Copyright (C) 2018 Alaskan Emily, Transnat Games.
% 
%   This software is provided 'as-is', without any express or implied
%   warranty.  In no event will the authors be held liable for any damages
%   arising from the use of this software.
%
%   Permission is granted to anyone to use this software for any purpose,
%   including commercial applications, and to alter it and redistribute it
%   freely, subject to the following restrictions:
%
%   1. The origin of this software must not be misrepresented; you must not
%      claim that you wrote the original software. If you use this software
%      in a product, an acknowledgment in the product documentation would be
%      appreciated but is not required.
%   2. Altered source versions must be plainly marked as such, and must not be
%      misrepresented as being the original software.
%   3. This notice may not be removed or altered from any source distribution.

:- module audio_loader.

%==============================================================================%
% "Just do it" module to load a Cinnamon sound given a path.
:- interface.
%==============================================================================%

:- use_module io.
:- use_module cinnamon.

% load(Path, Ctx, Sound, !IO)
:- pred load(string, cinnamon.driver, io.res(cinnamon.sound), io.io, io.io).
:- mode load(in, in, out, di, uo) is det.

%==============================================================================%
:- implementation.
%==============================================================================%

:- use_module maybe.
:- use_module bitmap.
:- use_module int.
:- import_module list.

:- use_module fjogg.
:- use_module mopus.

% shorthand
:- type io_input == io.binary_input_stream.

% Read in a page.
% Read out a packet.
:- pred read_page(io_input, cinnamon.format, list(bitmap.bitmap),
    cinnamon.loader, cinnamon.loader,
    mopus.decoder, mopus.decoder, 
    io.io, io.io).
:- mode read_page(in, in, in, di, uo, di, uo, di, uo) is det.

:- pred read(io_input, cinnamon.driver, io.res(cinnamon.sound), io.io, io.io).
:- mode read(in, in, out, di, uo) is det.

load(Path, Drv, Sound, !IO) :-
    io.open_binary_input(Path, InputResult, !IO),
    (
        InputResult = io.ok(Input),
        read(Input, Drv, Sound, !IO)
    ;
        InputResult = io.error(Err),
        Sound = io.error(Err)
    ).

read(Input, Drv, Result, !IO) :-
    fjogg.read_page(Input, PageResult, !IO),
    ( PageResult = io.ok(Page), fjogg.packet_out(Page, _, Size) ->
        mopus.init(Input, Size, MaybeDecoder, !IO),
        (
            MaybeDecoder = maybe.ok({Decoder, NumChan}),
            ( NumChan = 1 ->
                Speakers = cinnamon.mono
            ;
                Speakers = cinnamon.stereo
            ),
            Format = cinnamon.format(cinnamon.signed16, Speakers),
            cinnamon.create_loader(Format, 48000, Drv, LoaderResult),
            (
                LoaderResult = io.ok(LoaderIn),
                read_page(Input, Format, [], LoaderIn, LoaderOut, Decoder, _, !IO),
                cinnamon.finalize(LoaderOut, Sound),
                Result = io.ok(Sound)
            ;
                LoaderResult = io.error(Err),
                Result = io.error(Err)
            )
        ;
            MaybeDecoder = maybe.error(E),
            Result = io.error(io.make_io_error(E))
        )
    ;
        Result = io.error(io.make_io_error("Invalid ogg file"))
    ).

:- pred read_from_page(io_input, cinnamon.format, fjogg.page,
    list(bitmap.bitmap), list(bitmap.bitmap), 
    cinnamon.loader, cinnamon.loader,
    mopus.decoder, mopus.decoder, 
    io.io, io.io).

:- mode read_from_page(in, in, in, in, out, di, uo, di, uo, di, uo) is det.

% Shorthand just to allow us to use state variables for the lists.
:- pred append_list(list(bitmap.bitmap)::in,
    list(bitmap.bitmap)::in, list(bitmap.bitmap)::out) is det.
append_list(End, Start, Out) :- list.append(Start, End, Out).

% LastData is data from a continued packet on the last page, or nothing.
read_page(Stream, Format, LastData, !Loader, !Decoder, !IO) :-
    fjogg.read_page(Stream, PageResult, !IO),
    ( PageResult = io.ok(Page) ->
        read_from_page(Stream, Format, Page, LastData, LeftOver, !Loader, !Decoder, !IO),
        % The last packet, if incomplete, is left for us to handle here.
        read_page(Stream, Format, LeftOver, !Loader, !Decoder, !IO)
    ;
        true % Pass...TODO: Print error if Stream is not empty.
    ).

read_from_page(Stream, Format, PageIn, BufferIn, BufferOut, !Loader, !Decoder, !IO) :-
    ( if
        fjogg.packet_out(PageIn, PageOut, Size)
    then
        InitBitmap = bitmap.init(int.unchecked_left_shift(Size, 3)),
        io.read_bitmap(Stream, 0, Size, InitBitmap, Bitmap, BytesRead, ReadResult, !IO),
        (
            ReadResult = io.ok,
            ( if
                BytesRead = Size
            then
                ( if
                    fjogg.last_packet(PageOut)
                then
                    ( if
                        fjogg.packet_crosses_page(PageOut)
                    then
                        BufferOut = [Bitmap|[]]
                    else
                        list.append(BufferIn, [Bitmap|[]], AllBuffersIn),
                        bitmap.append_list(AllBuffersIn) = DataBuffer,
                        mopus.decode_16(DataBuffer, PCMBuffer, !Decoder),
                        cinnamon.put_data(!Loader, PCMBuffer),
                        BufferOut = []
                    )
                else
                    list.append(BufferIn, [Bitmap|[]], AllBuffersIn),
                    bitmap.append_list(AllBuffersIn) = DataBuffer,
                    Format = cinnamon.format(Type, _),
                    ( if
                        Type = cinnamon.signed16
                    then
                        mopus.decode_16(DataBuffer, PCMBuffer, !Decoder),
                        cinnamon.put_data(!Loader, PCMBuffer),
                        read_from_page(Stream, Format, PageOut, [], BufferOut, !Loader, !Decoder, !IO)
                    else if
                        Type = cinnamon.float
                    then
                        mopus.decode_float(DataBuffer, PCMBuffer, !Decoder),
                        cinnamon.put_data(!Loader, PCMBuffer),
                        read_from_page(Stream, Format, PageOut, [], BufferOut, !Loader, !Decoder, !IO)
                    else
                        io.write_string("Invalid format\n", !IO),
                        BufferOut = []
                    )
                )
            else
                io.write_string("Unexpected EOF\n", !IO),
                BufferOut = []
            )
        ;
            ReadResult = io.error(Err),
            io.write_string("Error reading buffer ", !IO),
            io.write_string(io.error_message(Err), !IO),
            io.nl(!IO),
            BufferOut = []
        )
    else
        BufferOut = []
    ).
