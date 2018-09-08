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

:- type input == io.binary_input_stream.

% load(Path, Ctx, Sound, !IO)
:- pred load(string, cinnamon.driver, io.res(cinnamon.sound), io.io, io.io).
:- mode load(in, in, out, di, uo) is det.

%==============================================================================%
:- implementation.
%==============================================================================%

:- use_module maybe.
:- import_module pair.
:- import_module list.

:- use_module fjogg.
:- use_module mopus.
:- use_module buffer.

% Read in a page.
% Read out a packet.

:- pred read_page(input, cinnamon.format, list(buffer.buffer),
    cinnamon.loader, cinnamon.loader,
    mopus.decoder, mopus.decoder, 
    io.io, io.io).
:- mode read_page(in, in, in, di, uo, di, uo, di, uo) is det.

:- pred read(input, cinnamon.driver, io.res(cinnamon.sound), io.io, io.io).
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
            MaybeDecoder = maybe.yes((Decoder - NumChan)),
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
            MaybeDecoder = maybe.no,
            Result = io.error(io.make_io_error("Could not initialize Opus"))
        )
    ;
        Result = io.error(io.make_io_error("Invalid ogg file"))
    ).

:- pred read_from_page(input, cinnamon.format, fjogg.page,
    list(buffer.buffer), list(buffer.buffer), 
    cinnamon.loader, cinnamon.loader,
    mopus.decoder, mopus.decoder, 
    io.io, io.io).

:- mode read_from_page(in, in, in, in, out, di, uo, di, uo, di, uo) is det.

% Shorthand just to allow us to use state variables for the lists.
:- pred append_list(list(buffer.buffer)::in,
    list(buffer.buffer)::in, list(buffer.buffer)::out) is det.
append_list(End, Start, Out) :- list.append(Start, End, Out).

% LastData is data from a continued packet on the last page, or nothing.
read_page(Input, Format, LastData, !Loader, !Decoder, !IO) :-
    fjogg.read_page(Input, PageResult, !IO),
    ( PageResult = io.ok(Page) ->
        read_from_page(Input, Format, Page, LastData, LeftOver, !Loader, !Decoder, !IO),
        % The last packet, if incomplete, is left for us to handle here.
        read_page(Input, Format, LeftOver, !Loader, !Decoder, !IO)
    ;
        true % Pass...TODO: Print error if Input is not empty.
    ).

read_from_page(Input, Format, PageIn, BufferIn, BufferOut, !Loader, !Decoder, !IO) :-
    ( fjogg.packet_out(PageIn, PageOut, Size) ->
        buffer.read(Input, Size, BufferResult, !IO),
        (
            BufferResult = io.ok(Buffer),
            ( fjogg.last_packet(PageOut) ->
                ( fjogg.packet_crosses_page(PageOut) ->
                    BufferOut = [Buffer|[]]
                ;
                    buffer.concatenate(BufferIn, AllBuffersIn),
                    buffer.append(AllBuffersIn, Buffer) = DataBuffer,
                    mopus.decode_16(DataBuffer, PCMBuffer, !Decoder),
                    cinnamon.put_data(!Loader, PCMBuffer),
                    BufferOut = []
                )
            ;
                buffer.concatenate(BufferIn, AllBuffersIn),
                buffer.append(AllBuffersIn, Buffer, DataBuffer),
                Format = cinnamon.format(Type, _),
                ( Type = cinnamon.signed16 ->
                    mopus.decode_16(DataBuffer, PCMBuffer, !Decoder),
                    cinnamon.put_data(!Loader, PCMBuffer),
                    read_from_page(Input, Format, PageOut, [], BufferOut, !Loader, !Decoder, !IO)
                ; Type = cinnamon.float ->
                    mopus.decode_float(DataBuffer, PCMBuffer, !Decoder),
                    cinnamon.put_data(!Loader, PCMBuffer),
                    read_from_page(Input, Format, PageOut, [], BufferOut, !Loader, !Decoder, !IO)
                ;
                    io.write_string("Invalid format\n", !IO),
                    BufferOut = []
                )
            )
        ;
            BufferResult = io.error(_, Err),
            io.write_string("Error reading buffer ", !IO),
            io.write_string(io.error_message(Err), !IO),
            io.nl(!IO),
            BufferOut = []
        )
    ;
        BufferOut = []
    ).
