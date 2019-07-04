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

:- module mopus.

%==============================================================================%
% Bindings for the Opus audio codec reference implementation.
% This is partially tailored for Opus inside Ogg containers, as the decode preds
% will skip packets that start with the string "OpusTags", which is metadata in
% Ogg+Opus streams, and the init preds that accept buffers or binary input look
% for the "OpusHead" packet which is the first packet in Ogg+Opus streams.
:- interface.
%==============================================================================%

:- use_module io.
:- use_module maybe.
:- use_module bitmap.

%------------------------------------------------------------------------------%

:- type decoder.

% Shorthand
:- type io_input == io.binary_input_stream.

%------------------------------------------------------------------------------%

% TODO: Expose Opus error codes!

% Uses the first packet of an Ogg stream to initialize the decoder
% init(Packet, Decoder, NumChannels)
:- pred init(bitmap.bitmap::in, decoder::uo, int::uo) is semidet.

% Reads in the first packet of an Ogg stream to initialize the decoder
:- pred init(io_input, int, maybe.maybe_error({decoder, int}), io.io, io.io).
:- mode init(in, in, uo, di, uo) is det.

% init2(SampleRate, Channels, Decoder, Channels).
:- pred init2(int::in, int::in, decoder::uo, int::uo) is semidet.

% decode_16(EncodedInput, PCM16Output, !Decoder)
:- pred decode_16(bitmap.bitmap, bitmap.bitmap, decoder, decoder).
:- mode decode_16(in, uo, di, uo) is det.

% decode_float(EncodedInput, PCMFloatOutput, !Decoder)
:- pred decode_float(bitmap.bitmap, bitmap.bitmap, decoder, decoder).
:- mode decode_float(in, uo, di, uo) is det.

%==============================================================================%
:- implementation.
%==============================================================================%

:- import_module int.
:- use_module string.

:- type error --->
    ok ;
    bad_arg ;
    buffer_too_small ;
    internal_error ;
    invalid_packet ;
    unimplemented ;
    invalid_state ;
    alloc_fail.

:- pragma foreign_import_module("C", io).
:- pragma foreign_decl("C", "#include <opus/opus.h>").
:- pragma foreign_decl("C", "#include ""mercury_bitmap.h"" ").
:- pragma foreign_decl("C", "struct MOpus_Decoder{OpusDecoder*dec;int nchan;};").

:- pragma foreign_enum("C", error/0,
    [ok - "OPUS_OK",
    bad_arg - "OPUS_BAD_ARG",
    buffer_too_small - "OPUS_BUFFER_TOO_SMALL",
    internal_error - "OPUS_INTERNAL_ERROR",
    invalid_packet - "OPUS_INVALID_PACKET",
    unimplemented - "OPUS_UNIMPLEMENTED",
    invalid_state - "OPUS_INVALID_STATE",
    alloc_fail - "OPUS_ALLOC_FAIL"]).

:- pragma foreign_decl("C", "const char *MOpus_NameError(int);").

:- pragma foreign_type("C", decoder, "struct MOpus_Decoder *").

:- pragma foreign_decl("C", "void MOpus_DecoderFinalizer(void *ptr, void *data);").
:- pragma foreign_code("C",
    "
    void MOpus_DecoderFinalizer(void *ptr, void *data){
        struct MOpus_Decoder *decoder = (struct MOpus_Decoder*)ptr;
        opus_decoder_destroy(decoder->dec);
    }
    ").

:- pragma foreign_code("C",
    "
    const char *MOpus_NameError(int err){
#define MOPUS_ERROR_CASE(WHAT) case OPUS_ ## WHAT: return #WHAT
        switch(err){
            MOPUS_ERROR_CASE(OK);
            MOPUS_ERROR_CASE(BAD_ARG);
            MOPUS_ERROR_CASE(BUFFER_TOO_SMALL);
            MOPUS_ERROR_CASE(INTERNAL_ERROR);
            MOPUS_ERROR_CASE(INVALID_PACKET);
            MOPUS_ERROR_CASE(UNIMPLEMENTED);
            MOPUS_ERROR_CASE(INVALID_STATE);
            MOPUS_ERROR_CASE(ALLOC_FAIL);
            default: return ""<UNKNOWN>"";
        }
#undef MOPUS_ERROR_CASE
    }
    ").

:- pragma foreign_export("C", decode_16(in, uo, di, uo), "MOpus_Decode16").
:- pragma foreign_export("C", decode_float(in, uo, di, uo), "MOpus_DecodeFloat").

init(Stream, Len, MaybeDecoder, !IO) :-
    InitBitmap = bitmap.init(int.unchecked_left_shift(Len, 3)),
    io.read_bitmap(Stream, 0, Len, InitBitmap, Bitmap, BytesRead, Result, !IO),
    (
        Result = io.ok,
        ( if
            BytesRead = Len
        then 
            % Enough data for a head packet...
            ( if
                init(Bitmap, Decoder, C)
            then
                MaybeDecoder = maybe.ok({Decoder, C})
            else
                MaybeDecoder = maybe.error("Invalid stream")
            )
        else
            MaybeDecoder = maybe.error("Unexpected EOF")
        )
    ;
        Result = io.error(Err),
        MaybeDecoder = maybe.error(string.append("", io.error_message(Err)))
    ).

:- pragma foreign_proc("C", init2(SampleRate::in, Chans::in, Out::uo, NChan::uo),
    [will_not_throw_exception, promise_pure, thread_safe],
    "
        OpusDecoder *const decoder = MR_GC_malloc_atomic(opus_decoder_get_size(Chans));
        Out = MR_GC_malloc(sizeof(struct MOpus_Decoder));
        Out->dec = decoder;
        Out->nchan = NChan = Chans;
        SUCCESS_INDICATOR = opus_decoder_init(decoder, SampleRate, Chans) == OPUS_OK;
    ").

:- pragma foreign_proc("C", init(Buffer::in, Out::uo, NChan::uo),
    [will_not_throw_exception, promise_pure, thread_safe],
    "
        const char sig[] = ""OpusHead"";
        const MR_Integer buffer_size = Buffer->num_bits << 3;
        const unsigned char *const uchar_data = (unsigned char *)Buffer->elements;
        if(buffer_size < 19){
#ifndef NDEBUG
            fputs(""[MOpus] Error: Buffer too small. Size is "", stderr);
            fprintf(stderr, ""%i\\n"", (int)buffer_size);
#endif
            SUCCESS_INDICATOR = 0;
        }
        else if(memcmp(uchar_data, sig, sizeof(sig)-1) != 0){
#ifndef NDEBUG
            unsigned i;
            fputs(""[MOpus] Error: Invalid signature in buffer of size "", stderr);
            fprintf(stderr, ""%i\\n"", buffer_size);
            fputs(""[MOpus] Error: Wanted "", stderr);
            for(i = 0; i < sizeof(sig) - 2; i++){
                fprintf(stderr, ""0x%X, "", sig[i]);
            }
            fprintf(stderr, ""0x%X\\n"", sig[7]);
            fputs(""[MOpus] Error: Got    "", stderr);
            for(i = 0; i < sizeof(sig) - 2; i++){
                fprintf(stderr, ""0x%X, "", uchar_data[i]);
            }
            fprintf(stderr, ""0x%X\\n"", uchar_data[7]);
            for(i = 0; i < 19; i++){
                fprintf(stderr, ""0x%X, "", uchar_data[i]);
            }
#endif
            SUCCESS_INDICATOR = 0;
        }
        else{
            const unsigned nchan = uchar_data[9];
            const unsigned ver = uchar_data[8];
            if(ver != 1 || nchan == 0 || nchan > 2){
#ifndef NDEBUG
                fputs(""[MOpus] Error: Invalid number of channels:"", stderr);
                fprintf(stderr, ""%i\\n"", nchan);
#endif
                SUCCESS_INDICATOR = 0;
            }
            else{ /* Very evil. Stash the decoder at the end of the struct. */
                const unsigned size = sizeof(struct MOpus_Decoder) + opus_decoder_get_size(nchan);
                Out = MR_GC_malloc_atomic(size);
                Out->dec = (OpusDecoder*)(Out+1);
                Out->nchan = nchan;

                {
                    const int err = opus_decoder_init(Out->dec, 48000, nchan);
                    if(err != OPUS_OK){
                        SUCCESS_INDICATOR = 0;
#ifndef NDEBUG
                        fputs(""[MOpus] Error: libopus error: "", stderr);
                        fputs(MOpus_NameError(err), stderr); fputc('\\n', stderr);
#endif
                        MR_GC_free(Out);
                        Out = NULL;
                    }
                    else{
                        SUCCESS_INDICATOR = 1;
                        NChan = nchan;
                        MR_GC_register_finalizer(Out, MOpus_DecoderFinalizer, NULL);
                    }
                }
            }
        }
    ").

:- pragma foreign_proc("C", decode_16(In::in, Out::uo, Decoder0::di, Decoder1::uo),
    [will_not_throw_exception, promise_pure, thread_safe],
    "
        const unsigned nchan = Decoder0->nchan;
        const unsigned max_samples =  5760 * nchan,
            max_bytes = max_samples << 1;
        Out = MR_GC_malloc_atomic(sizeof(MR_BitmapType)+max_bytes);
        Out->num_bits = 0;

        OpusDecoder *const dec = Decoder0->dec;
        
        /* If this is not a header page, then decode it. */
        if(!(In->num_bits >= (8<<3) && memcmp(In->elements, ""OpusTags"", 8) == 0)){
            const int num = opus_decode(dec,
                (const void*)(In->elements), In->num_bits >> 3,
                (void*)(Out->elements), max_samples, 0);
            if(num > 0)
                Out->num_bits = (num * (nchan << 1)) << 3;
        }
        Decoder1 = Decoder0;
    ").

:- pragma foreign_proc("C", decode_float(In::in, Out::uo, Decoder0::di, Decoder1::uo),
    [will_not_throw_exception, promise_pure, thread_safe],
    "

        const unsigned nchan = Decoder0->nchan;
        const unsigned max_samples =  5760 * nchan,
            max_bytes = max_samples << 1;
        Out = MR_GC_malloc_atomic(sizeof(MR_BitmapType)+max_bytes);
        Out->num_bits = 0;

        OpusDecoder *const dec = Decoder0->dec;
        
        /* If this is not a header page, then decode it. */
        if(!(In->num_bits >= (8<<3) && memcmp(In->elements, ""OpusTags"", 8) == 0)){
            const int num = opus_decode_float(dec,
                (const void*)(In->elements), In->num_bits >> 3,
                (float*)(Out->elements), max_samples, 0);
            if(num > 0)
                Out->num_bits = (num * (nchan << 1)) << 3;
        }
        Decoder1 = Decoder0;
    ").
