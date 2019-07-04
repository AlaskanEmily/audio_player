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

:- module cinnamon.

%==============================================================================%
% Bindings for Cinnamon.
% These were derived from OpenAL bindings. There may be a few anachronisms.
:- interface.
%==============================================================================%

:- use_module io.
:- use_module bitmap.

:- type driver.
:- type loader.
:- type sound.

:- type sample_type --->
    signed8 ;
    signed16 ;
    signed32 ;
    float ;
    double ;
    ulaw.

:- type speaker_config --->
    mono ;
    stereo ;
    quad.

:- type format ---> format(sample_type, speaker_config).

:- pred open_driver(io.res(driver), io.io, io.io).
:- mode open_driver(uo, di, uo) is det.

:- pred create_loader(format, int, driver, io.res(loader)).
:- mode create_loader(in, in, in, uo) is det.
:- mode create_loader(di, in, in, uo) is det.

:- pred create_loader(sample_type, speaker_config, int, driver, io.res(loader)).
:- mode create_loader(in, in, in, in, uo) is det.

:- pred put_data(loader::di, loader::uo, bitmap.bitmap::bitmap.bitmap_ui) is det.

:- pred finalize(loader::di, sound::uo) is det.

:- pred play(sound::in, io.io::di, io.io::uo) is det.
:- pred stop(sound::in, io.io::di, io.io::uo) is det.

%==============================================================================%
:- implementation.
%==============================================================================%

:- pragma foreign_decl("C",
    "
    #include ""cin_format.h""
    #include ""cin_sound.h""
    #include ""cin_driver.h""
    #include ""cin_loader.h""
    ").

:- pragma foreign_import_module("C", bitmap).

:- pragma foreign_type("C", driver, "struct Cin_Driver*").
:- pragma foreign_type("C", loader, "struct Cin_Loader*").
:- pragma foreign_type("C", sound, "struct Cin_Sound*").

:- pragma foreign_enum("C", sample_type/0, [
    signed8 - "Cin_eFormatS8",
    signed16 - "Cin_eFormatS16",
    signed32 - "Cin_eFormatS32",
    float - "Cin_eFormatFloat32",
    double - "Cin_eFormatFloat64",
    ulaw - "Cin_eFormatULaw8"]).

:- pragma foreign_enum("C", speaker_config/0, [
    mono - "1",
    stereo - "2",
    quad - "4"]).

:- pragma foreign_decl("C", "void MCin_DriverFinalizer(void *drv, void *arg);").
:- pragma foreign_code("C", "void MCin_DriverFinalizer(void *drv, void *arg){
        (void)arg;
        Cin_DestroyDriver(drv);
    }").

:- pragma foreign_decl("C", "void MCin_LoaderFinalizer(void *ld, void *arg);").
:- pragma foreign_code("C", "void MCin_LoaderFinalizer(void *ld, void *arg){
        (void)arg;
        struct Cin_Sound *const snd = malloc(Cin_StructSoundSize());
        Cin_LoaderFinalize(ld, snd);
        Cin_DestroySound(snd);
        free(snd);
    }").

:- pragma foreign_decl("C", "void MCin_SoundFinalizer(void *snd, void *arg);").
:- pragma foreign_code("C", "void MCin_SoundFinalizer(void *snd, void *arg){
        (void)arg;
        Cin_DestroySound(snd);
    }").

% Wrappers for use inside the foreign procs.
:- func create_driver_error(string) = io.res(driver).
create_driver_error(Err) = io.error(io.make_io_error(Err)).
:- pragma foreign_export("C", create_driver_error(in) = (out), "MCin_CreateDriverError").

:- func create_driver_ok(driver) = io.res(driver).
create_driver_ok(Drv) = io.ok(Drv).
:- pragma foreign_export("C", create_driver_ok(in) = (out), "MCin_CreateDriverOK").

:- func create_loader_error(string) = io.res(loader).
create_loader_error(Err) = io.error(io.make_io_error(Err)).
:- pragma foreign_export("C", create_loader_error(in) = (out), "MCin_CreateLoaderError").

:- func create_loader_ok(loader) = io.res(loader).
create_loader_ok(Ld) = io.ok(Ld).
:- pragma foreign_export("C", create_loader_ok(in) = (out), "MCin_CreateLoaderOK").

:- pragma foreign_proc("C", open_driver(Out::uo, IO0::di, IO1::uo),
    [will_not_throw_exception, promise_pure, thread_safe, tabled_for_io],
    "
        IO1 = IO0;
        struct Cin_Driver *const driver =
            MR_GC_malloc(Cin_StructDriverSize());
        const enum Cin_DriverError err = Cin_CreateDriver(driver);
        if(err != Cin_eDriverSuccess){
            MR_GC_free(driver);
            Out = MCin_CreateDriverError(""Cinnamon: Error creating driver"");
        }
        else{
            Out = MCin_CreateDriverOK(driver);
        }
    ").

create_loader(format(Type, Speakers), Rate, Driver, Out) :-
    create_loader(Type, Speakers, Rate, Driver, Out).

:- pragma foreign_proc("C",
    create_loader(Format::in, Speakers::in, Rate::in, Drv::in, Out::uo),
    [will_not_throw_exception, promise_pure, thread_safe],
    "
        struct Cin_Loader *const loader =
            MR_GC_malloc_atomic(Cin_StructLoaderSize());
        const enum Cin_LoaderError err =
            Cin_CreateLoader(loader, Drv, Rate, Speakers, Format);
        switch(err){
            case Cin_eLoaderSuccess:
                /* MR_GC_register_finalizer(loader, MCin_LoaderFinalizer, loader); */
                Out = MCin_CreateLoaderOK(loader);
                break;
            case Cin_eLoaderUnsupportedFormat:
                Out = MCin_CreateLoaderError(""Cinnamon: Unsupported format"");
                MR_GC_free(loader);
                break;
            case Cin_eLoaderInvalidFormat:
                Out = MCin_CreateLoaderError(""Cinnamon: Invalid format"");
                MR_GC_free(loader);
                break;
            case Cin_eLoaderUnsupportedNumChannels:
                Out = MCin_CreateLoaderError(""Cinnamon: Unsupported number of channels"");
                MR_GC_free(loader);
                break;
            case Cin_eLoaderUnsupportedSampleRate:
                Out = MCin_CreateLoaderError(""Cinnamon: Unsupported sample rate"");
                MR_GC_free(loader);
                break;
            case Cin_eLoaderFailure:
            default:
                Out = MCin_CreateLoaderError(""Cinnamon: Error creating loader"");
                MR_GC_free(loader);
                break;
        }
    ").

:- pragma foreign_proc("C", put_data(In::di, Out::uo, BMP::bitmap.bitmap_ui),
    [does_not_affect_liveness, will_not_call_mercury,
    will_not_throw_exception, promise_pure, thread_safe, tabled_for_io],
    "
        Out = In;
        Cin_LoaderPut((Out = In), BMP->elements, BMP->num_bits >> 3);
    ").

:- pragma foreign_proc("C", finalize(Loader::di, Sound::uo),
    [does_not_affect_liveness, will_not_throw_exception,
    promise_pure, thread_safe, tabled_for_io],
    "
        Sound = MR_GC_malloc_atomic(Cin_StructSoundSize());
        Cin_LoaderFinalize(Loader, Sound);
        /* MR_GC_register_finalizer(Sound, MCin_SoundFinalizer, Sound); */
    ").

:- pragma foreign_proc("C", play(Sound::in, IO0::di, IO1::uo),
    [will_not_call_mercury, does_not_affect_liveness, will_not_throw_exception,
    promise_pure, thread_safe, tabled_for_io],
    "  IO1 = IO0; Cin_SoundPlay(Sound); ").

:- pragma foreign_proc("C", stop(Sound::in, IO0::di, IO1::uo),
    [will_not_call_mercury, does_not_affect_liveness, will_not_throw_exception,
    promise_pure, thread_safe, tabled_for_io],
    "  IO1 = IO0; Cin_SoundStop(Sound); ").
