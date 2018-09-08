:- module audio_player.

:- interface.

:- use_module io.

:- pred main(io.io, io.io).
:- mode main(di, uo) is det.

:- implementation.

:- use_module audio_loader.
:- use_module cinnamon.

main(!IO) :-
    cinnamon.open_driver(DriverResult, !IO),
    (
        DriverResult = io.error(Error),
        io.error_message(Error, Msg),
        io.write_string("Could not create driver: ", !IO), io.write_string(Msg, !IO), io.nl(!IO)
    ;
        DriverResult = io.ok(Driver),
        % Hardcoded for now...
        audio_loader.load("spiders.opus", Driver, SoundResult, !IO),
        (
            SoundResult = io.error(Error),
            io.error_message(Error, Msg),
            io.write_string("Could open sound: ", !IO), io.write_string(Msg, !IO), io.nl(!IO)
        ;
            SoundResult = io.ok(Sound),
            cinnamon.play(Sound, !IO),
            io.write_string("Playing...\nPress enter to quit.\n", !IO),
            io.read_char(_, !IO)
        )
    ).
