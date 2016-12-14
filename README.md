Rudimentary
===========

For Mac only right now probably.

A practice buddy for ear training and piano. 

To compile, assuming you have Haskell platform installed:

    $ cabal sandbox init
    $ cabal install --only-dependencies
    $ cabal configure
    $ cabal build

Then start up your MIDI synth.  I've included `SimpleSynth.app` to make it easy
to get going, but you can use anything you like.

    $ open SimpleSynth.app

and run

    $ ./rudimentary

and navigate to `localhost:3000` in a web browser (only tested on Chrome).  You
should see a nice web interface.  Set the MIDI device on your synth and at the
top of the page to the same MIDI bus, and you're off.
