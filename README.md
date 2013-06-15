# mouse-driver

This is a little library which uses Java's java.awt.Robot class to drive the mouse cursor.
This allows to draw pictures within other applications (such as MS Paint and the like).
The driver makes use of natural cubic splines in order to smoothen mouse movements.

## Usage

Start up a REPL (if you have leiningen installed just type: "lein repl").
Open up your favorite graphical editor and set it up so that you can draw lines with the left mouse button. Then
switch back to the REPL and type: "(demo)". Switch back to your editor and watch what happens ;-)

## License

Copyright © 2013 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
