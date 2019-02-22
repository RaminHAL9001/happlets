# Happlets: the Haskell Applets API

This package is provides the Happlets API that is common to all
Happlet back-end provider libraries. Along with this package, it is
necessary to install a back-end library such as
'happlets-lib-gtk'. Currently Gtk+ version 2 with a Cairo-based canvas
is the only back-end available. Gtk+ version 3, Xlib, SDL2, GLUT, and
(possibly) Wayland back-ends are all feasible and may be implemented
in the future.

The goal of the Happlets project is to allow you to create very
simple, thread-safe applications that contain nothing more than a
single window with a drawing canvas that can respond to user input
events, like mouse clicks, key-presses, or frame animation events. The
intention is to create a minimal programming platform for small,
single-purpose graphical applications which simply displays some
interactive graphic, for example a plot of some data, or a simple
game. Naturally, the Happlet program can be arbitrarily complex, but
it may be better to consider other, FRP-based solutions if managing
events becomes too difficult.

A happlet back-end provider may provide additional widget API
functions, but using these APIs will result in your applet being
dependent on the specific back-end and will thus not be as portable as
programming for the "happlets" package alone. Happlets aims to achieve
a balance between being as minimal as possible, but still provide
enough graphics drawing primitives to be useful for the widest range
of possible applications.

The Gtk+ v2 back-end provider can be downloaded from this link:

https://hackage.haskell.org/package/happlets-lib-gtk

**NOTE:** Happlets uses OS bound threads ('Control.Concurrent.forkOS')
to provide multithreading capabilities, when building an executable
application, be sure to use the `-threaded` option in GHC-OPTIONS when
building executables.
