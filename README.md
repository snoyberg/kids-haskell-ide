# Kids Haskell IDE

This is a minimalistic environment to allow my kids to play around
with Haskell, without dealing with any of the pesky details of files,
terminals, or editors. It provides an interface with a code editor on
the left, and output of running the program on the right. It requires
that the Stack executable be on the path at runtime.

Basic usage: inside this directory, run `stack run`. After building a
bunch of stuff, you should get a web page open.

Caveats:

* The Javascript code is pretty weak. In particular, I don't think it
  handles the server shutting down or putting the computer to sleep
  particularly well.
* No mobile browser support.
* This allows a client to run arbitrary code on your machine! Given
  the intended usage, and assuming basic sane firewall settings on
  your system, that should be fine. But massive caveat emptor!

For now, this is not any kind of major project, I'm just putting
together the bare minimum for my kids' needs. There are many better
tools out there for most purposes, some that I've been told of:

* [Code World](https://code.world/)
* [haskell.do](http://haskell.do/)
* [jupyter-haskell](https://github.com/gibiansky/jupyter-haskell)

Consider this project if:

1. You're interested in reviewing a tiny Haskell web app codebase
2. You're looking for something less featureful than the above, and
   don't mind fixing breakages

## Future improvements

I have no timeline on implementing any of this, nor concrete plans to
do it.

* Remove the dependency on Stack at runtime, see [Stack issue
  #4261](https://github.com/commercialhaskell/stack/issues/4261)
* Improve the Javascript situation for errors occurring
* Lock down security of the server more, allowing only localhost
  connections
