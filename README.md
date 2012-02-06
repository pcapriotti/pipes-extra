Experiments with [pipes][1].

 * `Control.Pipe.Binary`: Functions for dealing with bytes. Ported from the
   equivalent module in [Conduit][2].
 * `Control.Pipe.Conduit`: Adapters to convert conduits, sink and sources to pipes.
 * `Control.Pipe.Zlib`: Pipes to deal with zipped data. Ported from conduit.
 * `Control.Pipe.Network`: Utilities to deal with sockets. Ported from conduit.
 * `Control.Pipe.Combinators`: General-purpose pipes and combinators.

Note that `Control.Pipe.Binary` uses `ResourceT` from the [Conduit][2] package to
guarantee finalization of handles.

 [1]: https://github.com/Gabriel439/Haskell-Pipes-Library
 [2]: http://hackage.haskell.org/package/conduit
