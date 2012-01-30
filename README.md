Experiments with [pipes][1].

 * `Control.Pipe.Guarded`: A drop-in replacement for Pipes, providing an extra
   primitive `tryAwait` which returns `Nothing` when the upstream pipe
   terminates.
 * `Control.Pipe.Binary`: Functions for dealing with bytes. Ported from the
   equivalent module in [Conduit][2].
 * `Control.Pipe.Conduit`: Adapters to convert conduits, sink and sources to pipes.
 * `Control.Pipe.Zlib`: Pipes to deal with zipped data.

Note that `Control.Pipe.Binary` uses `ResourceT` from the [Conduit][2] package to
guarantee finalization of handles.

 [1]: https://github.com/Gabriel439/Haskell-Pipes-Library
 [2]: http://hackage.haskell.org/package/conduit
