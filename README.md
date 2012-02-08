Collection of basic utilities for [pipes][1].

 * **pipes-extra**: Basic combinators, file readers and writers, and conduit adapters.
 * **pipes-network**: Utilities to deal with sockets. Ported from conduit.
 * **pipes-zlib**: Pipes to deal with zipped data. Ported from conduit.

Note that `Control.Pipe.Binary` uses `ResourceT` from the [Conduit][2] package
to guarantee finalization of handles.

 [1]: https://github.com/Gabriel439/Haskell-Pipes-Library
 [2]: http://hackage.haskell.org/package/conduit
