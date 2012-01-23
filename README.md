Experiments with [pipes][1].

 * `Control.Pipe.Guarded`: A drop-in replacement for Pipes providing an extra
   primitive `tryAwait` which returns `Nothing` when the upstream pipe
   terminates to allow finalization of resources.
 * `Control.Pipe.Binary`: Function for dealing with bytes. Ported from the
   equivalent module in Conduit.

Note that pipes-extra has a dependency on the conduit package because the file
producers and consumers use `ResourceT` to guarantee finalization of handles.

 [1]: https://github.com/Gabriel439/Haskell-Pipes-Library
