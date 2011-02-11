Habaz
=====

Building
--------

1. Set up Haskell Platform or ghc+cabal 
2. install [wxHaskell](http://haskell.org/haskellwiki/WxHaskell/Building).
4. run `cabal build`

Testing
-------

*Automated tests*: 

1. `cabal install hstest`
2. `cabal install HUnit`
3. `runghc Setup.hs test`

TODO: proper unit testing
TODO: why `cabal test` doesn't do anything?

*Interactive testing*: load a module in `ghci` and run functions from the console

Other Docs
----------

* [FIBS CLIP](http://www.fibs.com/fibs_interface.html)

