Habaz
=====

Building
--------

1. Set up [Haskell Platform](http://hackage.haskell.org/platform/) (works with 2011.2.0.1, don't know about other versions) or ghc+cabal
2. install [wxHaskell](http://haskell.org/haskellwiki/WxHaskell/Building).
3. run `cabal configure` followed by `cabal build`

Testing
-------

_Automated tests_: 

1. `cabal install test-framework`
2. `cabal install TODO`
3. `cabal configure --enable-tests`
4. `cabal build`
5. `cabal test` -- this is currently broken on Windows, see [ticket #843](http://hackage.haskell.org/trac/hackage/ticket/843)

_Interactive testing_: 

Load a module in `ghci` and run functions from the console. 

Other Docs
----------

* [FIBS client protocol](http://www.fibs.com/fibs_interface.html)

