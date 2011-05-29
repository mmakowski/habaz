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
2. `cabal install test-framework-quickcheck2`
3. `cabal install test-framework-hunit`
4. `cabal configure --enable-tests`
5. `cabal build`
6. `cabal test` 

Note that `cabal test` is currently broken on Windows, see [ticket #843](http://hackage.haskell.org/trac/hackage/ticket/843). Despite that you will still be able to see the test results in `dist/test/Habaz-0.1.0-all-tests.log`.

_Interactive testing_: 

Load a module in `ghci` and run functions from the console. 

Other Docs
----------

* [FIBS client protocol](http://www.fibs.com/fibs_interface.html)

