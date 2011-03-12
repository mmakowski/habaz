Habaz
=====

Building
--------

1. Set up [Haskell Platform](http://hackage.haskell.org/platform/) (works with 2011.2.0.0, don't know about other versions) or ghc+cabal
2. install [wxHaskell](http://haskell.org/haskellwiki/WxHaskell/Building).
4. run `cabal build`

Testing
-------

_Automated tests_: 

1. `cabal install hstest`
2. `cabal install HUnit`
3. `cabal install QuickCheck-1.2.0.0` -- hstest doesn't currently support QuickCheck 2
3. `runghc Setup.hs test`

Note that `Setup.hs` has a hard-coded version of QuickCheck 2 to hide. This might be updated if a different version of QuickCheck is installed.

TODO: why `cabal test` doesn't do anything?

_Interactive testing_: 

Load a module in `ghci` and run functions from the console. You might need to hide QuickCheck 2 package: `:set -hide-package QuickCheck-2.4.0.1` so that QuickCheck 1 is used.

Other Docs
----------

* [FIBS client protocol](http://www.fibs.com/fibs_interface.html)

