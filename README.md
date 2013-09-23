# Setup

As long as Welshy is not on Hackage:

	cabal sandbox init
	cabal sandbox add-source ../welshy
	cabal install --only-dependencies

(This is going to take a while...)

If you need a REPL, use the following:

    ghci -no-user-package-db -package-db .cabal-sandbox/*.d

(`Ctrl-C` doesn't work properly in `cabal repl`.)