all:
	make -C modes/haskell-mode
	make -C modes/helm
	make -C modes/ghci-ng
	make -C modes/flycheck
	cd pac && cabal build
