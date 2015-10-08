all:
	make -C modes/haskell-mode
	make -C modes/helm
	make -C modes/magit
	cd modes/ghci-ng && stack init && stack build && rm stack.yaml && cd -
	make -C modes/flycheck
	cd pac && cabal build && cd -
