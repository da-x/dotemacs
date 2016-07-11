all:
	make -C modes/company-mode
	make -C modes/haskell-mode
	make -C modes/helm
	make -C modes/magit
	make -C modes/js2-mode
	make -C modes/flycheck
	cd pac && cabal build && cd -
