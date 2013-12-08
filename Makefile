.PHONY: install clean

install:
	cabal sandbox init
	cabal install --bindir=$(HOME)/.cabal/bin
	cp cabal-list-outdated.sh $(HOME)/.cabal/bin

clean:
	rm -rf .cabal-sandbox dist cabal cabal.sandbox.config
