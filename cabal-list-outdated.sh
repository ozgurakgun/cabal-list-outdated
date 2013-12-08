#!/bin/bash

cabal list --installed > cabal-list-installed.txt
cabal sandbox hc-pkg list > ghc-pkg-list.txt

cabal-list-outdated cabal-list-installed.txt ghc-pkg-list.txt

rm cabal-list-installed.txt ghc-pkg-list.txt
