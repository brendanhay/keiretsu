SHELL := /usr/bin/env bash

.PHONY: clean install lint dist

all: install lint

clean:
	cabal-dev clean

install:
	cabal-dev install --disable-documentation --disable-library-coverage

lint:
	hlint src

ghci:
	cabal-dev ghci
