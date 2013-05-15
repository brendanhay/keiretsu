SHELL := /usr/bin/env bash

.PHONY: clean install lint ghci

all: build lint

clean:
	-rm -f .configured
	cabal-dev clean

build: .configured
	cabal-dev build

install:
	cabal-dev install -j --disable-documentation --disable-library-coverage

lint:
	hlint src

ghci:
	cabal-dev ghci

.configured:
	cabal-dev configure
	touch .configured
