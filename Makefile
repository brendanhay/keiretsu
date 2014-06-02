SHELL := /usr/bin/env bash

.PHONY: build clean lint

all: deps build

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

deps: cabal.sandbox.config
	cabal install -j \
	 --disable-documentation \
	 --disable-library-coverage \
	 --only-dependencies

clean:
	-rm -rf dist cabal.sandbox.config .cabal-sandbox
	cabal clean

lint:
	hlint src

cabal.sandbox.config:
	cabal sandbox init
