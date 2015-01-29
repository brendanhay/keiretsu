SHELL := /usr/bin/env bash

.PHONY: build clean lint

all: install build

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: cabal.sandbox.config
	cabal install -j \
	 --disable-documentation \
	 --only-dependencies

clean:
	-rm -rf dist cabal.sandbox.config .cabal-sandbox
	cabal clean

lint:
	hlint src

cabal.sandbox.config:
	cabal sandbox init
