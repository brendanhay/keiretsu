SHELL := /usr/bin/env bash
FLAGS := -j --disable-documentation --disable-library-coverage

.PHONY: test lint

all: build

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: $(DEPS) cabal.sandbox.config
	cabal install $(FLAGS)

clean:
	-rm -rf dist cabal.sandbox.config .cabal-sandbox
	cabal clean

lint:
	hlint src

cabal.sandbox.config:
	cabal sandbox init && cabal configure
