## Keiretsu

[![Build Status](https://secure.travis-ci.org/brendanhay/keiretsu.png)](http://travis-ci.org/brendanhay/keiretsu)

## Table of Contents

* [Introduction](#introduction)
* [Functionality](#functionality)
    - [Development](#development)
    - [Integration Testing](#integration-testing)
* [Compatibility](#compatibility)
* [Installation](#installation)
* [Configuration](#configuration)
    - [Service Under Test](#service-under-test)
    - [Dependencies](#dependencies)
* [Running](#running)
* [Contributing](#contributing)
* [Licence](#licence)


## Introduction

Keiretsu is an orchestration manager primarily designed for local development
and integration testing.

It allows you to specify dependencies that should be running/available before
the start of the local application, triggers setup/teardown hooks,
and applies a shared and consistent environment to all child processes ensuring
consistent and discoverable configuration.


## Functionality

### Development

> TODO

### Integration Testing

> TODO


## Compatibility

> TODO


## Installation

A GHC with base libraries `>= 4.5` are required.

```shell
cabal install
```


## Configuration

### Service Under Test

#### Intfile

> TODO

### Dependencies

#### Procfile

> TODO

#### Makefile

> TODO


## Running

> TODO


## Contributing

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/keiretsu/issues).


## Licence

keiretsu is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
