## Keiretsu

[![Build Status](https://secure.travis-ci.org/brendanhay/keiretsu.png)](http://travis-ci.org/brendanhay/keiretsu)

## Table of Contents

* [Introduction](#introduction)
* [Configuration](#configuration)
    - [Intfile](#intfile)
    - [Procfile](#procfile)
    - [.env](#.env)
* [Contributing](#contributing)
* [Licence](#licence)


## Introduction

Keiretsu is an orchestration manager primarily designed for local development
and integration testing.

It allows you to specify dependencies that should be running/available before
the start of the local application, triggers setup/teardown hooks,
and applies a shared and consistent environment to all child processes ensuring
consistent and discoverable configuration.


## Installation

GHC `7.6.2` or later is required to compile Keiretsu.

```shell
cabal install
```


## Configuration

There are 3 configuration file formats which Keiretsu will read, controlling
various aspects such as dependencies, processes, and the environment.

### Intfile

The `Intfile` specifies project dependencies with a `key: value` line based format.

An example `Intfile` which specifies two dependencies Keiretsu should inspect as follows:

```ruby
users: ../users_service
images: ../images_service
```

This will make Keirestu recurse into those sub-directories and continue looking
for the various configuration file types.

### Procfile

Keiretsu uses an identical format to [foreman's](https://github.com/ddollar/foreman) `Procfile`
to describe processes to run.

An example `Procfile` specifying both web and redis proctypes:

```ruby
redis: redis-server --port $PORT
web: ./dist/build/web/web -p $PORT
```

This will make Keiretsu will start two processes for this dependency.

> Proctypes are simply unique names used to identify the related command within
> a dependencies' scope.

> Currently multiline proctypes are not supported.

### .env

> TODO


## Contributing

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/keiretsu/issues).


## Licence

keiretsu is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
