# Keiretsu

## Table of Contents

* [Introduction](#introduction)
* [Configuration](#configuration)
    - [Intfile](#intfile)
    - [Procfile](#procfile)
    - [.env](#env)
* [Contributing](#contributing)
* [Licence](#licence)


## Introduction

Keiretsu is an orchestration manager primarily designed for local development
and integration testing.

It allows you to specify dependencies that should be running/available before
the start of the local application, triggers setup/teardown hooks,
and applies a consistent environment to all child processes ensuring deterministic
and repeatable configuration.


## Installation

GHC `7.6.2` or later is required to compile Keiretsu.

```shell
cabal install
```


## Configuration

There are 3 configuration file formats which Keiretsu will read, controlling
various aspects such as dependencies, processes, and the environment.

An example for a Haskell project named `proxy` with two dependencies, `users`
and `images` would look as follows:

```
+ images/
|   + dist/
|   |   + build/
|   |   |   + images/
|   |   |       + images
|   + src/
|   |   + Main.hs
|   + .env
|   + Intfile
|   + Procfile
+ proxy/
|   + dist/
|   |   + build/
|   |   |   + proxy/
|   |   |       + proxy
|   + src/
|   |   + Main.hs
|   + .env
|   + Intfile
|   + Procfile
+ users/
    + dist/
    |   + build/
    |   |   + users/
    |   |       + users
    + src/
    |   + Main.hs
    + .env
    + Intfile
    + Procfile
```


### Intfile

The `Intfile` specifies project dependencies with a `key: value` line based format.

An example `Intfile` for the `proxy` service (from above) which specifies two dependencies on
`users` and `images`:

```
users: ../users
images: ../images
```

This will make Keirestu recurse into those sub-directories and continue looking
for the various configuration file types.

### Procfile

Keiretsu uses an identical format to [foreman's](https://github.com/ddollar/foreman) `Procfile`
to describe processes.

An example `Procfile` for the `users` service (from above) specifying both
web and redis proctypes:

```
redis: redis-server --port $PORT
web: ./dist/build/web/web -p $PORT
```

This will make Keiretsu will start two processes for this dependency.

> Proctypes are simply unique names used to identify the related command within
> a dependencies' scope.
> Currently multiline proctypes are not supported.

### .env

`.env` files are used to supplement the process environment with configuration
values for a specific dependency.

Again, using the example project layout from above if the `images` service had
a `.env` file in the project directory with the following:

```
IMAGES_TMP: /var/tmp/images-service
```

The key/value pairs would be loaded into the environment and available to
all processes.


## Contributing

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/keiretsu/issues).


## Licence

keiretsu is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
