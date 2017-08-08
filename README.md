# thor

> thor

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Linux Build Status](https://travis-ci.org/richfitz/thor.svg?branch=master)](https://travis-ci.org/richfitz/thor)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/richfitz/thor?svg=true)](https://ci.appveyor.com/project/richfitz/thor)
[![codecov.io](https://codecov.io/github/richfitz/thor/coverage.svg?branch=master)](https://codecov.io/github/richfitz/thor?branch=master)

Interface to [lmdb](https://github.com/LMDB/lmdb).

See [here](http://symas.com/mdb/doc/) for docs

## Installation

```r
devtools::install_github("richfitz/thor", upgrade = FALSE)
```

## Usage

```r
env <- thor::mdb_env(tempfile())
env$put("hello", "world")
env$get("hello") # world
```

## Source

Currently at 0.9.21 (tag: LMDB_0.9.21) - see https://github.com/LMDB/lmdb and https://github.com/LMDB/lmdb/releases

From this distribution we use the files

* lmdb.h
* mdb.c
* midl.c
* midl.h

## License

MIT + file LICENSE © [Rich FitzJohn](https://github.com/richfitz).  The package contains included code from lmdb which it itself under the "OpenLDAP Public License" - see [`inst/LICENSE.lmdb`](inst/LICENSE.lmdb) for details
