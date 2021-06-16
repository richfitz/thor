# thor

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/)
[![Linux Build Status](https://travis-ci.org/richfitz/thor.svg?branch=master)](https://travis-ci.org/richfitz/thor)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/richfitz/thor?svg=true)](https://ci.appveyor.com/project/richfitz/thor)
[![codecov.io](https://codecov.io/github/richfitz/thor/coverage.svg?branch=master)](https://codecov.io/github/richfitz/thor?branch=master)
[![](http://www.r-pkg.org/badges/version/thor)](https://cran.r-project.org/package=thor)



An R interface to [LMDB](https://github.com/LMDB/lmdb).  LMDB is an embedded transactional key-value store and this package provides R mappings to it.  It wraps the entire LMDB interface, except for support for duplicated keys.

## Documentation

The package comes with a vignette that describes the main features of the package and of LMDB - see [here](https://richfitz.github.io/thor/articles/thor.html) for a version online.  The package also has reference documentation for all methods.  It may be useful to refer to the [LMDB documentation](http://lmdb.tech/doc) along side the reference documentation for some details (but hopefully not too much).

## Usage

Everything starts by creating an environment (which lives at a point in the file system), and then using methods of the environment object to interact with the database


```r
env <- thor::mdb_env(tempfile())
env
```

```
## <mdb_env>
##   Informational:
##     path()
##     flags()
##     info()
##     stat()
##     maxkeysize()
##     maxreaders()
##   Transactions:
##     begin(db = NULL, write = FALSE, sync = NULL, metasync =  ...
##     with_transaction(fun, db = NULL, write = FALSE)
##   Databases:
##     open_database(key = NULL, reversekey = FALSE, create = TRUE)
##     drop_database(db, delete = TRUE)
##   Management:
##     sync(force = FALSE)
##     copy(path, compact = FALSE)
##     close()
##     destroy()
##     reader_list()
##     reader_check()
##   Helpers:
##     get(key, missing_is_error = TRUE, as_raw = NULL, db = NULL)
##     put(key, value, overwrite = TRUE, append = FALSE, db = NULL)
##     del(key, db = NULL)
##     exists(key, db = NULL)
##     list(starts_with = NULL, as_raw = FALSE, size = NULL, db ...
##     mget(key, as_raw = NULL, db = NULL)
##     mput(key, value, overwrite = TRUE, append = FALSE, db =  ...
##     mdel(key, db = NULL)
```



```r
env$put("hello", "world")
```

```
## NULL
```

```r
env$exists("hello")
```

```
## [1] TRUE
```

```r
env$get("hello") # world
```

```
## [1] "world"
```

```r
env$del("hello")
```

```
## [1] TRUE
```

LMDB is _transactional_, and `thor` exposes this like so:


```r
txn <- env$begin(write = TRUE)
txn
```

```
## <mdb_txn>
##   Informational:
##     id()
##     stat()
##   Finish:
##     commit()
##     abort(cache = TRUE)
##   Cursors:
##     cursor()
##   Data:
##     get(key, missing_is_error = TRUE, as_proxy = FALSE, as_r ...
##     put(key, value, overwrite = TRUE, append = FALSE)
##     del(key)
##     exists(key)
##     list(starts_with = NULL, as_raw = FALSE, size = NULL)
##     mget(key, as_proxy = FALSE, as_raw = NULL)
##     mput(key, value, overwrite = TRUE, append = FALSE)
##     mdel(key)
##     replace(key, value, as_raw = NULL)
##     pop(key, as_raw = NULL)
##   Compare:
##     cmp(a, b)
```

Only one write transaction is active at a given point in time.  There can be an unlimited number of read transactions.

```
txn$put("key", "value")
env$get("key", missing_is_error = FALSE) # NULL - not committed yet
txn$commit()
env$get("key") # new transactions see the value
```

There is a cursor interface for advanced features (see the vignette).  Both keys and values can be strings or binary value, the latter working well with `serialize`.  For efficient use from R, `thor` extends the LMDB interface to implement bulk reads, writes and deletes (`mget`, `mput` and `mdel`).

## Performance

lmdb is an extremely fast database, but this package may be much less fast than the underlying library.  In order to make the interface safe to use from R, there is quite a bit of error checking, and the length of time involved in calling methods in R6 objects is orders of magnitude slower than performing an action on an lmdb database (this is not R6's fault and primarily caused by the cost of S3 method lookup for `$` on an object with a class attribute).  The vectorised functions will help here (e.g., `mget`, `mput`), so prefer these where practical if speed is a concern.

## Installation

Install from CRAN with

```r
install.packages("thor")
```

If you want to try the development version from github, you can install with

```r
devtools::install_github("richfitz/thor", upgrade = FALSE)
```

## License

MIT + file LICENSE © [Rich FitzJohn](https://github.com/richfitz).  The package contains included code from lmdb which it itself under the "OpenLDAP Public License" - see [`inst/LICENSE.lmdb`](inst/LICENSE.lmdb) for details
