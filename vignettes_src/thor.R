## ---
## title: "thor"
## author: "Rich FitzJohn"
## date: "`r Sys.Date()`"
## output: rmarkdown::html_vignette
## vignette: >
##   %\VignetteIndexEntry{thor}
##   %\VignetteEngine{knitr::rmarkdown}
##   %\VignetteEncoding{UTF-8}
## ---

##+ echo = FALSE
set.seed(1)

## ## Introduction

## `thor` provides an wrapper around LMBD; the lightning memory-mapped
## database.  This is an embedded key-value store; there is no server
## (like SQLite) - the database exists purely on disk and uses file
## locking to manage concurrent access between processes.

## Key-value stores are simple systems for persistently storing values
## against keys.  In the case of `thor`, both the keys and the data
## can be strings or (raw) data.  This provides a low-level building
## block on which other applications can be built.  The complications
## come from trying to efficiently query the store, or patterns like
## "add a new value but only if the previous value was `foo`".

## This package does not provide a faithful 1:1 mapping of the
## underlying LMDB C API because that requires too much care at the R
## level not to crash R!  Instead, probably at the cost of some
## performance, `thor` provides a set of wrappers that try to prevent
## crashes by invalidating objects in the correct order.  The approach
## taken in is very similar to the [python interface to LMDB;
## `py-lmdb`](https://github.com/dw/py-lmdb).

## Because the whole point of interacting with a database is side
## effects, `thor` uses [R6](https://cran.r-project.org/package=R6)
## for the interface.  This has the unfortunate effect of complicating
## the documentation somewhat because R's documentation is focussed
## heavily on _functions_ and the package provides only one function
## (`thor::mdb_env`) with everything else happening through *methods*
## of this object, and the objects that it creates.

## `thor` tries to expose the underlying LMDB interface in a nested
## set of objects of increasing power (and complexity).  The objects
## that the package provides are

## * `mdb_env`: the environment object, which is the interface to the
##   database file.  Everything starts here!
##
## * `mdb_dbi`: a database handle.  Multiple databases may be stored
##   within a single environment and if more than one is used then
##   this object is passed about to control which database things
##   affect.
##
## * `mdb_txn`: a transaction object.  LMDB is a *transactional*
##   database and this object is used to carry out actions within a
##   transaction (such as getting and putting data).
##
## * `mdb_cursor`: a cursor.  To go beyond basic `get`/`put`, cursors*
##   *are required.  These can be used to iterate through the ##
##   *database, and to find entries.
##
## * `mdb_proxy`: a proxy for a result.  This is used to defer copying
##   data from the database into R for as long as possible.  It's a
##   bit of an experiment so we'll see how useful it turns out to be.
##
## All of these objects have their own help pages, even though only
## `mdb_env` has an actual function.  On those help pages every public
## function described (this is the same set that is printed when
## displaying the objects).  There are other functions that can be
## reached using `$` - functions beginning with a `.` should be
## considered **private**; using these can crash R.  Other functions
## (such as `format`) exist because of the way thor uses R6.

## For basic operations, one can just use the `mdb_env` object and
## ignore the rest of the package.  To do more interesting things,
## you'll need transactions (`mdb_txn`), and then perhaps you'll need
## cursors (`mdb_cursor`).  The proxy objects are available if you use
## transactions.

## ## The environment

## The first step is to create an "environment"; this holds one or
## more "databases" (though in the most simple case you can forget
## that detail and just treat the environment as a database).
env <- thor::mdb_env(tempfile())

## The first argument to `thor::mdb_env` is the filename - this
## is a directory where the database files will be kept.  Here I am
## using a temporary file for the database.

## As an R6 object, the database environment has a number of methods
## that can be used to perform actions on the database.  The print
## method groups these by theme:
env

## The last group `Helpers` are wrappers that let you ignore the
## transactional nature of LMDB if you just want to do really simple
## things.

## The database is currently empty:
env$list()

## But we can add some data to it:
for (i in 1:10) {
  env$put(ids::adjective_animal(),
          ids::random_id())
}

## Now there are 10 _keys_ in the database, each holding a value:
keys <- env$list()
keys

## LMDB stores keys in sorted order (not necessarily R's sorted order
## - you can see how LMDB sorts things with the `cmp` method of a
## transaction - see `?mdb_txn`), so `list` will return things in that
## order.

## Each _key_ has a _value_ (in this case just a hex string)
env$get(keys[[1]])

## Delete a key with
env$del(keys[[1]])

## and now there are only 9 keys
length(env$list())

## Test for existence of a key with `exists`
env$exists(keys[[1]])
env$exists(keys[[2]])

## The `mget` method will get multiple keys at once, `mset` will set
## multiple key/value pairs at once and `mdel` will delete multiple
## keys at once.

env$mdel(keys)

## For anything more complicated than this you would want to use
## transactions (see below).

## The `Informational` methods all return information about the state
## of the LMDB environment;

## The path that the data is stored in
env$path()

## which will contain two files - the actual data and a lock file (see
## lmdb's documentation for more on these).
dir(env$path())

## Flags that the environment was opened with (this corresponds to the
## arguments to the `thor::mdb_env` function)
env$flags()

## A couple of different forms of (somewhat cryptic) information about
## the state of the environment
env$info()
env$stat()

## (Note `entries` in `env$stat()` is the number of keys in the
## database)

## ## Transactions

## LMDB is _transactional_; everything that happens to the database,
## read or write, happens as a transaction.  For a write transaction
## either the whole transaction happens or none of it happens.  For
## both read and write transactions, the "view" of the database is
## consistent from the beginning to the end of a transaction.  So if
## you have a read transaction and while it is doing things a write
## transaction writes to the database, the read transaction does not
## "see" these changes.  You can only have one write transaction at
## once, but as many read transactions as you'd like.
txn <- env$begin(write = TRUE)

## As for `mdb_env`, the transaction object prints methods grouped by
## theme
txn

## ### Simple operations (put, get, del, etc)

## To insert data into the database, use the `put` method
txn$put("key", "value")

## ...to get it back out again, use the `get` method
txn$get("key")

## ...to delete it, use the `del` method, which returns `TRUE` if
## the object was deleted and `FALSE` if not
txn$del("key")
txn$del("key")

## To test if an key exists or not, use the `exists` method (which
## uses a cursor internally - see below)
txn$exists("key")

## The helper functions `mget`, `mput` and `mdel` functions do `get` /
## `put` and `del` to multiple keys at once, more efficiently than
## looping in R:
values <- ids::sentence(length(keys), style = "sentence")
txn$mput(keys, values)

## To list keys, use `list`
txn$list()

## And to fetch multiple values (`as_raw` is explained below)
txn$mget(keys[1:3], as_raw = FALSE)

## Or delete multiple values
txn$mdel(keys[1:3])

## `exists` is itself always vectorised
txn$exists(keys)

## Because the database is transactional, we can now either use
## `txn$commit()` to save the changes or `txn$abort()` to discard the
## changes.

## ### Multiple transactions at once

## As well as being able to roll back a transaction, the other
## function they serve is that each transaction gets a consistent view
## of the database.  At this point we have one write transaction
## running, but it's not committed yet.  So if we start another
## transaction, it will not see any of the uncommitted "changes" that
## our transaction has made:
txn_new <- env$begin()
txn_new$list()

## (or equivalently, `env$list()`).  Because of the design of
## LMDB, you cannot have multiple active write transactions at once
##+ error = TRUE
env$put("key", "value")

##+ error = TRUE
env$begin(write = TRUE)

## (if a write transaction is made by another process against the same
## LMDB database, then it will wait for our transaction to complete
## before its write transaction will start - this will cause R to be
## unresponsive during this time)

## Let's commit the changes made:
txn$commit()

## After being committed a transaction cannot be reused:
##+ error = TRUE
txn$list()

## New transactions can now see the changes
env$list()

## But importantly *old ones can't*
txn_new$list()

## This is because the old transaction has a consistent view of the
## database - from the point that it starts to the point that it ends,
## a read-only transaction will see the same data and a read-write
## transaction will only see changes that it has made.

## (cleaning things up a little)
txn_new$abort()
env$mdel(keys)

## ### Non-string data

## thor (and LMDB) can handle two types of data; strings (as above)
## and raw vectors.  Raw vectors can be used to serialise R objects
## using `serialize`, which allows storing of arbitrary data.  This is
## the approach taken by
## [`redux`](https://cran.r-project.org/package=redux) among other
## packages.

## All strings can be represented in raw vectors but the reverse is
## not true; character strings may not contain the null byte and the
## resulting string may not make sense.  thor uses the presence of a
## null byte as a heuristic when it needs to test if a value is raw or
## not.

## So the string "hello" can be converted to raw:
charToRaw("hello")

## But the set of bytes `2a 00 ff` cannot be:
##+ error = TRUE
rawToChar(as.raw(c(42, 0, 255)))

## This poses some problems for specifying and predicting return
## types, which will be explored below. thor tries hard to set the
## return type predictably; a few boolean arguments to the function
## determine the type rather than the contents of the data.
txn <- env$begin(write = TRUE)

## First, this is why one might want to store raw data in a database.
## Suppose we want to store the contents of `mtcars` as a value.  It's
## not a string so we can't do
##+ error = TRUE
txn$put("mtcars", mtcars)

## First we should _serialise_ it to raw:
mtcars_ser <- serialize(mtcars, NULL)

## which creates a fairly long string of bytes
str(mtcars_ser)

## converting back from this to an R object is easy with `unserialize`
identical(unserialize(mtcars_ser), mtcars)

txn$put("mtcars", mtcars_ser)
txn$list()

## When fetching the data, thor will work out that this is raw data
## and return a raw vector:
class(txn$get("mtcars"))

## So we can now store and retrieve arbitrary R objects into the
## database.
identical(unserialize(txn$get("mtcars")), mtcars)
txn$del("mtcars")

## Automatic type detection is a mixed blessing (like pitfalls with
## `sapply`) and thor provides mechanisms for taming it.

## Here are two values as raw vectors - one that can be converted to a
## string and one that can't
bytes <- as.raw(c(42, 0, 255))
string <- charToRaw("hello!")

txn$put("bytes", bytes)
txn$put("string", string)

## The value of the return type is determined both by the value of the
## object and by the value of the argument `as_raw`.

## | stored  | `as_raw`  | result     |
## |---------|-----------|------------|
## | string  | `NULL`    | character  |
## | string  | `FALSE`   | character  |
## | string  | `TRUE`    | raw        |
## | bytes   | `NULL`    | character  |
## | bytes   | `FALSE`   | error      |
## | bytes   | `TRUE`    | raw        |

## for example
txn$get("string")

## is character because `as_raw` is `NULL` and the value _can_ be
## represented as a string, while
txn$get("bytes")

## is raw because the value cannot be represented as a string.
## Specifying `as_raw = TRUE` will _always_ return raw because
## everything can be represented as raw.  And specifying `as_raw =
## FALSE` will throw an error for a value that cannot be converted
## into a string.

## For `mget`, it's a bit trickier because we need to check _every_
## value as they come out to see if it's a string or a character.  The
## rules here are:

## | stored  | `as_raw`  | container | contents    |
## |---------|-----------|-----------|-------------|
## | string  | `NULL`    | list      | character   |
## | string  | `FALSE`   | character | (character) |
## | string  | `TRUE`    | list      | raw         |
## | bytes   | `NULL`    | list      | raw         |
## | bytes   | `FALSE`   | error     | (error)     |
## | bytes   | `TRUE`    | list      | raw         |
## | mixed   | `NULL`    | list      | mixed       |
## | mixed   | `FALSE`   | error     | (error)     |
## | mixed   | `NULL`    | list      | raw         |

## That is, if `as_raw = FALSE` we return a character or error if this
## is not possible, otherwise (`as_raw = TRUE`, `as_raw = NULL`) we
## always return a list.  This should make programming with because
## the value of `as_raw` entirely predicts the container type.  Within
## the container, the rule for contents is the same as for `get()`.

## So, the default (`as_raw = NULL`) returns a list with auto-detected
## types for each element:
txn$mget(c("string", "bytes"))

## Or we could get both as raw
txn$mget(c("string", "bytes"), as_raw = TRUE)

## But because one of the values is binary, we can't do this:
##+ error = TRUE
txn$mget(c("string", "bytes"), as_raw = FALSE)

## But if we only pull strings it's ok:
txn$mget(c("string", "string"), as_raw = FALSE)

txn$abort()

## ## Caveats

## LMDB will allow multiple process to access the database at the same
## time, but enforce only one write transaction.  **However** to make
## that work relies on file locking.  The LMDB documentation covers
## issues around more detail - all the issues there apply to `thor`,
## though some of them are ensured by the thor's design (and because R
## is single threaded some do not really affect us).
##
## * crashed processes may leave stale lockfiles that may need to be
##   removed by `reader_check()`
##
## * do not use LMDB database on remote systems, even between
##   processes on the same host, as file locking and memory map sync
##   may be unreliable.  This may be disappointing, but if you have
##   multiple hosts you really do need a server based solution, not a
##   file based one.
##
## * avoid long-lived transactions, as they can cause the database
##   size to grow quickly.
