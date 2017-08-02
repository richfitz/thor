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

## ## The environment

## The first step is to create an "environment"; this holds one or
## more "databases" (though in the most simple case you can forget
## that detail and just treat the environment as a database).
env <- thor::mdb_env(tempfile())

## ## Transactions

## lmdb is _transactional_; everything that happens to the database,
## read or write, happens as a transaction.  For a write transaction
## either the whole transaction happens or none of it happens.  For
## both read and write transactions, the "view" of the database is
## consistent from the beginning to the end of a transaction.  So if
## you have a read transaction and while it is doing things a write
## transaction writes to the database, the read transaction does not
## "see" these changes.  You can only have one write transaction at
## once, but as many read transactions as you'd like.
txn <- env$begin(write = TRUE)

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

txn$put("key", "value")
txn$put("key2", "another value")
txn$put("foo", "bar")
txn$exists("key")

## To list all keys in the database, use the `list` method
txn$list()

## which takes an argument `starts_with`:
txn$list("k")
txn$list("f")
txn$list("x")

## (the `exists` and `list` methods are extensions to the lmdb api)

## Because the database is transactional, we can now either use
## `txn$commit()` to save the changes (writing `key`, `key2` and `foo`
## to the database) or `txn$abort()` to discard the changes.
txn$abort()

## ### Non-string data

## thor (and lmdb) can handle two types of data; strings (as above)
## and raw vectors.  Raw vectors can be used to serialise R objects
## using `serialize`, which allows storing of arbitrary data.  This is
## the approach taken by
## [`redux`](https://cran.r-project.org/package=redux) amongst other
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

txn$abort()

## ### Multi-value operations

## (this whole section is an extension to the lmdb api)

txn <- env$begin(write = TRUE)

## As an multi-value analog to `get`, `put` and `del`, thor implements
## `mget`, `mput` and `mdel` to add, fetch and delete multiple values
## at once.  The names are influenced by Redis.

keys <- letters[1:8]
values <- strrep(toupper(keys), 4)

## To put these 8 values into the database at once:
txn$mput(keys, values)

## All 8 values are now in the db:
txn$list()

## We can fetch all values at once:
txn$mget(keys)

## `mget` here always returns a list because `as_raw` is applied to
## each element in turn and any one could require storing as raw.  To
## force a string, vector use `as_raw = FALSE`:
txn$mget(keys, as_raw = FALSE)

## Similarly, to force a raw vector, use `as_raw = TRUE`:
txn$mget(keys, as_raw = TRUE)

## The `exists` method is also vectorised:
txn$exists(letters[1:10])
