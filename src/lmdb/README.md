This the library code from lmdb - see https://github.com/LMDB/lmdb

Currently at [4d21543](https://github.com/LMDB/lmdb/tree/4d21543)

From the lmdb distribution we use the files

* `lmdb.h`
* `mdb.c`
* `midl.c`
* `midl.h`

verbatim.  The licence for these files is installed with the package - see [`inst/LICENSE.lmdb`](../../inst/LICENSE.lmdb)

To update the version, edit the [`version`](version) file in this directory, and then (from the `thor` root) run

```
./scripts/update_lmdb.R
```
