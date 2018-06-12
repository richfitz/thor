##' Storr driver for thor.  This allows thor to be used as a storage
##' backend with the storr package and presents a higher level content
##' addressable key/value store suitable for storing R objects.
##' @title Thor driver for storr
##' @param env A thor environment
##' @param prefix An optional prefix.  If given, use a \code{:} as the
##'   last character for nice looking keys (e.g., \code{storr:} will
##'   generate keys like \code{storr:keys:namespace:name}.  If not
##'   given then we assume that storr is the only user of this
##'   database and if \code{destroy} is called it will delete the
##'   entire database.
##' @param hash_algorithm Optional hash algorithm to use.  Defaults to
##'   md5, or whatever the existing algorithm is if the database has
##'   been opened.  You cannot mix algorithms.
##' @param default_namespace The default namespace to store objects
##'   in.  Defaults to \code{objects}, as does other storr drivers.
##' @export
storr_thor <- function(env, prefix = "", hash_algorithm = NULL,
                       default_namespace = "objects") {
  storr::storr(driver_thor(env, prefix, hash_algorithm),
               default_namespace)
}


##' @export
##' @rdname storr_thor
##' @importFrom storr join_key_namespace
driver_thor <- function(env, prefix = "", hash_algorithm = NULL) {
  R6_driver_thor$new(env, prefix, hash_algorithm)
}


R6_driver_thor <- R6::R6Class(
  "driver_thor",

  cloneable = FALSE,

  public = list(
    env = NULL,
    prefix = NULL,
    traits = list(accept = "raw", throw_missing = TRUE),
    hash_algorithm = NULL,

    initialize = function(env, prefix, hash_algorithm) {
      self$env <- env
      self$prefix <- prefix
      self$hash_algorithm <- driver_thor_config(
        env, prefix, "hash_algorithm", hash_algorithm, "md5", TRUE)
    },

    type = function() {
      "thor"
    },

    destroy = function() {
      ## This is actually a bit nasty; do we delete the whole thing?
      ## Perhaps if the prefix is null.
      if (nzchar(self$prefix)) {
        keys <- self$env$list(self$prefix)
        self$env$mdel(keys)
      } else {
        ## delete all keys
        self$env$destroy()
      }
      invisible()
    },

    get_hash = function(key, namespace) {
      self$env$get(self$name_key(key, namespace),
                   as_raw = FALSE, missing_is_error = TRUE)
    },

    mget_hash = function(key, namespace) {
      kn <- join_key_namespace(key, namespace)
      if (kn$n == 0L) {
        return(character(0))
      }
      res <- self$env$mget(self$name_key(kn$key, kn$namespace),
                           as_raw = FALSE)
      res[!nzchar(res)] <- NA_character_
      res
    },

    set_hash = function(key, namespace, hash) {
      self$env$put(self$name_key(key, namespace), hash)
    },

    mset_hash = function(key, namespace, hash) {
      if (length(hash) == 0L) {
        return()
      }
      self$env$mput(self$name_key(key, namespace), hash)
    },

    get_object = function(hash) {
      res <- self$env$get(self$name_hash(hash),
                          as_raw = TRUE, missing_is_error = TRUE)
      unserialize(res)
    },

    mget_object = function(hash) {
      if (length(hash) == 0) {
        return(list())
      }
      res <- self$env$mget(self$name_hash(hash), as_raw = TRUE)
      i <- !vapply(res, is.null, logical(1))
      res[i] <- lapply(res[i], unserialize)
      res
    },

    set_object = function(hash, value) {
      self$env$put(self$name_hash(hash), value)
    },

    mset_object = function(hash, value) {
      ## TODO: probably storr should avoid passing in zero-length
      ## requests for iformation throughout (all four m*et functions).
      if (length(value) == 0L) {
        return()
      }
      self$env$mput(self$name_hash(hash), value)
    },

    exists_hash = function(key, namespace) {
      self$env$exists(self$name_key(key, namespace))
    },

    exists_object = function(hash) {
      self$env$exists(self$name_hash(hash))
    },

    del_hash = function(key, namespace) {
      ## TODO: should del_hash and exists_hash be doing the proper
      ## naming thing?
      kn <- join_key_namespace(key, namespace)
      self$env$mdel(self$name_key(kn$key, kn$namespace))
    },

    del_object = function(hash) {
      self$env$mdel(self$name_hash(hash))
    },

    list_hashes = function() {
      start <- sprintf("%sdata:%s", self$prefix, "")
      str_drop_start(self$env$list(start), start)
    },

    list_keys = function(namespace) {
      start <- self$name_key("", namespace)
      str_drop_start(self$env$list(start), start)
    },

    list_namespaces = function() {
      ## For this to work, consider disallowing ":" in namespace
      ## names, or sanitising them on the way in?
      start <- sprintf("%skeys:", self$prefix)
      keys <- str_drop_start(self$env$list(start), start)
      unique(sub(":.*", "", keys))
    },

    name_key = function(key, namespace) {
      sprintf("%skeys:%s:%s", self$prefix, namespace, key)
    },

    name_hash = function(hash) {
      sprintf("%sdata:%s", self$prefix, hash)
    }
  ))


driver_thor_config <- function(env, prefix, name, value, default,
                               must_agree) {
  path_opt <- sprintf("%sconfig:%s", prefix, name)

  load_value <- function() {
    if (env$exists(path_opt)) {
      value <- env$get(path_opt, as_raw = FALSE)
      storage.mode(value) <- storage.mode(default)
    } else {
      value <- default
    }
    value
  }

  if (is.null(value)) {
    value <- load_value()
  } else if (must_agree && env$exists(path_opt)) {
    value_prev <- load_value()
    if (value != value_prev) {
      ## NOTE: this duplicates some of the code in
      ## storr:::ConfigError, but once I work out some sort of general
      ## config system (if!) I could reuse something here.
      msg <- sprintf("Incompatible value for %s (existing: %s, requested: %s)",
                     name, value_prev, value)
      stop(msg)
    }
  }
  if (!env$exists(path_opt)) {
    env$put(path_opt, value)
  }

  value
}
