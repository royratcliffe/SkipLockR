db <- new.env()
on.exit(if (!is.null(db$pool)) pool::poolClose(db$pool))

#' Default PostgreSQL database pool
#'
#' Lazily creates a default pool for PostgreSQL connections. Call this first at
#' some point in your application's initialisation procedure. Raises an error if
#' a new default pool cannot connect to the database.
#'
#' @param ... Extra arguments for \code{pool::\link[pool]{dbPool}}
#' @return Database pool, an environment
#' @export
postgresDefaultDBPool <- function(...) {
  if (is.null(db$pool)) assign("pool", pool::dbPool(RPostgres::Postgres(), ...), envir = db)
  db$pool
}

#' Call function with pooled PostgreSQL connection
#' @param func Function called with one database connection argument
#' @export
withTransaction <- function(func) pool::poolWithTransaction(db$pool, func)

#' Call function with pooled connection
#' @param what Called with one database connection argument
#' @export
withConnection <- function(what) {
  conn <- pool::poolCheckout(db$pool)
  on.exit(pool::poolReturn(conn))
  do.call(what, list(conn))
}

#' Wait for any PostgreSQL notification
#'
#' Uses the default pool. The default pool uses the default PostgreSQL
#' connection.
#'
#' @param ... Extra arguments for PostgreSQL notification wait,
#'   \code{RPostgres::\link[RPostgres]{postgresWaitForNotify}}.
#'   Available parameters include:
#'   * timeout Seconds to wait, one by default.
#' @export
waitForNotify <- function(...) {
  conn <- pool::poolCheckout(db$pool)
  on.exit(pool::poolReturn(conn))
  RPostgres::postgresWaitForNotify(conn, ...)
}
