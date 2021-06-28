db <- new.env()

#' Default PostgreSQL database pool
#'
#' Lazily creates a default pool for PostgreSQL connections. Call this first at
#' some point in your application's initialisation procedure. Raises an error if
#' a new default pool cannot connect to the database.
#'
#' @param ... Extra arguments for \code{pool::\link[pool]{dbPool}}
#' @return Database pool, an environment
#' @export
postgres.default.db.pool <- function(...) {
  if (is.null(db$pool)) {
    assign("pool", pool::dbPool(RPostgres::Postgres(), ...), envir = db)
  }
  db$pool
}

#' Call function with pooled PostgreSQL connection
#' @param func Function called with one database connection argument
#' @export
withTransaction <- function(func) pool::poolWithTransaction(db$pool, func)

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
wait.for.notify <- function(...) {
  notify <- NULL
  withTransaction(function(conn)
    notify <<- RPostgres::postgresWaitForNotify(conn, ...))
  notify
}
