#' Run migrations
#'
#' Creates the migration table if it does not already exist. Assumes that the
#' schema already exists; fails otherwise.
#'
#' Intersects the migration table with the migration directory. Runs the missing
#' migration scripts in lexicographic order. Implementation relies on \link{DBI}
#' package and its functional interface.
#'
#' @param dir Migration directory
#' @param name Migration table
#' @export
migrateSchema <- function(dir, name = DBI::Id(schema = "schema", table = "migrations")) {
  files <- sort(tools::list_files_with_exts(dir, "R"))
  names(files) <- tools::file_path_sans_ext(basename(files))
  repeat {
    migrations <- withTransaction(function(conn) {
      if (!DBI::dbExistsTable(conn, name))
        DBI::dbCreateTable(conn, name, fields = c(id = DBI::dbDataType(conn, "")))
      DBI::dbReadTable(conn, name)$id
    })
    diff <- files[setdiff(names(files), migrations)]
    if (length(diff) == 0) break
    withTransaction(function(conn) {
      sys.source(diff[1], envir = list2env(list(conn = conn)))
      DBI::dbAppendTable(conn, name, data.frame(id = names(diff)[1]))
    })
  }
}
