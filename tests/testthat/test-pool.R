user <- "postgres"

# Could not initialise default postgres database. If postgres is running
# check that the environment variables PGHOST, PGPORT,
# PGUSER, PGPASSWORD, and PGDATABASE, are defined and
# point to your database.
skip_if_not(suppressMessages(RPostgres::postgresHasDefault(user = user)), "default postgres database required but is not found")

test_that("default works", {
  expect_type(postgres.default.db.pool(user = user), "environment")
})
