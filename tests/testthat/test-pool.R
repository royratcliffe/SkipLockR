test_that("default works", {
  expect_type(postgres.default.db.pool(user = "postgres"), "environment")
})
