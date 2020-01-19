context("Tests for utility functions")

test_that("require_columns", {

  df <- tibble::tribble(
    ~val, ~n, ~colA,
    4,    4,   "b",
  )
  expect_silent(require_columns(df,
                                c("val", "colA")))
  expect_silent(require_columns(df,
                                c()))
  expect_error(require_columns(df,
                               c("colA", "colC", "n", "whatCol")),
               "Missing required columns: colC, whatCol")
})
