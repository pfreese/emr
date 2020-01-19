context("Hello")

test_that("hello", {
  actual <- hello()
  expect_equal(actual, "Hello, world!")
})
