context("Hello")

test_that("hello", {
  expect_output(hello(), "Hello, world!")
})
