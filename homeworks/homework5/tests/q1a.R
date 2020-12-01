library(testthat)

test_metadata = "
cases:
- hidden: false
  name: q1a1
  points: 1
- hidden: false
  name: q1a2
  points: 1
name: q1a

"

test_that("q1a1", {
  expect_equal(log_posterior(c(-1, 1)), -11.49, tol=1e-2)
})

test_that("q1a2", {
  expect_equal(log_posterior(c(0, 0)), -13.86, tol=1e-2)
})
