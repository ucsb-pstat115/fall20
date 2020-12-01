library(testthat)

test_metadata = "
cases:
- hidden: false
  name: q1b
  points: 1
name: q1b

"

test_that("q1b", {
  expect_equal(ld50_posterior_mean, 1.2, tol=1e-1)
})
