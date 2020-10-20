library(testthat)

test_metadata = "
cases:
- hidden: false
  name: q1b1
  points: 0
- hidden: false
  name: q1b6
  points: 1
name: q1b

"

test_that("q1b1", {
  tol = 1e-4
  expect_equal(alpha_A, 120)
  expect_equal(beta_A, 10)
  expect_equal(alpha_B, 12)
  expect_equal(beta_B, 1)
})

test_that("q1b6", {
  expect_true(all(length(alpha_A_quantile) == 2, length(alpha_B_quantile) == 2))
})
