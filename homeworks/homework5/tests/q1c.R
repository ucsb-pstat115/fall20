library(testthat)

test_metadata = "
cases:
- hidden: false
  name: q1c
  points: 1
name: q1c

"

test_that("q1c", {
  expect_true(alpha_ess_new > alpha_ess)
})
