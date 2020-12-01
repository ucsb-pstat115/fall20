library(testthat)

test_metadata = "
cases:
- hidden: false
  name: q2a
  points: 1
name: q2a

"

test_that("q2a", {
  expect_true(empirical_sd > true_sd)
})
