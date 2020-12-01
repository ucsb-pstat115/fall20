library(testthat)

test_metadata = "
cases:
- hidden: false
  name: q2d1
  points: 1
- hidden: false
  name: q2d1
  points: 1
name: q2d

"

test_that("q2d1", {
  expect_true(rmse_partial_pooling < rmse_complete_pooling)
})

test_that("q2d1", {
  expect_true(rmse_partial_pooling < rmse_no_pooling)
})
