library(testthat)

test_metadata = "
cases:
- hidden: false
  name: q3c1
  points: 5
name: q3c

"

test_that("q3c1", {
  expect_length(lambda_hats, 37)
})
