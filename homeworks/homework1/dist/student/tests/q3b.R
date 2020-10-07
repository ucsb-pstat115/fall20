library(testthat)

test_metadata = "
cases:
- hidden: false
  name: q3b1
  points: 2
name: q3b

"

test_that("q3b1", {
  expect_length(lambda_chapter, 37)
})
