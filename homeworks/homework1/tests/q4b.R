library(testthat)

test_metadata = "
cases:
- hidden: false
  name: q4b1
  points: 2
name: q4b

"

test_that("q4b1", {
  expect_length(counts, 1000)
})
