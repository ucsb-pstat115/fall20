library(testthat)

test_metadata = "
cases:
- hidden: false
  name: q1c1
  points: 0
- hidden: false
  name: q1c2
  points: 0
name: q1c

"

test_that("q1c1", {
  expect_length(posterior_means, 50)
})

test_that("q1c2", {
  expect_true(all(posterior_means < 20))
  expect_true(all(posterior_means > 5))
})
