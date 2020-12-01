library(testthat)

test_metadata = "
cases:
- hidden: false
  name: q2b1
  points: 1
- hidden: false
  name: q2b1
  points: 1
name: q2b

"

test_that("q2b1", {
  expect_true(sqrt(mean((phat_mle - val)^2)) < sqrt(mean((phat_pooled - val)^2)))
})


test_that("q2b1", {
  expect_true(abs(sqrt(mean((phat_mle - val)^2)) - sqrt(mean((phat_pooled - val)^2))) < 0.05)
})
