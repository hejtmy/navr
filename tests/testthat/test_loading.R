context("Loading and validating")
test_that("Data can be loaded", {
  navr_log <- navr_object$data
  #navr log should be leaded in testthat.R
  expect_equal(nrow(navr_log), 34169)
})

test_that("Data are validated properly", {
})
