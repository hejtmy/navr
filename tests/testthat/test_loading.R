context("Loading and validating")
test_that("Data can be loaded", {
  data("navr_log")
  expect_equal(nrow(navr_log), 34169)
})

test_that("Data are validated properly", {
})
