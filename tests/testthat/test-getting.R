context("Getting and subsetting")
obj <- navr_object
test_that("Data can filtered with time", {
  obj_filtered <- filter_times(obj, c(58227, 58242))
  expect_equal(nrow(obj_filtered$data), 412)

  obj_prepped <- navr::add_times_since_start(obj)
  expect_silent(obj_filtered <- filter_times(obj_prepped, c(0, Inf), zero_based = T))
  expect_equal(nrow(obj_filtered$data), nrow(obj$data))

  expect_silent(obj_filtered <- filter_times(obj_prepped, matrix(c(0,100,200,300), ncol=2, byrow = TRUE), zero_based = TRUE))
  expect_equal(nrow(obj_filtered$data), 5091)
})


test_that("Can select time_diff", {
  expect_error(get_time_diffs(obj))
  obj_prepped <- add_time_columns(obj)
  expect_type(obj_prepped$data$time_diff, "double")
})
