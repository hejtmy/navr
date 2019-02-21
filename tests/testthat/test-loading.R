context("Loading and validating")
test_that("Data can be loaded", {
  navr_log <- navr_object$data
  obj <- NavrObject()
  expect_s3_class(obj, "navr")
  #navr log should be leaded in testthat.R
  expect_equal(nrow(navr_log), 34169)
  expect_silent(load_position_data(NavrObject(), navr_log))
  expect_silent(load_position_data(obj, navr_log))
  navr_log_bad <- navr_log
  navr_log_bad <- rename_column(navr_log_bad, "timestamp", "Time")
  expect_warning(load_position_data(obj, navr_log_bad))
})

test_that("Data are validated properly", {
  navr_log <- navr_object$data
  expect_true(is_navr_data(navr_log))
  navr_log_bad <- rename_column(navr_log, "timestamp", "Time")
  expect_false(is_navr_data(navr_log_bad))
  navr_log_bad <- rename_column(navr_log, "position_x", "Position.x")
  expect_false(is_navr_data(navr_log_bad))
})

test_that("Dataframes can be automatically prepared", {
  navr_log_bad <- navr_object$data
  navr_log_bad <- rename_column(navr_log_bad, "timestamp", "Time")
  navr_log_bad <- rename_column(navr_log_bad, "rotation_x", "Rotation.x")
  navr_log_bad <- rename_column(navr_log_bad, "position_x", "Position.x")
  navr_log_test <- prepare_column_names(navr_log_bad)
  expect_identical(navr_log_test, navr_object$data)
})
