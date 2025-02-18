test_that("testing smoothing", {
  obj <- navr_object
  expect_silent(obj_smoothed <- smooth_positions(obj, type = "median", points = 5))
  expect_silent(obj_smoothed <- smooth_positions(obj, type = "spline"))
  expect_true(!all(navr_object$data$speed == obj_smoothed$data$speed))

  expect_silent(obj_smoothed <- smooth_positions(obj, type = "median", points = 5, recalculate_stats = FALSE))
})