context("Visualising")
DIR <- system.file("extdata", package = "navr")

obj <- navr_object
obj_prep <- navr_object_preprocessed

test_that("graphs don't throw errors", {
  expect_silent(plot_path(obj))
  #test add direction arrow
  #test add limits
  #test adding background
  #testa adding points
  #test adding path
  expect_silent(plot_position_heatmap(obj))

})

test_that("tests speed plotting", {
  expect_error(plot_speed(obj)) #speed column missing
  expect_silent(plot_speed(obj_prep)) #speed present
})

test_that("tests plot elements", {
  plt <- create_minimal_plot()
})

test_that("tests custom geoms", {
  plt <- create_minimal_plot()
  expect_silent(plt + geom_navr_background(paste0(DIR, "/megamap5.png")))
  expect_silent(plt + geom_navr_circle(c(0,1), 2))
  expect_silent(plt + geom_navr_points(list(start = c(0,0))))
  expect_silent(plt + geom_navr_direction(c(0,0), 180, 5, color = "red"))
  expect_silent(plt + geom_navr_heatmap(obj, 10))
  expect_silent(plt + geom_navr_limits(obj))
  expect_silent(plt + geom_navr_obj_timeseries(obj_prep, "position_x"))
  expect_silent(plt + geom_navr_path_limits(obj, 500))
  times <- obj_prep$data$timestamp[c(1000, 5500, 18000)]
  expect_silent(plt + geom_navr_path_events(obj_prep, times))
})


test_that("tests navr path points", {
  plt <- create_minimal_plot()
  event_times <- c(0,10)
  expect_silent(plt + geom_navr_path_points(obj_prep, times = event_times))
  expect_silent(plt + geom_navr_path_events(obj_prep, event_times = event_times))
  expect_equal(geom_navr_path_events(obj_prep, event_times, size = 2, color = "blue"),
               geom_navr_path_points(obj_prep, event_times, size = 2, color = "blue"))
})

test_that("tests navr path segments", {
  plt <- create_minimal_plot()
  # need matrix
  event_times <- c(0,10)
  out <- c()
  expect_warning(out <- geom_navr_path_segments(obj_prep, times = event_times))
  expect_null(out)
  # need matrix with two rows
  event_times <- matrix(data = c(0,5,10,5,10,5), nrow = 3)
  expect_warning(out <- geom_navr_path_segments(obj_prep, times = event_times))
  expect_null(out)

  event_times <- matrix(data = c(0,5,10,5), nrow = 2)
  expect_silent(plt + geom_navr_path_segments(obj_prep, times = event_times))
  # should return the same as the geomn_navr_path_events
  expect_silent(plt + geom_navr_path_events(obj_prep, event_times = event_times))
  expect_equal(geom_navr_path_events(obj_prep, event_times, size = 2, color = "blue"),
               geom_navr_path_segments(obj_prep, event_times, size = 2, color = "blue"))
})

test_that("tests timeseries", {
  expect_silent(plt <- create_minimal_plot() + geom_navr_obj_timeseries(obj_prep, "position_x"))
  times <- obj_prep$data$timestamp[c(1000, 5500, 18000)]
  expect_silent(plt + geom_navr_timeseries_events(times))
  expect_silent(plt + geom_navr_timeseries_events(times, durations = rep(5, length(times))))
  expect_warning(plt + geom_navr_timeseries_events(times, durations = rep(5, length(times)-1)))
})
