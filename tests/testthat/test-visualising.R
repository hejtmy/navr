context("Visualising")
DIR <- system.file("extdata", package = "navr")

test_that("graphs don't throw errors", {
  expect_silent(plot_path(navr_object))
  #test add direction arrow
  #test add limits
  #test adding background
  #testa adding points
  #test adding path
  expect_silent(plot_position_heatmap(navr_object))

})

test_that("tests speed plotting", {
  expect_error(plot_speed(navr_object)) #speed column missing
  expect_silent(plot_speed(navr_object_preprocessed)) #speed present
})


test_that("tests plot elements", {
  plt <- create_minimal_plot()
})


test_that("tests custom geoms", {
  plt <- create_minimal_plot()
  expect_silent(plt + geom_navr_backround(paste0(DIR, "/megamap5.png")))
  expect_silent(plt + geom_navr_circle(c(0,1), 2))
  expect_silent(plt + geom_navr_points(list(start = c(0,0))))
  expect_silent(plt + geom_navr_direction(c(0,0), 180, 5, color = "red"))
  expect_silent(plt + geom_navr_heatmap(navr_object, 10))
  expect_silent(plt + geom_navr_limits(navr_object))
})
