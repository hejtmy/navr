context("Visualising")


test_that("graphs don't throw errors",{
  expect_silent(plot_path(obj))
  #test add direction arrow
  #test add limits
  #test adding background
  #testa adding points
  #test adding path
  expect_silent(plot_position_heatmap(obj))

})

test_that("tests speed plotting", {
  expect_failure(obj) #speed column missing
  expect_silent(plot_speed(obj_prep)) #speed present
  expect_silent(plot_speed(obj_prep)) #speed present

})
