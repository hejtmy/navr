context("Visualising")
obj <- navr_object

test_that("graphs don't throw errors",{
  expect_silent(plot_path(obj))
  #test add direction arrow
  #test add limits
  #test adding background
  #testa adding points
  #test adding path
})
