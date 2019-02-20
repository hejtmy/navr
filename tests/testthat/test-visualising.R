context("Visualising")
obj <- navr_object

test_that("graphs don't throw errors",{
  expect_silent(plot_path(obj))
})
