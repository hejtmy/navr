context("Getting and subsetting")
test_that("Data can filtered with time", {

  obj_filtered <- filter_times(obj, c("58227", "58242"))
  expect_equal(nrow(obj_filtered$data), 412)
  obj_filtered <- filter_times(obj, c("58227", "58242"))
  obj_filtered <- filter_times(obj, c(0, Inf), zero_based = T)
  expect_equal(nrow(obj_filtered$data), nrow(obj$data))
})
