context("Getting and subsetting")
test_that("Data can filtered with time", {
  obj <- navr_object
  obj_filtered <- filter_times(obj, c("58227", "58242"))
  expect_equal(nrow(obj_filtered$data), 412)

  obj_prepped <- navr::add_times_since_start(obj)
  obj_filtered <- filter_times(obj_prepped, c(0, Inf), zero_based = T)
  expect_equal(nrow(obj_filtered$data), nrow(obj$data))
})
