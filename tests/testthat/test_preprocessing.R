context('Preprocessing data')

test_that("Testing distance calculations", {
  obj <- navr_object
  obj_dist <- add_distances(obj)
  expect_equal(round(sum(obj_dist$data$distance)), 14865) #needs stupid rounding
  expect_equal(sum(obj_dist$data$distance), obj_dist$data$distance_total[nrow(obj_dist$data)])
})
