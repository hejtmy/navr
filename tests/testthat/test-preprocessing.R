context('Preprocessing data')
obj <- navr_object
test_that("Testing distance calculations", {
  obj_dist <- add_distances(obj)
  expect_equal(round(sum(obj_dist$data$distance)), 14865) #needs stupid rounding
  expect_equal(sum(obj_dist$data$distance),
               obj_dist$data$distance_total[nrow(obj_dist$data)])
})

test_that("Testing speed calculations", {
  obj_prep <- add_distances(obj)
  obj_prep <- add_speeds(obj_prep)
  expect_type(obj_prep$data$speed, "double")
})

test_that("Testing angle calculations", {
  obj_prep <- add_angle_differences(obj)
  expect_type(obj_prep$data$rotation_x_diff, "double")
  expect_type(obj_prep$data$rotation_y_diff, "double")
})
