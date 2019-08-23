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

test_that("testing selection of unreal speeds", {
  expect_warning(pick_unreal_speeds(obj, 3, "std"))
  expect_null(pick_unreal_speeds(obj))
  #adding speeds now
  obj_prep <- add_distances(obj)
  obj_prep <- add_speeds(obj_prep)
  expect_silent(pick_unreal_speeds(obj_prep, 3, "std"))
  i_unreal <- pick_unreal_speeds(obj_prep, 3, "std")
  expect_type(i_unreal, "integer")
})

test_that("tests removal of unreal speeds", {
  expect_warning(remove_unreal_speeds(obj, indices=1:5))
  obj <- navr_object_preprocessed
  expect_error(remove_unreal_speeds(obj, type = "std"))
  expect_error(remove_unreal_speeds(obj, cutoff = 3))
  expect_silent(remove_unreal_speeds(obj, indices=1:5))
  expect_silent(obj_speed <- remove_unreal_speeds(obj, type = "std", cutoff = 3))
  expect_gt(obj$data$distance_total[nrow(obj$data)], obj_speed$data$distance_total[nrow(obj_speed$data)])

  #distance calcluations work as intended
  expect_silent(obj_speed <- remove_unreal_speeds(obj, type = "std", cutoff = 3, total_recalculate = F))
  expect_equal(obj$data$distance_total[nrow(obj$data)], obj_speed$data$distance_total[nrow(obj_speed$data)])

  # pick speed and remove speeds are the same
  obj_speed <- remove_unreal_speeds(obj, type = "std", cutoff = 3)
  i_unreal <- pick_unreal_speeds(obj, 3, "std")
  obj_speed_2 <- remove_unreal_speeds(obj, indices=i_unreal)
  expect_equal(obj_speed, obj_speed_2)
})

test_that("testing search onset", {

})

test_that("testing search stops", {

})
