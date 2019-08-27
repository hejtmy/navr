context("Onset searching")
obj <- navr_object_preprocessed
obj <- remove_unreal_speeds(obj, type = "std", cutoff = 3, total_recalculate = T)
obj <- smooth_speed(obj, type = "median", points = 11)

test_that("testing search onset", {
  expect_silent(ls <- search_onsets(obj, speed_threshold = 5, min_duration = 2))
  expect_gt(length(ls$time_since_start), 0)
  expect_silent(ls <- search_onsets(obj, speed_threshold = 5, min_duration = 2, still_duration = 2))
  expect_gt(length(ls$time_since_start), 0)
  expect_silent(ls2 <- search_onsets(obj, speed_threshold = 5, min_duration = 2, still_speed_threshold = 5, still_duration = 2))
  expect_equal(ls, ls2)
  expect_silent(ls <- search_onsets(obj, speed_threshold = 500, min_duration = 2, still_duration = 2))
  expect_length(ls$time_since_start, 0)
  expect_silent(ls <- search_onsets(obj, speed_threshold = 1, min_duration = 200))
  expect_length(ls$time_since_start, 0)
  expect_silent(ls <- search_onsets(obj, speed_threshold = 1, min_duration = 1, still_duration = 200))
  expect_length(ls$time_since_start, 0)
})

test_that("testing search stops", {
  expect_silent(ls <- search_stops(obj, speed_threshold = 5, min_duration = 2))
  expect_gt(length(ls$time_since_start), 0)
  expect_silent(ls <- search_stops(obj, speed_threshold = 0, min_duration = 0))
  expect_length(ls$time_since_start, 0)
  expect_silent(ls <- search_stops(obj, speed_threshold = 1, min_duration = 1000))
  expect_length(ls$time_since_start, 0)
})