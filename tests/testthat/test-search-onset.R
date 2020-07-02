context("Onset searching")
obj <- navr_object_preprocessed
obj <- remove_unreal_speeds(obj, type = "std", cutoff = 3, total_recalculate = T)
obj <- smooth_speed(obj, type = "median", points = 11)

test_that("testing search onset", {
  expect_silent(ls <- search_onsets(obj, speed_threshold = 5, min_duration = 2))
  expect_equal(c("time", "time_since_start", "duration"), names(ls))
  expect_equal(length(ls$time), length(ls$time_since_start), length(ls$duration))
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

test_that("testing search onset with pauses", {
  expect_silent(res <- search_onsets(obj, speed_threshold = 5, min_duration = 2, pause_duration = 1))
  expect_silent(res2 <- search_onsets(obj, speed_threshold = 5, min_duration = 2, pause_duration = 0))
  expect_gt(length(res$time_since_start), length(res2$time_since_start))
  res <- search_onsets(obj, speed_threshold = 5, min_duration = 2)
  expect_equal(res, res2)
})

test_that("testing search stops", {
  expect_silent(ls <- search_stops(obj, speed_threshold = 5, min_duration = 2))
  expect_gt(length(ls$time_since_start), 0)
  expect_silent(ls <- search_stops(obj, speed_threshold = 0, min_duration = 0))
  expect_length(ls$time_since_start, 0)
  expect_silent(ls <- search_stops(obj, speed_threshold = 1, min_duration = 1000))
  expect_length(ls$time_since_start, 0)
})

test_that("testing searching for deliberation stops", {
  expect_silent(ls <- search_deliberation_stops(obj, speed_threshold = 5, min_duration = 2, min_rotation = 0))
  expect_silent(ls_stop <- search_stops(obj, speed_threshold = 5, min_duration = 2))
  expect_equal(sum(ls_stop$time_since_start - ls$time_since_start), 0)
  expect_warning(ls <- search_deliberation_stops(obj, speed_threshold = 5, min_duration = 2, min_rotation = 10)) #waring because of deprecated add_angle_difference
  expect_gt(length(ls_stop$time_since_start), length(ls$time_since_start))
})


