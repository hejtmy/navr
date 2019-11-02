test_that("angle to 180 works", {
  expect_equal(angle_to_180(359), -1)
  expect_equal(angle_to_180(180), -180)
  expect_equal(angle_to_180(179), 179)
  expect_equal(angle_to_180(90), 90)
  expect_equal(angle_to_180(0), 0)
  expect_equal(angle_to_180(360), 0)
  expect_equal(angle_to_180(-90), -90)
  expect_equal(angle_to_180(-270), 90)
})

test_that("angle diff works", {
  expect_warning(angle_diff(1:5, 330:335))
  expect_silent(res <- angle_diff(1:5, 330:334))
  expect_true(all(res == -31))
  expect_equal(angle_diff(270, 0), 90)
  expect_equal(angle_diff(0, 270), -90)
})
