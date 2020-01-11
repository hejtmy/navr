context("Area addition")

obj <- navr_object
obj_prep <- navr_object_preprocessed

rectangle_area <- AreaObject("test square", "rectangle", matrix(c(0, 0, 1, 0, 1, 1, 0, 1), ncol=2, byrow=T))

test_that("creating column works", {
  expect_silent(res <- add_areas(obj_prep, list(rectangle_area)))
  expect_true(has_areas(res))
  wrong_area <- AreaObject("wrong", "non existent type", matrix(c(0, 0, 1, 0, 1, 1, 0, 1), ncol=2, byrow=T))
  expect_warning(res <- add_areas(obj_prep, list(wrong_area)))
})
