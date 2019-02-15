library(testthat)
data("navr_log")
obj <- navr::navr_object()
obj$data <- navr_log

test_check("navr")
