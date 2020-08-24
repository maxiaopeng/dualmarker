

library(testthat)
library(dualmarker)
context("dm_4qudrant_response")


test_that("four quadrant response", {
  # cut method 'median
  res <-
    dm_4quadrant_response(
      data = iris,
      response = "Species",
      response_pos = "setosa",
      response_neg = NULL,
      marker1 = "Sepal.Length",
      marker2 = "Sepal.Width",
      num_cut_method = "median",
      na_rm = TRUE
    )
})
