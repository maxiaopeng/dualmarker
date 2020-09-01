library(dualmarker)
library(testthat)

test_that("dm_response_4quad_plot", {
  x <- c(5,3,12,9)
  n <- c(8, 10, 20, 18)
  g <- dm_response_4quad_chart(x = x, n = n)
  expect_is(g, "ggplot")
})
