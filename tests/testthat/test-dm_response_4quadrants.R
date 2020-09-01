

library(testthat)
library(dualmarker)
context("dm_4qudrant_response")


test_that("four quadrant response", {
  # cut method 'median
  res <-
    dm_response_4quad(
      data = iris,
      response = "Species",
      response.pos = "setosa",
      response.neg = NULL,
      marker1 = "Sepal.Length",
      marker2 = "Sepal.Width",
      m1.num.cut = "median",
      m2.num.cut = "median",
      na.rm = TRUE
    )
  expect_length(res,4)
  expect_equal(names(res), c("pos.n","total.n","stats.4quad","param"))

  # gep_CXCL13 + mut_ARID1A
  res <-
    dm_response_4quad(
      data = clin_bmk_IMvigor210,
      response = "binaryResponse",
      response.pos = "CR/PR",
      response.neg = "SD/PD",
      marker1 = "gep_CXCL13",
      marker2 = "mut_ARID1A",
      m2.cat.pos = "YES", m2.cat.neg = "NO",
      na.rm = TRUE
    )
  expect_length(res,4)
  expect_equal(names(res), c("pos.n","total.n","stats.4quad","param"))

  # TMB + gepscore_TGFb.19gene
  res <-
    dm_response_4quad(
      data = clin_bmk_IMvigor210,
      response = "binaryResponse",
      response.pos = "CR/PR",
      response.neg = "SD/PD",
      marker1 = "TMB",
      marker2 = "gepscore_TGFb.19gene",
      na.rm = TRUE
    )
  expect_length(res,4)
  expect_equal(names(res), c("pos.n","total.n","stats.4quad","param"))

  # TMB + gepscore_TGFb.19gene
  res <-
    dm_response_4quad(
      data = clin_bmk_IMvigor210,
      response = "binaryResponse",
      response.pos = "CR/PR",
      response.neg = "SD/PD",
      marker1 = "TMB",
      marker2 = "gepscore_TGFb.19gene",
      na.rm = TRUE
    )
  expect_length(res,4)
  expect_equal(names(res), c("pos.n","total.n","stats.4quad","param"))

})
