library(dualmarker)

test_that("dm survival analysis between four quadrants", {
  res <- dm_survival_4quad(
    data = clin_bmk_IMvigor210,
    time = "os", event = "censOS",
    marker1 = "TMB",
    marker2 = "gepscore_TGFb.19gene")
  expect_length(res, 2)
  expect_equal(names(res), c("stats", "param"))
  expect_true(ncol(res$param)>0)
})
