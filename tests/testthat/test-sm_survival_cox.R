test_that("sm survival cox analysis", {
  res <-  sm_cox(data = clin_bmk_IMvigor210,
                 time = "os", event ="censOS",
                 marker = "TMB",
                 confound.factor = "Baseline.ECOG.Score")
  expect_is(res, "data.frame")
})
