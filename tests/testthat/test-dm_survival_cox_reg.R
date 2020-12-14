test_that("dm survival cox regression", {
  res1 <- dm_cox(data = clin_bmk_IMvigor210,
                 time = "os", event = "censOS",
                 marker1 = "TMB", marker2 = "gepscore_TGFb.19gene",
                 m1.binarize = F, m2.binarize = F)
  expect_is(res1, "data.frame")
  # confounding factor
  res2 <- dm_cox(data = clin_bmk_IMvigor210,
                 time = "os", event = "censOS",confound.factor = "Sex",
                 marker1 = "TMB", marker2 = "gepscore_TGFb.19gene",
                 m1.binarize = F, m2.binarize = F)
  expect_is(res2, "data.frame")

  # mut-ARID1A + CXCL13
  res3 <- dm_cox(data = clin_bmk_IMvigor210,
                time = "os", event = "censOS",
                marker1 = "mut_ARID1A",
                marker2 = "gep_CXCL13",
                m1.binarize = T,m1.cat.pos = "YES",m1.cat.neg ="NO",
                 m2.binarize = F)
  expect_is(res3, "data.frame")
})


test_that("comparing cox model", {
  coxph.fit1 <- coxph(formula = Surv(os, censOS) ~ mut_ARID1A + gep_CXCL13, data = clin_bmk_IMvigor210)
  coxph.fit2 <- coxph(formula = Surv(os, censOS) ~ mut_ARID1A * gep_HMGB1, data = clin_bmk_IMvigor210)

  anova(coxph.fit1, coxph.fit2)

})
