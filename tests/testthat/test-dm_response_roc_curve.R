test_that("ROC cuve", {
  g1 <- dm_roc_curve(data = clin_bmk_IMvigor210, response = "binaryResponse",
         response.pos = "CR/PR",
         response.neg = "SD/PD",
         marker1 = "TMB",
         marker2 = "gepscore_TGFb.19gene", logit.reg = T, logit.reg.int = T)
  expect_is(g1,"ggplot")


  g2 <- dm_roc_curve(data = clin_bmk_IMvigor210, response = "binaryResponse",
               response.pos = "CR/PR",
               response.neg = "SD/PD",
               marker1 = "mut_ARID1A",
               m1.cat.pos = "YES", m1.cat.neg = "NO",
               marker2 = "gep_CXCL13", logit.reg = T, logit.reg.int = T)
  expect_is(g2,"ggplot")
})
