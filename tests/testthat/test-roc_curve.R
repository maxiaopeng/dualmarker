test_that("ROC cuve", {
  g1 <- dm_roc(data = clin_bmk_imvigor210, response = "binaryResponse",
         response_pos = "CR/PR",
         response_neg = "SD/PD",
         marker1 = "TMB",
         marker2 = "gepscore_gene19", logistic_reg = T, logistic_reg_int = T)
  expect_is(g1,"ggplot")


  g2 <- dm_roc(data = clin_bmk_imvigor210, response = "binaryResponse",
               response_pos = "CR/PR",
               response_neg = "SD/PD",
               marker1 = "mut_ARID1A",
               m1_cat_pos = "YES", m1_cat_neg = "NO",
               marker2 = "gep_CXCL13", logistic_reg = T, logistic_reg_int = T)
  expect_is(g2,"ggplot")
})
