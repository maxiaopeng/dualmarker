test_that("cutpoint works", {
  # median
  cp1 <- cutpoint(x = clin_bmk_IMvigor210$TMB, method = "median")
  expect_is(cp1, "numeric")

  # test
  res <- binarize_data(x = clin_bmk_IMvigor210$IC.Level,
                cat.pos = c("IC1","IC2+"),
                cat.neg = "IC0", return.binary = T)
  expect_length(res, 2)
})


test_that("cindex",{
  # coxph.fit <- coxph(formula = Surv(os, censOS) ~ TMB, data = clin_bmk_IMvigor210)
  # CPE::phcpe(coxph.fit, CPE.SE=TRUE)## 0.56
  #
  # rm(coxph.fit)
  # coxph.fit <- coxph(formula = Surv(os, censOS) ~ TMB + gepscore_TGFb.19gene, data = clin_bmk_IMvigor210)
  # CPE::phcpe(coxph.fit, CPE.SE=TRUE, out.ties = T)## 0.56
})
