test_that("sm response logit", {
  # 1. numeric marker
  res <- sm_logit(data = clin_bmk_IMvigor210,
                    response = "binaryResponse",
                    response.pos = "CR/PR", response.neg = "SD/PD",
                    marker = "gepscore_TGFb.19gene", binarize = F)
  expect_true(ncol(res)>0)

  # 1. numeric marker, dichotomous
  res <- sm_logit(data = clin_bmk_IMvigor210,
                  response = "binaryResponse",
                  response.pos = "CR/PR", response.neg = "SD/PD",
                  marker = "gepscore_TGFb.19gene", binarize = T,
                  num.cut = "median")
  expect_true(ncol(res)>0)
  # 2. catorical biomarker
  res <- sm_logit(data = clin_bmk_IMvigor210,
                  response = "binaryResponse",
                  response.pos = "CR/PR", response.neg = "SD/PD",
                  marker = "mut_ARID1A", binarize = T,
                  cat.pos = "YES", cat.neg ="NO")
  expect_true(ncol(res)>0)
})
