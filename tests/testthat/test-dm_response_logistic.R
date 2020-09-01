test_that("dm_response_logit.reg", {
  # TMB + gepscore_TGFb.19gene
  res1 <- dm_logit(
    data = clin_bmk_IMvigor210,
    response = "binaryResponse",
    response.pos = "CR/PR",
    response.neg = "SD/PD",
    marker1 = "TMB",
    marker2 = "gepscore_TGFb.19gene",
    m1.binarize = F,
    m2.binarize = F
  )
  expect_is(res1, "data.frame")
  # TMB + gepscore_TGFb.19gene
  res2 <- dm_logit(
    data = clin_bmk_IMvigor210,
    response = "binaryResponse",
    response.pos = "CR/PR",
    response.neg = "SD/PD",
    marker1 = "TMB",
    marker2 = "gepscore_TGFb.19gene",
    confound.factor = c("Sex"),
    m1.binarize = F,
    m2.binarize = F
  )
  expect_is(res2, "data.frame")

  # mut_ARID1A + gep_CXCL13
  res3 <- dm_logit(
    data = clin_bmk_IMvigor210,
    response = "binaryResponse",
    response.pos = "CR/PR",
    response.neg = "SD/PD",
    marker1 = "mut_ARID1A",
    marker2 = "gep_CXCL13",
    m1.binarize = T,
    m2.binarize = F,
    m1.cat.pos = "YES",
    m1.cat.neg ="NO"
  )
  expect_is(res3, "data.frame")
  # mut_ARID1A + gep_CXCL13
  res4 <- dm_logit(
    data = clin_bmk_IMvigor210,
    response = "binaryResponse",
    response.pos = "CR/PR",
    response.neg = "SD/PD",
    marker1 = "mut_ARID1A",
    marker2 = "gep_CXCL13",
    m1.binarize = T,
    m2.binarize = F,
    m1.cat.pos = "YES",
    m1.cat.neg ="NO",
    confound.factor = c("Sex")
  )
  expect_is(res4, "data.frame")
})
