test_that("sm_searchM by numeric variables", {

  candidates <- str_subset(colnames(clin_bmk_IMvigor210),"gepscore_")

  # basic
  res.sm.logit <- sm_searchM_logit(data = clin_bmk_IMvigor210,
                   response = "binaryResponse",
                   response.pos = "CR/PR", response.neg = "SD/PD",
                   candidates = candidates,
                   binarize = F)
  expect_is(res.sm.logit, 'data.frame')

  # with confounding factor
  res.sm.logit <- sm_searchM_logit(
    data = clin_bmk_IMvigor210,
   response = "binaryResponse",
   response.pos = "CR/PR", response.neg = "SD/PD",
   candidates = candidates,
   confound.factor = "Immune.phenotype",
   binarize = F)
  expect_is(res.sm.logit, 'data.frame')

  # as dichotomous variable
  res.sm.logit <- sm_searchM_logit(
    data = clin_bmk_IMvigor210,
    response = "binaryResponse",
    response.pos = "CR/PR", response.neg = "SD/PD",
    candidates = candidates, binarize = T,
    num.cut = "median", auc=T)
  expect_is(res.sm.logit, 'data.frame')

  g <- sm_searchM_topPlot(res.sm.logit)
  expect_is(g, "ggplot")
})


test_that("sm_searchM_logit by dichotomous variable", {
  candidates <- str_subset(colnames(clin_bmk_IMvigor210),"mut_")
  # basic
  res.sm.logit <- sm_searchM_logit(
    data = clin_bmk_IMvigor210,
    response = "binaryResponse",
    response.pos = "CR/PR", response.neg = "SD/PD",
    candidates = candidates, binarize = T,
    cat.pos = "YES", cat.neg = "NO")
  expect_is(res.sm.logit, 'data.frame')
  # add confounding factor
  res.sm.logit <- sm_searchM_logit(
    data = clin_bmk_IMvigor210,
    response = "binaryResponse",
    response.pos = "CR/PR", response.neg = "SD/PD",
    candidates = candidates, binarize = T,
    cat.pos = "YES", cat.neg = "NO",
    confound.factor = c("Baseline.ECOG.Score"))
  expect_is(res.sm.logit, 'data.frame')

  g <- sm_searchM_topPlot(res.sm.logit)
  expect_is(g, "ggplot")
})


test_that("sm_searchM_cox using numeric", {
  candidates <- str_subset(colnames(clin_bmk_IMvigor210),"gepscore_")
  # basic
  res.sm.cox <- sm_searchM_cox(
    data = clin_bmk_IMvigor210,
    time = "os", event = "censOS",
    candidates = candidates,
    binarize = F,
    confound.factor = c("Baseline.ECOG.Score"))
  expect_is(res.sm.cox, 'data.frame')

  g <- sm_searchM_topPlot(res.sm.cox)
  expect_is(g, "ggplot")
})


test_that("sm_searchM_cox using dichotomous variable ", {
  candidates <- str_subset(colnames(clin_bmk_IMvigor210),"mut_")
  # basic
  res.sm.cox <- sm_searchM_cox(
    data = clin_bmk_IMvigor210,
    time = "os", event = "censOS",
    candidates = candidates,
    binarize = T,
    cat.pos = "YES", cat.neg = "NO",
    confound.factor = c("Baseline.ECOG.Score"))
  expect_is(res.sm.cox, 'data.frame')

  g <- sm_searchM_topPlot(res.sm.cox)
  expect_is(g, "ggplot")
})
