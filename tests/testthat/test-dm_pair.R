
##################
## TMB + gepscore TGFb signature
##################

test_that("dm_pair works", {
  # 1. cut by median
  res <- dm_pair(
    data = clin_bmk_IMvigor210,
    response = "binaryResponse",
    response.pos="CR/PR",
    response.neg="SD/PD",
    marker1 = "TMB",
    marker2 = "gepscore_TGFb.19gene",
    m1.num.cut = "median", m2.num.cut = "median",
    time = "os",
    event = "censOS")
  expect_length(res, 2)
  if(F){
    (ofile <- tempfile(pattern = "", fileext = ".pdf"))
    pdf(ofile)
    purrr::walk(res$plot, print)
    dev.off()
  }
})

##################
## mut_ARID1A + gep_CXCL13 signature
##################
test_that("mut_ARID1A + gep_CXCL13", {
  # 1. cut by median
  res <- dm_pair(
    data = clin_bmk_IMvigor210,
    response = "binaryResponse",
    response.pos="CR/PR",
    response.neg="SD/PD",
    time = "os",
    event = "censOS",
    marker1 = "mut_ARID1A",
    marker2 = "gep_CXCL13",
    m1.cat.pos = "YES",
    m1.cat.neg = "NO",
    m2.num.cut = "median")
  expect_length(res, 2)
  if(F){
    (ofile <- tempfile(pattern = "", fileext = ".pdf"))
    pdf(ofile)
    purrr::walk(res$plot, print)
    dev.off()
  }
})

##################
## mut_ARID1A + IC.Level signature
##################
test_that("mut_ARD1A + IC.level", {
  res <- dm_pair(
    data = clin_bmk_IMvigor210,
    response = "binaryResponse",
    response.pos="CR/PR",
    response.neg="SD/PD",
    marker1 = "mut_ARID1A",
    marker2 = "IC.Level",
    m1.cat.pos = "YES",
    m1.cat.neg = "NO",
    m2.cat.pos = "IC2+",
    m2.cat.neg = c("IC0","IC1"),
    time = "os",
    event = "censOS")

  expect_length(res, 2)
  if(F){
    (ofile <- tempfile(pattern = "", fileext = ".pdf"))
    pdf(ofile)
    purrr::walk(res$plot, print)
    dev.off()
  }
})
