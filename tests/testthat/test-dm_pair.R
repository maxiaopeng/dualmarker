
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
    m1.num.cut = "median",
    m2.num.cut = "median",
    time = "os",
    event = "censOS")
  expect_length(res, 4)
  if(F){
    (ofile <- tempfile(pattern = "", fileext = ".pdf"))
    pdf(ofile)
    purrr::walk(res$response.plot, print)
    purrr::walk(res$survival.plot, print)
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
    #response = "binaryResponse",
    #response.pos="CR/PR",
    #response.neg="SD/PD",
    time = "os",
    event = "censOS",
    marker1 = "mut_ARID1A",
    marker2 = "gep_CXCL13",
    m1.cat.pos = "YES",
    m1.cat.neg = "NO",
    m2.num.cut = "median")
  expect_length(res, 4)
  if(F){
    (ofile <- tempfile(pattern = "", fileext = ".pdf"))
    pdf(ofile)
    purrr::walk(res$response.plot, print)
    purrr::walk(res$survival.plot, print)
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

  expect_length(res, 4)
  if(F){
    (ofile <- tempfile(pattern = "", fileext = ".pdf"))
    pdf(ofile)
    purrr::walk(res$response.plot, print)
    purrr::walk(res$survival.plot, print)
    dev.off()
  }
})

#######################
### mut_ARID1A + mut_TP53
######################
test_that("mut_ARID1A + mut_TP53", {
  res <- dm_pair(
    data = clin_bmk_IMvigor210,
    # response
    response = "binaryResponse",
    response.pos = "CR/PR",
    response.neg = "SD/PD",
    label.response.pos = "R",
    label.response.neg = "NR",
    # survival info
    time = "os",
    event = "censOS",
    # marker1
    marker1 = "mut_ARID1A",
    m1.cat.pos = "YES",
    m1.cat.neg = "NO",
    label.m1 = "ARID1A-mut",
    label.m1.pos = "MUT",
    label.m1.neg = "WT",
    # marker2
    marker2 = "mut_TP53",
    m2.cat.pos =  "YES",
    m2.cat.neg = "NO",
    label.m2.pos = "MUT",
    label.m2.neg = "WT",
    label.m2 = "TP53-mut",
    # palette
    palette.other =  "lancet",
    palette.4quadrant = "lancet",
    # na.rm
    na.rm.response = F
  )
  expect_length(res, 4)
  if(F){
    (ofile <- tempfile(pattern = "", fileext = ".pdf"))
    pdf(ofile)
    purrr::walk(res$response.plot, print)
    purrr::walk(res$survival.plot, print)
    dev.off()
  }
})
