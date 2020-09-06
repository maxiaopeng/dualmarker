context("KMplot test")

test_that("dm_KMplot", {
  g <- .dm_KMplot(data = clin_bmk_IMvigor210,
            time = "os", event = "censOS",
            marker1 = "TMB", marker2 = "gepscore_TGFb.19gene")
  expect_equal(length(g), 3)
  expect_equal(names(g), c("marker1","marker2","dualmarker"))
})


test_that("dm_survival_scatter_chart", {
  g1 <- .dm_survival_scatter_chart(
    data = clin_bmk_IMvigor210,
    time = "os", event = "censOS",
    marker1 = "TMB", marker2 = "gepscore_TGFb.19gene",
    m1.num.cut = "mean+sd", m2.num.cut = "median")
  expect_equal(length(g1), 3)
  expect_equal(names(g1), c("marker1","marker2","dualmarker"))

  # with response
  g2 <- .dm_survival_scatter_chart(
    data = clin_bmk_IMvigor210,
    time = "os", event = "censOS",
    marker1 = "TMB", marker2 = "gepscore_TGFb.19gene",
    response = "binaryResponse",
    response.pos = "CR/PR", response.neg = "SD/PD")
  expect_equal(length(g2), 3)
  expect_equal(names(g2), c("marker1","marker2","dualmarker"))
})




test_that("dm_surv_plot", {
  # continuous variables
  g1 <- dm_surv_plot(
    data = clin_bmk_IMvigor210,
    time = "os", event = "censOS",
    marker1 = "TMB", marker2 = "gepscore_TGFb.19gene",
    m1.num.cut = "median", m2.num.cut = "median",
    response = "binaryResponse",
    response.pos = "CR/PR", response.neg = "SD/PD")
  expect_length(g1, 3)
  expect_equal(names(g1), c("marker1","marker2","dualmarker"))

  # strip plot
  g2 <- dm_surv_plot(
    data = clin_bmk_IMvigor210,
    marker1 = "mut_ARID1A", marker2 = "gep_CXCL13",
    response = "binaryResponse", response.pos = "CR/PR", response.neg = "SD/PD",
    m1.cat.pos = "YES", m1.cat.neg = "NO",
    m2.num.cut = "median",
    time = "os", event = "censOS"
  )
  expect_length(g2, 3)
  # strip plot
  g3 <- dm_surv_plot(
      data = clin_bmk_IMvigor210,
      time = "os", event = "censOS",
      marker1 = "gep_CXCL13", marker2 = "mut_ARID1A",
      response = "binaryResponse", response.pos = "CR/PR", response.neg = "SD/PD",
      m1.num.cut = "median",
      m2.cat.pos = "YES", m2.cat.neg = "NO")
  expect_length(g3, 3)

  # dm_jitter
  g4 <- dm_surv_plot(
    data = clin_bmk_IMvigor210,
    time = "os", event = "censOS",
    marker1 = "mut_ARID1A", marker2 = "IC.Level",
    response = "binaryResponse", response.pos = "CR/PR",response.neg = "SD/PD",
    m1.cat.pos = "YES", m1.cat.neg = "NO",
    m2.cat.pos = c("IC1","IC2+"), m2.cat.neg = c("IC0"),
  )
  expect_length(g4, 3)
})

