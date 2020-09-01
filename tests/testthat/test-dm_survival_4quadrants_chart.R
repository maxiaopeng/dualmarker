context("quadrant survival chart")

test_that("quadrant survival chart", {
  g1 <- dm_survival_4quad_chart(data = clin_bmk_IMvigor210, time = "os", event = "censOS", marker1 = "TMB",
                            marker2 = "gepscore_TGFb.19gene", m1.num.cut = "median", m2.num.cut = "median")
  expect_is(g1, "ggplot")

  g2 <- dm_survival_4quad_chart(data = clin_bmk_IMvigor210, time = "os", event = "censOS", marker1 = "mut_ARID1A",
                            marker2 = "gep_CXCL13",  m1.cat.pos = "YES", m1.cat.neg = "NO", m2.num.cut = "median")
  expect_is(g2, "ggplot")

  g3 <- dm_survival_4quad_chart(data = clin_bmk_IMvigor210, time = "os", event = "censOS", marker1 = "mut_ARID1A",
                            marker2 = "IC.Level",
                            m1.cat.pos = "YES", m1.cat.neg = "NO",
                            m2.cat.pos = c("IC0","IC1"), m2.cat.neg = c("IC2+"))
  expect_is(g3, "ggplot")
})
