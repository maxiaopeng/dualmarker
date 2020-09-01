context("KMplot test")

test_that("KMplot", {
  g <- dm_KMplot(data = clin_bmk_IMvigor210,
            time = "os", event = "censOS",
            marker1 = "TMB", marker2 = "gepscore_TGFb.19gene")
  expect_equal(length(g), 3)
  expect_equal(names(g), c("marker1","marker2","dualmarker"))
})
