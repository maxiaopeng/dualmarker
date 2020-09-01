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
