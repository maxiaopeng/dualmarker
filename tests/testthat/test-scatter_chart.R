test_that("scatter plot", {
  # scatter plot using iris
  data <- iris %>% filter(Species %in% c("setosa", "versicolor"))
  g1 <-
    dm_scatter_chart(
      data = data,
      num_cut_method = "median",
      marker1 = "Sepal.Length",
      marker2 = "Sepal.Width",
      response = "Species",
      response_pos = "setosa",
      response_neg = "versicolor"
    )
  expect_is(g1, "ggplot")
  # strip plot
  g2 <-
    dm_scatter_chart(
      data = clin_bmk_imvigor210,
      marker1 = "mut_ARID1A",
      marker2 = "gep_CXCL13",
      num_cut_method = "median",
      response = "binaryResponse",
      response_pos = "CR/PR",
      response_neg = "SD/PD",
      m1_cat_pos = "YES",
      m1_cat_neg = "NO"
    )
  expect_is(g2, "ggplot")
  # strip plot
  g3 <-
    dm_scatter_chart(
      data = clin_bmk_imvigor210,
      marker1 = "gep_CXCL13",
      marker2 = "mut_ARID1A",
      num_cut_method = "median",
      response = "binaryResponse",
      response_pos = "CR/PR",
      response_neg = "SD/PD",
      m2_cat_pos = "YES",
      m2_cat_neg = "NO"
    )
  expect_is(g3, "ggplot")

  # dm_jitter
  g4 <- dm_scatter_chart(
    data = clin_bmk_imvigor210,
    marker1 = "mut_ARID1A",
    marker2 = "Sex",
    response = "binaryResponse",
    response_pos = "CR/PR",
    response_neg = "SD/PD",
    m1_cat_pos = "YES",
    m1_cat_neg = "NO",
    m2_cat_pos = "F",
    m2_cat_neg = "M"
  )
  expect_is(g4, "ggplot")
})
