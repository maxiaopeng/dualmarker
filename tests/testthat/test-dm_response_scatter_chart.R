test_that("scatter plot", {
  # scatter plot using iris
  data <- iris %>% filter(Species %in% c("setosa", "versicolor"))
  g1 <- dm_scatter_chart(
      data = data,
      marker1 = "Sepal.Length",
      marker2 = "Sepal.Width",
      response = "Species",
      response.pos = "setosa",
      response.neg = "versicolor",
      m1.num.cut = "mean+sd",
      m2.num.cut = "mean+sd",
      size = "Petal.Length",
      alpha = "Petal.Width")
  expect_is(g1, "ggplot")

  # test response = NULL
  g1.2 <- dm_scatter_chart(
    data = data,
    marker1 = "Sepal.Length",
    marker2 = "Sepal.Width",
    response = NULL,
    response.pos = NULL,
    response.neg = NULL,
    m1.num.cut = "mean+sd",
    m2.num.cut = "mean+sd",
    size = "Petal.Length",
    alpha = "Petal.Width")
  expect_is(g1.2, "ggplot")

  # strip plot
  g2 <- dm_scatter_chart(
      data = clin_bmk_IMvigor210,
      marker1 = "mut_ARID1A",
      marker2 = "gep_CXCL13",
      response = "binaryResponse",
      response.pos = "CR/PR",
      response.neg = "SD/PD",
      m1.cat.pos = "YES",
      m1.cat.neg = "NO",
      m2.num.cut = "median",
      size = "os", alpha = "censOS"
    )
  expect_is(g2, "ggplot")
  # strip plot
  g3 <-
    dm_scatter_chart(
      data = clin_bmk_IMvigor210,
      marker1 = "gep_CXCL13",
      marker2 = "mut_ARID1A",
      response = "binaryResponse",
      response.pos = "CR/PR",
      response.neg = "SD/PD",
      m1.num.cut = "median",
      m2.cat.pos = "YES",
      m2.cat.neg = "NO",
      size = "os", alpha = "censOS"
    )
  expect_is(g3, "ggplot")

  # dm_jitter
  g4 <- dm_scatter_chart(
    data = clin_bmk_IMvigor210,
    marker1 = "mut_ARID1A",
    marker2 = "IC.Level",
    response = "binaryResponse",
    response.pos = "CR/PR",
    response.neg = "SD/PD",
    m1.cat.pos = "YES",
    m1.cat.neg = "NO",
    m2.cat.pos = "IC0",
    m2.cat.neg = c("IC1","IC2+"),
    size = "os", alpha = "censOS"
  )
  expect_is(g4, "ggplot")
})
