library(testthat)

test_that("dm boxplot", {
    # sm boxplot
    g1 <- .sm_boxplot(data = iris, response = "Species", marker = "Sepal.Length")
    expect_is(g1, "ggplot")
    # sm barplot
    data <- iris %>% dplyr::filter(Species %in% c("setosa","versicolor"))
    data$Sepal.Length.level <- cut(data$Sepal.Length, breaks = 2, labels = c("low","high"))
    g2 <- .sm_barplot(data = data, response = "Species",
                marker = "Sepal.Length.level", percent = T)
    expect_is(g2, "ggplot")

    # dm boxplot
    g3 <- dm_response_boxplot(data = iris, response = "Species",
               response.pos = "setosa",
               response.neg = "versicolor",
               marker1 = "Sepal.Length",
               marker2  = "Sepal.Width")
    expect_is(g3, "ggplot")
    # dm barplot
    g4 <- dm_response_boxplot(data = mtcars, response = "vs",
               response.pos = "1",
               response.neg = "0",
               marker2 = "wt",
               marker1  = "gear", m1.datatype = "cat",
               m1.cat.pos = c("5"), m1.cat.neg = c("3","4"))
    expect_is(g4, "ggplot")
})
