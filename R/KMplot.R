##' KMplot of dual markers
##'
##' KMplot of dual markers, for each single markers and dual markers
##'
##' @param data data frame
##' @param time survival time
##' @param event survival event
##' @param marker1 categorical variables with 2 levels
##' @param marker2 categorical variables with 2 levels
##' @param label_m1 label for marker1
##' @param label_m2 label for marker2
##' @param na_rm remove NA
##' @return list of ggplot object, for marker1, marker2 and dualmarker
.dm_KMplot_core <- function(data,
                            time,
                            event,
                            marker1,
                            marker2,
                            label_m1 = marker1,
                            label_m2 = marker2,
                            na_rm = T) {
  label.max.len <- 40
  assert_that(class(data[[marker1]]) == "factor",
              nlevels(data[[marker1]]) == 2,
              msg = "marker1 should be factor with 2 levels")
  assert_that(class(data[[marker2]]) == "factor",
              nlevels(data[[marker2]]) == 2,
              msg = "marker2 should be factor with 2 levels")

  # marker1
  label_m1 <- str_sub(label_m1, end = label.max.len)
  data[[label_m1]] <-
    factor(data[[marker1]], levels = rev(levels(data[[marker1]])))
  survfit.m1 <-
    .survfit(
      data = data,
      var = label_m1,
      time = time,
      event = event
    )
  km.m1 <- .survplot(survfit = survfit.m1, data = data)

  # marker2
  label_m2 <- str_sub(label_m2, end = label.max.len)
  assert_that(label_m1 != label_m2,
              msg = "label_m1 should NOT equal to label_m2")
  data[[label_m2]] <-
    factor(data[[marker2]], levels = rev(levels(data[[marker2]])))
  survfit.m2 <-
    .survfit(
      data = data,
      var = label_m2,
      time = time,
      event = event
    )
  km.m2 <- .survplot(survfit = survfit.m2, data = data)

  # dual marker
  if (na_rm) {
    data %<>% drop_na(!!sym(marker1),!!sym(marker2))
  }
  data$.region <- .label_quadrant(data[[marker1]], data[[marker2]])
  data$.group <- paste(data[[marker1]], data[[marker2]], sep = "_")
  mapper <- dplyr::select(data, .region, .group) %>% unique()

  tmp <- as.data.frame(color.quadrant.2) %>%
    rownames_to_column(".region") %>%
    set_colnames(c(".region", "color")) %>%
    left_join(mapper, . , by = ".region") %>%
    arrange(.group)

  survfit.md <-
    .survfit(
      data = data,
      var = ".group",
      time = time,
      event = event
    )
  km.md <-
    .survplot(survfit = survfit.md,
              data = data,
              palette = as.character(tmp$color))
  list(marker1 = km.m1,
       marker2 = km.m2,
       dualmarker = km.md)
}


##' KM plot for dual marker
##'
##' @param data data frame
##' @param time survival time
##' @param event survival event
##' @param marker1 marker1
##' @param marker2 marker2
##' @param num_cut_method method to cut numeric variables
##' @param response response response
##' @param response_pos positive response response
##' @param response_neg negative response response
##' @param m1_datatype datatype of marker1
##' @param m1_label_pos label for positive response of marker1
##' @param m1_label_neg label for negative response of marker1
##' @param m1_cat_pos positive value(s) of marker1 if marker1 is categorical
##' @param m1_cat_neg negative value(s) of marker1 if marker1 is categorical
##' @param m2_datatype datatype of marker2
##' @param m2_label_pos label for positive response of marker2
##' @param m2_label_neg label for positive response of marker2
##' @param m2_cat_pos positive value(s) of marker2 if marker2 is categorical
##' @param m2_cat_neg positive value(s) of marker2 if marker2 is categorical
##' @param na_rm
dm_KMplot <- function(data,
                      time,
                      event,
                      marker1,
                      marker2,
                      num_cut_method = "median",
                      response = NULL,
                      response_pos = NULL,
                      response_neg = NULL,
                      m1_datatype = "auto",
                      m1_cat_pos = NULL,
                      m1_cat_neg = NULL,
                      m1_label_pos = NULL,
                      m1_label_neg = NULL,
                      m2_datatype = "auto",
                      m2_cat_pos = NULL,
                      m2_cat_neg = NULL,
                      m2_label_pos = NULL,
                      m2_label_neg = NULL,
                      na_rm = T) {
  assert_that(all(c(time, event, marker1, marker2) %in%  colnames(data)),
                          msg = "time, event, marker1, marker2 are required")
  # marker1
  res <-
    binarize_data(
      x = data[[marker1]],
      datatype = m1_datatype,
      num_cut_method = num_cut_method,
      return_binary = F,
      label_pos = m1_label_pos,
      label_neg = m1_label_neg,
      cat_pos = m1_cat_pos,
      cat_neg = m1_cat_neg,
      response = data[[response]],
      response_pos = response_pos,
      response_neg = response_neg
    )
  data$.m1 <- res$data
  # marker2
  res <-
    binarize_data(
      x = data[[marker2]],
      datatype = m2_datatype,
      num_cut_method = num_cut_method,
      return_binary = F,
      label_pos = m2_label_pos,
      label_neg = m2_label_neg,
      cat_pos = m2_cat_pos,
      cat_neg = m2_cat_neg,
      response = data[[response]],
      response_pos = response_pos,
      response_neg = response_neg
    )
  data$.m2 <- res$data
  g.list <- .dm_KMplot_core(
    data = data,
    time = time,
    event = event,
    marker1 = ".m1",
    marker2 = ".m2",
    label_m1 = marker1,
    label_m2 = marker2,
    na_rm = na_rm
  )
  #arrange_ggsurvplots(x = g.list, ncol=2, nrow = 2, print = T,
  #                    risk.table.height = 0.4)
  g.list
}
