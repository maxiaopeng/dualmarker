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
##' @param na.rm remove NA
##' @return list of ggplot object, for marker1, marker2 and dualmarker
.dm_KMplot_core <- function(data,
                            time,
                            event,
                            marker1,
                            marker2,
                            label_m1 = marker1,
                            label_m2 = marker2,
                            na.rm = T,
                            km.pval=T,
                            km.risk.table = T, ...) {
  label.max.len <- 40
  assert_that(class(data[[marker1]]) == "factor",
              nlevels(data[[marker1]]) == 2,
              msg = "marker1 should be factor with 2 levels")
  assert_that(class(data[[marker2]]) == "factor",
              nlevels(data[[marker2]]) == 2,
              msg = "marker2 should be factor with 2 levels")
  .assert_colname(data, c(time, event, marker1, marker2))

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
  km.m1 <- .survplot(survfit = survfit.m1, data = data,
                     km.pval = km.pval, km.risk.table = km.risk.table, ...)

  # marker2
  label_m2 <- str_sub(label_m2, end = label.max.len)
  assert_that(label_m1 != label_m2,
              msg = "label_m1 should NOT equal to label_m2")
  data[[label_m2]] <-
    factor(data[[marker2]], levels = rev(levels(data[[marker2]])))
  survfit.m2 <- .survfit( data = data, var = label_m2, time = time,event = event)
  km.m2 <- .survplot(survfit = survfit.m2, data = data, km.pval = km.pval, km.risk.table = km.risk.table, ...)

  # dual marker
  if (na.rm) {
    data %<>% drop_na(!!sym(marker1),!!sym(marker2))
  }
  data$.region <- .label_quadrant(data[[marker1]], data[[marker2]])
  data$.group <- paste(data$.region, data[[marker1]], data[[marker2]], sep = "_")
  mapper <- dplyr::select(data, .region, .group) %>% unique()
  mapper$.region %<>% as.character()

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
    .survplot(survfit = survfit.md, data = data, palette = as.character(tmp$color),
              km.pval = km.pval, km.risk.table = km.risk.table, ...)
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
##' @param m1.datatype datatype of marker1
##' @param m1.label.pos label for positive response of marker1
##' @param m1.label.neg label for negative response of marker1
##' @param m1.cat.pos positive value(s) of marker1 if marker1 is categorical
##' @param m1.cat.neg negative value(s) of marker1 if marker1 is categorical
##' @param m2.datatype datatype of marker2
##' @param m2.label.pos label for positive response of marker2
##' @param m2.label.neg label for positive response of marker2
##' @param m2.cat.pos positive value(s) of marker2 if marker2 is categorical
##' @param m2.cat.neg positive value(s) of marker2 if marker2 is categorical
##' @param m1.num.cut cut method for m1
##' @param m2.num.cut cut method for m2
##' @param na.rm remove NA
dm_KMplot <- function(data, time, event,
                      marker1, marker2,
                      response = NULL,
                      response.pos = NULL,
                      response.neg = NULL,
                      m1.datatype = "auto",
                      m1.num.cut = "median",
                      m1.cat.pos = NULL,
                      m1.cat.neg = NULL,
                      m1.label.pos = NULL,
                      m1.label.neg = NULL,
                      m2.datatype = "auto",
                      m2.num.cut = "median",
                      m2.cat.pos = NULL,
                      m2.cat.neg = NULL,
                      m2.label.pos = NULL,
                      m2.label.neg = NULL,
                      na.rm = T,
                      km.pval= T,
                      km.risk.table=T, ...) {
  .assert_colname(data, c(time, event, marker1, marker2))
  # marker1
  res <-
    binarize_data( x = data[[marker1]],
      datatype = m1.datatype, return.binary = F,
      num.cut = m1.num.cut,  cat.pos = m1.cat.pos, cat.neg = m1.cat.neg,
      label.pos = m1.label.pos, label.neg = m1.label.neg
    )
  data$.m1 <- res$data
  # marker2
  res <-
    binarize_data( x = data[[marker2]],
      datatype = m2.datatype, return.binary = F,
      num.cut = m2.num.cut, cat.pos = m2.cat.pos, cat.neg = m2.cat.neg,
      label.pos = m2.label.pos, label.neg = m2.label.neg
    )
  data$.m2 <- res$data
  g.list <- .dm_KMplot_core(
    data = data, time = time, event = event,
    marker1 = ".m1", marker2 = ".m2",
    label_m1 = marker1, label_m2 = marker2,
    na.rm = na.rm, km.pval = km.pval, km.risk.table = km.risk.table, ...
  )
  #arrange_ggsurvplots(x = g.list, ncol=2, nrow = 2, print = T,
  #                    risk.table.height = 0.4)
  g.list
}
