
#' plot ROC curve for dual markers
#'
#' draw ROC curve for marker1, marker2 and fitted values by logistic regression w/ or w/o interaction between two markers
#'
#' @param data data.frame
#' @param response response
#' @param response_pos positive values of response
#' @param response_neg negative values of response, default NULL, i.e.g all values except response_pos
#' @param marker1 marker1
#' @param marker2 marker2
#' @param m1_cat_pos if m1 is categorical variable, the positve values
#' @param m1_cat_neg if m1 is categorical variable, the negative values
#' @param m2_cat_pos if m2 is categorical variable, the positve values
#' @param m2_cat_neg if m2 is categorical variable, the negative values
#' @param logistic_reg add logistic regression curve, no interaction, default TRUE
#' @param logistic_reg_int add logistic regression(with interaction) curve, default FALSE

#'
#' @return ROC curve, ggplot object
dm_roc <- function(data,
                   response,
                   response_pos,
                   response_neg = NULL,
                   marker1,
                   marker2,
                   m1_cat_pos = NULL,
                   m1_cat_neg = NULL,
                   m2_cat_pos = NULL,
                   m2_cat_neg = NULL,
                   logistic_reg = TRUE,
                   logistic_reg_int = FALSE) {
  data$.response <- binarize_cat(
    x = data[[response]],
    pos = response_pos,
    neg = response_neg,
    return_binary = T
  )
  .summary.auc.ci <- function(res.roc) {
    ci <- pROC::ci.auc(res.roc)
    paste0("AUC:",
           round(ci[2], 3),
           "(",
           round(ci[1], 3),
           ",",
           round(ci[3], 3),
           ")")

  }
  # ROC
  if (datatype_num_cat(data[[marker1]]) == "cat" &&
      !is.null(m1_cat_pos)) {
    data[[marker1]] <-
      binarize_cat(data[[marker1]],
                   pos = m1_cat_pos,
                   neg = m1_cat_neg,
                   return_binary = T)
  }
  if (datatype_num_cat(data[[marker2]]) == "cat" &&
      !is.null(m1_cat_pos)) {
    data[[marker2]] <-
      binarize_cat(data[[marker2]],
                   pos = m2_cat_pos,
                   neg = m2_cat_neg,
                   return_binary = T)
  }
  roc.m1 <- pROC::roc(data$.response, data[[marker1]])
  roc.m2 <- pROC::roc(data$.response, data[[marker2]])
  roc.list <- list(roc.m1, roc.m2) %>%
    setNames(c(str_sub(marker1, end = 40),
               str_sub(marker2, end = 40)))
  # AUC
  anno.m1 <- paste0(marker1, "\n", .summary.auc.ci(roc.m1))
  anno.m2 <- paste0(marker2, "\n", .summary.auc.ci(roc.m2))
  anno <- c(anno.m1, anno.m2)
  # logistic regression
  if (logistic_reg) {
    d <-
      data %>% tidyr::drop_na(.response,!!sym(marker1),!!sym(marker2))
    fml.md <-
      paste0(".response ~ ", marker1, "+", marker2) %>% as.formula()
    logit.md <-
      stats::glm(
        formula = fml.md,
        data = d,
        family = binomial(link = "logit")
      )
    roc.md <- pROC::roc(d$.response, fitted(logit.md))
    roc.list$dualmarker <- roc.md
    anno.md <- paste0("dual-marker\n", .summary.auc.ci(roc.md))
    anno <- c(anno, anno.md)
    if (logistic_reg_int) {
      fml.md.int <-
        paste0(".response ~ ", marker1, "*", marker2) %>% as.formula()
      logit.md.int <- stats::glm(
        formula = fml.md.int,
        data = d,
        family = binomial(link = "logit")
      )
      roc.md.int <- pROC::roc(d$.response, fitted(logit.md.int))
      roc.list$dualmarker_int <- roc.md.int
      anno.md.int <- paste0("dual-marker(interaction)\n",
                            .summary.auc.ci(roc.md.int))
      anno <- c(anno, anno.md.int)
    }
  }

  # plot
  g <- pROC::ggroc(roc.list, aes = c("color")) + theme_bw()
  g <- g + geom_segment(aes(
    x = 1,
    xend = 0,
    y = 0,
    yend = 1
  ),
  color = "grey",
  linetype = "dashed")

  # label AUC
  #g <- g +  annotate("text", x = .25, y = .25,
  #                    label = paste(anno.m1, anno.m2, sep = "\n"))
  g +
    scale_color_discrete(name = "", labels = anno) +
    theme(legend.position = c(.75, .25))
}
