
#' plot ROC curve for dual markers
#'
#' draw ROC curve for marker1, marker2 and fitted values by logistic regression w/ or w/o interaction between two markers
#'
#' @param data data.frame
#' @param response response
#' @param response.pos positive values of response
#' @param response.neg negative values of response, default NULL, i.e.g all values except response.pos
#' @param marker1 marker1
#' @param marker2 marker2
#' @param m1.cat.pos if m1 is categorical variable, the positve values
#' @param m1.cat.neg if m1 is categorical variable, the negative values
#' @param m2.cat.pos if m2 is categorical variable, the positve values
#' @param m2.cat.neg if m2 is categorical variable, the negative values
#' @param logit.reg add logistic regression curve, no interaction, default TRUE
#' @param logit.reg.int add logistic regression(with interaction) curve, default FALSE
#' @return ROC curve, ggplot object
dm_roc_curve <- function(data,
                   response,
                   response.pos,
                   response.neg = NULL,
                   marker1,
                   marker2,
                   m1.cat.pos = NULL,
                   m1.cat.neg = NULL,
                   m2.cat.pos = NULL,
                   m2.cat.neg = NULL,
                   logit.reg = TRUE,
                   logit.reg.int = FALSE,
                   palette = "default") {
  data$.response <- binarize_cat(
    x = data[[response]],
    pos = response.pos,
    neg = response.neg,
    return.binary = T
  )
  .summary.auc.ci <- function(res.roc) {
    ci <- pROC::ci.auc(res.roc)
    paste0("AUC:", round(ci[2], 3),
           "(", round(ci[1], 3),
           ",", round(ci[3], 3),")")
  }
  # ROC
  if (datatype_num_cat(data[[marker1]]) == "cat" &&
      !is.null(m1.cat.pos)) {
    data[[marker1]] <-
      binarize_cat(data[[marker1]],
                   pos = m1.cat.pos,
                   neg = m1.cat.neg,
                   return.binary = T)
  }
  if (datatype_num_cat(data[[marker2]]) == "cat" &&
      !is.null(m2.cat.pos)) {
    data[[marker2]] <-
      binarize_cat(data[[marker2]],
                   pos = m2.cat.pos,
                   neg = m2.cat.neg,
                   return.binary = T)
  }
  roc.m1 <- pROC::roc(data$.response, data[[marker1]], quiet = TRUE)
  roc.m2 <- pROC::roc(data$.response, data[[marker2]], quiet = TRUE)
  roc.list <- list(roc.m1, roc.m2) %>%
    setNames(c(str_sub(marker1, end = 40),
               str_sub(marker2, end = 40)))
  # AUC
  anno.m1 <- paste0(marker1, "\n", .summary.auc.ci(roc.m1))
  anno.m2 <- paste0(marker2, "\n", .summary.auc.ci(roc.m2))
  anno <- c(anno.m1, anno.m2)
  # logistic regression
  if (logit.reg) {
    d <-  data %>% tidyr::drop_na(.response,!!sym(marker1),!!sym(marker2))
    fml.md <- paste0(".response ~ ", marker1, "+", marker2) %>%
      as.formula()
    logit.md <- stats::glm(
        formula = fml.md, data = d,
        family = binomial(link = "logit")
      )
    roc.md <- pROC::roc(d$.response, fitted(logit.md), quiet = TRUE)
    roc.list$dualmarker <- roc.md
    anno.md <- paste0("dual-marker\n", .summary.auc.ci(roc.md))
    anno <- c(anno, anno.md)
    if (logit.reg.int) {
      fml.mdi <-
        paste0(".response ~ ", marker1, "*", marker2) %>% as.formula()
      logit.mdi <- stats::glm(
        formula = fml.mdi,
        data = d,
        family = binomial(link = "logit")
      )
      roc.mdi <- pROC::roc(d$.response, fitted(logit.mdi), quiet = TRUE)
      roc.list$dualmarker_int <- roc.mdi
      anno.mdi <- paste0("dual-marker(interaction)\n",
                            .summary.auc.ci(roc.mdi))
      anno <- c(anno, anno.mdi)
    }
  }

  # plot
  g <- pROC::ggroc(roc.list, aes = c("color")) + theme_bw()
  g <- g + geom_segment(
    aes( x = 1, xend = 0, y = 0,yend = 1 ),
    color = "grey",
    linetype = "dashed")

  # label AUC
  k <- 2
  if(logit.reg) k <- k + 1
  if(logit.reg.int) k <- k + 1

  g <- g +
    theme(legend.position = c(.75, .25)) +
    scale_color_manual(name = "", labels = anno,
                       values = ggpubr::get_palette(palette = palette, k=k))
  g
}
