

##' @description
##' draw ROC curve for marker1, marker2 and fitted values by logistic regression
##' @param data
##' @param outcome
##' @param outcome_pos
##' @param outcome_neg
##' @param marker1
##' @param marker2
##' @return ggplot object
dm_roc <- function(data, outcome, outcome_pos, outcome_neg=NULL,
                   marker1, marker2,
                   m1_cat_pos = NULL, m1_cat_neg =NULL,
                   m2_cat_pos = NULL, m2_cat_neg = NULL,
                   logistic.reg=T, logistic.reg.int=F){
  data$.outcome <- binarize_cat(x = data[[outcome]],
                                pos = outcome_pos, neg = outcome_neg, return.binary = T)
  .summary.auc.ci <- function(res.roc){
    #auc <- auc(res.roc)
    ci <- ci.auc(res.roc)
    summary <- paste0("AUC:", round(ci[2],3),
                      "(", round(ci[1],3), ",",
                      round(ci[3],3), ")")
    list(auc = auc, ci = ci, summary =  summary)
  }
  # ROC
  #roc.fml <- paste0(".outcome~", marker1,"+", marker2) %>% as.formula
  #roc.list <- roc(roc.fml, data= data)
  if(datatype_num_cat(data[[marker1]]) == "cat" && !is.null(m1_cat_pos)){
    data[[marker1]] <- binarize_cat(data[[marker1]], pos = m1_cat_pos, neg = m1_cat_neg, return.binary = T)
  }
  if(datatype_num_cat(data[[marker2]]) == "cat" && !is.null(m1_cat_pos)){
    data[[marker2]] <- binarize_cat(data[[marker2]], pos = m2_cat_pos, neg = m2_cat_neg, return.binary = T)
  }
  roc.m1 <- roc(data$.outcome, data[[marker1]])
  roc.m2 <- roc(data$.outcome, data[[marker2]])
  roc.list <- list(roc.m1, roc.m2) %>%
    setNames(c(str_sub(marker1, end = 40),
               str_sub(marker2, end = 40)))
  # AUC
  anno.m1 <- paste0(marker1, "\n", .summary.auc.ci(roc.m1)$summary)
  anno.m2 <- paste0(marker2, "\n", .summary.auc.ci(roc.m2)$summary)
  anno <- c(anno.m1, anno.m2)
  # logistic regression
  if(logistic.reg){
    d <- data %>% drop_na(.outcome, !!sym(marker1), !!sym(marker2))
    fml.md <- paste0( ".outcome ~ ", marker1, "+", marker2) %>% as.formula()
    logit.md <- glm(formula = fml.md, data = d, family = binomial(link="logit"))
    roc.md <- roc(d$.outcome, fitted(logit.md))
    roc.list$dualmarker <- roc.md
    anno.md <- paste0("dual-marker\n", .summary.auc.ci(roc.md)$summary)
    anno <- c(anno, anno.md)
    if(logistic.reg.int){
      fml.md.int <- paste0(".outcome ~ ", marker1, "*", marker2) %>% as.formula()
      logit.md.int <- glm(formula = fml.md.int, data = d, family = binomial(link="logit"))
      roc.md.int <- roc(d$.outcome, fitted(logit.md.int))
      roc.list$dualmarker_int <- roc.md.int
      anno.md.int <- paste0("dual-marker(interaction)\n", .summary.auc.ci(roc.md.int)$summary)
      anno <- c(anno, anno.md.int)
    }
  }

  # plot
  g <- pROC::ggroc(roc.list, aes = c("color")) + theme_bw()
  g <- g + geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                        color="grey", linetype="dashed")

  # label AUC
  #g <- g +  annotate("text", x = .25, y = .25,
  #                    label = paste(anno.m1, anno.m2, sep = "\n"))
  g +
    scale_color_discrete(name = "", labels = anno)+
    theme(legend.position=c(.75,.25))
}
