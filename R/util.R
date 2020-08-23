#source("~/working/Biomarker_integrated_reporter/src/my_functions.R")
# library(assertthat)
# library(magrittr)
# library(dplyr)
# library(tibble)
# library(tidyr)
# library(purrr)
# library(ggplot2)
# library(stringr)
# library(survival)
# library(survminer)
# library(ggforce)
# library(GGally)
# library(survminer)
# library(pROC)
# library(rcompanion)
# #library(gridExtra)
# library(cowplot)

#' attach library
#'
#' @import survival
#' @import ggplot2
#' @importFrom survminer ggsurvplot
#' @import pROC
#' @importFrom  tidyr drop_na
#' @import magrittr
#' @importFrom assertthat assert_that
#' @import stringr
#' @import dplyr
#' @import tibble
.attach_library <- function(){

}


#' get datatype "num" or "cat"
#'
#' @param x vector
#' @return "num" or "cat"
#' @examples
#'
datatype_num_cat <- function(x){
  if(class(x) %in% c("factor","character")){
    res <- "cat"
  }else if(class(x) %in% c("numeric","integer","float", "double")){
    res <- "num"
  }else{
    stop("not cat or num, please check\n")
  }
  res
}

#' cutpoint
#'
#' get cutpoint
#' @param x vector
#' @param method median(default), roc, optimal_surv
#' @param outcome used if method='roc'
#' @param outcome_pos used if method='roc', positive outcome label
#' @param outcome_neg used if method='roc', negative outcome label, if NULL, use all other samples except outcome_pos
#' @param auc.best.method used if method='roc'
#' @param time used if method='optimal_surv_cut', survival time used for
#' @param surv.minprop used if method='optimal_surv_cut', min proportion in high and low group
#' @return cutpoint
#' @export
cutpoint <- function(x, method="median",
                         outcome=NULL, outcome_pos=NULL, outcome_neg=NULL,
                         auc.best.method = "youden",
                         time = NULL, event=NULL, surv.minprop=0.3){
  assertthat::assert_that(all(class(x) %in% c("numeric","integer")),
              msg = "x should be numeric")
  min <- min(x, na.rm = T)
  max <- max(x, na.rm = T)
  cutpoint <- NA
  if(method == "median"){
    cutpoint <- median(x, na.rm = T)
  }else if(method %in% c("roc")){
    assertthat::assert_that(! is.null(outcome) && !is.null(outcome_pos),
                msg= "outcome and outcome_pos are required")
    df <- data.frame(predictor = x,
                       response = outcome)
    cutpoint <- auc_stats(data = df, response="response",
                            control = outcome_neg,
                            case = outcome_pos,
                            predictor = "predictor",
                            best.method = auc.best.method)$threhold[1]
  }else if(method %in% c("optimal_surv")){
    assertthat::assert_that(! is.null(time) && !is.null(event),
                msg="time and event are required")
    df <- data.frame(time = time, event = event, feature = x)
    surv.cutp <- do.call("surv_cutpoint",
                         list(data= df, time= "time",
                              event= "event",
                              variables = "feature",
                              minprop=surv.minprop))
    cutpoint <- surv.cutp$cutpoint[1,1]
  }else{
    stop("method should be [median, roc, optimal_surv_cut]")
  }
  assertthat::assert_that(cutpoint >= min && cutpoint <= max,
              msg = paste0("cut point should be [",min, ",", max,"]"))
  cutpoint
}

#' binarize numeric values
#'
#' binarize numeric values by predefined cutpoint
#'
#' @param x vector of numeric
#' @param cutpoint the cutpoint
#' @param outcome necessary if cut = "youden_ROC"
#' @param label_pos label for 'positive' group, default 'high'
#' @param label_neg label for 'negative' group, default 'low'
#' @return binary value
#' @export
binarize_num <- function(x, cutpoint, return.binary=F,
                         label_pos = "high", label_neg = "low"){
  min <- min(x, na.rm = T)
  max <- max(x, na.rm = T)
  assertthat::assert_that(cutpoint >= min && cutpoint <= max,
              msg = paste0("cut point should be [",min, ",", max,"]"))
  breaks <- c(min -1, cutpoint, max +1)
  if(return.binary){
    out <- cut(x, breaks = breaks, labels = c(0,1)) %>%
      as.character() %>% as.integer()
  }else{
    out <- cut(x, breaks = breaks, labels = c(label_neg,label_pos))
  }
  out
}

#' binarize categorical variable
#' @param x vector
#' @param pos positive values
#' @param neg negative values, if NULL, use all other values except 'pos', the non-pos,
#' non-neg values in x will be converted to NA
#' @param return.binary return the binary 0(negative), 1(positive), default FALSE
#' @label_pos label for positive values, default 'pos'
#' @label_neg label for negative values, default 'neg'
binarize_cat <- function(x, pos, neg=NULL, return.binary=F,
                         label_pos = "pos", label_neg = "neg"){

  assertthat::assert_that(all(class(x) %in% c("factor","character")), msg = "x should be factor/character")
  assertthat::assert_that(!all(is.na(x)), msg =" all x is NA, can't be cutted")
  if("factor" %in% class(x)){ x <- as.character(x) }
  all.val <- na.omit(unique(x))
  if(is.null(neg)) neg <- setdiff(all.val, pos)
  assertthat::assert_that(all(c(pos, neg) %in% all.val), msg = "pos or neg isn't in x")
  out <- base::rep(NA, times = length(x))
  if(return.binary){
    out[x %in% pos] <- 1
    out[x %in% neg] <- 0
    out <- as.integer(out)
  }else{
    out[x %in% pos] <- label_pos
    out[x %in% neg] <- label_neg
    out <- factor(out, levels = c(label_neg, label_pos))
  }
  out
}


#' binarize categorical or numeric data
#'
#' @param x input vector
#' @param datatype "num" or "cat", default "auto", which will get by \code{\link{datatype_num_cat}(x)}
#' @param return.binary return the binary 0(negative), 1(positive), default FALSE
#' @param label_pos label for positive values, default 'pos'
#' @param label_neg label for negative values, default 'neg'
#' @param cat.pos if x is categorical, the positive group values
#' @param cat.neg if x is categorical, the negative group values
#' @param num_cut_method
#' @param outcome
#' @param outcome_pos
#' @param outcome_neg
#' @param auc.best.method
#' @param time
#' @param event
#' @param surv.minprop
binarize_data <- function(x,
                        datatype="auto",
                        return.binary = F, label_pos=NULL, label_neg =NULL,
                        cat.pos=NULL, cat.neg=NULL,
                        num_cut_method="median",
                        outcome=NULL, outcome_pos=NULL, outcome_neg=NULL,
                        auc.best.method = "youden",
                        time = NULL, event=NULL, surv.minprop=0.3){
  assertthat::assert_that(datatype %in% c("auto","num","cat"), msg =" datatype should be [auto, num, cat]")
  cutpoint <- NA
  if(datatype=="auto"){
      datatype <- datatype_num_cat(x)
  }
  if( datatype == "num"){
    assertthat::assert_that(num_cut_method %in% c("roc", "median", "optimal_surv"),
                msg = "num_cut_method should be [roc, median, optimal_surv]")
    cutpoint <- cutpoint(x = x, method= num_cut_method, outcome= outcome,
                         outcome_pos= outcome_pos, outcome_neg= outcome_neg,
                         auc.best.method = auc.best.method,
                         time = time , event=event,
                         surv.minprop=surv.minprop)
    if(is.null(label_pos)) label_pos <- "high"
    if(is.null(label_neg)) label_neg <- "low"
    m <- binarize_num(x = x, cutpoint = cutpoint, return.binary = return.binary,
                      label_neg = label_neg, label_pos = label_pos)
  }else{
    if(is.null(label_pos)) label_pos <- "pos"
    if(is.null(label_neg)) label_neg <- "neg"
    assertthat::assert_that(!is.null(cat.pos), msg="cat.pos should not be NULL")
    m <- binarize_cat(x = x, pos = cat.pos, neg = cat.neg, return.binary = return.binary,
                      label_pos = label_pos, label_neg = label_neg)
  }
  list(data = m, cutpoint = cutpoint)
}

#' auc_stats
#'
#' get statistics of AUC
#'
#' @param data
#' @param response
#' @example auc_stats(data = iris, response = "Species", case = "setosa",
#' control = "versicolor", predictor = "Sepal.Length")
#' @export
auc_stats <- function(data, response, case, control=NULL, predictor, best.method="youden"){
  suppressPackageStartupMessages(require(pROC))
  assertthat::assert_that(all(c(response, predictor) %in% colnames(data)),
              msg = paste(response, "or", predictor, "not in data"))
  if(is.null(control)) control <- setdiff(na.omit(unique(data[[response]])), case)
  # remove non-case and non-control record
  keep <- data[[response]] %in% c(case, control)
  if(any(!keep)){
    print(paste0("warning: ",sum(!keep), " records have no expected response, removed\n"))
    data <- data[keep,]
  }
  data$.response <- ifelse(data[[response]] %in% case,"case", "control")
  # roc
  res.roc <- roc(data$.response, data[[predictor]],
                 levels = c("control", "case"),
                 ci = T, auc = T, plot =F)
  res.coords <- coords(res.roc, "best",
                       ret=c("threshold", "specificity", "sensitivity", "accuracy",
                             "ppv", "npv"),
                       transpose = FALSE,
                       best.method = best.method)
  tibble(control = toString(control),
         case = toString(case),
         direction = res.roc$direction,
         n.control = length(res.roc$control),
         n.case = length(res.roc$cases),
         n.all = length(res.roc$control) + length(res.roc$case),
         auc = round(res.roc$auc,digits = 3),
         lower.95 = res.roc$ci[1],
         upper.95 = res.roc$ci[3],
         threhold = res.coords$threshold,
         specificity.threhold = res.coords$specificity,
         sensitivity.threhold = res.coords$sensitivity,
         accuracy.threhold = res.coords$accuracy,
         ppv.threhold = res.coords$ppv,
         npv.threhold = res.coords$npv)
}

#' @description add adjusted pvalues to the data
add_adjust_pvalues <- function(data, pvalues, method="BH"){
  res <- map_dfc(data[, pvalues, drop=F], .f = ~{
    stats::p.adjust(p = .x, method = method)
  })
  colnames(res) <- paste0(pvalues,".adj")
  bind_cols(data, res)
}

.label_pvalue <- function(p){
  ifelse(p < 0.001, "***",
         ifelse(p < 0.01, "**",
                ifelse(p<0.05, "*",
                       ifelse(p<0.1, "?", ""))))
}

label_pvalues <- function(data, pvalues){
  res <- map_dfc(data[, pvalues, drop=F], .f = ~{
    .label_pvalue(p = .x)
  })
  colnames(res) <- paste0(pvalues,".sign")
  bind_cols(data, res)
}


#' Title
#'
#' @param data
#' @param var
#' @param time
#' @param event
#'
#' @return
#' @import survival
#' @import magrittr
.survfit <- function(data, var, time, event){
  data[[event]] %<>% as.character() %>% as.integer()
  assertthat::assert_that(all(data[[event]] %in% c(0,1)), msg = "event should be 0 and 1")
  str.formula <- paste0("Surv(", time,",", event,") ~ ", var)
  survfit <- do.call("survfit",
                     list(as.formula(str.formula),data=data, conf.type = 'log-log')) # log-log to get the same result with SAS
  survfit
}



#' KMplot(internal)
#'
#' @param survfit survfit model
#' @param data
#' @param ... parameters for ggsurvplot
#' @return ggplot
#' @importFrom survminer ggsurvplot
.survplot <- function(survfit, data, ...){
  ggsurvplot(survfit, data=data,
             pval = T,risk.table = T,
             ...)
}

#' the first level as negative, 2nd level as positve
#' for (x1, x2):
#' (pos, pos) => R1, first quadrant
#' (neg, pos) => R2, 2nd quadrant
#' (neg, neg) => R3, 3rd quadrant
#' (pos, neg) => R4, 4th quadrant
#' @param m1 factor with 2 levels
#' @param m2 factor with 2 levels
#'
.label.quadrant <- function(m1, m2){
  assertthat::assert_that(class(m1) == "factor" && nlevels(m1) == 2,
              msg = "marker1 should be factor with 2 levels")
  assertthat::assert_that(class(m2) == "factor" && nlevels(m2) == 2,
              msg = "marker2 should be factor with 2 levels")
  m1.neg <- levels(m1)[1]
  m1.pos <- levels(m1)[2]
  m2.neg <- levels(m2)[1]
  m2.pos <- levels(m2)[2]
  case_when(m1 %in% m1.pos & m2 %in% m2.pos ~ "R1",
            m1 %in% m1.neg & m2 %in% m2.pos ~ "R2",
            m1 %in% m1.neg & m2 %in% m2.neg ~ "R3",
            m1 %in% m1.pos & m2 %in% m2.neg ~ "R4",
            TRUE ~ ""
  ) %>% factor(., levels = c("R1","R2","R3","R4"))
}




