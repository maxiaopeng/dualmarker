#source("~/working/Biomarker_integrated_reporter/src/my_functions.R")
library(assertthat)
library(magrittr)
library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(ggplot2)
library(stringr)
library(survival)
library(survminer)
library(ggforce)
library(GGally)
library(survminer)
library(pROC)
library(rcompanion)
#library(gridExtra)
library(cowplot)

color.quadrant.1 <- c(R1="#F9BEC0",R2="#CFECF2", R3="#F1F1F1", R4="#CFD7E4")
# npg
t1 <- ggsci::pal_npg(alpha=0.1)(4)
color.quadrant.1 <- t1[c(1,2,4,3)] %>% setNames(c("R1","R2","R3","R4"))
t2 <- ggsci::pal_npg()(4)
color.quadrant.2 <- t2[c(1,2,4,3)] %>% setNames(c("R1","R2","R3","R4"))

load("inst/extdata/my_IMvigor210_data.RData")
datatype.num.cat <- function(x){
  if(class(x) %in% c("factor","character")){
    res <- "cat"
  }else if(class(x) %in% c("numeric","integer","float", "double")){
    res <- "num"
  }else{
    stop("not cat or num, please check\n")
  }
  res
}

##' cutpoint
##' get cutpoint
##' @param x
##' @param method median(default), roc, optimal_surv
##' @param outcome used if method='roc'
##' @param outcome.pos used if method='roc', positive outcome label
##' @param outcome.neg used if method='roc', negative outcome label, if NULL, use all other samples except outcome.pos
##' @param auc.best.method used if method='roc'
##' @param surv.time used if method='optimal_surv_cut', survival time used for
##' @param surv.minprop used if method='optimal_surv_cut', min proportion in high and low group
##' @return cutpoint
##' @export
cutpoint <- function(x, method="median",
                         outcome=NULL, outcome.pos=NULL, outcome.neg=NULL,
                         auc.best.method = "youden",
                         surv.time = NULL, surv.event=NULL, surv.minprop=0.3){
  assert_that(all(class(x) %in% c("numeric","integer")),
              msg = "x should be numeric")
  min <- min(x, na.rm = T)
  max <- max(x, na.rm = T)
  cutpoint <- NA
  if(method == "median"){
    cutpoint <- median(x, na.rm = T)
  }else if(method %in% c("roc")){
    assert_that(! is.null(outcome) && !is.null(outcome.pos),
                msg= "outcome and outcome.pos are required")
    df <- data.frame(predictor = x,
                       response = outcome)
    cutpoint <- auc.stats(data = df, response="response",
                            control = outcome.neg,
                            case = outcome.pos,
                            predictor = "predictor",
                            best.method = auc.best.method)$threhold[1]
  }else if(method %in% c("optimal_surv")){
    assert_that(! is.null(surv.time) && !is.null(surv.event),
                msg="surv.time and surv.event are required")
    suppressPackageStartupMessages(require(survminer))
    df <- data.frame(time = surv.time, event = surv.event, feature = x)
    surv.cutp <- do.call("surv_cutpoint",
                         list(data= df, time= "time",
                              event= "event",
                              variables = "feature",
                              minprop=surv.minprop))
    cutpoint <- surv.cutp$cutpoint[1,1]
  }else{
    stop("method should be [median, roc, optimal_surv_cut]")
  }
  assert_that(cutpoint >= min && cutpoint <= max,
              msg = paste0("cut point should be [",min, ",", max,"]"))
  cutpoint
}

##' binarize numeric values
##'
##' binarize numeric values using median(cut="median"), ROC youden cutoff(cut="youden"),
##' optimal survival(cut="opt_surv") or custome values(e.g. cut=1)
##'
##' param x vector of numeric
##' param cut can be 'median',"youden"
##' param outcome necessary if cut = "youden_ROC"
##' param label.pos label for the 'high' values, default 'high'
##' param label.neg label for the 'low' values, default 'low'
##' return binary value
##' @export
binarize.num <- function(x, cutpoint, return.binary=F,
                         label.pos = "pos", label.neg = "neg"){
  min <- min(x, na.rm = T)
  max <- max(x, na.rm = T)
  assert_that(cutpoint >= min && cutpoint <= max,
              msg = paste0("cut point should be [",min, ",", max,"]"))
  breaks <- c(min -1, cutpoint, max +1)
  if(return.binary){
    out <- cut(x, breaks = breaks, labels = c(0,1)) %>%
      as.character() %>% as.integer()
  }else{
    out <- cut(x, breaks = breaks, labels = c(label.neg,label.pos))
  }
  out
}

##' binarize categorical variable
##' @param x vector
##' @param pos positive values
##' @param neg negative values, if NULL, use all other values except 'pos', the non-pos, non-neg values in x will be converted to NA
##' @param return.binary return the binary 0(negative), 1(positive)
##' @label.pos label for positive values, default 'pos'
##' @label.neg label for negative values, default 'neg'
binarize.cat <- function(x, pos, neg=NULL, return.binary=F, label.pos = "pos", label.neg = "neg"){
  assert_that(all(class(x) %in% c("factor","character")), msg = "x should be factor/character")
  assert_that(!all(is.na(x)), msg =" all x is NA, can't be cutted")
  if("factor" %in% class(x)){ x <- as.character(x) }
  all.val <- na.omit(unique(x))
  if(is.null(neg)) neg <- setdiff(all.val, pos)
  assert_that(all(c(pos, neg) %in% all.val), msg = "pos or neg isn't in x")
  out <- base::rep(NA, times = length(x))
  if(return.binary){
    out[x %in% pos] <- 1
    out[x %in% neg] <- 0
    out <- as.integer(out)
  }else{
    out[x %in% pos] <- label.pos
    out[x %in% neg] <- label.neg
    out <- factor(out, levels = c(label.neg, label.pos))
  }
  out
}


##' @description
##' binarize data for numeric and categorical dataytpe
binarize.data <- function(x,
                        datatype="auto",
                        return.binary = F, label.pos="pos", label.neg ="neg",
                        cat.pos=NULL, cat.neg=NULL,
                        num.cut.method="median",
                        outcome=NULL, outcome.pos=NULL, outcome.neg=NULL,
                        auc.best.method = "youden",
                        surv.time = NULL, surv.event=NULL, surv.minprop=0.3){
  assert_that(datatype %in% c("auto","num","cat"), msg =" datatype should be [auto, num, cat]")
  cutpoint <- NA
  if(datatype=="auto"){
      datatype <- datatype.num.cat(x)
  }
  if( datatype == "num"){
    assert_that(num.cut.method %in% c("roc", "median", "optimal_surv"),
                msg = "num.cut.method should be [roc, median, optimal_surv]")
    cutpoint <- cutpoint(x = x, method= num.cut.method, outcome= outcome,
                         outcome.pos= outcome.pos, outcome.neg= outcome.neg,
                         auc.best.method = auc.best.method,
                         surv.time = surv.time , surv.event=surv.event,
                         surv.minprop=surv.minprop)
    m <- binarize.num(x = x, cutpoint = cutpoint, return.binary = return.binary,
                      label.neg = "low", label.pos = "high")
  }else{
    assert_that(!is.null(cat.pos), msg="cat.pos should not be NULL")
    m <- binarize.cat(x = x, pos = cat.pos, neg = cat.neg, return.binary = return.binary,
                      label.pos = label.pos, label.neg = label.neg)
  }
  list(data = m, cutpoint = cutpoint)
}

##' auc.stats
##' get statistics of AUC
##' @param data
##' @param response
##' @example
##' auc.stats(data = iris, response = "Species", case = "setosa", control = "versicolor", predictor = "Sepal.Length")
##' @export
##' @import pROC
auc.stats <- function(data, response, case, control=NULL, predictor, best.method="youden"){
  suppressPackageStartupMessages(require(pROC))
  assert_that(all(c(response, predictor) %in% colnames(data)),
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

##' @description add adjusted pvalues to the data
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
