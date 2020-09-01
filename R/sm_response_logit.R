
#' single marker logit regression(core)
#'
#' @param data data frame
#' @param response response variable
#' @param marker  marker
#' @param confound.factor confound.factor
#' @param na.rm remove NA
#'
#' @return summary of logit model
#'
.sm_logit_core <- function(data, response, marker, confound.factor=NULL, na.rm=T, auc=F){
  .assert_colname(data, c(response, marker))
  assert_that(all(levels(data[[response]]) == c("neg","pos")),
              msg = "response level should be ['neg','pos']")
  data <- data %>% dplyr::rename(.response = !!sym(response),
                                 .m = !!sym(marker))
  if(na.rm){
    data %<>% tidyr::drop_na(.response, .m)
  }
  str.cf <- ""
  if(!is.null(confound.factor)){
    str.cf <- paste0("+",paste(confound.factor, collapse = "+"))
  }
  fml.m <- paste0(".response ~ .m", str.cf)
  logit.m <- stats::glm(formula = as.formula(fml.m),
                         data = data, family = binomial(link="logit"))
  .summary_logit(logit.m, auc = auc)
}

#' Single marker logit regression
#'
#' @param data data frame
#' @param response response
#' @param response.pos positive values for response
#' @param response.neg negative values for response
#' @param marker marker
#' @param confound.factor confonding factor
#' @param binarize binarize for marker, default FALSE
#' @param num.cut  cut method/value for numeric marker
#' @param cat.pos positive values for marker if marker is categorical
#' @param cat.neg negative values for marker if marker is categorical
#' @param na.rm remove NA
#'
#' @return
sm_logit <- function(data, response, response.pos, response.neg=NULL,
                              marker, confound.factor=NULL,
                              binarize = F,
                              num.cut = "none",
                              cat.pos = NULL, cat.neg = NULL,
                              na.rm=T, auc=F){
  .assert_colname(data, c(response, marker))
  # prep .response, .m1, .m2
  data$.response <- binarize_cat(x = data[[response]],
                                 pos = response.pos, neg = response.neg)
  if(binarize){
    tmp <- binarize_data(x = data[[marker]], return.binary = T,
                         cat.pos = cat.pos, cat.neg = cat.neg,
                         num.cut = num.cut)
    data$.m <- tmp$data
    cutpoint <- tmp$cutpoint
  }else{
    data$.m <- data[[marker]]
    cutpoint <- NA
  }

  # run logit regression
  out.logit <- .sm_logit_core(data = data,
                              response = ".response",
                              marker = ".m",
                              confound.factor= confound.factor,
                              na.rm = na.rm, auc=auc)
  # extract key logit info
  out.logit.coef <- out.logit$coef %>%
    dplyr::filter( term %in% ".m") %>%
    .collapse_df(df = ., id_cols = "term",
                 var_cols = c("estimate","p.value"))
  if(auc){
    out.logit.auc <-  out.logit$auc
  }else{
    out.logit.auc <- NULL
  }
  out.logit.aic <- out.logit$glance

  out.logit.key <- do.call(dplyr::bind_cols, list(out.logit.coef,out.logit.auc,out.logit.aic))

  # basic info
  out.basic <- tibble(response = response,
                      response.pos = toString(response.pos),
                      response.neg = toString(response.neg),
                      marker=marker,
                      confound.factor = toString(confound.factor),
                      cutpoint = cutpoint,
                      cat.pos = toString(cat.pos),
                      cat.neg = toString(cat.neg))
  # merge basic and key logit info
  bind_cols(out.basic, out.logit.key)
}


