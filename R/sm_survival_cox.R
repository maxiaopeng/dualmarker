

#' dm_cox_core
#'
#' Evaluate the logistic regression of dual marker
#'
#' @param data dataframe
#' @param time time
#' @param event event
#' @param marker marker
#' @param confound.factor confound.factor
#' @param na.rm  remove NA
#'
#' @return stats
.sm_cox_core <- function(data, time, event, marker, confound.factor=NULL, na.rm=T){
  data %<>% dplyr::rename(.time = !!sym(time),
                          .event = !!sym(event),
                          .m = !!sym(marker))
  if(na.rm){data %<>% tidyr::drop_na(.time, .event, .m)}
  str.cf <- ""
  if(!is.null(confound.factor)){
    str.cf <- paste0("+",paste(confound.factor, collapse = "+"))
  }
  surv <- paste0("Surv(.time, .event)")
  fml.m <- paste0( surv, " ~ .m", str.cf)
  cox.m <- coxph(formula =  as.formula(fml.m), data = data)
  .summary_cox(cox.m)
}

#' dual maker cox analysis
#'
#' dual marker survival analysis using Cox regression
#'
#' @param data dataframe
#' @param time survival time
#' @param event survival event
#' @param na.rm remove NA
#' @param marker marker
#' @param binarize binarize
#' @param cat.pos positive value(s) if marker is categorical
#' @param cat.neg  negative value(s) if marker is categorical
#' @param num.cut marker cut method, [none, roc, median, mean]
#' @param confound.factor  confounding factors
#'
#' @return summary of cox model
sm_cox <- function(data, time, event,
                   marker, confound.factor=NULL,
                   binarize = F,
                   num.cut = "none",
                   cat.pos = NULL, cat.neg = NULL,
                   na.rm=T){
  .assert_colname(data, c(time, event, marker, confound.factor))
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
  # run cox regression
  out.cox <- .sm_cox_core(data = data,
                          time = time, event = event,
                          marker = ".m",
                          confound.factor = confound.factor,
                          na.rm = na.rm)
  # extract key cox info
  out.cox.coef <- out.cox$coef %>%
    .collapse_df(df = ., id_cols = c("term"), var_cols = c("estimate","p.value"))
  out.cox.aic <- out.cox$glance
  out.cox.cmp.model <- out.cox$cmp.model
  out.cox.key <- dplyr::bind_cols( out.cox.coef, out.cox.aic, out.cox.cmp.model)

  # basic info
  out.basic <- tibble(time = time,
                      even = event,
                      marker=marker,
                      confound.factor = toString(confound.factor),
                      cutpoint = cutpoint,
                      cat.pos = toString(cat.pos),
                      cat.neg = toString(cat.neg))
  bind_cols(out.basic, out.cox.key)
}
