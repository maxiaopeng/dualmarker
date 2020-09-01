
#' summary of cox model
#'
#' @param cox.model cox model
#'
#' @return a list of 'coef','glance' and 'cox.zph'
#' @export
.summary_cox <- function(cox.model){
  assert_that(is(cox.model, "coxph"), msg = "cox.model is not coxph object")

  # coef
  coef <- broom::tidy(cox.model)
  #cox.summ <- summary(cox.model)
  #coef <- cbind(cox.summ$coefficients,
  #                      cox.summ$conf.int) %>%
  #  as.data.frame() %>%
  #  rownames_to_column("term")
  #coef <- coef[,c(1,2,3,4,6,9,10)] %>%
  #  setNames(c("term","coef","HR","coef.se", "pval",
  #             "HR.lower95","HR.upper95"))
  # glance
  glance <- broom::glance(cox.model)

  # others
  #cox.zph <- cox.zph(cox.model)$table

  list(coef = coef, glance = glance)
}

#' dm_cox_core
#'
#' Evaluate the logistic regression of dual marker
#'
#' @param data dataframe
#' @param marker1 marker1
#' @param marker2 marker2
#' @return stats
.dm_cox_core <- function(data, time, event, marker1, marker2, confound.factor=NULL, na.rm=T){
  if(na.rm){
    data <- tidyr::drop_na(data, !!sym(time), !!sym(event),
                           !!sym(marker1), !!sym(marker2))
  }
  data %<>% dplyr::rename(.time = !!sym(time),
                           .event = !!sym(event),
                           .m1 = !!sym(marker1),
                           .m2 = !!sym(marker2))
  str.cf <- ""
  if(!is.null(confound.factor)){
    str.cf <- paste0("+",paste(confound.factor, collapse = "+"))
  }
  surv <- paste0("Surv(.time, .event)")
  fml.m0 <- paste0( surv, " ~ 1", str.cf)
  fml.m1 <- paste0( surv, " ~ .m1", str.cf)
  fml.m2 <- paste0( surv, " ~ .m2", str.cf)
  fml.md <- paste0( surv, " ~ .m1 + .m2", str.cf)
  fml.mdi <- paste0( surv , " ~ .m1 * .m2", str.cf)
  cox.m0 <- coxph(formula =  as.formula(fml.m0), data = data)
  cox.m1 <- coxph(formula =  as.formula(fml.m1), data = data)
  cox.m2 <- coxph(formula =  as.formula(fml.m2), data = data)
  cox.md <- coxph(formula =  as.formula(fml.md), data = data)
  cox.mdi <- coxph(formula =  as.formula(fml.mdi), data = data)

  # summary of model
  summ <- list(m1 = cox.m1, m2 = cox.m2,
               md = cox.md, mdi = cox.mdi) %>%
    map(.f = .summary_cox)
  # model
  summ.coef <- map(summ, "coef") %>% bind_rows(.id = "model")
  summ.glance <- map(summ, "glance") %>% bind_rows(.id = "model")
  #
  pval.m1.vs.null <- anova(cox.m1, cox.m0, test ="Chisq")$`P(>|Chi|)`[2]
  pval.m2.vs.null <- anova(cox.m2, cox.m0, test ="Chisq")$`P(>|Chi|)`[2]
  pval.m1.vs.md <- anova(cox.m1, cox.md, test ="Chisq")$`P(>|Chi|)`[2]
  pval.m2.vs.md <- anova(cox.m2, cox.md, test ="Chisq")$`P(>|Chi|)`[2]
  pval.m1.vs.mdi <- anova(cox.m1, cox.mdi, test ="Chisq")$`P(>|Chi|)`[2]
  pval.m2.vs.mdi <- anova(cox.m2, cox.mdi, test ="Chisq")$`P(>|Chi|)`[2]
  cmp.model <- tibble(pval.m1.vs.null = pval.m1.vs.null,
                      pval.m2.vs.null = pval.m2.vs.null,
                      pval.m1.vs.md = pval.m1.vs.md,
                      pval.m2.vs.md = pval.m2.vs.md,
                      pval.m1.vs.mdi = pval.m1.vs.mdi,
                      pval.m2.vs.mdi = pval.m2.vs.mdi)
  list(coef = summ.coef, glance = summ.glance, cmp.model = cmp.model)
}

#' dual maker cox analysis
#'
#' dual marker survival analysis using Cox regression
#'
#' @param data dataframe
#' @param marker1 marker1
#' @param marker2 marker2
#' @param time survival time
#' @param event survival event
#' @param m1.binarize  binarize m1, default FALSE
#' @param m2.binarize binarize m2, default FALSE
#' @param m1.num.cut cut method/value(s) if marker is numeric
#' @param m1.cat.pos positive value(s) of marker1 if it is categorical
#' @param m1.cat.neg negative value(s) of marker1 if it is categorical
#' @param m2.num.cut cut method/value(s) if marker2 is numeric
#' @param m2.cat.pos positive value(s) of marker2 if it is categorical
#' @param m2.cat.neg negative value(s) of marker2 if it is categorical
#' @param na.rm remove NA
#' @param confound.factor  coufounding factors
#'
#' @return summary of cox model
dm_cox <- function(data, time, event,
                   marker1, marker2,confound.factor=NULL,
                   m1.binarize, m2.binarize,
                   m1.num.cut = "median", m1.cat.pos = NULL, m1.cat.neg = NULL,
                   m2.num.cut = "median", m2.cat.pos = NULL, m2.cat.neg = NULL,
                   na.rm=T){
  .assert_colname(data, c(time, event, marker1, marker2, confound.factor))
  if(m1.binarize){
    tmp <- binarize_data(x = data[[marker1]], return.binary = T,
                         cat.pos = m1.cat.pos, cat.neg = m1.cat.neg,
                         num.cut = m1.num.cut)
    data$.m1 <- tmp$data
    cutpoint.m1 <- tmp$cutpoint
  }else{
    data$.m1 <- data[[marker1]]
    cutpoint.m1 <- NA
  }
  if(m2.binarize){
    tmp <- binarize_data(x = data[[marker2]], return.binary = T,
                         cat.pos = m2.cat.pos, cat.neg = m2.cat.neg,
                         num.cut = m2.num.cut)
    data$.m2 <- tmp$data
    cutpoint.m2 <- tmp$cutpoint
  }else{
    data$.m2 <- data[[marker2]]
    cutpoint.m2 <- NA
  }

  # run cox regression
  out.cox <- .dm_cox_core(data = data,
                          time = time, event = event,
                          confound.factor = confound.factor,
                          marker1 = ".m1", marker2 = ".m2",
                          na.rm = na.rm)
  # extract key cox info
  out.cox.coef <- out.cox$coef %>%
    .collapse_df(df = ., id_cols = c("model","term"),
                 var_cols = c("estimate","p.value"))
  out.cox.aic <- out.cox$glance %>%
    .collapse_df(df = ., id_cols = "model", var_cols = c("AIC"))
  out.cox.cmp.model <- out.cox$cmp.model

  out.cox.key <- bind_cols( out.cox.coef, out.cox.aic, out.cox.cmp.model)

  # basic info
  out.basic <- tibble(time = time,
                      even = event,
                      m1=marker1, m2 =marker2,
                      confound.factor = toString(confound.factor),
                      cutpoint_m1 = cutpoint.m1,
                      cutpoint_m2 = cutpoint.m2,
                      m1.cat.pos = toString(m1.cat.pos),
                      m1.cat.neg = toString(m1.cat.neg),
                      m2.cat.pos = toString(m2.cat.pos),
                      m2.cat.neg = toString(m2.cat.neg))
  bind_cols(out.basic, out.cox.key)
}

