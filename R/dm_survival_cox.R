
#' summary of cox model
#'
#' @param cox.model cox model
#'
#' @return a list of 'coef','glance' and 'cox.zph'
.summary_cox <- function(cox.model, cindex=T){
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

  # c-index
  res.cindex <- NA
  if(cindex){
    errFlag <- F
    res.cindex <- tryCatch({
      cindex_stats(coxph.fit = cox.model) %>%
        dplyr::select(CPE)
    }, error = function(e) errFlag <<- T,
    warning = function(w) errFlag <<- T
    )
    if(errFlag) res.cindex <- NA
  }

  list(coef = coef, glance = glance, cindex = res.cindex)
}

#' dm_cox_core
#'
#' Evaluate the logistic regression of dual marker
#'
#' @param data dataframe
#' @param marker1 marker1
#' @param marker2 marker2
#' @return stats
.dm_cox_core <- function(data, time, event, marker1, marker2, covariates=NULL,  cindex=T){
  data <- tidyr::drop_na(data, !!sym(time), !!sym(event),
                           !!sym(marker1), !!sym(marker2))
  data %<>% dplyr::rename(.time = !!sym(time),
                           .event = !!sym(event),
                           .m1 = !!sym(marker1),
                           .m2 = !!sym(marker2))
  str.cf <- ""
  if(!is.null(covariates)){
    str.cf <- paste0("+",paste(covariates, collapse = "+"))
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
  summ <- list(SM1 = cox.m1, SM2 = cox.m2,
               DM = cox.md, DMI = cox.mdi) %>%
    map(.f = .summary_cox, cindex = cindex)
  # model
  summ.coef <- map(summ, "coef") %>% bind_rows(.id = "model")
  summ.glance <- map(summ, "glance") %>% bind_rows(.id = "model")
  summ.cindex <- map(summ, "cindex") %>% bind_rows(.id = "model")

  #
  pval.m1.vs.null <- anova(cox.m1, cox.m0, test ="Chisq")$`P(>|Chi|)`[2]
  pval.m2.vs.null <- anova(cox.m2, cox.m0, test ="Chisq")$`P(>|Chi|)`[2]
  pval.m1.vs.md <- anova(cox.m1, cox.md, test ="Chisq")$`P(>|Chi|)`[2]
  pval.m2.vs.md <- anova(cox.m2, cox.md, test ="Chisq")$`P(>|Chi|)`[2]
  pval.m1.vs.mdi <- anova(cox.m1, cox.mdi, test ="Chisq")$`P(>|Chi|)`[2]
  pval.m2.vs.mdi <- anova(cox.m2, cox.mdi, test ="Chisq")$`P(>|Chi|)`[2]
  cmp.model <- tibble(pval_SM1_vs_NULL = pval.m1.vs.null,
                      pval_SM2_vs_NULL = pval.m2.vs.null,
                      pval_SM1_vs_DM = pval.m1.vs.md,
                      pval_SM2_vs_DM = pval.m2.vs.md,
                      pval_SM1_vs_DMI = pval.m1.vs.mdi,
                      pval_SM2_vs_DMI = pval.m2.vs.mdi)
  list(coef = summ.coef, glance = summ.glance,
       cindex = summ.cindex, cmp.model = cmp.model)
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
#' @param covariates  covariates
#'
#' @return summary of cox model
dm_cox <- function(data, time, event,
                   marker1, marker2,
                   covariates=NULL,
                   m1.binarize = F,
                   m2.binarize = F,
                   m1.num.cut = "median",
                   m1.cat.pos = NULL,
                   m1.cat.neg = NULL,
                   m2.num.cut = "median",
                   m2.cat.pos = NULL,
                   m2.cat.neg = NULL,
                   cindex = T)
{
  .assert_colname(data, c(time, event, marker1, marker2, covariates))
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
                          covariates = covariates,
                          marker1 = ".m1", marker2 = ".m2")
  # extract key cox info
  out.cox.coef <- out.cox$coef %>%
    .collapse_df(df = ., id_cols = c("model","term"),
                 var_cols = c("estimate","p.value"))
  out.cox.aic <- out.cox$glance %>%
    .collapse_df(df = ., id_cols = "model", var_cols = c("AIC"))
  if(cindex){
    out.cox.cindex <- out.cox$cindex %>%
      .collapse_df(df = ., id_cols = "model", var_cols = c("CPE"))
  }else{
    out.cox.cindex <- NULL
  }

  out.cox.cmp.model <- out.cox$cmp.model

  out.cox.key <- bind_cols( out.cox.coef, out.cox.aic, out.cox.cindex, out.cox.cmp.model)

  # basic info
  out.basic <- tibble(time = time,
                      even = event,
                      m1=marker1, m2 =marker2,
                      covariates = toString(covariates),
                      cutpoint_m1 = cutpoint.m1,
                      cutpoint_m2 = cutpoint.m2,
                      m1_cat_pos = toString(m1.cat.pos),
                      m1_cat_neg = toString(m1.cat.neg),
                      m2_cat_pos = toString(m2.cat.pos),
                      m2_cat_neg = toString(m2.cat.neg))
  out <- bind_cols(out.basic, out.cox.key)
  colnames(out) %<>% str_replace_all("\\.m","m")
  out
}

