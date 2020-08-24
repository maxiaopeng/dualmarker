
summary_cox <- function(cox.model){
  cox.summ <- summary(cox.model)

  # coef
  coef <- cbind(cox.summ$coefficients,
                        cox.summ$conf.int) %>%
    as.data.frame() %>%
    rownames_to_column("term")
  coef <- coef[,c(1,2,3,4,6,9,10)] %>%
    setNames(c("term","coef","HR","coef.se", "pval",
               "lower.95","upper.95"))
  # test
  test <- bind_rows(cox.summ[c("waldtest", "logtest", "sctest")], .id = "test.method")
  # cox.zph
  cox.zph <- cox.zph(cox.model)$table
  other <- list(test = test, cox.zph = cox.zph)
  # summary
  summary <- tibble(pval.waldtest = cox.summ$waldtest[3])
  list(coef = coef, summary = summary)
}

##' dm_cox_core
##' evaluate the logistic regression of dual marker
##' @param data
##' @param marker1
##' @param marker2
##'
.dm_cox_core <- function(data, time, event, marker1, marker2, na.rm=T){
  if(na.rm){
    data <- drop_na(data, !!sym(time), !!sym(event), !!sym(marker1), !!sym(marker2))
  }
  surv <- paste0("Surv(",time,",", event,")")
  fml.m0 <- paste0( surv, "~ 1") %>% as.formula()
  fml.m1 <- paste0( surv, " ~ ", marker1) %>% as.formula()
  fml.m2 <- paste0( surv, " ~ ", marker2) %>% as.formula()
  fml.md <- paste0( surv, " ~ ", marker1, "+", marker2) %>% as.formula()
  fml.md.int <- paste0( surv , " ~ ", marker1, "*", marker2) %>% as.formula()
  cox.m0 <- coxph(formula = fml.m0, data = data)
  cox.m1 <- coxph(formula = fml.m1, data = data)
  cox.m2 <- coxph(formula = fml.m2, data = data)
  cox.md <- coxph(formula = fml.md, data = data)
  cox.md.int <- coxph(formula = fml.md.int, data = data)

  summ.m1 <- summary_cox(cox.m1)
  summ.m2 <- summary_cox(cox.m2)
  summ.md <- summary_cox(cox.md)
  summ.md.int <- summary_cox(cox.md.int)

  # m1
  out.m1 <- summ.m1$summary
  out.m1$HR <- summ.m1$coef$HR[1]
  #out.m1$pval.vsNull <- anova(cox.m0, cox.m1, test ="Chisq")$`P(>|Chi|)`[2]
  colnames(out.m1) %<>% paste0(.,"_m1")
  # m2
  out.m2 <- summ.m2$summary
  out.m2$HR <- summ.m2$coef$HR[1]
  #out.m2$pval.vsNull <- anova(cox.m0, cox.m2, test ="Chisq")$`P(>|Chi|)`[2]
  colnames(out.m2) %<>% paste0(.,"_m2")

  # dual marker, no interaction
  out.md <- summ.md$summary
  #out.md$pval.vsNull <- anova(logit.m0, logit.md, test ="Chisq")$`Pr(>Chi)`[2]
  out.md$pval.vsM1 <- anova(cox.m1, cox.md, test ="Chisq")$`P(>|Chi|)`[2]
  out.md$pval.vsM2 <- anova(cox.m2, cox.md, test ="Chisq")$`P(>|Chi|)`[2]
  colnames(out.md) %<>% paste0(.,"_md")

  # dual marker with interaction
  out.md.int <- summ.md.int$summary
  out.md.int$pval.vsM1 <- anova(cox.m1, cox.md.int, test ="Chisq")$`P(>|Chi|)`[2]
  out.md.int$pval.vsM2 <- anova(cox.m2, cox.md.int, test ="Chisq")$`P(>|Chi|)`[2]
  out.md.int$pval.vsMd <- anova(cox.md, cox.md.int, test ="Chisq")$`P(>|Chi|)`[2]
  colnames(out.md.int) %<>% paste0(.,"_md.int")
  # merge all
  bind_cols(out.m1, out.m2, out.md, out.md.int)
}

##' dm_cox
##' evaluate the logistic regression of dual marker
##' @param data
##' @param response
##' @param response_pos
##' @param response_neg
##' @param marker1
##' @param marker2
##' @param num_cut_method marker cut method, [none, roc, median]
##' @return dual marker logistic regression summary
dm_cox <- function(data, time, event,
                   marker1, marker2,
                   binarization = F,
                   num_cut_method="none",
                   m1_cat_pos = NULL, m1_cat_neg = NULL,
                   m2_cat_pos = NULL, m2_cat_neg = NULL,
                   na.rm=T){
  cutpoint.m1 <- NA
  cutpoint.m2 <- NA
  # run logistic regression
  if(!binarization){
    data$.m1 <- data[[marker1]]
    data$.m2 <- data[[marker2]]
    out <- .dm_cox_core(data = data,
                        time= time, event=event,
                        marker1 = ".m1", marker2 = ".m2", na.rm = na.rm)
  }else{
    # m1
    res <- binarize_data(x = data[[marker1]], datatype = "auto",
                cat_pos = m1_cat_pos, cat_neg = m1_cat_neg,
                num_cut_method = num_cut_method)
    data$.m1 <- res$data
    cutpoint.m1 <- res$cutpoint
    # m2
    res <- binarize_data(x = data[[marker2]], datatype = "auto",
                       cat_pos = m2_cat_pos, cat_neg = m2_cat_neg,
                       num_cut_method = num_cut_method)
    data$.m2 <- res$data
    cutpoint.m2 <- res$cutpoint

    out <- .dm_cox_core(data = data,
                          time = time, event = event,
                          marker1 = ".m1", marker2 = ".m2",
                          na.rm = na.rm)
  }

  out.basic <- tibble(time = time,
                      surv.even = event,
                      m1=marker1, m2 =marker2,
                      marker.cut.method = num_cut_method,
                      cutpoint.m1 = cutpoint.m1,
                      cutpoint.m2 = cutpoint.m2)
  bind_cols(out.basic, out)
}

