##' summarize logistic regression model
##' @param logit.model logistic regression model
summary_logit <- function(logit.model){
  # 1. parameters
  coef <- broom::tidy(logit.model)

  # 2 Likelihood Ratio Test
  #lrt <- anova(update(logit.model, ~1), logit.model, test="Chisq")
  #lrt <- res.nag$Likelihood.ratio.test
  # equal to
  # lmtest::lrtest(logit.model) # likehood ratio test

  # 3 Hosmer-Lemeshow Test
  #library(MKmisc)
  #MKmisc::HLgof.test(fit = fitted(logit.model), obs = logit.model$data$.outcome)

  # 4 pesudo-R
  #res.nag <- rcompanion::nagelkerke(logit.model)
  #pR2 <- res.nag$Pseudo.R.squared.for.model.vs.null

  # 5. ROC
  auc <- auc.lower95 <- auc.upper95 <- NA
  errFlag <- F
  res.auc <- tryCatch({
    d <- logit.model$data
    d$.fitted <- fitted(logit.model)
    response <- all.vars(logit.model$formula)[1]
    response.level <- levels(d[[response]])
    auc_stats(data = d, response = response,
              case= response.level[2],
              control= response.level[1],
              predictor = ".fitted")
  }, error = function(e) errFlag <<- T,
  warning = function(w) errFlag <<- T
  )
  if(!errFlag){
    auc <- res.auc$auc[1]
    auc.lower95 <- res.auc$lower.95[1]
    auc.upper95 <- res.auc$upper.95[1]
  }
  estimate <- coef %>% select(term, estimate) %>%
    as.data.frame %>% column_to_rownames("term") %>%
    t %>% as_tibble()
  colnames(estimate) %<>% paste0("estimate.", .) %>%
    make.names() %>% str_replace_all("\\.+","\\.") %>%
    str_replace("\\.$","")
  summary <- tibble(deviance = logit.model$deviance,
                    deviance.null = logit.model$null.deviance,
                    deviance.diff = logit.model$null.deviance - logit.model$deviance,
                    AIC = logit.model$aic,
                    #pR2.McFadden = pR2[1,1],
                    #pR2.CoxSnell = pR2[2,1],
                    #pR2.CraggUhler = pR2[3,1]
                    auc = auc,
                    auc.lower95 = auc.lower95,
                    auc.upper95 = auc.upper95)
  summary <- bind_cols(summary, estimate)
  # return
  c(coef = list(coef),
    summary = list(summary))
}

##' dm_logit
##' evaluate the logistic regression of dual marker
##' @param data
##' @param outcome factor, level should be [neg, pos]
##' @param outcome_pos
##' @param outcome_neg
##' @param marker1
##' @param marker2
.dm_logit_core <- function(data, outcome, marker1, marker2, binary.regression=T, na.rm=T){
  if(na.rm){
    data %<>% drop_na( !!sym(outcome), !!sym(marker1), !!sym(marker2))
  }
  assertthat::assert_that(all(levels(data[[outcome]]) == c("neg","pos")),
              msg = "outcome level should be ['neg','pos']")
  if(binary.regression){
    fml.m0 <- paste0( outcome, "~ 1") %>% as.formula()
    fml.m1 <- paste0( outcome, " ~ ", marker1) %>% as.formula()
    fml.m2 <- paste0( outcome, " ~ ", marker2) %>% as.formula()
    fml.md <- paste0( outcome, " ~ ", marker1, "+", marker2) %>% as.formula()
    fml.md.int <- paste0( outcome , " ~ ", marker1, "*", marker2) %>% as.formula()
    logit.m0 <- glm(formula = fml.m0, data = data, family = binomial(link="logit"))
    logit.m1 <- glm(formula = fml.m1, data = data, family = binomial(link="logit"))
    logit.m2 <- glm(formula = fml.m2, data = data, family = binomial(link="logit"))
    logit.md <- glm(formula = fml.md, data = data, family = binomial(link="logit"))
    logit.md.int <- glm(formula = fml.md.int, data = data, family = binomial(link="logit"))
  }else{
    d <- data %>% dplyr::group_by( !!sym(outcome), !!sym(marker1), !!sym(marker2)) %>%
      dplyr::count() %>%
      spread(key = outcome, value = "n") %>% ungroup()
    fml.m0 <- paste0( "cbind(pos, neg) ~ 1") %>% as.formula()
    fml.m1 <- paste0( "cbind(pos, neg) ~ ", marker1) %>% as.formula()
    fml.m2 <- paste0( "cbind(pos, neg) ~ ",marker2) %>% as.formula()
    fml.md <- paste0( "cbind(pos, neg) ~ ", marker1, "+", marker2) %>% as.formula()
    fml.md.int <- paste0( "cbind(pos, neg) ~ ", marker1, "*", marker2) %>% as.formula()
    logit.m0 <- glm(formula = fml.m0, data = d, family = binomial(link="logit"))
    logit.m1<- glm(formula = fml.m1, data = d, family = binomial(link="logit"))
    logit.m2 <- glm(formula = fml.m2, data = d, family = binomial(link="logit"))
    logit.md <- glm(formula = fml.md, data = d, family = binomial(link="logit"))
    logit.md.int <- glm(formula = fml.md.int, data = d, family = binomial(link="logit"))
  }
  # summary of model
  summ.m1 <- summary_logit(logit.m1)
  summ.m2 <- summary_logit(logit.m2)
  summ.md <- summary_logit(logit.md)
  summ.md.int <- summary_logit(logit.md.int)

  # m1
  out.m1 <- summ.m1$summary %>% dplyr::select(-deviance.null)
  out.m1$pval.vsNull <- anova(logit.m0, logit.m1, test ="Chisq")$`Pr(>Chi)`[2]
  colnames(out.m1) %<>% paste0(.,"_m1")
  out.m1 <- cbind(deviance.null = summ.m1$summary$deviance.null, out.m1)
  # m2
  out.m2 <- summ.m2$summary %>% dplyr::select(-deviance.null)
  out.m2$pval.vsNull <- anova(logit.m0, logit.m2, test ="Chisq")$`Pr(>Chi)`[2]
  colnames(out.m2) %<>% paste0(.,"_m2")
  # dual marker, no interaction
  out.md <- summ.md$summary %>% dplyr::select(-deviance.null)
  #out.md$pval.vsNull <- anova(logit.m0, logit.md, test ="Chisq")$`Pr(>Chi)`[2]
  out.md$pval.vsM1 <- anova(logit.m1, logit.md, test ="Chisq")$`Pr(>Chi)`[2]
  out.md$pval.vsM2 <- anova(logit.m2, logit.md, test ="Chisq")$`Pr(>Chi)`[2]
  colnames(out.md) %<>% paste0(.,"_md")

  # dual marker with interaction
  out.md.int <- summ.md.int$summary %>% dplyr::select(-deviance.null)
  out.md.int$pval.vsM1 <- anova(logit.m1, logit.md.int, test ="Chisq")$`Pr(>Chi)`[2]
  out.md.int$pval.vsM2 <- anova(logit.m2, logit.md.int, test ="Chisq")$`Pr(>Chi)`[2]
  out.md.int$pval.vsMd <- anova(logit.md, logit.md.int, test ="Chisq")$`Pr(>Chi)`[2]
  colnames(out.md.int) %<>% paste0(.,"_md.int")
  # merge all
  bind_cols(out.m1, out.m2, out.md, out.md.int)
}

##' dm_logit
##' evaluate the logistic regression of dual marker
##' @param data
##' @param outcome
##' @param outcome_pos
##' @param outcome_neg
##' @param marker1
##' @param marker2
##' @param num_cut_method marker cut method, [none, roc, median]
##' @return dual marker logistic regression summary
dm_logit <- function(data, outcome, outcome_pos, outcome_neg=NULL,
                     marker1, marker2,
                     binarization = F,
                     binary.regression=T,
                     num_cut_method="none",
                     m1_cat_pos = NULL, m1_cat_neg = NULL,
                     m2_cat_pos = NULL, m2_cat_neg = NULL,
                     na.rm=T){
  cutpoint.m1 <- NA
  cutpoint.m2 <- NA
  data$.outcome <- binarize_cat(x = data[[outcome]],
                                pos = outcome_pos, neg = outcome_neg)
  # run logistic regression
  if(!binarization){
    data$.m1 <- data[[marker1]]
    data$.m2 <- data[[marker2]]
    out <- .dm_logit_core(data = data,
                     outcome = ".outcome",
                     marker1 = ".m1", marker2 = ".m2",
                     binary.regression = T,
                     na.rm = na.rm)
  }else{
    res.4quadrant <- dm_4quadrant(data = data, outcome = outcome,
                                  outcome_pos = outcome_pos, outcome_neg = outcome_neg,
                                  marker1 = marker1, marker2 = marker2, num_cut_method = num_cut_method,
                                  m1_cat_pos = m1_cat_pos, m1_cat_neg = m1_cat_neg,
                                  m2_cat_pos = m2_cat_pos, m2_cat_neg = m2_cat_neg,
                                  na.rm = na.rm)
    out <- .dm_logit_core(data = res.4quadrant$data,
                     outcome = '.outcome',
                     marker1 = ".m1", marker2 = ".m2",
                     binary.regression = binary.regression,
                     na.rm = na.rm)
    cutpoint.m1 <- res.4quadrant$param$cutpoint.m1
    cutpoint.m2 <- res.4quadrant$param$cutpoint.m2
  }

  out.basic <- tibble(outcome = outcome,
                      outcome_pos = toString(outcome_pos),
                      outcome_neg = toString(outcome_neg),
                      m1=marker1, m2 =marker2,
                      marker.cut.method = num_cut_method,
                      cutpoint.m1 = cutpoint.m1,
                      cutpoint.m2 = cutpoint.m2)
  bind_cols(out.basic, out)
}

##' plot logistic regression
##' @param dm.summ
dm_logit_plot <- function(dm.summ, type = "deviance.diff"){
  # explain variables
  d <- dm.summ %>% dplyr::select(contains(type)) %>%
    dplyr::select(-contains("pval")) %>%
    gather()
  d %>%
    ggplot(aes(x=key,y = value))+
    geom_bar(stat = "identity")+
    scale_x_discrete(labels = c(dm.summ$m1,dm.summ$m2, "Dual","Dual_int"))+
    labs(y = type, x= "")
}

