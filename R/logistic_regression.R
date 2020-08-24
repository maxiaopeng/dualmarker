###########################################
### logistic regression of dual markers
###########################################

#' summarize logistic regression model
#'
#' summarize logistic regression model, including the coefficient, AIC, and ROC/AUC of fitted values
#'
#' @param logistic_model logistic regression model
#' @return list of 'coef' and 'summary'
summary_logistic_reg <- function(logistic_model){
  # 1. parameters
  coef <- broom::tidy(logistic_model)

  # 2 Likelihood Ratio Test
  #lrt <- anova(update(logistic_model, ~1), logistic_model, test="Chisq")
  #lrt <- res.nag$Likelihood.ratio.test
  # equal to
  # lmtest::lrtest(logistic_model) # likehood ratio test

  # 3 Hosmer-Lemeshow Test
  #library(MKmisc)
  #MKmisc::HLgof.test(fit = fitted(logistic_model), obs = logistic_model$data$.response)

  # 4 pesudo-R
  #res.nag <- rcompanion::nagelkerke(logistic_model)
  #pR2 <- res.nag$Pseudo.R.squared.for.model.vs.null

  # 5. ROC
  auc <- auc.lower95 <- auc.upper95 <- NA
  errFlag <- F
  res.auc <- tryCatch({
    d <- logistic_model$data
    d$.fitted <- fitted(logistic_model)
    response <- all.vars(logistic_model$formula)[1]
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
  summary <- tibble(deviance = logistic_model$deviance,
                    deviance.null = logistic_model$null.deviance,
                    deviance.diff = logistic_model$null.deviance - logistic_model$deviance,
                    AIC = logistic_model$aic,
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

#' dual marker analysis for logistic regression(core)
#'
#' evaluate the logistic regression of dual marker
#'
#' @param data data frame
#' @param response factor, 2 level, 'neg' and 'pos'
#' @param marker1
#' @param marker2
.dm_logit_core <- function(data, response, marker1, marker2, binary_regression=T, na_rm=T){
  if(na_rm){
    data %<>% drop_na( !!sym(response), !!sym(marker1), !!sym(marker2))
  }
  assert_that(all(levels(data[[response]]) == c("neg","pos")),
              msg = "response level should be ['neg','pos']")
  if(binary_regression){
    fml.m0 <- paste0( response, "~ 1") %>% as.formula()
    fml.m1 <- paste0( response, " ~ ", marker1) %>% as.formula()
    fml.m2 <- paste0( response, " ~ ", marker2) %>% as.formula()
    fml.md <- paste0( response, " ~ ", marker1, "+", marker2) %>% as.formula()
    fml.md.int <- paste0( response , " ~ ", marker1, "*", marker2) %>% as.formula()
    logit.m0 <- stats::glm(formula = fml.m0, data = data, family = binomial(link="logit"))
    logit.m1 <- stats::glm(formula = fml.m1, data = data, family = binomial(link="logit"))
    logit.m2 <- stats::glm(formula = fml.m2, data = data, family = binomial(link="logit"))
    logit.md <- stats::glm(formula = fml.md, data = data, family = binomial(link="logit"))
    logit.md.int <- stats::glm(formula = fml.md.int, data = data, family = binomial(link="logit"))
  }else{
    d <- data %>% dplyr::group_by( !!sym(response), !!sym(marker1), !!sym(marker2)) %>%
      dplyr::count() %>%
      tidyr::spread(key = response, value = "n") %>% ungroup()
    fml.m0 <- paste0( "cbind(pos, neg) ~ 1") %>% as.formula()
    fml.m1 <- paste0( "cbind(pos, neg) ~ ", marker1) %>% as.formula()
    fml.m2 <- paste0( "cbind(pos, neg) ~ ",marker2) %>% as.formula()
    fml.md <- paste0( "cbind(pos, neg) ~ ", marker1, "+", marker2) %>% as.formula()
    fml.md.int <- paste0( "cbind(pos, neg) ~ ", marker1, "*", marker2) %>% as.formula()
    logit.m0 <- stats::glm(formula = fml.m0, data = d, family = binomial(link="logit"))
    logit.m1<- stats::glm(formula = fml.m1, data = d, family = binomial(link="logit"))
    logit.m2 <- stats::glm(formula = fml.m2, data = d, family = binomial(link="logit"))
    logit.md <- stats::glm(formula = fml.md, data = d, family = binomial(link="logit"))
    logit.md.int <- stats::glm(formula = fml.md.int, data = d, family = binomial(link="logit"))
  }
  # summary of model
  summ.m1 <- summary_logistic_reg(logit.m1)
  summ.m2 <- summary_logistic_reg(logit.m2)
  summ.md <- summary_logistic_reg(logit.md)
  summ.md.int <- summary_logistic_reg(logit.md.int)

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

#' Logistic regression for dual marker
#'
#' Evaluate the logistic regression of dual marker
#'
#' @param data data.frame
#' @param response response variables
#' @param response_pos positive values of response
#' @param response_neg negative values of response
#' @param marker1 marker1 variable
#' @param marker2 marker2 variable
#' @param binarization
#' @param binary_regression
#' @param m1_cat_pos positive value(s) if marker1 is categorical
#' @param m1_cat_neg negative value(s) if marker1 is categorical
#' @param m2_cat_pos positive value(s) if marker2 is categorical
#' @param m2_cat_neg negative value(s) if marker2 is categorical
#' @param na_rm remove NA, default TRUE
#' @param num_cut_method marker cut method. Possible values can be 'none'(default) and 'roc', 'median'
#' @return summary of dual marker logistic regression
#' @export
dm_logit <- function(data, response, response_pos, response_neg=NULL,
                     marker1, marker2,
                     binarization = F,
                     binary_regression=T,
                     num_cut_method="none",
                     m1_cat_pos = NULL, m1_cat_neg = NULL,
                     m2_cat_pos = NULL, m2_cat_neg = NULL,
                     na_rm=T){
  cutpoint.m1 <- NA
  cutpoint.m2 <- NA
  data$.response <- binarize_cat(x = data[[response]],
                                pos = response_pos, neg = response_neg)
  # run logistic regression
  if(!binarization){
    data$.m1 <- data[[marker1]]
    data$.m2 <- data[[marker2]]
    out <- .dm_logit_core(data = data,
                     response = ".response",
                     marker1 = ".m1", marker2 = ".m2",
                     binary_regression = T,
                     na_rm = na_rm)
  }else{
    res.4quadrant <- dm_4quadrant_response(data = data, response = response,
                                  response_pos = response_pos, response_neg = response_neg,
                                  marker1 = marker1, marker2 = marker2, num_cut_method = num_cut_method,
                                  m1_cat_pos = m1_cat_pos, m1_cat_neg = m1_cat_neg,
                                  m2_cat_pos = m2_cat_pos, m2_cat_neg = m2_cat_neg,
                                  na_rm = na_rm)
    out <- .dm_logit_core(data = res.4quadrant$data,
                     response = '.response',
                     marker1 = ".m1", marker2 = ".m2",
                     binary_regression = binary_regression,
                     na_rm = na_rm)
    cutpoint.m1 <- res.4quadrant$param$cutpoint.m1
    cutpoint.m2 <- res.4quadrant$param$cutpoint.m2
  }

  out.basic <- tibble(response = response,
                      response_pos = toString(response_pos),
                      response_neg = toString(response_neg),
                      m1=marker1, m2 =marker2,
                      marker.cut.method = num_cut_method,
                      cutpoint.m1 = cutpoint.m1,
                      cutpoint.m2 = cutpoint.m2)
  bind_cols(out.basic, out)
}

# dm_logit_plot <- function(dm.summ, type = "deviance.diff"){
#   # explain variables
#   d <- dm.summ %>% dplyr::select(contains(type)) %>%
#     dplyr::select(-contains("pval")) %>%
#     tidyr::gather()
#   d %>%
#     ggplot(aes(x=key,y = value))+
#     geom_bar(stat = "identity")+
#     scale_x_discrete(labels = c(dm.summ$m1,dm.summ$m2, "Dual","Dual_int"))+
#     labs(y = type, x= "")
# }

