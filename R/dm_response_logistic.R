#' summarize logistic regression model
#'
#' summarize logistic regression model, including the coefficient, AIC, and ROC/AUC of fitted values
#'
#' @param logit.model logistic regression model
#' @return list of 'coef' and 'summary'
.summary_logit <- function(logit.model, auc=F){
  # 1. coef
  coef <- broom::tidy(logit.model)
  glance <- broom::glance(logit.model)

  # 2 Likelihood Ratio Test
  #lrt <- anova(update(logit.model, ~1), logit.model, test="Chisq")
  #lrt <- res.nag$Likelihood.ratio.test
  # equal to
  # lmtest::lrtest(logit.model) # likehood ratio test

  # 3 Hosmer-Lemeshow Test
  #library(MKmisc)
  #MKmisc::HLgof.test(fit = fitted(logit.model), obs = logit.model$data$.response)

  # 4 pesudo-R
  #res.nag <- rcompanion::nagelkerke(logit.model)
  #pR2 <- res.nag$Pseudo.R.squared.for.model.vs.null

  # 5. ROC
  res.auc <- NA
  if(auc){
    errFlag <- F
    res.auc <- tryCatch({
      d <- logit.model$data
      d$.fitted <- fitted(logit.model)
      response <- all.vars(logit.model$formula)[1]
      response.level <- levels(d[[response]])
      auc_stats(data = d, response = response,
                case= response.level[2],
                control= response.level[1],
                predictor = ".fitted") %>%
        dplyr::select(auc, auc.lower95, auc.upper95)
    }, error = function(e) errFlag <<- T,
    warning = function(w) errFlag <<- T
    )
    if(errFlag)  res.auc <- NA
  }
  list(coef = coef,
    glance = glance,
    auc = res.auc)
}


#' dual marker analysis for logistic regression(core)
#'
#' evaluate the logistic regression of dual marker
#'
#' @param data data frame
#' @param response factor, 2 level, 'neg' and 'pos'
#' @param marker1 marker1
#' @param marker2 marker2
.dm_logit_core <- function(data, response, marker1, marker2, confound.factor=NULL, na.rm=T, auc=F){
  .assert_colname(data, c(response, marker1, marker2))
  assert_that(all(levels(data[[response]]) == c("neg","pos")),
              msg = "response level should be ['neg','pos']")
  data <- data %>% dplyr::rename(.response = !!sym(response),
                                 .m1 = !!sym(marker1),
                                 .m2 = !!sym(marker2))
  if(na.rm){
    data %<>% tidyr::drop_na(.response, .m1, .m2)
  }
  str.cf <- ""
  if(!is.null(confound.factor)){
    str.cf <- paste0("+",paste(confound.factor, collapse = "+"))
  }
  fml.m0 <- paste0(".response ~ 1", str.cf)
  fml.m1 <- paste0(".response ~ .m1", str.cf)
  fml.m2 <- paste0(".response ~ .m2", str.cf)
  fml.md <- paste0(".response ~ .m1 + .m2", str.cf)
  fml.mdi <- paste0(".response ~ .m1 * .m2", str.cf)
  logit.m0 <- stats::glm(formula = as.formula(fml.m0), data = data, family = binomial(link="logit"))
  logit.m1 <- stats::glm(formula = as.formula(fml.m1), data = data, family = binomial(link="logit"))
  logit.m2 <- stats::glm(formula = as.formula(fml.m2), data = data, family = binomial(link="logit"))
  logit.md <- stats::glm(formula = as.formula(fml.md), data = data, family = binomial(link="logit"))
  logit.mdi <- stats::glm(formula = as.formula(fml.mdi), data = data, family = binomial(link="logit"))
  # summary of model
  summ <- list(m1 = logit.m1, m2 =logit.m2,
               md =logit.md, mdi = logit.mdi) %>%
    map(.f = .summary_logit, auc=auc)

  # model
  summ.coef <- map(summ, "coef") %>% bind_rows(.id = "model")
  summ.glance <- map(summ, "glance") %>% bind_rows(.id = "model")
  summ.auc <- map(summ, "auc") %>% bind_rows(.id = "model")
  # model comparison
  pval.m1.vs.null <- anova(logit.m0, logit.m1, test ="Chisq")$`Pr(>Chi)`[2]
  pval.m2.vs.null <- anova(logit.m0, logit.m2, test ="Chisq")$`Pr(>Chi)`[2]
  pval.m1.vs.md <- anova(logit.m1, logit.md, test ="Chisq")$`Pr(>Chi)`[2]
  pval.m2.vs.md <- anova(logit.m2, logit.md, test ="Chisq")$`Pr(>Chi)`[2]
  pval.m1.vs.mdi <- anova(logit.m1, logit.mdi, test ="Chisq")$`Pr(>Chi)`[2]
  pval.m2.vs.mdi <- anova(logit.m2, logit.mdi, test ="Chisq")$`Pr(>Chi)`[2]
  cmp.model <- tibble(pval.m1.vs.null = pval.m1.vs.null,
                     pval.m2.vs.null= pval.m2.vs.null,
                     pval.m1.vs.md = pval.m1.vs.md,
                     pval.m2.vs.md = pval.m2.vs.md,
                     pval.m1.vs.mdi = pval.m1.vs.mdi,
                     pval.m2.vs.mdi = pval.m2.vs.mdi)
  list(coef = summ.coef, glance = summ.glance, auc = summ.auc, cmp.model = cmp.model)
}

#' Logistic regression for dual marker
#'
#' Evaluate the logistic regression of dual marker
#'
#' @param data data.frame
#' @param response response variables
#' @param response.pos positive values of response
#' @param response.neg negative values of response
#' @param marker1 marker1 variable
#' @param marker2 marker2 variable
#' @param m1.num.cut cut method/value(s) if marker1 is numeric
#' @param m1.cat.pos positive value(s) if marker1 is categorical
#' @param m1.cat.neg negative value(s) if marker1 is categorical
#' @param m2.num.cut cut method/value(s) if marker2 is numeric
#' @param m2.cat.pos positive value(s) if marker2 is categorical
#' @param m2.cat.neg negative value(s) if marker2 is categorical
#' @param na.rm remove NA, default TRUE
#' @return summary of dual marker logistic regression
dm_logit <- function(data, response, response.pos, response.neg=NULL,
                     marker1, marker2,confound.factor=NULL,
                     m1.binarize, m2.binarize,
                     m1.num.cut = "median", m1.cat.pos = NULL, m1.cat.neg = NULL,
                     m2.num.cut = "median", m2.cat.pos = NULL, m2.cat.neg = NULL,
                     na.rm=T, auc=F){
  .assert_colname(data, c(marker1, marker2, response))
  # prep .response, .m1, .m2
  data$.response <- binarize_cat(x = data[[response]],
                                pos = response.pos, neg = response.neg)
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
  # run logit regression
  out.logit <- .dm_logit_core(data = data,
                     response = ".response",
                     marker1 = ".m1", marker2 = ".m2",
                     confound.factor= confound.factor,
                     auc = auc,
                     na.rm = na.rm)
  # extract key logit info
  out.logit.coef <- out.logit$coef %>%
    dplyr::filter(! term %in% "(Intercept)") %>%
    .collapse_df(df = ., id_cols = c("model","term"),
                 var_cols = c("estimate","p.value"))
  if(auc){
    out.logit.auc <-  out.logit$auc %>%
      .collapse_df(df = ., id_cols = "model", var_cols = c("auc"))
  }else{
    out.logit.auc <- NULL
  }
  out.logit.aic <- out.logit$glance %>%
    .collapse_df(df = ., id_cols = "model", var_cols = c("AIC"))
  out.logit.cmp.model <- out.logit$cmp.model

  out.logit.key <- do.call(bind_cols, list(out.logit.coef,out.logit.auc,
                                           out.logit.aic, out.logit.cmp.model))

  # basic info
  out.basic <- tibble(response = response,
                      response.pos = toString(response.pos),
                      response.neg = toString(response.neg),
                      m1=marker1, m2 =marker2,
                      confound.factor = toString(confound.factor),
                      cutpoint.m1 = cutpoint.m1,
                      cutpoint.m2 = cutpoint.m2,
                      m1.cat.pos = toString(m1.cat.pos),
                      m1.cat.neg = toString(m1.cat.neg),
                      m2.cat.pos = toString(m2.cat.pos),
                      m2.cat.neg = toString(m2.cat.neg))
  # merge basic and key logit info
  bind_cols(out.basic, out.logit.key)
}

# dm_logit_plot <- function(dm.summ, type = "deviance.diff"){
#   # explain variablesF
#   d <- dm.summ %>% dplyr::select(contains(type)) %>%
#     dplyr::select(-contains("pval")) %>%
#     tidyr::gather()
#   d %>%
#     ggplot(aes(x=key,y = value))+
#     geom_bar(stat = "identity")+
#     scale_x_discrete(labels = c(dm.summ$m1,dm.summ$m2, "Dual","Dual_int"))+
#     labs(y = type, x= "")
# }
