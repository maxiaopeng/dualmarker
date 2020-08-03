##' summarize logistic regression model
##' @param logit.model logistic regression model
##' @export
sm_summary_logit <- function(logit.model){
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
    auc.stats(data = d, response = response,
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
##' @param outcome.pos
##' @param outcome.neg
##' @param marker1
##' @param marker2
.dm_logit <- function(data, outcome, marker1, marker2, binary.regression=T, na.rm=T){
  if(na.rm){
    data %<>% drop_na( !!sym(outcome), !!sym(marker1), !!sym(marker2))
  }
  assert_that(all(levels(data[[outcome]]) == c("neg","pos")),
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
    d <- data %>% group_by( !!sym(outcome), !!sym(marker1), !!sym(marker2)) %>%
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
  summ.m1 <- sm_summary_logit(logit.m1)
  summ.m2 <- sm_summary_logit(logit.m2)
  summ.md <- sm_summary_logit(logit.md)
  summ.md.int <- sm_summary_logit(logit.md.int)

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
  colnames(out.md.int) %<>% paste0(.,"_md.int")
  # merge all
  bind_cols(out.m1, out.m2, out.md, out.md.int)
}

##' dm_logit
##' evaluate the logistic regression of dual marker
##' @param data
##' @param outcome
##' @param outcome.pos
##' @param outcome.neg
##' @param marker1
##' @param marker2
##' @param num.cut.method marker cut method, [none, roc, median]
##' @return dual marker logistic regression summary
dm_logit <- function(data, outcome, outcome.pos, outcome.neg=NULL,
                     marker1, marker2, num.cut.method="none",
                     m1.cat.pos = NULL, m1.cat.neg = NULL,
                     m2.cat.pos = NULL, m2.cat.neg = NULL,
                     na.rm=T){
  n.marker.cat = 0 # number of markers which is binary, 0, 1 or 2
  cutpoint.m1 <- NA
  cutpoint.m2 <- NA
  # prep .outcome
  data$.outcome <- binarize.cat(x = data[[outcome]],
                                pos = outcome.pos, neg = outcome.neg)
  assert_that(num.cut.method %in% c("none","roc", "median"),
              msg = "num.cut.method should be [none, roc, median]")
  # prep .m1
  if(datatype.num.cat(data[[marker1]]) == "num"){
    if(num.cut.method=="none"){
      data$.m1 <- data[[marker1]]
    }else{
      cutpoint.m1 <- cutpoint(x = data[[marker1]],
                              method = num.cut.method,
                              outcome = data[[outcome]],
                              outcome.pos = outcome.pos,
                              outcome.neg = outcome.neg)
      data$.m1 <- binarize.num(x = data[[marker1]],
                               cutpoint = cutpoint.m1,
                               label.neg = "low", label.pos = "high")
      n.marker.cat <- n.marker.cat+1
      }
  }else{
    assert_that(!is.null(m1.cat.pos) && !is.null(m1.cat.neg), msg="m1.cat.pos, m1.cat.neg should not be NULL")
    data$.m1 <- binarize.cat(x = data[[marker1]], pos = m1.cat.pos, neg = m1.cat.neg)
  }
  # prep .m2
  if(datatype.num.cat(data[[marker2]]) == "num"){
    if(num.cut.method=="none"){
      data$.m2 <- data[[marker2]]
    }else{
      cutpoint.m2 <- cutpoint(x = data[[marker2]],
                              method = num.cut.method,
                              outcome = data[[outcome]],
                              outcome.pos = outcome.pos,
                              outcome.neg = outcome.neg)
      data$.m2 <- binarize.num(x = data[[marker2]],
                               cutpoint = cutpoint.m2,
                               label.neg = "low", label.pos = "high")
      n.marker.cat <- n.marker.cat+1
    }
  }else{
    assert_that(!is.null(m2.cat.pos) && !is.null(m2.cat.neg), msg="m2.cat.neg, m2.cat.neg should not be NULL")
    data$.m2 <- binarize.cat(x = data[[marker2]], pos = m1.cat.pos, neg = m2.cat.neg)
  }
  # run logistic regression
  out <- .dm_logit(data = data,
                   outcome = '.outcome',
                   marker1 = ".m1", marker2 = ".m2",
                   binary.regression = n.marker.cat!=2,
                   na.rm = na.rm)
  out.basic <- tibble(outcome = outcome,
                      outcome.pos = toString(outcome.pos),
                      outcome.neg = toString(outcome.neg),
                      m1=marker1, m2 =marker2,
                      marker.cut.method = num.cut.method,
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

##' the first level as negative, 2nd level as positve
##' for (x1, x2):
##' (pos, pos) => R1, first quadrant
##' (neg, pos) => R2, 2nd quadrant
##' (neg, neg) => R3, 3rd quadrant
##' (pos, neg) => R4, 4th quadrant
##' @param m1 factor with 2 levels
##' @param m2 factor with 2 levels
##'
.label.quadrant <- function(m1, m2){
  assert_that(class(m1) == "factor" && nlevels(m1) == 2,
              msg = "marker1 should be factor with 2 levels")
  assert_that(class(m2) == "factor" && nlevels(m2) == 2,
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
            )
}

##' @param data
##' @param outcome should be [ pos, neg ]
.dm_4quadrant_count <- function(data, outcome=".outcome", marker1=".m1", marker2=".m2",na.rm=T){
  # check input
  assert_that(class(data[[outcome]]) == "factor" &&
                nlevels(data[[outcome]]) == 2,
              msg = "outcome should be factors with 2 levels")
  assert_that(all(levels(data[[outcome]]) == c("neg","pos")),
              msg = "outcome level should be ['neg','pos']")
  assert_that(class(data[[marker1]]) == "factor" &&
                nlevels(data[[marker1]]) == 2,
              msg = "marker1 should be factors with 2 levels")
  assert_that(class(data[[marker2]]) == "factor" &&
                nlevels(data[[marker2]]) == 2,
              msg = "marker2 should be factors with 2 levels")
  # prep data
  if(na.rm){
    data %<>% drop_na( !!sym(outcome), !!sym(marker1), !!sym(marker2))
  }
  data$.quadrant <- .label.quadrant(data[[marker1]], data[[marker2]])
  total.n <- data %>% group_by(.quadrant) %>% count() %>% pull(n)
  names(total.n) <- c("R1","R2","R3","R4")
  pos.n <- data %>% dplyr::filter(!!sym(outcome) %in% "pos") %>%
    group_by(.quadrant) %>% count() %>% pull(n)
  names(pos.n) <- c("R1","R2","R3","R4")
  list(pos.n= pos.n, total.n = total.n)
}

##' dm_4quadrant
##' evaluate the logistic regression of dual marker
##' @param data
##' @param outcome
##' @param outcome.pos
##' @param outcome.neg
##' @param marker1
##' @param marker2
##' @param num.cut.method marker cut method, [none, roc, median]
##' @return dual marker logistic regression summary
dm_4quadrant <- function(data, outcome, outcome.pos, outcome.neg=NULL,
                     marker1, marker2, num.cut.method="none",
                     m1.cat.pos = NULL, m1.cat.neg = NULL,
                     m2.cat.pos = NULL, m2.cat.neg = NULL,
                     na.rm=T){
  cutpoint.m1 <- NA
  cutpoint.m2 <- NA
  # prep .outcome
  data$.outcome <- binarize.cat(x = data[[outcome]],
                                pos = outcome.pos, neg = outcome.neg)
  # prep .m1
  if(datatype.num.cat(data[[marker1]]) == "num"){
    assert_that(num.cut.method %in% c("roc", "median"),
                msg = "num.cut.method should be [roc, median]")
    cutpoint.m1 <- cutpoint(x = data[[marker1]],
                            method = num.cut.method,
                            outcome = data[[outcome]],
                            outcome.pos = outcome.pos,
                            outcome.neg = outcome.neg)
    data$.m1 <- binarize.num(x = data[[marker1]],
                             cutpoint = cutpoint.m1,
                             label.neg = "low", label.pos = "high")
  }else{
    assert_that(!is.null(m1.cat.pos) && !is.null(m1.cat.neg),
                msg="m1.cat.pos, m1.cat.neg should not be NULL")
    data$.m1 <- binarize.cat(x = data[[marker1]], pos = m1.cat.pos, neg = m1.cat.neg)
  }
  # prep .m2
  if(datatype.num.cat(data[[marker2]]) == "num"){
    assert_that(num.cut.method %in% c("roc", "median"),
                msg = "num.cut.method should be [roc, median]")
    cutpoint.m2 <- cutpoint(x = data[[marker2]],
                            method = num.cut.method,
                            outcome = data[[outcome]],
                            outcome.pos = outcome.pos,
                            outcome.neg = outcome.neg)
    data$.m2 <- binarize.num(x = data[[marker2]],
                             cutpoint = cutpoint.m2,
                             label.neg = "low", label.pos = "high")
  }else{
    assert_that(!is.null(m2.cat.pos) && !is.null(m2.cat.neg),
                msg="m2.cat.neg, m2.cat.neg should not be NULL")
    data$.m2 <- binarize.cat(x = data[[marker2]], pos = m1.cat.pos, neg = m2.cat.neg)
  }
  # run 4quadrant analysis
  out <- .dm_4quadrant_count(data, outcome=".outcome",
                marker1=".m1", marker2=".m2",na.rm=T)
  out$data <- data
  out$stats <- .quadrant.transform(out$pos.n, out$total.n)
  out$param <- tibble(outcome = outcome,
                  outcome.pos = toString(outcome.pos),
                  outcome.neg = toString(outcome.neg),
                  m1=marker1, m2 =marker2,
                  marker.cut.method = num.cut.method,
                  cutpoint.m1 = cutpoint.m1,
                  cutpoint.m2 = cutpoint.m2)
  out
}
