

##' dual biomarker plot and statistics
##'
dm_pair <- function(data, outcome, outcome.pos, outcome.neg=NULL,
                    marker1, marker2, surv.time, surv.event,
                    num.cut.method = "median",
                    m1.cat.pos = NULL, m1.cat.neg = NULL,
                    m2.cat.pos = NULL, m2.cat.neg = NULL){
  ### single marker
  if(is.null(outcome.neg)) {
    outcome.neg <- setdiff(unique(data[[outcome]]), outcome.pos)
  }
  # g.sm <- data %>%
  #   dplyr::filter( !!sym(outcome) %in% c(outcome.pos, outcome.neg)) %>%
  #   dplyr::select(one_of(marker1, marker2,outcome)) %>%
  #   GGally::ggpairs(aes_string(color = outcome, alpha=0.5))
  g.sm <- dm_boxplot(data = data, outcome = outcome, outcome.pos = outcome.pos,
                     outcome.neg = outcome.neg,
                     marker1 = marker1, marker2 =  marker2,
                     m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
                     m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg)

  # scatterplot
  g.scatter <- dm_scatter_chart(data = data,
                                outcome = outcome,
                                outcome.pos=outcome.pos,
                                outcome.neg=outcome.neg,
                                marker1 = marker1,
                                marker2 = marker2,
                                num.cut.method = num.cut.method,
                                m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
                                m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg)
  ### four quadrant visualization
  res.4quad <- dm_4quadrant(data = clin.bmk,
                            outcome = outcome,
                            outcome.pos= outcome.pos,
                            outcome.neg= outcome.neg,
                            marker1  = marker1,
                            marker2 =  marker2,
                            num.cut.method = num.cut.method,
                            m1.cat.pos = m1.cat.pos,
                            m1.cat.neg = m1.cat.neg,
                            m2.cat.pos = m2.cat.pos,
                            m2.cat.neg = m2.cat.neg,
                            na.rm = T)

  # four quadrant plot
  g.4quad.response <- quadrant_response_chart(x = res.4quad$pos.n, n = res.4quad$total.n)

  ### logistic regression model
  # as continuous variables
  res.logit <- dm_logit(data = clin.bmk,
                        outcome = outcome,
                        outcome.pos= outcome.pos,
                        outcome.neg= outcome.neg,
                        marker1 = marker1,
                        marker2 = marker2,
                        num.cut.method = num.cut.method,
                        m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
                        m2.cat.pos = m1.cat.pos, m2.cat.neg = m2.cat.neg,
                        binarization = F)
  # barplot
  g.logit <- dm_logit_plot(res.logit)

  # ROC
  g.roc <- dm_roc(data = data,
                  outcome = outcome,
                  outcome.pos= outcome.pos,
                  outcome.neg= outcome.neg,
                  marker1 = marker1,
                  marker2 = marker2,
                  m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
                  m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg,
                  logistic.reg = T,
                  logistic.reg.int = T)
  # survival
  g.km <- dm_KMplot(data = data,
                    surv.time = surv.time,
                    surv.event = surv.event,
                    marker1 = marker1,
                    marker2 = marker2,
                    num.cut.method = num.cut.method,
                    outcome = outcome,
                    outcome.pos = outcome.pos,
                    outcome.neg = outcome.neg,
                    m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
                    m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg)
  res.cox <- dm_cox(data = data,
                    surv.time = surv.time, surv.event = surv.event,
                    marker1 = marker1, marker2 = marker2,
                    binarization = F, num.cut.method = num.cut.method,
                    m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
                    m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg)
  g.4quad.surv <- quadrant_survival_chart(data = data, time = surv.time,
                                          event = surv.event, marker1 = marker1,
                                          marker2 = marker2, num.cut.method = num.cut.method,
                                          m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
                                          m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg)

  g.all <- list(sm = g.sm,
                scatterplot = g.scatter,
                four.quadrant.response = g.4quad.response,
                logit = g.logit,
                roc = g.roc,
                km = g.km,
                four.quadrant.surv = g.4quad.surv)
  res <- list(summary.4quad = res.4quad,
              summary.logit = res.logit,
              summay.cox = res.cox
  )
  list(plot = g.all, stats = res)
}
