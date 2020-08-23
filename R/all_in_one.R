

##' dual biomarker plot and statistics
##'
dm_pair <- function(data, outcome, outcome_pos, outcome_neg=NULL,
                    marker1, marker2, time, event,
                    num_cut_method = "median",
                    m1_cat_pos = NULL, m1_cat_neg = NULL,
                    m2_cat_pos = NULL, m2_cat_neg = NULL){
  ### single marker
  if(is.null(outcome_neg)) {
    outcome_neg <- setdiff(unique(data[[outcome]]), outcome_pos)
  }
  # g.sm <- data %>%
  #   dplyr::filter( !!sym(outcome) %in% c(outcome_pos, outcome_neg)) %>%
  #   dplyr::select(one_of(marker1, marker2,outcome)) %>%
  #   GGally::ggpairs(aes_string(color = outcome, alpha=0.5))
  g.sm <- dm_boxplot(data = data, outcome = outcome, outcome_pos = outcome_pos,
                     outcome_neg = outcome_neg,
                     marker1 = marker1, marker2 =  marker2,
                     m1_cat_pos = m1_cat_pos, m1_cat_neg = m1_cat_neg,
                     m2_cat_pos = m2_cat_pos, m2_cat_neg = m2_cat_neg)

  # scatterplot
  g.scatter <- dm_scatter_chart(data = data,
                                outcome = outcome,
                                outcome_pos=outcome_pos,
                                outcome_neg=outcome_neg,
                                marker1 = marker1,
                                marker2 = marker2,
                                num_cut_method = num_cut_method,
                                m1_cat_pos = m1_cat_pos, m1_cat_neg = m1_cat_neg,
                                m2_cat_pos = m2_cat_pos, m2_cat_neg = m2_cat_neg)
  ### four quadrant visualization
  res.4quad <- dm_4quadrant(data = clin.bmk,
                            outcome = outcome,
                            outcome_pos= outcome_pos,
                            outcome_neg= outcome_neg,
                            marker1  = marker1,
                            marker2 =  marker2,
                            num_cut_method = num_cut_method,
                            m1_cat_pos = m1_cat_pos,
                            m1_cat_neg = m1_cat_neg,
                            m2_cat_pos = m2_cat_pos,
                            m2_cat_neg = m2_cat_neg,
                            na.rm = T)

  # four quadrant plot
  g.4quad.response <- quadrant_response_chart(x = res.4quad$pos.n, n = res.4quad$total.n)

  ### logistic regression model
  # as continuous variables
  res.logit <- dm_logit(data = clin.bmk,
                        outcome = outcome,
                        outcome_pos= outcome_pos,
                        outcome_neg= outcome_neg,
                        marker1 = marker1,
                        marker2 = marker2,
                        num_cut_method = num_cut_method,
                        m1_cat_pos = m1_cat_pos, m1_cat_neg = m1_cat_neg,
                        m2_cat_pos = m1_cat_pos, m2_cat_neg = m2_cat_neg,
                        binarization = F)
  # barplot
  g.logit <- dm_logit_plot(res.logit)

  # ROC
  g.roc <- dm_roc(data = data,
                  outcome = outcome,
                  outcome_pos= outcome_pos,
                  outcome_neg= outcome_neg,
                  marker1 = marker1,
                  marker2 = marker2,
                  m1_cat_pos = m1_cat_pos, m1_cat_neg = m1_cat_neg,
                  m2_cat_pos = m2_cat_pos, m2_cat_neg = m2_cat_neg,
                  logistic.reg = T,
                  logistic.reg.int = T)
  # survival
  g.km <- dm_KMplot(data = data,
                    time = time,
                    event = event,
                    marker1 = marker1,
                    marker2 = marker2,
                    num_cut_method = num_cut_method,
                    outcome = outcome,
                    outcome_pos = outcome_pos,
                    outcome_neg = outcome_neg,
                    m1_cat_pos = m1_cat_pos, m1_cat_neg = m1_cat_neg,
                    m2_cat_pos = m2_cat_pos, m2_cat_neg = m2_cat_neg)
  res.cox <- dm_cox(data = data,
                    time = time, event = event,
                    marker1 = marker1, marker2 = marker2,
                    binarization = F, num_cut_method = num_cut_method,
                    m1_cat_pos = m1_cat_pos, m1_cat_neg = m1_cat_neg,
                    m2_cat_pos = m2_cat_pos, m2_cat_neg = m2_cat_neg)
  g.4quad.surv <- quadrant_survival_chart(data = data, time = time,
                                          event = event, marker1 = marker1,
                                          marker2 = marker2, num_cut_method = num_cut_method,
                                          m1_cat_pos = m1_cat_pos, m1_cat_neg = m1_cat_neg,
                                          m2_cat_pos = m2_cat_pos, m2_cat_neg = m2_cat_neg)

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
