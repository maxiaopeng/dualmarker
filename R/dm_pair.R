
#' Plot and statistics of dual marker pair
#'
#' Plot and statistics of dual markers that reveal the association between two markers, response and survival
#'
#' @param data data frame
#' @param response response variable
#' @param response.pos positive value(s) of response
#' @param response.neg negative value(s) of response, default NULL, all other values except 'response.pos'
#' @param time survival time
#' @param event survival event
#' @param marker1 marker1 variable
#' @param marker2 marker2 variable
#' @param m1.num.cut cut method or value for numeric marker1, default "median"
#' @param m2.num.cut cut method or value for numeric marker2, default "median"
#' @param m1.cat.pos positive value(s) if marker1 is categorical
#' @param m1.cat.neg negative value(s) if marker1 is categorical
#' @param m2.cat.pos positive value(s) if marker2 is categorical
#' @param m2.cat.neg negative value(s) if marker2 is categorical
#' @param plot.only plot only, no 'response.stats' or 'survival.stats', default FALSE
#' @return list of 'response.plot', 'response.stats', 'survival.plot' and 'survival.stats'
#' @examples
#'
#'  ### example1: two biomarkers of continuous variables
#'  res.pair <- dm_pair(
#'   data = clin_bmk_IMvigor210,
#'   response = "binaryResponse",
#'   response.pos = "CR/PR", response.neg = "SD/PD",
#'   marker1 = "TMB",
#'   m1.num.cut = "median",
#'   marker2 = "gepscore_TGFb.19gene",
#'   m2.num.cut = "median")
#'  res.pair$response.plot
#'  res.pair$response.stats

#'  ### example2: two biomarkers of continuous variables(custom cutoff)
#'  m1.cutoff <- quantile(clin_bmk_IMvigor210$TMB, 2/3, na.rm=T)
#'  m2.cutoff <- quantile(clin_bmk_IMvigor210$gepscore_TGFb.19gene, 1/3, na.rm=T)
#'  res.pair <- dm_pair(
#'   data = clin_bmk_IMvigor210,
#'   response = "binaryResponse",
#'   response.pos = "CR/PR", response.neg = "SD/PD",
#'   marker1 = "TMB",
#'   m1.num.cut = m1.cutoff,
#'   marker2 = "gepscore_TGFb.19gene",
#'   m2.num.cut = m2.cutoff)
#'  res.pair$response.plot
#'  res.pair$response.stats
#'  res.pair$survival.plot
#'  res.pair$survival.stats
#'
#'  ### example3: continuous variable + categorical variable
#'  res.pair <- dm_pair(
#'     data = clin_bmk_IMvigor210,
#'     # survival info
#'     time = "os", event = "censOS",
#'     # marker1
#'     marker1 = "mut_ARID1A",
#'     m1.cat.pos = "YES", m1.cat.neg = "NO",
#'     # marker2
#'     marker2 = "gep_CXCL13",
#'     m2.num.cut = "median")
#'  res.pair$survival.plot
#'  res.pair$survival.stats
#'
#'  ### example4: two biomarkers of categorical variable
#'  res.pair <- dm_pair(
#'     data = clin_bmk_IMvigor210,
#'     # response info
#'     response = "binaryResponse",
#'     response.pos = "CR/PR", response.neg = "SD/PD",
#'     # survival info
#'     time = "os", event = "censOS",
#'     # marker1
#'     marker1 = "mut_ARID1A",
#'     m1.cat.pos = "YES", m1.cat.neg = "NO",
#'     # marker2
#'     marker2 = "Immune.phenotype",
#'     m2.cat.pos = "inflamed", m2.cat.neg = c("desert", "excluded"))
#'  res.pair$response.plot
#'  res.pair$response.stats
#'  res.pair$survival.plot
#'  res.pair$survival.stats
#'
#' @export
dm_pair <- function(data, marker1, marker2,
                    response=NULL, response.pos=NULL, response.neg=NULL,
                    time=NULL, event=NULL,
                    m1.num.cut = "median", m1.cat.pos = NULL, m1.cat.neg = NULL,
                    m2.num.cut = "median", m2.cat.pos = NULL, m2.cat.neg = NULL,
                    plot.only=F){
  g.response.boxplot <- g.response.scatter <- g.response.4quad <- g.roc <- g.survplot <- g.surv.4quad <- NULL
  stats.response.4quad <- stats.surv.4quad <- stats.logit <- stats.cox <- NULL

  if(!is.null(response)){
    ### single marker
    if(is.null(response.neg)) {
      response.neg <- setdiff(unique(data[[response]]), response.pos)
    }
    g.response.boxplot <- dm_response_boxplot(data = data, response = response, response.pos = response.pos,
                                response.neg = response.neg,
                                marker1 = marker1, marker2 =  marker2,
                                m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
                                m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg)

    # scatterplot
    g.response.scatter <- dm_response_scatter_chart(data = data,response = response,
                                  response.pos=response.pos, response.neg=response.neg,
                                  marker1 = marker1, marker2 = marker2,
                                  m1.num.cut = m1.num.cut, m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
                                  m2.num.cut = m2.num.cut, m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg)
    ### four quadrant visualization
    stats.response.4quad <- dm_response_4quad(
      data = data,
      response = response,
      response.pos= response.pos,
      response.neg= response.neg,
      marker1  = marker1,
      marker2 =  marker2,
      m1.num.cut = m1.num.cut,
      m1.cat.pos = m1.cat.pos,
      m1.cat.neg = m1.cat.neg,
      m2.num.cut = m2.num.cut,
      m2.cat.pos = m2.cat.pos,
      m2.cat.neg = m2.cat.neg,
      na.rm = T)

    # four quadrant plot
    g.response.4quad <- dm_response_4quad_chart(x = stats.response.4quad$pos.n,
                                                n = stats.response.4quad$total.n,
                                                title = paste0("marker1(x-axis):", marker1, "  marker2(y-axis): ", marker2))

    ### logistic regression model
    if(!plot.only){
      stats.logit <- dm_logit(data = data,
                              response = response,
                              response.pos= response.pos,
                              response.neg= response.neg,
                              marker1 = marker1, marker2 = marker2,
                              m1.binarize = !is.null(m1.cat.pos),
                              m2.binarize = !is.null(m2.cat.pos),
                              m1.num.cut = m1.num.cut, m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
                              m2.num.cut = m2.num.cut, m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg)
    }
    # ROC
    g.roc <- dm_roc_curve(data = data,
                         response = response,
                         response.pos= response.pos,
                         response.neg= response.neg,
                         marker1 = marker1,
                         marker2 = marker2,
                         m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
                         m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg,
                         logit.reg = T,
                         logit.reg.int = T)
  }
  # survival
  if(!is.null(time)){
    g.survplot <- dm_surv_plot(data = data,
                      time = time,event = event,
                      response =response, response.pos = response.pos, response.neg = response.neg,
                      marker1 = marker1, marker2 = marker2,
                      m1.num.cut = m1.num.cut, m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
                      m2.num.cut = m2.num.cut, m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg)
    if(!plot.only){
      stats.cox <- dm_cox(
       data = data,
       time = time, event = event,
       marker1 = marker1, marker2 = marker2,
       m1.binarize = !is.null(m1.cat.pos),
       m2.binarize = !is.null(m2.cat.pos),
       m1.num.cut = m1.num.cut, m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
       m2.num.cut = m2.num.cut, m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg
       )
      stats.surv.4quad <- dm_survival_4quad(
        data= data, time = time, event = event,
        marker1 = marker1,marker2 = marker2,
        m1.num.cut = m1.num.cut, m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
        m2.num.cut = m2.num.cut, m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg)
    }
    g.surv.4quad <- dm_survival_4quad_chart(
      data = data,
      time = time, event = event,
      marker1 = marker1, marker2 = marker2,
      m1.num.cut = m1.num.cut,m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
      m2.num.cut = m2.num.cut, m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg)
  }
  # response
  response.plot <- list(boxplot = g.response.boxplot,
                        scatter.chart = g.response.scatter,
                        four.quadrant = g.response.4quad,
                        roc = g.roc)
  response.stats <- list(logit = stats.logit,
                         four.quadrant = stats.response.4quad)
  # survival
  survival.plot <- list(km.m1m2 = g.survplot$km.m1m2,
                        scatter.m1m2 = g.survplot$scatter.m1m2,
                        km.dualmarker = g.survplot$km.dualmarker,
                        km.dualmarker.facet = g.survplot$km.dualmarker.facet,
                        scatter.dualmarker = g.survplot$scatter.dualmarker,
                        four.quadrant = g.surv.4quad)
  survival.stats <- list(cox = stats.cox,
                         four.quadrant = stats.surv.4quad)
  # outcome
  list(response.plot = response.plot, response.stats = response.stats,
       survival.plot = survival.plot, survival.stats = survival.stats)
}
