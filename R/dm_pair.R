
#' plot and statistics of dual marker pair
#'
#' the plot and statistics of dual markers, including the response and survival analysis
#'
#' @param data data frame
#' @param response response variable
#' @param response.pos positive value(s) of response
#' @param response.neg negative value(s) of response, default NULL, all other values except 'response.pos'
#' @param time survival time
#' @param event survival event
#' @param marker1 marker1
#' @param marker2 marker2
#' @param m1.num.cut cut method/values for numeric marker1
#' @param m2.num.cut cut method/values for numeric marker2
#' @param m1.cat.pos positive value for categorical marker1
#' @param m1.cat.neg negative value for categorical marker1
#' @param m2.cat.pos positive value for categorical marker2
#' @param m2.cat.neg negative value for categorical marker2
#' @param plot.only no 'stats', default TRUE
#' @return list of 'plot' and 'stats'.
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
                                                n = stats.response.4quad$total.n)

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

  if(!is.null(time)){
    # survival
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
