
#' Visualization and statistics of dual marker pair
#'
#' Plot and statistics of dual markers that reveal the association between two markers,
#' response and survival. It takes the marker1, marker2, response or survival as input,
#' and returns plots (response.plot, survival.plot) and statistic results (response.stats,
#' survival.stats) from logistic regression for binary outcome(response) and Cox regression
#' for time-to-event outcome(survival).
#'
#' @param data data frame
#' @param response response variable
#' @param response.pos positive value(s) of response
#' @param response.neg negative value(s) of response, default NULL, i.e. all other values
#'   except 'response.pos'
#' @param time survival time
#' @param event survival event
#' @param marker1 marker1 variable
#' @param marker2 marker2 variable
#' @param covariates covariates
#' @param m1.num.cut cut method or value for numeric marker1, default "median", can be specific
#'  value or 'median', 'mean', 'mean+sd', 'mean-sd', 'upper-tertile', 'lower-tertile'
#' @param m2.num.cut cut method or value for numeric marker2, default "median"
#' @param m1.cat.pos positive value(s) if marker1 is categorical
#' @param m1.cat.neg negative value(s) if marker1 is categorical
#' @param m2.cat.pos positive value(s) if marker2 is categorical
#' @param m2.cat.neg negative value(s) if marker2 is categorical
#' @param label.m1 label of marker1, default using marker1 variable
#' @param label.m2 label of marker2, default using marker2 variable
#' @param label.m1.pos label of positive value(s) for marker1
#' @param label.m1.neg label of negative value(s) for marker1
#' @param label.m2.pos label of positive value(s) for marker2
#' @param label.m2.neg label of negative value(s) for marker2
#' @param palette.other the color palette to be used for coloring or filling by.
#' same as \code{\link[ggpubr]{get_palette}}
#' Allowed values include "grey" for grey color palettes; brewer palettes e.g. "RdBu", "Blues", ...; or custom
#'  color palette e.g. c("blue", "red"); and scientific journal palettes from ggsci R package,
#'  e.g.: "npg", "aaas", "lancet", "jco", "ucscgb", "uchicago", "simpsons" and "rickandmorty".
#' @param palette.4quadrant the color palette to be used for coloring or filling four quadrants.
#'   same with palette.other
#' @param output output 'stat' and/or 'plot', default c("stat", "plot"), i.e both stat and plot
#' @return list of 'response.plot', 'response.stats', 'survival.plot' and 'survival.stats'
#' @examples
#'
#'  ### example1: two biomarkers of continuous variables
#'  res.pair <- dm_pair(
#'   data = clin_bmk_IMvigor210,
#'   response = "binaryResponse",
#'   response.pos = "CR/PR",
#'   response.neg = "SD/PD",
#'   marker1 = "TMB",
#'   m1.num.cut = "median",
#'   marker2 = "gepscore_TGFb.19gene",
#'   m2.num.cut = "median"
#'  )
#'  res.pair$response.plot
#'  res.pair$response.stats
#'
#'  ### example2: two biomarkers of continuous variables(custom cutoff)
#'  m1.cutoff <- quantile(clin_bmk_IMvigor210$TMB, 2/3, na.rm=T)
#'  m2.cutoff <- quantile(clin_bmk_IMvigor210$gepscore_TGFb.19gene, 1/3, na.rm=T)
#'  res.pair <- dm_pair(
#'   data = clin_bmk_IMvigor210,
#'   response = "binaryResponse",
#'   response.pos = "CR/PR",
#'   response.neg = "SD/PD",
#'   marker1 = "TMB",
#'   m1.num.cut = m1.cutoff,
#'   marker2 = "gepscore_TGFb.19gene",
#'   m2.num.cut = m2.cutoff
#'  )
#'  res.pair$response.plot
#'  res.pair$response.stats
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
#'     m2.num.cut = "median"
#'  )
#'  res.pair$survival.plot
#'  res.pair$survival.stats
#'
#'  ### example4: two biomarkers of categorical variable
#'  res.pair <- dm_pair(
#'     data = clin_bmk_IMvigor210,
#'     # response info
#'     response = "binaryResponse",
#'     response.pos = "CR/PR",
#'     response.neg = "SD/PD",
#'     # survival info
#'     time = "os",
#'     event = "censOS",
#'     # marker1
#'     marker1 = "mut_ARID1A",
#'     m1.cat.pos = "YES",
#'     m1.cat.neg = "NO",
#'     label.m1.pos = "MUT",
#'     label.m1.neg ="WT",
#'     # marker2
#'     marker2 = "mut_TP53",
#'     m2.cat.pos = "YES",
#'     m2.cat.neg = "NO",
#'     label.m2.pos = "MUT",
#'     label.m2.neg = "WT",
#'     na.rm.response = F,
#'     na.rm.marker = F
#'  )
#'  res.pair$response.plot
#'  res.pair$response.stats
#'  res.pair$survival.plot
#'  res.pair$survival.stats
#'
#'  ### example5: two biomarkers of continuous variables with covariates
#'  res.pair <- dm_pair(
#'   data = clin_bmk_IMvigor210,
#'   response = "binaryResponse",
#'   response.pos = "CR/PR",
#'   response.neg = "SD/PD",
#'   marker1 = "TMB",
#'   m1.num.cut = "median",
#'   marker2 = "gepscore_TGFb.19gene",
#'   m2.num.cut = "median",
#'   covariates = "Immune.phenotype"
#'  )
#'  res.pair$response.stats
#'
#'  ### example6: adjust the palette, labels
#'  res.pair <- dm_pair(
#'   data = clin_bmk_IMvigor210,
#'   response = "binaryResponse",
#'   response.pos = "CR/PR",
#'   response.neg = "SD/PD",
#'   label.response.pos = "R",
#'   label.response.neg = "NR",
#'   marker1 = "TMB",
#'   m1.num.cut = "median",
#'   label.m1.pos = "TMB_hi",
#'   label.m1.neg = "TMB_lo",
#'   marker2 = "gepscore_TGFb.19gene",
#'   m2.num.cut = "median",
#'   label.m2 = "TGFbeta",
#'   label.m2.pos = "TGFb_hi",
#'   label.m2.neg = "TGFb_lo",
#'   palette.4quadrant = "jco",
#'   palette.other = "jco"
#'  )
#'  res.pair$response.plot
#'
#' @export
dm_pair <- function(data,
                    # response
                    response = NULL,
                    response.pos = NULL,
                    response.neg = NULL,
                    label.response.pos = "pos",
                    label.response.neg = "neg",
                    # survival
                    time=NULL,
                    event=NULL,
                    # marker1
                    marker1,
                    m1.datatype = "auto",
                    m1.num.cut = "median",
                    m1.cat.pos = NULL,
                    m1.cat.neg = NULL,
                    label.m1 = marker1,
                    label.m1.pos = NULL,
                    label.m1.neg = NULL,
                    # marker2
                    marker2,
                    m2.datatype = "auto",
                    m2.num.cut = "median",
                    m2.cat.pos = NULL,
                    m2.cat.neg = NULL,
                    label.m2 = marker2,
                    label.m2.pos = NULL,
                    label.m2.neg =  NULL,
                    # covariates
                    covariates = NULL,
                    # palette
                    palette.4quadrant = "default",
                    palette.other = "default",
                    # remove na
                    na.rm.response=F,
                    na.rm.marker = F,
                    output = c("plot","stat")
                    ){
  g.response.boxplot <- g.response.scatter <- g.response.4quad <- g.roc <- g.survplot <- g.surv.4quad <- NULL
  stats.response.4quad <- stats.surv.4quad <- stats.logit <- stats.cox <- NULL

  assertthat::assert_that(m1.datatype %in% c("auto","num","cat"),
                          msg = "m1.datatype should be ['auto','num','cat']")
  assertthat::assert_that(m2.datatype %in% c("auto","num","cat"),
                          msg = "m2.datatype should be ['auto','num','cat']")
  assertthat::assert_that(all(output %in% c("plot","stat")),
                          msg = "output should be ['plot','stat']")

  if(m1.datatype %in% c("auto")){
    m1.datatype <- datatype_num_cat(data[[marker1]])
  }
  if(m2.datatype %in% c("auto")){
    m2.datatype <- datatype_num_cat(data[[marker2]])
  }

  if(is.null(label.m1.pos)){
    label.m1.pos <- if(m1.datatype %in% c("num")) "high" else "pos"}
  if(is.null(label.m1.neg)){
    label.m1.neg <- if(m1.datatype %in% c("num")) "low" else "neg"}
  if(is.null(label.m2.pos)){
    label.m2.pos <- if(m2.datatype %in% c("num")) "high" else "pos"}
  if(is.null(label.m2.neg)){
    label.m2.neg <- if(m2.datatype %in% c("num")) "low" else "neg"}
  if(!is.null(response)){
    ### single marker
    if(is.null(response.neg)) {
      response.neg <- setdiff(unique(data[[response]]), response.pos)
    }
    if("plot" %in% output){
      g.response.boxplot <- dm_response_boxplot(
        data = data,
        response = response,
        response.pos = response.pos,
        response.neg = response.neg,
        marker1 = marker1,
        marker2 =  marker2,
        m1.datatype = m1.datatype,
        m1.cat.pos = m1.cat.pos,
        m1.cat.neg = m1.cat.neg,
        m2.datatype = m2.datatype,
        m2.cat.pos = m2.cat.pos,
        m2.cat.neg = m2.cat.neg,
        label.m1 = label.m1,
        label.m2 = label.m2,
        palette = palette.other,
        label.response.pos = label.response.pos,
        label.response.neg = label.response.neg,
        label.m1.pos = label.m1.pos,
        label.m1.neg = label.m1.neg,
        label.m2.pos = label.m2.pos,
        label.m2.neg = label.m2.neg,
        na.rm.response = na.rm.response,
        na.rm.marker = na.rm.marker
        )
    }

    # scatterplot
    if("plot" %in% output){
    g.response.scatter <- dm_response_scatter_chart(
      data = data,
      response = response,
      response.pos=response.pos,
      response.neg=response.neg,
      marker1 = marker1,
      marker2 = marker2,
      m1.num.cut = m1.num.cut,
      m1.cat.pos = m1.cat.pos,
      m1.cat.neg = m1.cat.neg,
      m2.num.cut = m2.num.cut,
      m2.cat.pos = m2.cat.pos,
      m2.cat.neg = m2.cat.neg,
      label.m1 = label.m1,
      label.m2 = label.m2,
      palette = palette.other,
      label.response.pos = label.response.pos,
      label.response.neg = label.response.neg,
      label.m1.pos = label.m1.pos,
      label.m1.neg = label.m1.neg,
      label.m2.pos = label.m2.pos,
      label.m2.neg = label.m2.neg,
      na.rm.response = na.rm.response)
    }
    ### four quadrant
    # stats
    if("stat" %in% output || "plot" %in% output){
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
      m2.cat.neg = m2.cat.neg)
    }

    # four quadrant plot
    if("plot" %in% output){
    g.response.4quad <- dm_response_4quad_chart(
      x = stats.response.4quad$pos.n,
      n = stats.response.4quad$total.n,
      label.m1.pos = label.m1.pos,
      label.m1.neg = label.m1.neg,
      label.m2.pos = label.m2.pos,
      label.m2.neg = label.m2.neg,
      label.m1 = label.m1,
      label.m2 = label.m2,
      palette = palette.other,
      palette.4quadrant = palette.4quadrant,
      title = paste0("Marker1(x):", label.m1, "   Marker2(y): ", label.m2)
      )
    }

    ### logistic regression model
    if("stat" %in% output){
      stats.logit <- dm_logit(
        data = data,
        response = response,
        response.pos= response.pos,
        response.neg= response.neg,
        marker1 = marker1,
        marker2 = marker2,
        m1.binarize = !is.null(m1.cat.pos),
        m2.binarize = !is.null(m2.cat.pos),
        m1.num.cut = m1.num.cut,
        m1.cat.pos = m1.cat.pos,
        m1.cat.neg = m1.cat.neg,
        m2.num.cut = m2.num.cut,
        m2.cat.pos = m2.cat.pos,
        m2.cat.neg = m2.cat.neg,
        covariates = covariates,
        auc = T
        )
    }
    # ROC
    if("plot" %in% output){
    g.roc <- dm_roc_curve(
      data = data,
      response = response,
      response.pos= response.pos,
      response.neg= response.neg,
      marker1 = marker1,
      marker2 = marker2,
      m1.cat.pos = m1.cat.pos,
      m1.cat.neg = m1.cat.neg,
      m2.cat.pos = m2.cat.pos,
      m2.cat.neg = m2.cat.neg,
      logit.reg = T,
      logit.reg.int = T,
      palette = palette.other)
    }
  }
  # survival
  if(!is.null(time)){
    if("plot" %in% output){
    g.survplot <- dm_surv_plot(
      data = data,
      time = time,
      event = event,
      response =response,
      response.pos = response.pos,
      response.neg = response.neg,
      label.response.pos = label.response.pos,
      label.response.neg = label.response.neg,
      marker1 = marker1,
      marker2 = marker2,
      m1.num.cut = m1.num.cut,
      m1.cat.pos = m1.cat.pos,
      m1.cat.neg = m1.cat.neg,
      m2.num.cut = m2.num.cut,
      m2.cat.pos = m2.cat.pos,
      m2.cat.neg = m2.cat.neg,
      label.m1 = label.m1,
      label.m2 = label.m2,
      label.m1.pos = label.m1.pos,
      label.m1.neg = label.m1.neg,
      label.m2.pos = label.m2.pos,
      label.m2.neg = label.m2.neg,
      palette = palette.other,
      palette.4quadrant =palette.4quadrant,
      na.rm.marker = na.rm.marker
      )
    }
    if("stat" %in% output){
      stats.cox <- dm_cox(
       data = data,
       time = time,
       event = event,
       marker1 = marker1,
       marker2 = marker2,
       m1.binarize = !is.null(m1.cat.pos),
       m2.binarize = !is.null(m2.cat.pos),
       m1.num.cut = m1.num.cut,
       m1.cat.pos = m1.cat.pos,
       m1.cat.neg = m1.cat.neg,
       m2.num.cut = m2.num.cut,
       m2.cat.pos = m2.cat.pos,
       m2.cat.neg = m2.cat.neg,
       covariates = covariates
       )
    }
    if("stat" %in% output){
      stats.surv.4quad <- dm_survival_4quad(
        data= data,
        time = time,
        event = event,
        marker1 = marker1,
        marker2 = marker2,
        m1.num.cut = m1.num.cut,
        m1.cat.pos = m1.cat.pos,
        m1.cat.neg = m1.cat.neg,
        m2.num.cut = m2.num.cut,
        m2.cat.pos = m2.cat.pos,
        m2.cat.neg = m2.cat.neg,
        label.m1.pos = label.m1.pos,
        label.m1.neg = label.m1.neg,
        label.m2.pos = label.m2.pos,
        label.m2.neg = label.m2.neg)
    }
    if("plot" %in% output){
    g.surv.4quad <- dm_survival_4quad_chart(
      data = data,
      time = time,
      event = event,
      marker1 = marker1,
      marker2 = marker2,
      m1.num.cut = m1.num.cut,
      m1.cat.pos = m1.cat.pos,
      m1.cat.neg = m1.cat.neg,
      m2.num.cut = m2.num.cut,
      m2.cat.pos = m2.cat.pos,
      m2.cat.neg = m2.cat.neg,
      label.m1.pos = label.m1.pos,
      label.m1.neg = label.m1.neg,
      label.m2.pos = label.m2.pos,
      label.m2.neg = label.m2.neg,
      label.m1 = label.m1,
      label.m2 = label.m2,
      palette.4quadrant = palette.4quadrant,
      title = paste0("Marker1(x):", label.m1, "   Marker2(y): ", label.m2)
      )
    }
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
  list(response.plot = response.plot,
       response.stats = response.stats,
       survival.plot = survival.plot,
       survival.stats = survival.stats)
}
