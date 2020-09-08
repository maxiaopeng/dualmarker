##' KMplot of dual markers
##'
##' KMplot of dual markers, for each single markers and dual markers
##'
##' @param data data frame
##' @param time survival time
##' @param event survival event
##' @param marker1 categorical variables with 2 levels
##' @param marker2 categorical variables with 2 levels
##' @param label.m1 label for marker1
##' @param label.m2 label for marker2
##' @param na.rm remove NA
##' @return list of ggplot object, for marker1, marker2 and dualmarker
.dm_KMplot_core <- function(data, time, event,
                            marker1, marker2,
                            label.m1 = marker1,
                            label.m2 = marker2,
                            na.rm = T, km.pval=T,
                            km.risk.table = T, ...) {
  assert_that(class(data[[marker1]]) == "factor",
              nlevels(data[[marker1]]) == 2,
              msg = "marker1 should be factor with 2 levels")
  assert_that(class(data[[marker2]]) == "factor",
              nlevels(data[[marker2]]) == 2,
              msg = "marker2 should be factor with 2 levels")
  .assert_colname(data, c(time, event, marker1, marker2))
  # marker1
  data[[label.m1]] <- factor(data[[marker1]], levels = rev(levels(data[[marker1]])))
  survfit.m1 <- .survfit( data = data, var = label.m1,
      time = time, event = event)
  km.m1 <- .survplot(survfit = survfit.m1, data = data,
                     km.pval = km.pval, km.risk.table = km.risk.table,
                     tables.height = 0.45,
                     ...)

  # marker2
  assert_that(label.m1 != label.m2,
              msg = "label.m1 should NOT equal to label.m2")
  data[[label.m2]] <-
    factor(data[[marker2]], levels = rev(levels(data[[marker2]])))
  survfit.m2 <- .survfit( data = data, var = label.m2, time = time,event = event)
  km.m2 <- .survplot(survfit = survfit.m2, data = data,
                     km.pval = km.pval, km.risk.table = km.risk.table,
                     tables.height = 0.45,
                     ...)

  # dual marker: 4 curve
  if (na.rm) {
    data %<>% drop_na(!!sym(marker1),!!sym(marker2))
  }
  data$.region <- .label_quadrant(data[[marker1]], data[[marker2]])
  data$.group <- paste(data$.region, data[[marker1]], data[[marker2]], sep = "_")

  keep <- data$.region %>% unique() %>% as.character() %>% sort()
  pal <- color.quadrant.2[keep] %>% base::unname()

  survfit.md <- .survfit(data = data, var = ".group", time = time,event = event)
  km.md <- .survplot(survfit = survfit.md,
                     data = data, palette = pal,
                     km.pval = km.pval, km.risk.table = km.risk.table,
                     tables.height = 0.5, ...)
  # dual marker: 2 curves
  survfit.md.facet <- .survfit(data = data, var = ".group", time = time,event = event)
  g.facet1 <- survminer::ggsurvplot(survfit.md.facet, data =data, facet.by = marker1, pval=T, palette = pal) +
    theme(legend.position = "none")
  g.facet2 <- survminer::ggsurvplot(survfit.md.facet, data =data, facet.by = marker2, pval=T, palette = pal)
  dualmarker.facet <- ggpubr::ggarrange(g.facet1, g.facet2, nrow=2, common.legend = T)

  # out
  list( marker1 = km.m1,
    marker2 = km.m2,
    dualmarker = km.md,
    dualmarker.facet = dualmarker.facet
    )
}

##' KM plot for dual marker
##'
##' @param data data frame
##' @param time survival time
##' @param event survival event
##' @param marker1 marker1
##' @param marker2 marker2
##' @param m1.datatype datatype of marker1
##' @param m1.label.pos label for positive response of marker1
##' @param m1.label.neg label for negative response of marker1
##' @param m1.cat.pos positive value(s) of marker1 if marker1 is categorical
##' @param m1.cat.neg negative value(s) of marker1 if marker1 is categorical
##' @param m2.datatype datatype of marker2
##' @param m2.label.pos label for positive response of marker2
##' @param m2.label.neg label for positive response of marker2
##' @param m2.cat.pos positive value(s) of marker2 if marker2 is categorical
##' @param m2.cat.neg positive value(s) of marker2 if marker2 is categorical
##' @param m1.num.cut cut method for m1
##' @param m2.num.cut cut method for m2
##' @param na.rm remove NA
.dm_KMplot <- function(data, time, event,
                      marker1, marker2,
                      # m1
                      m1.num.cut = "median", m1.cat.pos = NULL, m1.cat.neg = NULL, m1.label.pos = NULL,m1.label.neg = NULL,
                      # m2
                      m2.num.cut = "median", m2.cat.pos = NULL, m2.cat.neg = NULL, m2.label.pos = NULL, m2.label.neg = NULL,
                      na.rm = T, km.pval= T, km.risk.table=T,  ...) {
  .assert_colname(data, c(time, event, marker1, marker2))
  # marker1
  data$.m1 <- binarize_data( x = data[[marker1]], return.binary = F,
      num.cut = m1.num.cut,  cat.pos = m1.cat.pos, cat.neg = m1.cat.neg,
      label.pos = m1.label.pos, label.neg = m1.label.neg
    ) %>% .$data
  # marker2
  data$.m2 <- binarize_data(
    x = data[[marker2]], return.binary = F,
      num.cut = m2.num.cut, cat.pos = m2.cat.pos, cat.neg = m2.cat.neg,
      label.pos = m2.label.pos, label.neg = m2.label.neg
    ) %>% .$data
  # kmplot
  kmplots <- .dm_KMplot_core(
    data = data, time = time, event = event,
    marker1 = ".m1", marker2 = ".m2",
    label.m1 = "m1", label.m2 = "m2",
    na.rm = na.rm, km.pval = km.pval, km.risk.table = km.risk.table, ...
  )
  kmplots
}

#' scatter plot for survival info
#'
#' @param data dataframe
#' @param time time
#' @param event event
#' @param marker1 marker1
#' @param marker2 marker2
#' @param response response
#' @param response.pos positive values for response
#' @param response.neg negative values for response
#' @param m1.num.cut cut method/values if marker1 is numeric
#' @param m1.cat.pos positive value(s) if marker1 is categorical
#' @param m1.cat.neg negative value(s) if marker1 is categorical
#' @param m2.num.cut cut method/values if marker2 is numeric
#' @param m2.cat.pos positive value(s) if marker2 is categorical
#' @param m2.cat.neg negative value(s) if marker2 is categorical
.dm_survival_scatter_chart <- function(data, time, event,
                                       marker1, marker2,
                                       response=NULL, response.pos = NULL, response.neg = NULL,
                                       m1.num.cut = "median", m1.cat.pos =NULL, m1.cat.neg = NULL,
                                       m2.num.cut = "median", m2.cat.pos =NULL, m2.cat.neg = NULL){
  .assert_colname(data, c(time, event, marker1, marker2, response))
  .geom_type <- function(datatype)  if(datatype == "num") geom_point() else geom_jitter()
  m1.datatype <- datatype_num_cat(data[[marker1]])
  m2.datatype <- datatype_num_cat(data[[marker2]])
  data %<>%
    mutate(status = ifelse( !!sym(event) == 0, "censor",
                            ifelse( !!sym(event) == 1, "event", NA)))
  if(!is.null(response)){
    data$.response <- binarize_cat(data[[response]], pos = response.pos, neg = response.neg) %>%
      factor(levels = c("pos","neg"))
  }
  # marker1
  params.m1 <- list(x = marker1, y = time)
  if(!is.null(response)) params.m1$color <- ".response"
  scatter.m1 <- ggplot(data, do.call(aes_string, params.m1)) +
    .geom_type(m1.datatype)
  # marker2
  params.m2 <- list(x = marker2, y = time)
  if(!is.null(response)) params.m2$color <- ".response"
  scatter.m2 <- ggplot(data, do.call(aes_string, params.m2)) +
    .geom_type(m2.datatype)

  # dualmarker
  scatter.md <- dm_response_scatter_chart(data= data,
                                 response = response,response.pos = response.pos, response.neg = response.neg,
                                 size=time,
                                 #alpha = "status",
                                 marker1 = marker1, marker2= marker2,
                                 m1.num.cut = m1.num.cut, m1.cat.pos = m1.cat.pos,  m1.cat.neg = m1.cat.neg,
                                 m2.num.cut = m1.num.cut, m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg)
  # return
  list( marker1 = scatter.m1,
        marker2 = scatter.m2,
        dualmarker = scatter.md)
}


#' survival plot
#'
#' @param data dataframe
#' @param time time
#' @param event event
#' @param marker1 marker1
#' @param marker2 marker2
#' @param response response
#' @param response.pos positive value(s) for response
#' @param response.neg negative value(s) for response
#' @param m1.num.cut cut method/value(s) if marker1 is numeric
#' @param m1.cat.pos positive value if marker1 is categorical
#' @param m1.cat.neg negative value if marker1 is categorical
#' @param m2.num.cut cut method/value(s) if marker2 is numeric
#' @param m2.cat.pos positive value if marker2 is categorical
#' @param m2.cat.neg negative value if marker2 is categorical
#' @param na.rm remove NA
#' @param km.pval show pval for KM-plot
#' @param km.risk.table show risk.table for KM-plot
#' @param ... other parameter for ggsurvplot
#' @export
dm_surv_plot <- function(data, time, event,
                         marker1, marker2,
                         # response if any
                         response = NULL, response.pos = NULL, response.neg = NULL,
                         # m1
                         m1.num.cut = "median", m1.cat.pos = NULL, m1.cat.neg = NULL,
                         # m2
                         m2.num.cut = "median", m2.cat.pos = NULL, m2.cat.neg = NULL,
                         na.rm = T, km.pval= T, km.risk.table=T,  ...){
  scatterplots <- .dm_survival_scatter_chart(
    data = data, time = time, event = event,
    marker1 = marker1, marker2 = marker2,
    response= response, response.pos = response.pos, response.neg = response.neg,
    m1.num.cut = m1.num.cut , m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
    m2.num.cut = m2.num.cut , m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg)
  kmplots <- .dm_KMplot(data = data,
                        time =time, event= event,
                        marker1 = marker1, marker2 = marker2,
                        m1.num.cut = m1.num.cut, m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
                        m2.num.cut = m2.num.cut, m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg,
                        na.rm = na.rm,
                        km.risk.table = km.risk.table, km.pval = km.pval, ...)
  g.out <- list()
  g.out$km.m1m2 <- ggpubr::ggarrange(kmplots[[1]], kmplots[[2]], align = "h", nrow=1,
                                     labels = c(paste0("marker1: ", marker1),
                                                paste0("marker2: ", marker2)))
  g.out$km.dualmarker <- kmplots[[3]]
  g.out$km.dualmarker.facet <- kmplots[[4]]
  g.out$scatter.m1m2 <- ggpubr::ggarrange(
    scatterplots[[1]], scatterplots[[2]],
    align='h', labels=c(paste0("marker1: ", marker1),
                        paste0("marker2: ", marker2)),
    common.legend = T, nrow = 1)
  g.out$scatter.dualmarker <- scatterplots[[3]]
  g.out[c('km.m1m2','scatter.m1m2','km.dualmarker',"km.dualmarker.facet", 'scatter.dualmarker')]
}
