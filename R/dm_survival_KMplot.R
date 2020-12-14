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
##' @return list of ggplot object, for marker1, marker2 and dualmarker
.dm_KMplot_core <- function(data, time, event,
                            marker1, marker2,
                            label.m1 = marker1,
                            label.m2 = marker2,
                            km.pval=T,
                            km.risk.table = T,
                            palette = "default",
                            palette.4quadrant = "default",
                            p.adjust.method = "bonferroni",
                            na.rm.marker = T,
                            ...) {
  assert_that(class(data[[marker1]]) == "factor",
              nlevels(data[[marker1]]) == 2,
              msg = "marker1 should be factor with 2 levels")
  assert_that(class(data[[marker2]]) == "factor",
              nlevels(data[[marker2]]) == 2,
              msg = "marker2 should be factor with 2 levels")
  .assert_colname(data, c(time, event, marker1, marker2))
  color.4quadrant <- get_4colors(palette = palette.4quadrant)

  # marker1
  #data[[marker1]] <- factor(data[[marker1]], levels = rev(levels(data[[marker1]])))
  if(na.rm.marker){
    d <- data %>% dplyr::filter(!is.na(!!sym(marker1)))
  }else{
    d <- data
    d[[marker1]] %<>%  forcats::fct_explicit_na(na_level = "NA")
  }
  survfit.m1 <- .survfit( data = d,
                          var = marker1,
                          time = time, event = event)
  km.m1 <- .survplot(survfit = survfit.m1, data = d,
                     km.pval = km.pval, km.risk.table = km.risk.table,
                     tables.height = 0.45,
                     palette = palette,
                     ...)

  # marker2
  assert_that(marker1 != marker2,
              msg = "marker1 should NOT equal to marker2")
  #data[[marker2]] <-
  #  factor(data[[marker2]], levels = rev(levels(data[[marker2]])))
  if(na.rm.marker){
    d <- data %>% dplyr::filter(!is.na(!!sym(marker2)))
  }else{
    d <- data
    d[[marker2]] %<>%  forcats::fct_explicit_na(na_level = "NA")
  }
  survfit.m2 <- .survfit( data = d, var = marker2, time = time,event = event)
  km.m2 <- .survplot(survfit = survfit.m2, data = d,
                     km.pval = km.pval, km.risk.table = km.risk.table,
                     tables.height = 0.45,
                     palette = palette,
                     ...)

  # dual marker: 4 curve
  data %<>% drop_na(!!sym(marker1),!!sym(marker2))
  data$.region <- .label_quadrant(data[[marker1]], data[[marker2]])
  data$.group <- paste(data$.region, data[[marker1]], data[[marker2]], sep = "_") %>% factor()

  keep <- data$.region %>% unique() %>% as.character() %>% sort()
  color.4quadrant <- color.4quadrant[keep] %>% base::unname()

  survfit.md <- .survfit(data = data, var = ".group", time = time,event = event)
  km.md <- .survplot(survfit = survfit.md,
                     data = data,
                     palette = color.4quadrant,
                     km.pval = km.pval, km.risk.table = km.risk.table,
                     tables.height = 0.5, ...)

  # dual marker: 2 curves
  fml <- paste0("Surv(", time, ",", event, ") ~ .group") %>% as.formula()
  pval.facet1 <- surv_pvalue_facet(data = data, formula = fml, facet.by = marker1)
  pval.facet2 <- surv_pvalue_facet(data = data, formula = fml, facet.by = marker2)
  pval.facet.all <- bind_rows(pval.facet1, pval.facet2)
  pval.facet.all$padj <- p.adjust(pval.facet.all$pval, method = p.adjust.method)
  pval.facet.all %<>% mutate(
           padj.txt = ifelse(padj < 1e-04, "padj < 0.0001",
                             paste("padj =", signif(padj, 2))),
           pval.txt.all = paste0(pval.txt, "\n", padj.txt))

  g.facet1 <- survminer::ggsurvplot_facet(
    fit = survfit.md, data =data,
    facet.by = marker1, pval=F, palette = color.4quadrant) +
    geom_text(aes(label = pval.txt.all, x = pval.x, y = pval.y),
              data = pval.facet.all[c(1:2),], hjust = 0, lineheight = 0.7) +
    theme(legend.position = "none")

  g.facet2 <- survminer::ggsurvplot_facet(
    fit = survfit.md, data =data,
    facet.by = marker2, pval=F, palette = color.4quadrant) +
    geom_text(aes(label = pval.txt.all, x = pval.x, y = pval.y),
              data = pval.facet.all[c(3:4),], hjust = 0, lineheight = 0.7) +
    theme(legend.position = "none")

  dualmarker.facet <- ggpubr::ggarrange(
    g.facet1, g.facet2, nrow=2,
    common.legend = T, legend = "top")

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
.dm_KMplot <- function(data, time, event,
                      marker1, marker2,
                      # m1
                      m1.num.cut = "median", m1.cat.pos = NULL, m1.cat.neg = NULL,
                      label.m1 = marker1, label.m1.pos = NULL, label.m1.neg = NULL,
                      # m2
                      m2.num.cut = "median", m2.cat.pos = NULL, m2.cat.neg = NULL,
                      label.m2 = marker2, label.m2.pos = NULL, label.m2.neg = NULL,
                      km.pval= T, km.risk.table=T,
                      palette = "default",
                      palette.4quadrant = "default",
                      na.rm.marker = T,
                      ...) {
  .assert_colname(data, c(time, event, marker1, marker2))
  # marker1
  data$.m1 <- binarize_data( x = data[[marker1]], return.binary = F,
      num.cut = m1.num.cut,  cat.pos = m1.cat.pos, cat.neg = m1.cat.neg,
      label.pos = label.m1.pos, label.neg = label.m1.neg
    ) %>% .$data
  # marker2
  data$.m2 <- binarize_data(
    x = data[[marker2]], return.binary = F,
      num.cut = m2.num.cut, cat.pos = m2.cat.pos, cat.neg = m2.cat.neg,
      label.pos = label.m2.pos, label.neg = label.m2.neg
    ) %>% .$data
  # kmplot
  kmplots <- .dm_KMplot_core(
    data = data, time = time, event = event,
    marker1 = ".m1", marker2 = ".m2",
    label.m1 = label.m1, label.m2 = label.m2,
    km.pval = km.pval, km.risk.table = km.risk.table,
    palette = palette,
    palette.4quadrant = palette.4quadrant,
    na.rm.marker = na.rm.marker,
    ...
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
                                       label.m1 = marker1, label.m2 = marker2,
                                       response=NULL, response.pos = NULL, response.neg = NULL,
                                       label.response.pos = "pos", label.response.neg = "neg",
                                       m1.num.cut = "median", m1.cat.pos =NULL, m1.cat.neg = NULL,
                                       label.m1.pos = NULL, label.m1.neg= NULL,
                                       m2.num.cut = "median", m2.cat.pos =NULL, m2.cat.neg = NULL,
                                       label.m2.pos = NULL, label.m2.neg= NULL,
                                       palette = "default"){
  .assert_colname(data, c(time, event, marker1, marker2, response))
  m1.datatype <- datatype_num_cat(data[[marker1]])
  m2.datatype <- datatype_num_cat(data[[marker2]])
  data %<>%
    mutate(status = ifelse( !!sym(event) == 0, "censor",
                            ifelse( !!sym(event) == 1, "event", "NA")))

  if(!is.null(response)){
    data$.response <- binarize_cat(
      x = data[[response]],
      pos = response.pos,
      neg = response.neg,
      label.pos = label.response.pos,
      label.neg = label.response.neg) %>%
      factor(levels = c(label.response.pos, label.response.neg)) %>%
      forcats::fct_explicit_na(na_level = "NA")
  }
  # marker1
  scatter.m1 <- dm_response_scatter_chart(
    data= data,
    response = response, response.pos = response.pos, response.neg = response.neg,
    label.response.pos = label.response.pos, label.response.neg = label.response.neg,
    shape = "status",
    marker1 = marker1, marker2= time, label.m1 = label.m1,
    m1.num.cut = m1.num.cut, m1.cat.pos = m1.cat.pos,  m1.cat.neg = m1.cat.neg,
    label.m1.pos = label.m1.pos, label.m1.neg = label.m1.neg,
    palette = palette, na.rm.response = F) +
    labs(title = paste0("Marker1: ", label.m1), x = label.m1) +
    scale_shape_manual(values = c("censor"=13,"event"=1))+
    theme (plot.title = element_text (face = "bold"))

  # marker2
  scatter.m2 <- dm_response_scatter_chart(
    data= data,
    response = response, response.pos = response.pos, response.neg = response.neg,
    label.response.pos = label.response.pos, label.response.neg = label.response.neg,
    shape = "status",
    marker1 = marker2, marker2= time, label.m1 = label.m2,
    m1.num.cut = m2.num.cut, m1.cat.pos = m2.cat.pos,  m1.cat.neg = m2.cat.neg,
    label.m1.pos = label.m2.pos, label.m1.neg = label.m2.neg,
    palette = palette, na.rm.response = F) +
    labs(title = paste0("Marker2: ", label.m2), x = label.m2) +
    scale_shape_manual(values = c("censor"=13,"event"=1))+
    theme (plot.title = element_text (face = "bold"))

  # dualmarker
  scatter.md <- dm_response_scatter_chart(
     data= data,
     response = response,response.pos = response.pos, response.neg = response.neg,
     label.response.pos = label.response.pos, label.response.neg = label.response.neg,
     size=time,
     shape = "status",
     marker1 = marker1, marker2= marker2, label.m1 = label.m1, label.m2 = label.m2,
     m1.num.cut = m1.num.cut, m1.cat.pos = m1.cat.pos,  m1.cat.neg = m1.cat.neg,
     label.m1.pos = label.m1.pos, label.m1.neg = label.m1.neg,
     m2.num.cut = m2.num.cut, m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg,
     label.m2.pos = label.m2.pos, label.m2.neg = label.m2.neg,
     palette = palette, na.rm.response = F) +
   scale_shape_manual(values = c("censor"=13,"event"=1))
  # return
  list( marker1 = scatter.m1,
        marker2 = scatter.m2,
        dualmarker =scatter.md)
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
#' @param km.pval show pval for KM-plot
#' @param km.risk.table show risk.table for KM-plot
#' @param ... other parameter for ggsurvplot
dm_surv_plot <- function(data, time, event,
                         marker1, marker2,
                         label.m1 = marker1, label.m2 = marker2,
                         # response if any
                         response = NULL, response.pos = NULL, response.neg = NULL,
                         label.response.pos = "pos",  label.response.neg ="neg",
                         # m1
                         m1.num.cut = "median", m1.cat.pos = NULL, m1.cat.neg = NULL,
                         label.m1.pos = NULL, label.m1.neg = NULL,
                         # m2
                         m2.num.cut = "median", m2.cat.pos = NULL, m2.cat.neg = NULL,
                         label.m2.pos = NULL, label.m2.neg = NULL,
                         km.pval= T, km.risk.table=T,
                         palette = "default",
                         palette.4quadrant = "default",
                         na.rm.marker = T,
                         ...){
  scatterplots <- .dm_survival_scatter_chart(
    data = data, time = time, event = event,
    marker1 = marker1, marker2 = marker2,
    response= response, response.pos = response.pos, response.neg = response.neg,
    label.response.pos = label.response.pos, label.response.neg = label.response.neg,
    m1.num.cut = m1.num.cut , m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
    label.m1.pos = label.m1.pos, label.m1.neg=  label.m1.neg,
    m2.num.cut = m2.num.cut , m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg,
    label.m2.pos = label.m2.pos, label.m2.neg=  label.m2.neg,
    label.m1 = label.m1, label.m2 = label.m2,
    palette = palette)
  kmplots <- .dm_KMplot(data = data,
                        time =time, event= event,
                        marker1 = marker1, marker2 = marker2,
                        label.m1 = label.m1, label.m2 = label.m2,
                        m1.num.cut = m1.num.cut, m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
                        label.m1.pos = label.m1.pos, label.m1.neg =label.m1.neg,
                        m2.num.cut = m2.num.cut, m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg,
                        label.m2.pos = label.m2.pos, label.m2.neg = label.m2.neg,
                        km.risk.table = km.risk.table, km.pval = km.pval,
                        palette = palette,
                        palette.4quadrant = palette.4quadrant,
                        na.rm.marker= na.rm.marker,
                        ...)
  g.out <- list()
  g.out$km.m1m2 <- ggpubr::ggarrange(kmplots[[1]], kmplots[[2]], align = "h", nrow=1,
                                     labels = c(paste0("Marker1: ", label.m1),
                                                paste0("Marker2: ", label.m2)))
  g.out$km.dualmarker <- kmplots[[3]]
  g.out$km.dualmarker.facet <- kmplots[[4]]
  g.out$scatter.m1m2 <- ggpubr::ggarrange(
    scatterplots[[1]], scatterplots[[2]],
    align='hv',
    common.legend = T, nrow = 1, legend = "right")
  g.out$scatter.dualmarker <- scatterplots[[3]]
  g.out[c('km.m1m2','scatter.m1m2','km.dualmarker',"km.dualmarker.facet", 'scatter.dualmarker')]
}
