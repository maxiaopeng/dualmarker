#' survival plot of 4 quadrants
#'
#'
#' @param data dataframe
#' @param time survival time
#' @param event survival event
#' @param marker1 marker1
#' @param marker2 marker2
#' @param m1.datatype data type of marker1
#' @param m1.cat.pos positive value(s) if marker1 is categorical
#' @param m1.cat.neg  negative value(s) if marker1 is categorical
#' @param m2.datatype data type of marker1
#' @param m2.cat.pos positive value(s) if marker2 is categorical
#' @param m1.num.cut cut method/value for numeric marker1
#' @param m2.num.cut cut method/value for numeric marker2
#' @param m2.cat.neg negative value(s) if marker2 is categorical
#'
#' @return panel of plot
#' @export
dm_survival_4quad_chart <- function(data, time, event, marker1, marker2,
                                    m1.datatype ="auto", m1.num.cut="median",  m1.cat.pos = NULL, m1.cat.neg =NULL,
                                    m2.datatype = "auto", m2.num.cut="median", m2.cat.pos = NULL, m2.cat.neg = NULL){
  res <- dm_survival_4quad(data = data, time = time, event= event,
                       marker1 =marker1, marker2 = marker2,
                       m1.datatype = m1.datatype,m1.num.cut= m1.num.cut,
                       m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
                       m2.datatype = m2.datatype, m2.num.cut= m2.num.cut,
                       m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg)
  g.area.prop <- .quadrant_prop_chart(res$stats$records)
  # matrix
  d <- res$stats %>%
    mutate(label = paste0("median surv:\n",round(median,2), "(",
                          round(`0.95LCL`,2),"-",
                          round(`0.95UCL`,2),
                          ")"))
  g.matrix <- ggplot(data = d, aes(x = .m1, y = .m2)) +
    geom_tile(aes(fill=.quadrant), color="grey")+
    geom_text(aes(label = label))+
    scale_fill_manual(values = color.quadrant.1)+
    labs(x="",y="")+
    theme_void()+
    theme(legend.position = "none")
  # KMplot of four quradrant
  g.km <- dm_KMplot(data =data, time =time, event = event,
            marker1 = marker1, marker2 = marker2,
            m1.num.cut = m1.num.cut, m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
            m2.num.cut = m2.num.cut, m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg,
            km.pval = F, km.risk.table=F, legend="none") %>%
    .$dualmarker %>% .$plot
  # interaction-chart
  tmp <- res$stats
  tmp$median[is.na(tmp$median)] <- Inf
  g.interact.1 <- tmp %>%
    ggplot(aes(x = .m1, y = median, group = .m2))+
    #geom_line(aes(color = .m2, linetype= .m2))+
    geom_line(color="skyblue", linetype="dashed")+
    geom_label(aes(label = .quadrant, fill = .quadrant))+
    labs(x = "Marker1", y = "Median survival time") +
    theme_bw()+
    theme(legend.position = "none")+
    scale_fill_manual(values = color.quadrant.1)
  g.interact.2 <- res$stats %>%
    ggplot(aes(x = .m2, y = median, group = .m1))+
#    geom_line(aes(color = .m1, linetype= .m1))+
    geom_line(color="skyblue", linetype="dashed")+
    geom_label(aes(label = .quadrant, fill = .quadrant))+
    labs(x = "Marker2", y = "") +
    theme_bw()+
    theme(legend.position = "none")+
    scale_fill_manual(values = color.quadrant.1)

  g.interact <- cowplot::plot_grid(g.interact.1, g.interact.2, align = "b", nrow=1 )

  cowplot::plot_grid(g.area.prop, g.matrix, g.km, g.interact, align = "b", nrow=2 )
}


