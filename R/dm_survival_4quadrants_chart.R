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
dm_survival_4quad_chart <- function(data, time, event, marker1, marker2,
                                    m1.datatype ="auto", m1.num.cut="median",
                                    label.m1 = marker1,
                                    m1.cat.pos = NULL, m1.cat.neg =NULL,
                                    label.m1.pos = NULL, label.m1.neg = NULL,
                                    label.m2 = marker2,
                                    m2.datatype = "auto", m2.num.cut="median",
                                    label.m2.pos = NULL, label.m2.neg = NULL,
                                    m2.cat.pos = NULL, m2.cat.neg = NULL,
                                    palette.4quadrant = "default",
                                    title = ""){
  res <- dm_survival_4quad(data = data, time = time, event= event,
                       marker1 =marker1, marker2 = marker2,
                       m1.datatype = m1.datatype,m1.num.cut= m1.num.cut,
                       m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
                       label.m1.pos = label.m1.pos, label.m1.neg = label.m1.neg,
                       m2.datatype = m2.datatype, m2.num.cut= m2.num.cut,
                       m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg,
                       label.m2.pos = label.m2.pos, label.m2.neg = label.m2.neg
                       )
  g.area.prop <- .quadrant_prop_chart(res$stats$records,
                                      palette.4quadrant = palette.4quadrant)

  color.4quadrant <- get_4colors(palette = palette.4quadrant, alpha = 0.3)
  # matrix
  d <- res$stats %>%
    mutate(label = paste0("median surv:\n",round(median,2), "(",
                          round(`0.95LCL`,2),"-",
                          round(`0.95UCL`,2),
                          ")"))
  g.matrix <- ggplot(data = d, aes(x = .m1, y = .m2)) +
    geom_tile(aes(fill=.quadrant), color="grey")+
    geom_text(aes(label = label))+
    scale_fill_manual(values = color.4quadrant)+
    labs(x="",y="")+
    theme_void()+
    theme(legend.position = "none")
  # KMplot of four quradrant
  g.km <- .dm_KMplot(data =data, time =time, event = event,
            marker1 = marker1, marker2 = marker2,
            m1.num.cut = m1.num.cut, m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
            m2.num.cut = m2.num.cut, m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg,
            km.pval = T, km.risk.table=F,
            palette.4quadrant = palette.4quadrant) %>%
    .$dualmarker %>% .$plot

  # interaction-chart
  tmp <- res$stats
  tmp$median[is.na(tmp$median)] <- Inf
  g.interact.1 <- tmp %>%
    ggplot(aes(x = .m1, y = median, group = .m2))+
    geom_line(color="skyblue", linetype="dashed")+
    geom_label(aes(label = .quadrant, fill = .quadrant))+
    labs(x = label.m1, y = "Median survival time") +
    theme_bw()+
    theme(legend.position = "none")+
    theme(axis.text.x=element_text(size=rel(0.9))) +
    scale_fill_manual(values = color.4quadrant)
  g.interact.2 <- tmp %>%
    ggplot(aes(x = .m2, y = median, group = .m1))+
#    geom_line(aes(color = .m1, linetype= .m1))+
    geom_line(color="skyblue", linetype="dashed")+
    geom_label(aes(label = .quadrant, fill = .quadrant))+
    labs(x = label.m2, y = "") +
    theme_bw()+
    theme(legend.position = "none")+
    theme(axis.text.x=element_text(size=rel(0.9))) +
    scale_fill_manual(values = color.4quadrant)

  g.interact <- cowplot::plot_grid(g.interact.1, g.interact.2, align = "b", nrow=1 )

  g.plot <- cowplot::plot_grid(g.area.prop, g.matrix, g.km, g.interact,
                               align = "b", nrow=2, scale = c(0.9, .9, .9, 0.9), labels = "AUTO")
  g.title <- cowplot::ggdraw() +
    cowplot::draw_label(title, fontface = 'bold', x = 0, hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 20))
  cowplot::plot_grid(g.title, g.plot,ncol = 1, rel_heights = c(0.05, 1))
}


