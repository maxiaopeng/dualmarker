################################
### plot of four quadrants survival data
################################

quadrant_survival_chart <- function(data, time, event, marker1, marker2,
                                    num.cut.method = "median",
                                    m1.datatype ="auto",  m1.cat.pos = NULL, m1.cat.neg =NULL,
                                    m2.datatype = "auto", m2.cat.pos = NULL, m2.cat.neg = NULL){
  res <- dm_4quadrant_survival(data = data, time = time, event= event,
                       marker1 =marker1, marker2 = marker2,
                       num.cut.method= num.cut.method,
                       m1.datatype = m1.datatype,
                       m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
                       m2.datatype = m2.datatype,
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
  # interaction-chart
  g.interact.1 <- res$stats %>%
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

  cowplot::plot_grid(g.area.prop, g.matrix, g.interact, align = "b", nrow=2 )
}


if(F){
  quadrant_survival_chart(data = clin.bmk, time = "os", event = "censOS", marker1 = "TMB",
                          marker2 = "gepscore_gene19", num.cut.method = "median")
  quadrant_survival_chart(data = clin.bmk, time = "os", event = "censOS", marker1 = "mut_ARID1A",
                        marker2 = "gep_CXCL13", num.cut.method = "median", m1.cat.pos = "YES", m1.cat.neg = "NO")
  quadrant_survival_chart(data = clin.bmk, time = "os", event = "censOS", marker1 = "mut_ARID1A",
                               marker2 = "IC.Level", num.cut.method = "median",
                               m1.cat.pos = "YES", m1.cat.neg = "NO",
                               m2.cat.pos = c("IC0","IC1"), m2.cat.neg = c("IC2+"))
}
