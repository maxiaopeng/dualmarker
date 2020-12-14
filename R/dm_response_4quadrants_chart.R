################
## plot of four quadrants data
################

#' Four quadrant proportional area chart
#'
#' @param x vector of 4 numbers, for sample numbers of 1st, 2nd, 3rd and 4th quadrant
#' @return ggplot object
.quadrant_prop_chart <- function(x,
                                test_method="fisher.test",
                                palette.4quadrant = "default") {
  color.4quadrant <- get_4colors(palette = palette.4quadrant, alpha = 0.3)
  nx <- length(x)
  sqx <- sqrt(x)
  df <- data.frame(x=c(sqx[1],-sqx[2],-sqx[3],sqx[4])/2,
                   y=c(sqx[1],sqx[2],-sqx[3],-sqx[4])/2,
                   size=sqx, label=x,
                   region = c("R1","R2","R3","R4"))
  mm <- max(df$size)*1.1
  res.test <- NULL
  mat <- matrix(c(x[1],x[2],x[4],x[3]), nrow=2)
  if(test_method == "fisher.test"){
    res.test <- fisher.test(mat)
  }else if(test_method == "chisq.test"){
    res.test <- chisq.test(mat)
  }
  subtitle <- NULL
  if(!is.null(res.test)){
    subtitle <- paste0(
      test_method,": ",
      "pval=", signif(res.test$p.value,3))
  }
  ggplot(data=df, aes(x=x, y=y, width=size, height=size,
                      group=factor(size))) +
    geom_tile(aes(fill=region), color = "grey") +
    geom_text(aes(label=label), col= "black", size=5) +
    geom_hline(aes(yintercept=0), size=0.8, color = "grey") +
    geom_vline(aes(xintercept=0), size=0.8, color = "grey") +
    labs(title = paste0("total ", sum(x)),
         subtitle = subtitle)+
    coord_fixed() +
    xlim(c(-mm,mm)) + ylim(c(-mm,mm)) +
    theme_void() +
    scale_fill_manual(values = color.4quadrant)+
    theme(legend.position = "none")
}

#' Quadrant Interaction Chart
#'
#' line chart to show the response rate in each quadrant
#' @param x positive counts in 1st, 2nd, 3rd and 4th quadrant
#' @param n total counts in 1st, 2nd, 3rd and 4th quadrant
.quadrant_interact_chart <- function(x, n,
                                     label.m1.pos = "pos",
                                     label.m1.neg = "neg",
                                     label.m2.pos = "pos",
                                     label.m2.neg = "neg",
                                     label.m1 = "Marker1",
                                     label.m2 = "Marker2",
                                     palette.4quadrant = "default"){
  assert_that(all(x<=n), msg = "positive number should less than total number in each quadrant")
  color.4quadrant <- get_4colors(palette = palette.4quadrant, alpha = 0.3)
  data <- .quadrant_stats_response(x,n) %>%
    mutate(.m1.level = dplyr::recode(as.character(.m1.level), pos = label.m1.pos, neg = label.m1.neg) %>%
             factor(levels = c(label.m1.neg, label.m1.pos))) %>%
    mutate(.m2.level = dplyr::recode(as.character(.m2.level), pos = label.m2.pos, neg = label.m2.neg) %>%
             factor(levels = c(label.m2.neg, label.m2.pos)))

  g1 <- data %>%
    ggplot(aes(x = .m1.level, y = pct.pos, group = .m2.level))+
    geom_line(color="skyblue", linetype="dashed")+
    geom_label(aes(label = region, fill = region))+
    labs(x = label.m1, y = "Positive Percent") +
    theme_bw()+
    theme(legend.position = "none")+
    theme(axis.text.x=element_text(size=rel(0.9))) +
    scale_fill_manual(values = color.4quadrant)

  g2 <- data %>%
    ggplot(aes(x = .m2.level, y = pct.pos, group = .m1.level))+
    geom_line(color="skyblue", linetype="dashed")+
    geom_label(aes(label = region, fill = region))+
    labs(x = label.m2, y = "") +
    theme_bw()+
    theme(legend.position = "none")+
    theme(axis.text.x=element_text(size=rel(0.9))) +
    scale_fill_manual(values = color.4quadrant)
  cowplot::plot_grid(g1, g2, nrow=1)
}


#' Quadrant Matrix Chart
#'
#' 2X2 matrix plot showing positive ratio in each quadrant
#'
#' @param x positive counts in 1st, 2nd, 3rd and 4th quadrant
#' @param n total counts in 1st, 2nd, 3rd and 4th quadrant
.quadrant_matrix_chart <- function(x, n, palette.4quadrant = "default"){
  assert_that(all(x<=n), msg = "positive number should less than total number in each quadrant")
  color.4quadrant <- get_4colors(palette = palette.4quadrant, alpha = 0.3)
  data <- .quadrant_stats_response(x,n) %>%
    mutate(.m2.level = factor(.m2.level, levels =c("neg","pos")),
           .m1.level = factor(.m1.level, levels =c("neg","pos")),
           label = paste0(round(pct.pos,2)*100, "% (", n.pos, "/", n.total,")\n",
                          "(",
                          round(pos.lower95,2),
                          "-",
                          round(pos.upper95,2),
                          ")")
    )
  ggplot(data = data, aes(x = .m1.level, y = .m2.level)) +
    geom_tile(aes(fill=region), color="grey")+
    geom_text(aes(label = label))+
    scale_fill_manual(values = color.4quadrant)+
    labs(x="",y="")+
    theme_void()+
    theme(legend.position = "none")
}


#' Quadrant doughnut chart
#'
#' doughnut chart to show response rate in each quadrant
#'
#' The width of doughnut reflects the relative sample size in the quadrant, and
#' red proportion reflects the response rate
#'
#' @param x positive counts in 1st, 2nd, 3rd and 4th quadrant
#' @param n total counts in 1st, 2nd, 3rd and 4th quadrant
.quadrant_donut_chart <- function(x, n,
                                  label.m1.pos = "pos", label.m1.neg = "neg",
                                  label.m2.pos="pos", label.m2.neg ="neg",
                                  palette = "default"){
  assert_that(all(x<=n), msg = "positive number should less than total number in each quadrant")
  data <- .quadrant_stats_response(x = x, n = n) %>%
    mutate(.m1.level = factor(.m1.level, levels = c("neg", "pos"))) %>%
    mutate(.m2.level = factor(.m2.level, levels = c("pos", "neg"))) %>%
    mutate(n.total.relative = n.total / max(n.total))
  # pie plot
  d <- data %>%
    tidyr::gather(key = ".class", value="count", n.pos, n.neg) %>%
    mutate(.class = ifelse(.class %in% "n.pos", "pos", "neg") %>%
             factor(levels=c("pos","neg")))

  labeller.m1 <- function(x) ifelse(x %in% "pos", label.m1.pos, label.m1.neg)
  labeller.m2 <- function(x) ifelse(x %in% "pos", label.m2.pos, label.m2.neg)

  ggplot(data =d) +
    ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 1-n.total.relative, r = 1, amount = count,
                     fill = .class),
                 stat = 'pie', size=0)+
    facet_grid(cols = vars(.m1.level), rows = vars(.m2.level),
               labeller =  labeller(
                 .m1.level = labeller.m1,
                 .m2.level = labeller.m2)
               )+
    coord_fixed() +
    ggforce::theme_no_axes() +
    ##scale_fill_manual(values = c(color.quadrant.1, "pos"="#F8766DFF", "neg"="#00BFC4FF"))+
    theme(legend.position="none") +
    scale_fill_manual(values = ggpubr::get_palette(palette = palette, k=2))

  # theme.bar <- theme(axis.text.x=element_blank(),
  #                    axis.title.x = element_blank(),
  #                    axis.title.y = element_blank(),
  #                    panel.border = element_blank(),
  #                    panel.grid=element_blank(),
  #                    axis.ticks = element_blank(),
  #                    plot.title=element_text(size=14, face="bold"))
  # p <-  d %>%
  #   ggplot(aes(x="", y = count, fill= .class))+
  #   geom_bar(position="fill", aes(width= n.total), stat = "identity")+
  #   facet_grid(cols = vars(.m1.level), rows = vars(.m2.level))+
  #   # geom_text(aes(label = pct.pos2, y = 1- pct.pos/2),
  #   #           data = d.pie %>% filter(.class %in% c("1_pos")) %>% mutate(pct.pos2 = round(pct.pos,2)),
  #   #           size=1.2,color = "grey80") +
  #   geom_text(aes(x = -Inf, y = -Inf, label = region), size=2, color = "grey80")+
  #   theme.bar +
  #   coord_polar("y", start=0)+
  #   theme(legend.position="none")
}

#' Quadrant response chart
#'
#' panel of plots, showing per quadrant: 1) sample size using proportion chart,
#' 2) response rate using matrix chart and 3) donut chart, response difference
#' between quadrant using 4) line chart
#'
#' @param x positive number in 1st,2nd,3rd and 4th quadrant
#' @param n total number in 1st,2nd,3rd and 4th quadrant
#' @return Panel of 4 ggplot plots
dm_response_4quad_chart <- function(x,n, title = "",
                                    label.m1.pos = "pos",
                                    label.m1.neg = "neg",
                                    label.m2.pos = "pos",
                                    label.m2.neg = "neg",
                                    label.m1 = "Marker1",
                                    label.m2 = "Marker2",
                                    palette = "default",
                                    palette.4quadrant = "default"){
  assert_that(all(x >= 0), length(x)==4, is(x, "numeric"),
              msg = "positive number in each quadrant should be integer, with length 4")
  assert_that(all(n >= 0), length(n)==4, is(n, "numeric"),
              msg = "total number in each quadrant should be integer, with length 4")
  assert_that(all(x<=n),
              msg = "positive number should less than total number in each quadrant")
  p1 <- .quadrant_prop_chart(x = n, palette.4quadrant = palette.4quadrant)
  p2 <- .quadrant_matrix_chart(x = x, n = n, palette.4quadrant = palette.4quadrant)
  p3 <- .quadrant_donut_chart(x = x, n = n,
                              label.m1.pos = label.m1.pos,
                              label.m1.neg = label.m1.neg,
                              label.m2.pos = label.m2.pos,
                              label.m2.neg = label.m2.neg,
                              palette = palette)
  p4 <- .quadrant_interact_chart(x = x, n = n,
                                 label.m1.pos = label.m1.pos,
                                 label.m1.neg = label.m1.neg,
                                 label.m2.pos = label.m2.pos,
                                 label.m2.neg = label.m2.neg,
                                 label.m1 = label.m1,
                                 label.m2 = label.m2,
                                 palette.4quadrant = palette.4quadrant
                                 )
  g.plot <- cowplot::plot_grid(p1,p2,p3,p4, align = "b",
                               ncol = 2, scale = c(0.9, .9, .9, 0.9), labels = "AUTO")
  g.title <- cowplot::ggdraw() +
    cowplot::draw_label(title, fontface = 'bold', x = 0, hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 20))
  cowplot::plot_grid(g.title, g.plot,ncol = 1, rel_heights = c(0.05, 1)
  )
}
