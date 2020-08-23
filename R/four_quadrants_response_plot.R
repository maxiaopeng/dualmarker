################################
### plot of four quadrants data
################################

##' draw four quadrant proportional area chart
##'
##' this function plot
##'
##' @param x vector of 4 numbers, for 1st, 2nd, 3rd and 4th quadrant
##' @param col_quad color of quadrant
##' @param col_text color of text
##' @return ggplot object
.quadrant_prop_chart <- function(x,
                                test_method="fisher.test") {
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
    scale_fill_manual(values = color.quadrant.1)+
    theme(legend.position = "none")
}

##' bubble-pie plot to show positive ratio in each quadrant
##' @param x positive counts in 1st, 2nd, 3rd and 4th quadrant
##' @param n total counts in 1st, 2nd, 3rd and 4th quadrant
##' @example
##' x <- c(5,3,12,9)
##' n <- c(8, 10, 20, 18)
##' bubblepie_chart(x,n)

.quadrant_interact_chart <- function(x, n){
  data <- .quadrant.stats.response(x,n)
  g1 <- data %>%
    ggplot(aes(x = .m1.level, y = pct.pos, group = .m2.level))+
    #geom_line(aes(color = .m2.level, linetype=.m2.level))+
    geom_line(color="skyblue", linetype="dashed")+
    geom_label(aes(label = region, fill = region))+
    labs(x = "Marker1", y = "Positive Ratio") +
    theme_bw()+
    theme(legend.position = "none")+
    scale_fill_manual(values = color.quadrant.1)
  g2 <- data %>%
    ggplot(aes(x = .m2.level, y = pct.pos, group = .m1.level))+
    #geom_line(aes(color = .m1.level, linetype=.m1.level))+
    geom_line(color="skyblue", linetype="dashed")+
    geom_label(aes(label = region, fill = region))+
    labs(x = "Marker2", y = "") +
    theme_bw()+
    theme(legend.position = "none")+
    scale_fill_manual(values = color.quadrant.1)
  cowplot::plot_grid(g1, g2, nrow=1)
}


##' show positive ratio in each quadrant using matrix
##' @param x positive counts in 1st, 2nd, 3rd and 4th quadrant
##' @param n total counts in 1st, 2nd, 3rd and 4th quadrant
##' @example
##' x <- c(5,3,12,9)
##' n <- c(10, 10, 20, 18)
##' quadrant_matrix_chart(x,n)
.quadrant_matrix_chart <- function(x, n){
  data <- .quadrant.stats.response(x,n) %>%
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
    scale_fill_manual(values = color.quadrant.1)+
    labs(x="",y="")+
    theme_void()+
    theme(legend.position = "none")
}


##' draw donut plot
##' @param x positive counts in 1st, 2nd, 3rd and 4th quadrant
##' @param n total counts in 1st, 2nd, 3rd and 4th quadrant
##' @example
##' x <- c(5,3,12,9)
##' n <- c(8, 10, 20, 18)
##' bubblepie_chart(x,n)
##'
.quadrant_donut_chart <- function(x, n){
  data <- .quadrant.stats.response(x,n) %>%
    mutate(.m1.level = factor(.m1.level, levels = c("neg","pos")),
           .m2.level = factor(.m2.level, levels = c("pos","neg"))) %>%
    mutate(n.total.relative = n.total / max(n.total))
  # pie plot
  d <- data %>%
    gather(key = ".class", value="count", n.pos, n.neg) %>%
    mutate(.class = ifelse(.class %in% "n.pos", "pos", "neg") %>% factor(levels=c("pos","neg")))

  ggplot(data =d) +
    #geom_rect(aes(fill = region),xmin = -Inf,xmax = Inf,
    #          ymin = -Inf,ymax = Inf,alpha = .4) +
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 1-n.total.relative, r = 1, amount = count,
                     fill = .class),
                 stat = 'pie', size=0)+
    #geom_text(aes(x=0, y=0, label = region), color = "royalblue", size=3)+
    facet_grid(cols = vars(.m1.level), rows = vars(.m2.level))+
    coord_fixed() +
    theme_no_axes() +
    #scale_fill_manual(values = c(color.quadrant.1, "pos"="#F8766DFF", "neg"="#00BFC4FF"))+
    theme(legend.position="none")
  # scale_fill_brewer('', type = 'qual') +
  # theme(strip.background = element_blank(),
  # strip.text = element_blank(),
  # legend.position="none")
}

##' @param x
##' @param n
quadrant_response_chart <- function(x,n){
  p1 <- .quadrant_prop_chart(x = n)
  p2 <- .quadrant_matrix_chart(x = x, n = n)
  p3 <- .quadrant_donut_chart(x = x, n = n)
  p4 <- .quadrant_interact_chart(x = x, n = n)
  cowplot::plot_grid(p1,p2,p3,p4, align = "hv", axis = "l", ncol = 2)
  #gridExtra::arrangeGrob(p1, p2, p3,p4, nrow=2)
}
