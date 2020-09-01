##' convert positive number and total numbers in each quadrant to dataframe
##' @param x positive numbers in each quadrant
##' @param n total numbers in each quadrant
##' @example
##' x <- c(5,3,12,9)
##' n <- c(8, 10, 20, 18)
##' @return dataframe with n.total, n.pos, n.neg, pct.pos, pos.lower95, pos.upper95, region, .x1.level, .x2.level
.quadrant.transform <- function(x, n){
  out <- map2_df(.x = x, .y = n, .f = ~{
    res <- binom.test(.x, .y);
    tibble(
      n.total = .y,
      n.pos = .x,
      n.neg = .y-.x,
      pct.pos = res$estimate,
      pos.lower95 = res$conf.int[1],
      pos.upper95 = res$conf.int[2])
  })
  cbind(region = c("R1","R2","R3","R4"),
        .x1.level = c("pos","neg","neg","pos"),
        .x2.level = c("pos","pos","neg","neg"),
        out)
}

##' draw four quadrant proportional area chart
##'
##' this function plot
##'
##' @param x vector of 4 numbers, for 1st, 2nd, 3rd and 4th quadrant
##' @param col_quad color of quadrant
##' @param col_text color of text
##' @return ggplot object
##' @export
quadrant_prop_chart <- function(x,
                                test_method="fisher.test") {
  nx <- length(x)
  sqx <- sqrt(x)
  df <- data.frame(x=c(sqx[1],-sqx[2],-sqx[3],sqx[4])/2,
                   y=c(sqx[1],sqx[2],-sqx[3],-sqx[4])/2,
                   size=sqx, label=x,
                   region = c("R1","R2","R3","R4"))
  mm <- max(df$size)*1.1
  res.test <- NULL
  if(test_method == "fisher.test"){
    res.test <- matrix(x, nrow=2) %>% fisher.test()
  }else if(test_method == "chisq.test"){
    res.test <- matrix(x, nrow=2) %>% chisq.test()
  }
  subtitle <- NULL
  if(!is.null(res.test)){
    subtitle <- paste0(
      test_method,": ",
      "pval=", signif(res.test$p.value,3))
  }
  ggplot(data=df, aes(x=x, y=y, width=size, height=size,
                      group=factor(size))) +
    geom_tile(aes(fill=region)) +
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
##' @export
quadrant_interact_chart <- function(x, n){
  data <- .quadrant.transform(x,n)
  data %>%
    ggplot(aes(x = .x1.level, y = pct.pos, group = .x2.level))+
    geom_line(aes(color = .x2.level, linetype=.x2.level))+
    geom_label(aes(label = region, fill = region))+
    labs(x = "marker1", y = "Positive Ratio") +
    theme_bw()+
    theme(legend.position = "none")+
    scale_fill_manual(values = color.quadrant.1)
}


##' show positive ratio in each quadrant using matrix
##' @param x positive counts in 1st, 2nd, 3rd and 4th quadrant
##' @param n total counts in 1st, 2nd, 3rd and 4th quadrant
##' @example
##' x <- c(5,3,12,9)
##' n <- c(10, 10, 20, 18)
##' quadrant_matrix_chart(x,n)
##' @export
quadrant_matrix_chart <- function(x, n){
  data <- .quadrant.transform(x,n) %>%
    mutate(.x2.level = factor(.x2.level, levels =c("neg","pos")),
           .x1.level = factor(.x1.level, levels =c("neg","pos")),
           label = paste0(round(pct.pos,2)*100, "% (", n.pos, "/", n.total,")\n",
                          "(",
                          round(pos.lower95,2),
                          "-",
                          round(pos.upper95,2),
                          ")")
    )
  ggplot(data = data, aes(x = .x1.level, y = .x2.level)) +
    geom_tile(aes(fill=region), color="black")+
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
##' @export
quadrant_donut_chart <- function(x, n){
  data <- .quadrant.transform(x,n) %>%
    mutate(.x1.level = factor(.x1.level, levels = c("neg","pos")),
           .x2.level = factor(.x2.level, levels = c("pos","neg"))) %>%
    mutate(n.total.relative = n.total / max(n.total))
  # pie plot
  d <- data %>%
    gather(key = ".class", value="count", n.pos, n.neg) %>%
    mutate(.class = ifelse(.class %in% "n.pos", "pos", "neg") %>% factor(levels=c("neg","pos")))

  ggplot(data =d) +
    geom_rect(aes(fill = region),xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf,alpha = .4) +
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 1-n.total.relative, r = 1, amount = count,
                     fill = .class),
                 stat = 'pie', size=0)+
    geom_text(aes(x=0, y=0, label = region), color = "royalblue", size=3)+
    facet_grid(cols = vars(.x1.level), rows = vars(.x2.level))+
    coord_fixed() +
    theme_no_axes() +
    scale_fill_manual(values = c(color.quadrant.1, "pos"="#F8766D", "neg"="#00BFC4"))+
    theme(legend.position="none")
  # scale_fill_brewer('', type = 'qual') +
  # theme(strip.background = element_blank(),
  # strip.text = element_blank(),
  # legend.position="none")
}

##' @param x
##' @param n
quadrant_chart <- function(x,n){
  p1 <- quadrant_prop_chart(x = n)
  p2 <- quadrant_matrix_chart(x = x, n = n)
  p3 <- quadrant_donut_chart(x = x, n = n)
  p4 <- quadrant_interact_chart(x = x, n = n)
  gridExtra::grid.arrange(p1, p2, p3,p4, nrow=2)
}
