# https://towardsdatascience.com/modelling-binary-logistic-regression-using-r-research-oriented-modelling-and-interpretation-f67b3a954101
# https://www.r-bloggers.com/evaluating-logistic-regression-models/
# https://rcompanion.org/rcompanion/e_06.html


##' draw bubble-pie plot
##' @param x positive counts in 1st, 2nd, 3rd and 4th quadrant
##' @param n total counts in 1st, 2nd, 3rd and 4th quadrant
##' @example
##' x <- c(5,3,12,9)
##' n <- c(8, 10, 20, 18)
##' bubblepie_chart(x,n)
##' @export

bubblepie_chart2 <- function(x, n){
  data <- .quadrant.transform(x,n)
  # pie plot
  d <- data %>%
    mutate(.x1.level = factor(.x1.level, levels = c("neg","pos")),
           .x2.level = factor(.x2.level, levels = c("pos","neg"))) %>%
    gather(key = ".class", value="count", n.pos, n.neg) %>%
    mutate(.class = ifelse(.class %in% "n.pos", "1_pos", "2_neg"))

  bubblepie <- d %>%
    ggplot(aes(x="", y = count, fill= .class))+
    geom_bar(position="fill", aes(width= n.total), stat = "identity")+
    facet_grid(cols = vars(.x1.level), rows = vars(.x2.level))+
    # geom_text(aes(label = pct.pos2, y = 1- pct.pos/2),
    #           data = d %>% filter(.class %in% c("1_pos")) %>% mutate(pct.pos2 = round(pct.pos,2)),
    #           size=1.2,color = "grey80") +
    # geom_text(aes(x = -Inf, y = -Inf, label = region), size=2, color = "grey80")+
    coord_polar("y", start=0) +
    theme(axis.text.x=element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          panel.grid=element_blank(),
          axis.ticks = element_blank(),
          plot.title=element_text(size=14, face="bold"))
  #          strip.background = element_blank(),
  #          strip.text = element_blank(),
  #          legend.position="none"
  bubblepie
}


##' dualmarker_binaryOutcome
##'
##' this function plot
##'
##' param data dataframe
##' param outcome for binary outcome
##' return stats
##' return test result
##' return scatterplot
##' return bubblepie plot
##' @export

fun.quadrant.analysis <- function(data,
                                  class, class.pos, class.neg=NULL,
                                  x1, x1.datatype="auto", x1.cont.cut = NULL, x1.cat.pos = NULL, x1.cat.neg = NULL, x1.label.pos="pos",x1.label.neg="neg",
                                  x2, x2.datatype="auto", x2.cont.cut = NULL, x2.cat.pos = NULL, x2.cat.neg = NULL, x2.label.pos="pos",x2.label.neg="neg",
                                  stats.only = F,
                                  rm.na=T,
                                  scale.log = "none",
                                  pie.include.all=F,
                                  scale.pos.ratio = c(0, 1)){
  # if x1.cont.cut = NULL, use median as cutoff

  fun.check.colnames(data, c(class, x1, x2))
  fun.check.element(data[[class]], child.set = c(class.pos))
  if(is.null(class.neg)) class.neg <- setdiff(na.omit(unique(data[[class]])), class.pos)
  fun.check.element(data[[class]], child.set = c(class.neg))
  # class
  keep <- data[[class]] %in% c(class.pos, class.neg)
  if(any(!keep)){
    print(paste0(sum(!keep), " records have no class label, removed"))
    data <- data[keep,]
  }
  data$.class <- ifelse(data[[class]]%in% class.pos, "1_pos", "2_neg")
  data$.class.binary <- ifelse(data[[class]]%in% class.pos, 1,0)
  # .x1.level, .x2.level
  if(is.null(x1.datatype)) x1.datatype <- fun.class.catcont(data[[x1]])
  if(is.null(x2.datatype)) x2.datatype <- fun.class.catcont(data[[x2]])
  if(is.null(x1.cont.cut) && x1.datatype == "cont") {
    x1.cont.cut <- median(data[[x1]], na.rm = T)
    assert_that(!is.na(x1.cont.cut), msg = "x1 is not continous var")
  }
  if(is.null(x2.cont.cut) && x2.datatype == "cont"){
    x2.cont.cut <- median(data[[x2]], na.rm = T)
    assert_that(!is.na(x2.cont.cut), msg = "x2 is not continous var")
  }
  data$.x1.level <- fun.binarize(x=data[[x1]],datatype = x1.datatype, cont.cut = x1.cont.cut,
                                 cat.pos = x1.cat.pos, cat.neg = x1.cat.neg,
                                 label.pos = x1.label.pos, label.neg = x1.label.neg)
  data$.x2.level <- fun.binarize(x=data[[x2]],datatype = x2.datatype, cont.cut = x2.cont.cut,
                                 cat.pos = x2.cat.pos, cat.neg = x2.cat.neg,
                                 label.pos = x2.label.pos, label.neg = x2.label.neg)
  #data %<>% dplyr::filter(!is.na(.x1.level), !is.na(.x2.level))
  # x1, x2 rm NA
  if(rm.na){
    keep1 <- !is.na(data$.x1.level)
    if(any(!keep1)) cat(paste0(sum(!keep1), " records were removed, as x1 is NA, "))
    keep2 <- !is.na(data$.x2.level)
    if(any(!keep2)) cat(paste0(sum(!keep2), " records were removed, as x2 is NA"))
    keep <- keep1 & keep2
    if(any(!keep)) cat(paste0("totally ", sum(!keep)," records were removed"))
    data <- data[keep,]
  }
  # statistics
  data.0 <- data.1 <- data.2 <- data
  data.0$.x1.level <- data.1$.x1.level <- "all"
  data.0$.x2.level <- data.2$.x2.level <- "all"
  data.all <-   bind_rows(data.0, data.1, data.2, data)
  stats <- data.all %>%
    group_by(.x1.level, .x2.level) %>%
    group_modify(.f = ~{
      tibble(n.total = nrow(.x),
             n.pos = sum(.x$.class.binary, na.rm = T),
             n.neg = sum(1-.x$.class.binary,na.rm = T),
             pct.pos = mean(.x$.class.binary))
    }) %>% ungroup()
  stats %<>% mutate(region = case_when(.x1.level %in% x1.label.pos & .x2.level %in% x2.label.pos ~ "R1",
                                       .x1.level %in% x1.label.neg & .x2.level %in% x2.label.pos ~ "R2",
                                       .x1.level %in% x1.label.neg & .x2.level %in% x2.label.neg ~ "R3",
                                       .x1.level %in% x1.label.pos & .x2.level %in% x2.label.neg ~ "R4",
                                       .x1.level %in% "all" & .x2.level %in% x2.label.pos ~ "R12",
                                       .x1.level %in% "all" & .x2.level %in% x2.label.neg ~ "R34",
                                       .x1.level %in% x1.label.pos & .x2.level %in% "all" ~ "R14",
                                       .x1.level %in% x1.label.neg & .x2.level %in% "all" ~ "R23",
                                       .x1.level %in% "all" & .x2.level %in% "all" ~ "R1234",
                                       TRUE ~ "other" )) %>%
    mutate(.x1.level = factor(.x1.level, levels = c(x1.label.pos, x1.label.neg, "all")),
           .x2.level = factor(.x2.level, levels = c(x2.label.pos, x2.label.neg, "all"))
    ) %>%
    dplyr::select(region, .x1.level, .x2.level, everything())
  # fisher.test
  test.x1 <- stats %>% group_by(.x1.level) %>%
    group_modify(.f = ~{
      .x %>%
        dplyr::filter(.x2.level %in% c(x2.label.pos,x2.label.neg)) %>%
        dplyr::select(.x2.level, n.pos, n.neg) %>%
        as.data.frame() %>%
        column_to_rownames(".x2.level") %>%
        as.matrix() %>%
        fisher.test() %>% broom::tidy()
    }) %>% ungroup()
  test.x2 <- stats %>% group_by(.x2.level) %>%
    group_modify(.f = ~{
      .x %>%
        dplyr::filter(.x1.level %in% c(x1.label.pos,x1.label.neg)) %>%
        dplyr::select(.x1.level, n.pos, n.neg) %>%
        as.data.frame() %>%
        column_to_rownames(".x1.level") %>%
        as.matrix() %>%
        fisher.test() %>% broom::tidy()
    }) %>% ungroup()
  test.merge <- bind_rows(test.x1, test.x2) %>%
    mutate( cmp = case_when( .x1.level %in% x1.label.neg & is.na(.x2.level) ~ "R2vsR3",
                             .x1.level %in% x1.label.pos & is.na(.x2.level) ~ "R1vsR4",
                             .x1.level %in% "all" & is.na(.x2.level) ~ "R12vsR34",
                             is.na(.x1.level) & .x2.level %in% x2.label.neg ~ "R3vsR4",
                             is.na(.x1.level) & .x2.level %in% x2.label.pos ~ "R1vsR2",
                             is.na(.x1.level) & .x2.level %in% c("all") ~ "R14vsR23",
                             TRUE ~ "other")) %>%
    dplyr::select(cmp, .x1.level, .x2.level, everything())

  scatterplot <- barplot <- pieplot <- lineplot <- dotplot <- plot <- NULL
  if(!stats.only){
    if(x1.datatype == "cont" && x2.datatype == "cont"){
      # scatter plot
      scatterplot <- ggplot(data, aes_string(x = x1, y = x2, color = ".class"))+
        geom_point() +
        geom_hline(yintercept = x2.cont.cut, linetype="dashed", color = "skyblue")+
        geom_vline(xintercept = x1.cont.cut, linetype="dashed", color = "skyblue")+
        theme(legend.position="top")
      scatterplot <- base::switch(scale.log,
                                  "both" = scatterplot + scale_y_log10() + scale_x_log10(),
                                  "x" = scatterplot + scale_x_log10(),
                                  "y" = satterplot + scale_y_log10(),
                                  "none" = scatterplot
      )
    }else if(x1.datatype == "cont" || x2.datatype == "cont"){
      if(x1.datatype == "cat" ){
        scatterplot <- ggplot(data, aes_string(x = ".x1.level", y = x2))+
          geom_jitter(aes(color = .class), width = 0.2) +
          geom_hline(yintercept = x2.cont.cut, linetype="dashed", color = "skyblue")+
          labs(x = x1) +
          theme(legend.position="top")
      }else{
        scatterplot <- ggplot(data, aes_string(x = x1, y = ".x2.level"))+
          geom_jitter(aes(color = .class), width = 0.2) +
          geom_vline(xintercept = x1.cont.cut, linetype="dashed", color = "skyblue")+
          labs(y = x2)+
          theme(legend.position="top")
      }

    }else{
      #fml <- paste0(".class ~ .x1.level+ .x2.level ")
      #scatterplot <- vcd::mosaic(as.formula(fml), data = data)
      scatterplot <- ggplot(data=data) +
        geom_mosaic(aes(x=product(.x2.level, .x1.level), fill= .class))+
        labs(x = paste0(class,":", x1), y = x2)+
        theme(legend.position="top")
    }
    # pie plot
    d <- stats %>%
      gather(key = ".class", value="count", n.pos, n.neg) %>%
      mutate(.class = ifelse(.class %in% "n.pos", "1_pos", "2_neg")) %>%
      mutate(.x1.level = factor(.x1.level, levels = c(x1.label.neg,x1.label.pos,"all")),
             .x2.level = factor(.x2.level, levels = c(x2.label.neg,x2.label.pos,"all"))) %>%
      dplyr::filter(! is.na(.x1.level), !is.na(.x2.level))
    # barplot
    d.pie <- d %>%
      mutate(.x2.level = factor(.x2.level, levels = c(x2.label.pos,x2.label.neg,"all")))
    if(! pie.include.all)
      d.pie %<>%  dplyr::filter( !.x1.level %in% "all", !.x2.level %in% "all")
    theme.bar <- theme(axis.text.x=element_blank(),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       panel.border = element_blank(),
                       panel.grid=element_blank(),
                       axis.ticks = element_blank(),
                       plot.title=element_text(size=14, face="bold"))

    barplot <- d.pie %>%
      ggplot(aes(x=.class, y = count, fill= .class))+
      geom_bar(position="dodge", stat = "identity")+
      #annotate(geom = "text", x = 0.5, y = 0.5, label ="hello")+
      facet_grid(cols = vars(.x1.level), rows = vars(.x2.level))+
      theme.bar +
      theme(legend.title = element_blank())+
      labs(x = x1, y = x2)
    pieplot <- d.pie %>%
      ggplot(aes(x="", y = count, fill= .class))+
      geom_bar(position="fill", aes(width= n.total), stat = "identity")+
      facet_grid(cols = vars(.x1.level), rows = vars(.x2.level))+
      geom_text(aes(label = pct.pos2, y = 1- pct.pos/2),
                data = d.pie %>% filter(.class %in% c("1_pos")) %>% mutate(pct.pos2 = round(pct.pos,2)),
                size=1.2,color = "grey80") +
      geom_text(aes(x = -Inf, y = -Inf, label = region), size=2, color = "grey80")+
      theme.bar +
      coord_polar("y", start=0)+
      theme(legend.position="none")
    # lineplot
    lineplot <- d %>%
      dplyr::filter( !.x1.level %in% "all", !.x2.level %in% "all") %>%
      ggplot(aes(x = .x1.level, y = pct.pos, group = .x2.level))+
      geom_line(aes(color = .x2.level, linetype=.x2.level))+
      #geom_point(aes(size = n.total, shape = .x2.level))+
      geom_label(aes(label = region), color = "red")+
      labs(x = x1, title = paste0("x2:", x2)) +
      scale_y_continuous(limits = scale.pos.ratio)+
      theme(legend.position="top")

    dotplot <- d %>%
      unite(str.ratio, count, n.total, sep="/", remove = F) %>%
      filter(.class %in% c("1_pos")) %>%
      ggplot(aes(x = .x2.level,y = pct.pos))+
      geom_point()+
      facet_wrap(~.x1.level,nrow = 1)+
      geom_text_repel(aes(label = str.ratio), color = "blue", size=2)+
      geom_label(aes(label = region, y=0), color ="blue", size=3)+
      geom_hline(yintercept = d$pct.pos[d$.x1.level=="all" & d$.x2.level=="all" ],
                 color = "blue",
                 linetype= "dashed")+
      labs(x = x2)+
      scale_y_continuous(limits = scale.pos.ratio)

    plot <- cowplot::plot_grid(scatterplot, pieplot, lineplot, dotplot, align = "hv", axis = "l", ncol = 2)
  }
  list(data=data, stats = stats, test=test.merge,plot = plot)
}


# module-1. single marker
g.bxp1 <- ggboxplot(data = data, x = var.outcome, y = marker1) +
  stat_compare_means()
g.bxp2 <- ggboxplot(data = data, x = var.outcome, y = marker2) +
  stat_compare_means()

# module-2. inter-marker
# 2.1 scatterplot
g.scatterplot <- ggscatter(data = data, x = marker1, y = marker2)+
  stat_cor()
# 2.2 4-quadrant prop
g.quadProp <- plot_4quadrant_prop()


# color:
# R1: c(195, 214, 155), green
# R2: c(185, 205, 229), blue
# R3: c(217, 150, 148), red
# R4: c(254, 254, 210), yellow

# color, pembro TMB+GEP
# R1: c(249, 190, 192), red, #F9BEC0
# R2: c(207,236, 242), blue, #CFECF2
# R3: c(241,241, 241), grey, #F1F1F1
# R4: c(207, 215, 228), purple #CFD7E4

#rgb(249, 190, 192, max=255)
#rgb(207,236, 242, max=255)
#rgb(241,241, 241, max=255)
#rgb(207, 215, 228, max=255)


load("inst/extdata/my_IMvigor210_data.RData")
source("~/working/Biomarker_integrated_reporter/src/my_functions.R")
data <- clin.bmk
marker1 <- "TMB"
marker2 <- "gepscore_CD.8.T.effector"
outcome <- "binaryResponse"
outcome.pos <- "CR/PR"
outcome.neg <- "SD/PD"
surv.time <- "os"
surv.event <- "censOS"
dualmarker.plot <- function(data, marker1, marker2,
                            outcome, outcome.pos, outcome.neg=NULL,
                            surv.time, surv.event, quiet=F){
  # check input
  assert_that(all(c(outcome, marker1, marker2) %in% colnames(data)),
              msg = paste(c(marker1, marker2, outcome, "not in your data")))
  if(is.null(outcome.neg)) outcome.neg <- setdiff(na.omit(unique(data[[outcome]])), outcome.pos)
  # prep data
  keep <- data[[outcome]] %in% c(outcome.pos, outcome.neg)
  if(any(!keep)){
    if(!quiet) print(paste0("warning: ",sum(!keep), " records have no expected outcome, removed\n"))
    data <- data[keep,]
  }
  # model1: pair-wise correlation
  GGally::ggpairs()

  # module2. combine markers
  # 3.1 scatterplot

  # 3.2 bubble-pie plot

  # 3.3 interaction plot

}



# model evalution, Goodness of Fit, Likelihood Ratio Test
anova(m.logist1, m.logist2, test ="Chisq")

#library(lmtest)
#lrtest(m.logist1, m.logist2)

# Hosmer-Lemeshow Test
#library(MKmisc)
#HLgof.test(fit = fitted(mod_fit_one), obs = training$Class)

#library(ResourceSelection)
#hoslem.test(training$Class, fitted(mod_fit_one), g=10)

# AIC
AIC(m.logist1)
AIC(m.logist2)
# pseudo-R
pR1 <- nagelkerke(m.logist1)
pR2 <- nagelkerke(m.logist2)

#library(pscl)
#pR2(mod_fit_one)  # look for 'McFadden'
#
# varImp(mod_fit)


# 1. parameters
broom::tidy(logit.model)

# 2. goodness-of-fit
# 2.1 Likelihood Ratio Test
# 2.1.1: anova
anova(logit.model, test="Chisq")
# 2.1.2: Likelihood.ratio.test from nagelkerke
rcompanion::nagelkerke(logit.model)$Likelihood.ratio.test
# 2.1.3: lrtest
lmtest::lrtest(logit.model) # likehood ratio test

# 2.2. pesudo-R
(res.nagelkerke <- rcompanion::nagelkerke(logit.model))
# 2.3 Hosmer-Lemeshow Test
#library(MKmisc)
#MKmisc::HLgof.test(fit = fitted(logit.model), obs = logit.model$data$.outcome)

# 3. Statistical Tests for Individual Predictors
#pval for each parameters
#car::Anova(logit.model, type="II", test="Wald")
#car::Anova(logit.model, type="III", test="Wald")


# 5. Plot of standardized residuals
plot(fitted(logit.model), rstandard(logit.model))

# 6. comparison between models
# anova(logit.m1, logit.m2) #


##' @description
##' binarize x
##' @param x
##' @return vector with binary values: (label.pos, label.neg) or (0,1)
binarize.data <- function(x, datatype="auto",
                          num.cutpoint = NULL,
                          cat.pos=NULL, cat.neg = NULL,
                          as.binary=F,
                          label.pos = "pos", label.neg = "neg"){
  if("factor" %in% class(x)){ x <- as.character(x)}
  assert_that(datatype %in% c("auto","cat","num"),
              msg = "datatype should be [auto,num, cat]")
  if(datatype %in% "auto")
    datatype <- datatype.num.cat(x)
  if(datatype %in% "cat"){
    assert_that(!is.null(cat.pos), msg = "cat.pos is reqired")
    binarize.cat(x = x, pos = cat.pos, neg = cat.neg,
                 as.binary = as.binary,
                 label.pos = label.pos, label.neg = label.neg)
  }else if(all(datatype %in% "num")){
    assert_that(!is.null(num.cutpoint), msg = "num.cutpoint is reqired")
    binarize.num(x = as.numeric(x), cutpoint = num.cutpoint,
                 as.binary = as.binary, label.pos = label.pos, label.neg = label.neg)
  }
}

##' statistic outcome positive rate in each quadrant
##' @param binary.data
##' @return list of stat.quad and test.quad
##'
.stat4quad <- function(binary.data){
  assert_that( all(c(".outcome",".m1",".m2") %in% colnames(binary.data)),
               msg = ".outcome, .m1, .m2 are required")
  data <- binary.data
  data$.binary.outcome <- ifelse(data$.outcome == "pos", 1,
                                 ifelse(data$.outcome == "neg", 0, NA))
  data.0 <- data.1 <- data.2 <- data
  data.0$.m1 <- data.1$.m1 <- "all"
  data.0$.m2 <- data.2$.m2 <- "all"
  data.all <- bind_rows(data.0, data.1, data.2, data)
  stats <- data.all %>%
    group_by(.m1, .m2) %>%
    group_modify(.f = ~{
      n.total <- nrow(.x)
      n.pos <- sum(.x$.binary.outcome, na.rm = T)
      n.neg <- sum(1- .x$.binary.outcome, na.rm = T)
      ratio.est <- binom.test(n.pos, n.total)
      tibble(n.total = n.total,
             n.pos = n.pos,
             n.neg = n.neg,
             pos.ratio = ratio.est$estimate,
             pos.lower95 = ratio.est$conf.int[1],
             pos.upper95 = ratio.est$conf.int[2])
    }) %>% ungroup()
  stats %<>% mutate(region = case_when(.m1 %in% "pos" & .m2 %in% "pos" ~ "R1",
                                       .m1 %in% "neg" & .m2 %in% "pos" ~ "R2",
                                       .m1 %in% "neg" & .m2 %in% "neg" ~ "R3",
                                       .m1 %in% "pos" & .m2 %in% "neg" ~ "R4",
                                       .m1 %in% "all" & .m2 %in% "pos" ~ "R12",
                                       .m1 %in% "all" & .m2 %in% "neg" ~ "R34",
                                       .m1 %in% "pos" & .m2 %in% "all" ~ "R14",
                                       .m1 %in% "neg" & .m2 %in% "all" ~ "R23",
                                       .m1 %in% "all" & .m2 %in% "all" ~ "R1234",
                                       TRUE ~ "other" )) %>%
    mutate(.m1 = factor(.m1, levels = c("pos", "neg", "all")),
           .m2 = factor(.m2, levels = c("pos", "neg", "all"))
    ) %>%  dplyr::select(region, .m1, .m2, everything())
  # fisher.test
  test.x1 <- stats %>% group_by(.m1) %>%
    group_modify(.f = ~{
      .x %>%
        dplyr::filter(.m2 %in% c("pos","neg")) %>%
        dplyr::select(.m2, n.pos, n.neg) %>%
        as.data.frame() %>%
        column_to_rownames(".m2") %>%
        as.matrix() %>%
        fisher.test() %>% broom::tidy()
    }) %>% ungroup()
  test.x2 <- stats %>% group_by(.m2) %>%
    group_modify(.f = ~{
      .x %>%
        dplyr::filter(.m1 %in% c("pos","neg")) %>%
        dplyr::select(.m1, n.pos, n.neg) %>%
        as.data.frame() %>%
        column_to_rownames(".m1") %>%
        as.matrix() %>%
        fisher.test() %>% broom::tidy()
    }) %>% ungroup()
  test.merge <- bind_rows(test.x1, test.x2) %>%
    mutate( cmp = case_when( .m1 %in% "neg" & is.na(.m2) ~ "R2vsR3",
                             .m1 %in% "pos" & is.na(.m2) ~ "R1vsR4",
                             .m1 %in% "all" & is.na(.m2) ~ "R12vsR34",
                             is.na(.m1) & .m2 %in% "neg" ~ "R3vsR4",
                             is.na(.m1) & .m2 %in% "pos" ~ "R1vsR2",
                             is.na(.m1) & .m2 %in% c("all") ~ "R14vsR23",
                             TRUE ~ "other")) %>%
    dplyr::select(cmp, .m1, .m2, everything())
  list(stat.quad = stats, test.quad = test.merge)
}

##' @param data
##' @param marker1
##' @param targets
##' @return dataframe, each row is marker2
dm_corr <- function(data, marker1, targets, method="spearman"){
  names(targets) <- targets
  map_dfr(targets, .f = ~{
    tibble(cor = cor(data[[marker1]], data[[.x]],
                     use = "pairwise.complete.obs",
                     method = method))},
    .id = "marker2")
}

