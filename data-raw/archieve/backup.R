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
    dplyr::group_by(.x1.level, .x2.level) %>%
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
  test.x1 <- stats %>% dplyr::group_by(.x1.level) %>%
    group_modify(.f = ~{
      .x %>%
        dplyr::filter(.x2.level %in% c(x2.label.pos,x2.label.neg)) %>%
        dplyr::select(.x2.level, n.pos, n.neg) %>%
        as.data.frame() %>%
        column_to_rownames(".x2.level") %>%
        as.matrix() %>%
        fisher.test() %>% broom::tidy()
    }) %>% ungroup()
  test.x2 <- stats %>% dplyr::group_by(.x2.level) %>%
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
outcome_pos <- "CR/PR"
outcome_neg <- "SD/PD"
time <- "os"
event <- "censOS"
dualmarker.plot <- function(data, marker1, marker2,
                            outcome, outcome_pos, outcome_neg=NULL,
                            time, event, quiet=F){
  # check input
  assert_that(all(c(outcome, marker1, marker2) %in% colnames(data)),
              msg = paste(c(marker1, marker2, outcome, "not in your data")))
  if(is.null(outcome_neg)) outcome_neg <- setdiff(na.omit(unique(data[[outcome]])), outcome_pos)
  # prep data
  keep <- data[[outcome]] %in% c(outcome_pos, outcome_neg)
  if(any(!keep)){
    #if(!quiet) print(paste0("warning: ",sum(!keep), " records have no expected outcome, removed\n"))
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
binarize_data <- function(x, datatype="auto",
                          num.cutpoint = NULL,
                          cat.pos=NULL, cat.neg = NULL,
                          as.binary=F,
                          label.pos = "pos", label.neg = "neg"){
  if("factor" %in% class(x)){ x <- as.character(x)}
  assert_that(datatype %in% c("auto","cat","num"),
              msg = "datatype should be [auto,num, cat]")
  if(datatype %in% "auto")
    datatype <- datatype_num_cat(x)
  if(datatype %in% "cat"){
    assert_that(!is.null(cat.pos), msg = "cat.pos is reqired")
    binarize_cat(x = x, pos = cat.pos, neg = cat.neg,
                 as.binary = as.binary,
                 label.pos = label.pos, label.neg = label.neg)
  }else if(all(datatype %in% "num")){
    assert_that(!is.null(num.cutpoint), msg = "num.cutpoint is reqired")
    binarize_num(x = as.numeric(x), cutpoint = num.cutpoint,
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
    dplyr::group_by(.m1, .m2) %>%
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
  test.x1 <- stats %>% dplyr::group_by(.m1) %>%
    group_modify(.f = ~{
      .x %>%
        dplyr::filter(.m2 %in% c("pos","neg")) %>%
        dplyr::select(.m2, n.pos, n.neg) %>%
        as.data.frame() %>%
        column_to_rownames(".m2") %>%
        as.matrix() %>%
        fisher.test() %>% broom::tidy()
    }) %>% ungroup()
  test.x2 <- stats %>% dplyr::group_by(.m2) %>%
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


#' search marker2 by four-quadrant analysis
#'
#' find marker2 to combine with marker1 by four-quadrant analysis
#'
#' @param data dataframe
#' @param response response variable
#' @param response.pos positive value(s) of response variables
#' @param response.neg negative value(s) of response variables
#' @param marker1 marker1
#' @param m2.candidates candidate marker2
#' @param num.cut.method cut method for continous variable
#' @param m1.cat.pos positive value(s) for marker1 if marker1 is categorical
#' @param m1.cat.neg negative value(s) for marker1 if marker1 is categorical
#' @export
dm_searchM2_4quadrant <- function(data, response, response.pos, response.neg=NULL,
                                  marker1, m2.candidates,
                                  num.cut.method = "none",
                                  m1.cat.pos = NULL, m1.cat.neg = NULL, na.rm=T
){
  m2.candidates <- base::intersect(m2.candidates, colnames(data))
  assert_that(length(m2.candidates)>0, msg = "target features don't exist")
  names(m2.candidates) <- m2.candidates
  purrr::map_dfr(m2.candidates, .f = ~{
    res.quad <- dm_response_4quad(data = data, response = response,
                                       response.pos = response.pos, response.neg = response.neg,
                                       marker1 = marker1,
                                       marker2 = .x,
                                       num.cut.method = num.cut.method,
                                       m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
                                       na.rm = na.rm)
    out.basic <- tibble(m1 = marker1, m2 = .x, num.cut.method=num.cut.method)
    out.4quad.count <- res.quad$stats %>% dplyr::select(region, n.total, n.pos, pct.pos) %>%
      tidyr::gather(key = "key", value ="value", -region) %>%
      tidyr::unite(., col = ".id", region, key) %>%
      tidyr::spread(key = ".id", value="value")
    out.4quad.test <- res.quad$test %>% dplyr::select(comparison, p.value) %>%
      mutate(comparison = paste0("pval.", comparison)) %>%
      tidyr::spread(key = "comparison", value="p.value")
    estimate.m2 <- res.quad$test %>%
      dplyr::filter(comparison %in% "R12_vs_R34") %>%
      pull(estimate) %>% .[1]
    out.4quad.test$sign.m2 <- ifelse(estimate.m2>1, 1, -1)
    bind_cols(out.basic, out.4quad.count, out.4quad.test)
  })
}


#' plot search M2 result
#'
#' @param res_searchM2 result of search M2
#' @param max_n max M2s to show
#' @param pval_threh pvalue threhold
#' @return an ggplot object
#' @export
dm_searchM2_4quadrant_plot <- function(res_searchM2, max_n = 30, pval_threh = 0.01){
  max.label <- 30
  m1.name <- res_searchM2$m1[1]

  # filter M2
  col.pval <- c('pval.R1_vs_R4','pval.R2_vs_R3')
  pval.min <- do.call(base::pmin, c(as.list(res_searchM2[,col.pval]), na.rm=T))
  keep <- pval.min <= min(pval_threh, sort(pval.min)[max_n])
  res.searchM2.sig <- res_searchM2[keep,]

  d <- res.searchM2.sig %>%
    dplyr::select(m2,
                  sign.m2,
                  R12_vs_R34 = pval.R12_vs_R34,
                  R1_vs_R4 = pval.R1_vs_R4,
                  R2_vs_R3 = pval.R2_vs_R3)

  .f <- function(pval, sign){ - sign * log10(pval) }
  dd <- mutate_at(.tbl = d, .vars = vars(contains("_vs_")),
                  .funs = .f, sign = d$sign.m2) %>%
    mutate(m2 = str_sub(m2, end=max.label) %>% make.names(unique=T)) %>%
    mutate(m2 = reorder(m2, R12_vs_R34))

  dd %>%
    tidyr::gather(key = "comparison", value="pval", contains("_vs_")) %>%
    ggplot(aes(y = pval, x = m2, color = comparison))+
    geom_point()+
    geom_hline(yintercept = c(-log10(c(0.05, 0.01)),log10(c(0.05, 0.01))),
               linetype="dashed", color="grey")+
    theme_bw()+
    labs(y = "negative correlation <- signed log10-pValue -> positive correlation", x = "marker2",
         title = paste0("Significant marker2 to combine with ", m1.name))+
    coord_flip()

}


.summary_logit <- function(logistic_model){
  # 1. parameters
  coef <- broom::tidy(logistic_model)

  # 2 Likelihood Ratio Test
  #lrt <- anova(update(logistic_model, ~1), logistic_model, test="Chisq")
  #lrt <- res.nag$Likelihood.ratio.test
  # equal to
  # lmtest::lrtest(logistic_model) # likehood ratio test

  # 3 Hosmer-Lemeshow Test
  #library(MKmisc)
  #MKmisc::HLgof.test(fit = fitted(logistic_model), obs = logistic_model$data$.response)

  # 4 pesudo-R
  #res.nag <- rcompanion::nagelkerke(logistic_model)
  #pR2 <- res.nag$Pseudo.R.squared.for.model.vs.null

  # 5. ROC
  auc <- auc.lower95 <- auc.upper95 <- NA
  errFlag <- F
  res.auc <- tryCatch({
    d <- logistic_model$data
    d$.fitted <- fitted(logistic_model)
    response <- all.vars(logistic_model$formula)[1]
    response.level <- levels(d[[response]])
    auc_stats(data = d, response = response,
              case= response.level[2],
              control= response.level[1],
              predictor = ".fitted")
  }, error = function(e) errFlag <<- T,
  warning = function(w) errFlag <<- T
  )
  if(!errFlag){
    auc <- res.auc$auc[1]
    auc.lower95 <- res.auc$lower.95[1]
    auc.upper95 <- res.auc$upper.95[1]
  }
  estimate <- coef %>% select(term, estimate) %>%
    as.data.frame %>% column_to_rownames("term") %>%
    t %>% as_tibble()
  colnames(estimate) %<>% paste0("estimate.", .) %>%
    make.names() %>% str_replace_all("\\.+","\\.") %>%
    str_replace("\\.$","")
  summary <- tibble(deviance = logistic_model$deviance,
                    deviance.null = logistic_model$null.deviance,
                    deviance.diff = logistic_model$null.deviance - logistic_model$deviance,
                    AIC = logistic_model$aic,
                    auc = auc,
                    auc.lower95 = auc.lower95,
                    auc.upper95 = auc.upper95)
  summary <- bind_cols(summary, estimate)
  # return
  c(coef = list(coef),
    summary = list(summary))
}

#' dual marker analysis for logistic regression(core)
#'
#' evaluate the logistic regression of dual marker
#'
#' @param data data frame
#' @param response factor, 2 level, 'neg' and 'pos'
#' @param marker1 marker1
#' @param marker2 marker2
.dm_logit_core <- function(data, response, marker1, marker2, binary_regression=T, na.rm=T){
  .assert_colname(data, c(response, marker1, marker2))
  assert_that(all(levels(data[[response]]) == c("neg","pos")),
              msg = "response level should be ['neg','pos']")
  data <- data %>% dplyr::select(.response = !!sym(response),
                                 .m1 = !!sym(marker1),
                                 .m2 = !!sym(marker2))
  if(na.rm){
    data %<>% tidyr::drop_na(.response, .m1, .m2)
  }
  if(binary_regression){
    fml.m0 <- .response ~ 1
    fml.m1 <- .response ~ .m1
    fml.m2 <- .response ~ .m2
    fml.md <- .response ~ .m1 + .m2
    fml.mdi <- .response ~ .m1 * .m2
    logit.m0 <- stats::glm(formula = fml.m0, data = data, family = binomial(link="logit"))
    logit.m1 <- stats::glm(formula = fml.m1, data = data, family = binomial(link="logit"))
    logit.m2 <- stats::glm(formula = fml.m2, data = data, family = binomial(link="logit"))
    logit.md <- stats::glm(formula = fml.md, data = data, family = binomial(link="logit"))
    logit.mdi <- stats::glm(formula = fml.mdi, data = data, family = binomial(link="logit"))
  }else{
    d <- data %>% dplyr::group_by( .response, .m1, .m2) %>%
      dplyr::count() %>%
      tidyr::spread(key = response, value = "n") %>% ungroup()
    fml.m0 <- paste0( "cbind(pos, neg) ~ 1") %>% as.formula()
    fml.m1 <- paste0( "cbind(pos, neg) ~ .m1") %>% as.formula()
    fml.m2 <- paste0( "cbind(pos, neg) ~ .m2") %>% as.formula()
    fml.md <- paste0( "cbind(pos, neg) ~ .m1 + .m2") %>% as.formula()
    fml.mdi <- paste0( "cbind(pos, neg) ~ .m1 * .m2") %>% as.formula()
    logit.m0 <- stats::glm(formula = fml.m0, data = d, family = binomial(link="logit"))
    logit.m1<- stats::glm(formula = fml.m1, data = d, family = binomial(link="logit"))
    logit.m2 <- stats::glm(formula = fml.m2, data = d, family = binomial(link="logit"))
    logit.md <- stats::glm(formula = fml.md, data = d, family = binomial(link="logit"))
    logit.mdi <- stats::glm(formula = fml.mdi, data = d, family = binomial(link="logit"))
  }
  # summary of model
  summ <- list(m1 = logit.m1, m2 =logit.m2,
               md =logit.md, mdi = logit.mdi) %>%
    map(.f = .summary_logit)

  # model
  summ.coef <- map(summ, "coef") %>% bind_rows(.id = "model")
  summ.glance <- map(summ, "glance") %>% bind_rows(.id = "model")
  summ.auc <- map(summ, "auc") %>% bind_rows(.id = "model")
  # model comparison
  pval.m1.vs.null <- anova(logit.m0, logit.m1, test ="Chisq")$`Pr(>Chi)`[2]
  pval.m2.vs.null <- anova(logit.m0, logit.m2, test ="Chisq")$`Pr(>Chi)`[2]
  pval.m1.vs.md <- anova(logit.m1, logit.md, test ="Chisq")$`Pr(>Chi)`[2]
  pval.m2.vs.md <- anova(logit.m2, logit.md, test ="Chisq")$`Pr(>Chi)`[2]
  pval.m1.vs.mdi <- anova(logit.m1, logit.mdi, test ="Chisq")$`Pr(>Chi)`[2]
  pval.m2.vs.mdi <- anova(logit.m2, logit.mdi, test ="Chisq")$`Pr(>Chi)`[2]
  cmp.model <- tibble(pval.m1.vs.null = pval.m1.vs.null,
                      pval.m2.vs.null= pval.m2.vs.null,
                      pval.m1.vs.md = pval.m1.vs.md,
                      pval.m2.vs.md = pval.m2.vs.md,
                      pval.m1.vs.mdi = pval.m1.vs.mdi,
                      pval.m2.vs.mdi = pval.m2.vs.mdi)
  list(coef = summ.coef, glance = summ.glance, auc = summ.auc, cmp.model = cmp.model)
}

#' Logistic regression for dual marker
#'
#' Evaluate the logistic regression of dual marker
#'
#' @param data data.frame
#' @param response response variables
#' @param response.pos positive values of response
#' @param response.neg negative values of response
#' @param marker1 marker1 variable
#' @param marker2 marker2 variable
#' @param binarization binarize the marker variable, default FALSE
#' @param binary_regression binary regression, default TRUE
#' @param m1.cat.pos positive value(s) if marker1 is categorical
#' @param m1.cat.neg negative value(s) if marker1 is categorical
#' @param m2.cat.pos positive value(s) if marker2 is categorical
#' @param m2.cat.neg negative value(s) if marker2 is categorical
#' @param na.rm remove NA, default TRUE
#' @param num.cut.method marker cut method. Possible values can be 'none'(default) and 'roc', 'median'
#' @return summary of dual marker logistic regression
#' @export
dm_logit <- function(data, response, response.pos, response.neg=NULL,
                              marker1, marker2,
                              binarization = F,
                              binary_regression=T,
                              num.cut.method="none",
                              m1.cat.pos = NULL, m1.cat.neg = NULL,
                              m2.cat.pos = NULL, m2.cat.neg = NULL,
                              na.rm=T){
  cutpoint.m1 <- NA
  cutpoint.m2 <- NA
  data$.response <- binarize_cat(x = data[[response]],
                                 pos = response.pos, neg = response.neg)
  # run logistic regression
  if(!binarization){
    data$.m1 <- data[[marker1]]
    data$.m2 <- data[[marker2]]
    out.logit <- .dm_logit_core(data = data,
                                response = ".response",
                                marker1 = ".m1", marker2 = ".m2",
                                binary_regression = T,
                                na.rm = na.rm)
  }else{
    res.4quadrant <- dm_response_4quad(data = data, response = response,
                                            response.pos = response.pos, response.neg = response.neg,
                                            marker1 = marker1, marker2 = marker2, num.cut.method = num.cut.method,
                                            m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
                                            m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg,
                                            na.rm = na.rm)
    out.logit <- .dm_logit_core(data = res.4quadrant$data,
                                response = '.response',
                                marker1 = ".m1", marker2 = ".m2",
                                binary_regression = binary_regression,
                                na.rm = na.rm)
    cutpoint.m1 <- res.4quadrant$param$cutpoint.m1
    cutpoint.m2 <- res.4quadrant$param$cutpoint.m2
  }
  # extract the key logit info
  out.logit.coef <- out.logit$coef %>%
    dplyr::filter(! term %in% "(Intercept)") %>%
    .collapse_df(df = ., id_cols = c("model","term"),
                 var_cols = c("estimate","p.value"))
  out.logit.auc <-  out.logit$auc %>%
    .collapse_df(df = ., id_cols = "model", var_cols = c("auc"))
  out.logit.cmp.model <- out.logit$cmp.model
  out.logit.key <- bind_cols( out.logit.coef,out.logit.auc, out.logit.cmp.model )

  # basic info
  out.basic <- tibble(response = response,
                      response.pos = toString(response.pos),
                      response.neg = toString(response.neg),
                      m1=marker1, m2 =marker2,
                      marker.cut.method = num.cut.method,
                      cutpoint.m1 = cutpoint.m1,
                      cutpoint.m2 = cutpoint.m2)
  # merge basic and key logit info
  bind_cols(out.basic, out.logit.key)
}



data <- clin_bmk_IMvigor210
response <- "binaryResponse"
response.pos <- "CR/PR"
response.neg <- "SD/PD"
marker1 <- "TMB"
marker2 <- "gepscore_TGFb.19gene"
binarization = F
binary_regression=T
time <- "os"
event <- "censOS"
na.rm=T


#' plot search M2 result
#'
#' @param res_searchM2 result of search M2
#' @param max_n max M2s to show
#' @param pval_threh pvalue threhold
#' @return an ggplot object
#' @export
dm_searchM2_cox_plot <- function(res_searchM2, padj=F, max_n = 30, pval_threh = 0.01){
  max.label <- 30
  m1.name <- res_searchM2$m1[1]
  # filter Marker2
  col.pval <- c('pval.vsM1_md','pval.vsM1_mdi','pval.vsMd_mdi')
  pval.min <- do.call(base::pmin, c(as.list(res_searchM2[,col.pval]), na.rm=T))
  keep <- pval.min <= min(pval_threh, sort(pval.min)[max_n])
  res.searchM2.sig <- res_searchM2[keep,]

  d <- res.searchM2.sig %>%
    mutate(sign_m2 = sign(1 - HR_m2)) %>%
    dplyr::select(m2, sign_m2,
                  M1_vs_dual = pval.vsM1_md,
                  M1_vs_dual.int = pval.vsM1_mdi,
                  M2_vs_dual = pval.vsM2_md,
                  M2_vs_dual.int = pval.vsM2_mdi)

  .f <- function(pval, sign){ - sign * log10(pval) }
  dd <- mutate_at(.tbl = d, .vars = vars(contains("_vs_")),
                  .funs = .f, sign = d$sign_m2) %>%
    mutate(m2 = str_sub(m2, end= max.label) %>% make.names(unique=T)) %>%
    mutate(m2 = reorder(m2, M1_vs_dual))

  dd %>%
    tidyr::gather(key = "comparison", value="pval", contains("_vs_")) %>%
    ggplot(aes(y = pval, x = m2, color = comparison))+
    geom_point()+
    geom_hline(yintercept = c(-log10(c(0.05, 0.01)),log10(c(0.05, 0.01))),
               linetype="dashed", color="grey")+
    theme_bw()+
    labs(y = "inferior survival <- signed log10-pValue -> superior survival",
         x = "marker2",
         title = paste0("Significant marker2 to combine with ", m1.name))+
    coord_flip()
}


