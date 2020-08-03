##' scatterplot
##' scatter plot with cutoff lines
##' @param data
##' @param marker1
##' @param marker2
##' @param m1.cutpoint
##' @param cut
##' @param outcome
##' @param outcome.pos
##' @param outcome.neg
##' @return ggplot object
##' @export
.dm_scatterplot <- function(data, marker1, marker2,outcome,
                            m1.cutpoint, m2.cutpoint, outcome.pos, outcome.neg=NULL){
  # check input
  assert_that(all(c(outcome, marker1, marker2) %in% colnames(data)),
              msg = paste(c(marker1, marker2, outcome, "not in your data")))
  if(is.null(outcome.neg)) outcome.neg <- setdiff(na.omit(unique(data[[outcome]])), outcome.pos)
  # prep data
  keep <- data[[outcome]] %in% c(outcome.pos, outcome.neg)
  if(any(!keep)){
    print(paste0("warning: ",sum(!keep), " records have no expected outcome, removed\n"))
    data <- data[keep,]
  }
  data$.outcome <- ifelse(data[[outcome]] %in% outcome.pos, "pos","neg") %>%
    factor(levels = c("pos","neg"))
  data$region <- case_when(marker1 >= m1.cutpoint & marker2 >= m2.cutpoint ~ "R1",
                           marker1 < m1.cutpoint & marker2 >= m2.cutpoint ~ "R2",
                           marker1 < m1.cutpoint & marker2 < m2.cutpoint ~ "R3",
                           marker1 >= m1.cutpoint & marker2 < m2.cutpoint ~ "R4",
                           TRUE ~ "others")
  data %<>% dplyr::filter(region %in% c("R1","R2","R3","R4"))
  # plot
  ggplot(data = data,
         aes_string(x = marker1, y = marker2, color = ".outcome"))+
    annotate("rect", xmin = m1.cutpoint, xmax = Inf, ymin = m2.cutpoint, ymax = Inf, fill = color.quadrant.1[1], alpha=0.05) +
    annotate("rect", xmin = -Inf, xmax = m1.cutpoint, ymin = m2.cutpoint, ymax = Inf, fill = color.quadrant.1[2], alpha=0.05) +
    annotate("rect", xmin = -Inf, xmax = m1.cutpoint, ymin = -Inf, ymax = m2.cutpoint, fill = color.quadrant.1[3], alpha=0.05) +
    annotate("rect", xmin = m1.cutpoint, xmax = Inf, ymin = -Inf, ymax = m2.cutpoint, fill = color.quadrant.1[4], alpha=0.05) +
    geom_point() +
    geom_hline(yintercept = m2.cutpoint, linetype="dashed", color = "skyblue")+
    geom_vline(xintercept = m1.cutpoint, linetype="dashed", color = "skyblue")+
    #   scale_color_manual(values=c("pos" = "#F8766D", "neg"="black"))+
    theme_bw()
}

##' dm_stripplot
##'
##' boxplot for marker1(categorical), marker2(number) and outcome
##'
##' @param data
##' @param marker1
##' @param marker2
##' @param outcome
##' @param m1.cat.pos
##' @param m1.cat.neg
##' @param m2.cutpoint
##' @param outcome.pos
##' @param outcome.neg
##' @return ggplot object
.dm_stripplot <- function(data, marker1, marker2,outcome,
                       m1.cat.pos, m1.cat.neg=NULL,
                       m2.cutpoint,
                       outcome.pos, outcome.neg=NULL){
  # check input
  assert_that(all(c(outcome, marker1, marker2) %in% colnames(data)),
              msg = paste(c(marker1, marker2, outcome, "not in your data")))
  if(is.null(outcome.neg)) outcome.neg <- setdiff(na.omit(unique(data[[outcome]])), outcome.pos)
  # prep data
  keep <- data[[outcome]] %in% c(outcome.pos, outcome.neg)
  if(any(!keep)){
    print(paste0("warning: ",sum(!keep), " records have no expected outcome, removed\n"))
    data <- data[keep,]
  }
  data$.outcome <- ifelse(data[[outcome]] %in% outcome.pos, "pos","neg") %>%
    factor(levels = c("pos","neg"))
  data %<>% mutate( region = case_when(!!sym(marker1) %in% m1.cat.pos & !!sym(marker2) >= m2.cutpoint ~ "R1",
                                       !!sym(marker1) %in% m1.cat.neg & !!sym(marker2) >= m2.cutpoint ~ "R2",
                                       !!sym(marker1) %in% m1.cat.neg & !!sym(marker2) < m2.cutpoint ~ "R3",
                                       !!sym(marker1) %in% m1.cat.pos & !!sym(marker2) < m2.cutpoint ~ "R4",
                                       TRUE ~ "others")) %>%
    dplyr::filter(region %in% c("R1","R2","R3","R4"))

  ggplot(data, aes_string(x = marker1, y = marker2))+
    geom_blank() +
    annotate("rect", xmin = 1.5, xmax = Inf, ymin = m2.cutpoint, ymax = Inf, fill = color.quadrant.1[1], alpha=0.1) +
    annotate("rect", xmin = -Inf, xmax = 1.5, ymin = m2.cutpoint, ymax = Inf, fill = color.quadrant.1[2], alpha=0.1) +
    annotate("rect", xmin = -Inf, xmax = 1.5, ymin = -Inf, ymax = m2.cutpoint, fill = color.quadrant.1[3], alpha=0.1) +
    annotate("rect", xmin = 1.5, xmax = Inf, ymin = -Inf, ymax = m2.cutpoint, fill = color.quadrant.1[4], alpha=0.1) +
    #geom_boxplot(size=0.05, alpha=0.4, outlier.shape = "")+
    geom_jitter(aes_string(color = ".outcome"), width = 0.3) +
    #stat_signif(comparisons = list(c("YES","NO")))+
    geom_hline(yintercept = m2.cutpoint, linetype="dashed", color = "skyblue")+
    geom_vline(xintercept = 1.5, linetype="dashed", color = "skyblue")+
    #   scale_color_manual(values=c("pos" = "#F8766D", "neg"="black"))+
    theme_bw()
}

##' dm_jitter
##'
##' jitter for marker1(categorical), marker2(categorical) and outcome
##'
##' @param data
##' @param marker1
##' @param marker2
##' @param outcome
##' @param m1.cat.pos
##' @param m1.cat.neg
##' @param m2.cut
##' @param outcome.pos
##' @param outcome.neg
##' @return ggplot object
.dm_jitter <- function(data, marker1, marker2, outcome,
                      m1.cat.pos, m1.cat.neg=NULL,
                      m2.cat.pos, m2.cat.neg = NULL,
                      outcome.pos, outcome.neg=NULL){
  # check input
  assert_that(all(c(outcome, marker1, marker2) %in% colnames(data)),
              msg = paste(c(marker1, marker2, outcome, "not in your data")))
  if(is.null(outcome.neg)) outcome.neg <- setdiff(na.omit(unique(data[[outcome]])), outcome.pos)
  # prep data
  keep <- data[[outcome]] %in% c(outcome.pos, outcome.neg)
  if(any(!keep)){
    print(paste0("warning: ",sum(!keep), " records have no expected outcome, removed\n"))
    data <- data[keep,]
  }
  data$.outcome <- ifelse(data[[outcome]] %in% outcome.pos, "pos","neg") %>%
    factor(levels = c("pos","neg"))
  data %<>% mutate( region = case_when(!!sym(marker1) %in% m1.cat.pos & !!sym(marker2) %in% m2.cat.pos ~ "R1",
                                       !!sym(marker1) %in% m1.cat.neg & !!sym(marker2) %in% m2.cat.pos ~ "R2",
                                       !!sym(marker1) %in% m1.cat.neg & !!sym(marker2) %in% m2.cat.neg ~ "R3",
                                       !!sym(marker1) %in% m1.cat.pos & !!sym(marker2) %in% m2.cat.neg ~ "R4",
                                       TRUE ~ "others")) %>%
    dplyr::filter(region %in% c("R1","R2","R3","R4"))

  ggplot(data, aes_string(x = marker1, y = marker2))+
    geom_blank() +
    annotate("rect", xmin = 1.5, xmax = Inf, ymin = 1.5, ymax = Inf, fill = color.quadrant.1[1], alpha=0.1) +
    annotate("rect", xmin = -Inf, xmax = 1.5, ymin = 1.5, ymax = Inf, fill = color.quadrant.1[2], alpha=0.1) +
    annotate("rect", xmin = -Inf, xmax = 1.5, ymin = -Inf, ymax = 1.5, fill = color.quadrant.1[3], alpha=0.1) +
    annotate("rect", xmin = 1.5, xmax = Inf, ymin = -Inf, ymax = 1.5, fill = color.quadrant.1[4], alpha=0.1) +
    geom_jitter(aes_string(color = ".outcome"), width = 0.3) +
    geom_hline(yintercept = 1.5, linetype="dashed", color = "skyblue")+
    geom_vline(xintercept = 1.5, linetype="dashed", color = "skyblue")+
    theme_bw()
}


##' @description scatter chart for marker1, marker2, outcome is presented as the color of dot.
##' @export
dm_scatter_chart <- function(data, outcome, outcome.pos, outcome.neg=NULL,
                             marker1, marker2,
                             m1.datatype = "auto", m2.datatype = "auto",
                             num.cut.method = "median",
                             m1.cat.pos = NULL, m1.cat.neg = NULL,
                             m2.cat.pos = NULL, m2.cat.neg = NULL){
  if(m1.datatype == "auto"){
    m1.datatype <- datatype.num.cat(data[[marker1]])}
  if(m2.datatype == "auto"){
    m2.datatype <- datatype.num.cat(data[[marker2]])}
  if(m2.datatype == "num"){
    assert_that(num.cut.method %in% c("median","roc"),
                msg = "num.cut.method should be ['median', 'roc'], not 'none'")
    m2.cutpoint <- cutpoint(x = data[[marker2]], method = num.cut.method,
                            outcome = data[[outcome]], outcome.pos = outcome.pos,
                            outcome.neg = outcome.neg)
    #print("m2.cutpoint", m2.cutpoint)
    if(m1.datatype == "num"){
      m1.cutpoint <- cutpoint(x = data[[marker1]], method = num.cut.method,
                              outcome = data[[outcome]], outcome.pos = outcome.pos,
                              outcome.neg = outcome.neg)
      .dm_scatterplot(data = data, marker1 = marker1,
                      marker2 = marker2,outcome = outcome,
                      m1.cutpoint = m1.cutpoint,
                      m2.cutpoint = m2.cutpoint,
                      outcome.pos = outcome.pos,
                      outcome.neg= outcome.neg)
    }else{
      assert_that(!is.null(m1.cat.pos), msg = "m1.cat.pos should not be NULL")
      .dm_stripplot(data = data, marker1 = marker1, marker2 = marker2,outcome = outcome,
                    m1.cat.pos = m1.cat.pos, m1.cat.neg= m1.cat.neg,
                    m2.cutpoint = m2.cutpoint,
                    outcome.pos = outcome.pos, outcome.neg= outcome.neg)
    }
  }else if(m1.datatype == "num"){
    m1.cutpoint <- cutpoint(x = data[[marker1]], method = num.cut.method,
                            outcome = data[[outcome]], outcome.pos = outcome.pos,
                            outcome.neg = outcome.neg)
    assert_that(!is.null(m2.cat.pos), msg = "m2.cat.pos should not be NULL")
    .dm_stripplot(data = data, marker1 = marker2, marker2 = marker1,outcome = outcome,
                  m1.cat.pos = m2.cat.pos, m1.cat.neg= m2.cat.neg,
                  m2.cutpoint = m1.cutpoint,
                  outcome.pos = outcome.pos, outcome.neg= outcome.neg)
  }else{
    assert_that(!is.null(m1.cat.pos) && !is.null(m2.cat.pos),
                msg= "m1.cat.pos, m2.cat.pos should NOT be NULL")
    .dm_jitter(data = data, marker1 = marker1, marker2=marker2, outcome=outcome,
                           m1.cat.pos = m1.cat.pos, m1.cat.neg= m1.cat.neg,
                           m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg,
                           outcome.pos = outcome.pos , outcome.neg= outcome.neg)
  }
}


##' @description
##' draw ROC curve for marker1, marker2 and fitted values by logistic regression
##' @param data
##' @param outcome
##' @param outcome.pos
##' @param outcome.neg
##' @param marker1
##' @param marker2
##' @return ggplot object
dm_roc <- function(data, outcome, outcome.pos, outcome.neg=NULL,
                   marker1, marker2,
                   m1.cat.pos = NULL, m1.cat.neg =NULL,
                   m2.cat.pos = NULL, m2.cat.neg = NULL,
                   logistic.reg=T, logistic.reg.int=F){
  data$.outcome <- binarize.cat(x = data[[outcome]],
                                pos = outcome.pos, neg = outcome.neg, return.binary = T)
  .summary.auc.ci <- function(res.roc){
    #auc <- auc(res.roc)
    ci <- ci.auc(res.roc)
    summary <- paste0("AUC:", round(ci[2],3),
                      "(", round(ci[1],3), ",",
                      round(ci[3],3), ")")
    list(auc = auc, ci = ci, summary =  summary)
  }
  # ROC
  #roc.fml <- paste0(".outcome~", marker1,"+", marker2) %>% as.formula
  #roc.list <- roc(roc.fml, data= data)
  if(datatype.num.cat(data[[marker1]]) == "cat" && !is.null(m1.cat.pos)){
      data[[marker1]] <- binarize.cat(data[[marker1]], pos = m1.cat.pos, neg = m1.cat.neg, return.binary = T)
  }
  if(datatype.num.cat(data[[marker2]]) == "cat" && !is.null(m1.cat.pos)){
    data[[marker2]] <- binarize.cat(data[[marker2]], pos = m2.cat.pos, neg = m2.cat.neg, return.binary = T)
  }
  roc.m1 <- roc(data$.outcome, data[[marker1]])
  roc.m2 <- roc(data$.outcome, data[[marker2]])
  roc.list <- list(roc.m1, roc.m2) %>%
    setNames(c(str_sub(marker1, end = 40),
               str_sub(marker2, end = 40)))
  # AUC
  anno.m1 <- paste0(marker1, "\n", .summary.auc.ci(roc.m1)$summary)
  anno.m2 <- paste0(marker2, "\n", .summary.auc.ci(roc.m2)$summary)
  anno <- c(anno.m1, anno.m2)
  # logistic regression
  if(logistic.reg){
    d <- data %>% drop_na(.outcome, !!sym(marker1), !!sym(marker2))
    fml.md <- paste0( ".outcome ~ ", marker1, "+", marker2) %>% as.formula()
    logit.md <- glm(formula = fml.md, data = d, family = binomial(link="logit"))
    roc.md <- roc(d$.outcome, fitted(logit.md))
    roc.list$dualmarker <- roc.md
    anno.md <- paste0("dual-marker\n", .summary.auc.ci(roc.md)$summary)
    anno <- c(anno, anno.md)
    if(logistic.reg.int){
      fml.md.int <- paste0(".outcome ~ ", marker1, "*", marker2) %>% as.formula()
      logit.md.int <- glm(formula = fml.md.int, data = d, family = binomial(link="logit"))
      roc.md.int <- roc(d$.outcome, fitted(logit.md.int))
      roc.list$dualmarker_int <- roc.md.int
      anno.md.int <- paste0("dual-marker(interaction)\n", .summary.auc.ci(roc.md.int)$summary)
      anno <- c(anno, anno.md.int)
    }
  }

  # plot
  g <- pROC::ggroc(roc.list, aes = c("color")) + theme_bw()
  g <- g + geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                        color="grey", linetype="dashed")

  # label AUC
  #g <- g +  annotate("text", x = .25, y = .25,
  #                    label = paste(anno.m1, anno.m2, sep = "\n"))
  g +
    scale_color_discrete(name = "", labels = anno)+
    theme(legend.position=c(.75,.25))
}
############################
## test
############################
# dm_scatterplot
if(F){
  data <- iris %>% filter(Species %in% c("setosa", "versicolor"))
  m1 <- "Sepal.Length"
  m2 <- "Sepal.Width"
  outcome <- "Species"
  outcome.pos <- "setosa"
  outcome.neg <- "versicolor"
  m1.cutpoint <- median(data[[m1]])
  m2.cut <- median(data[[m2]])
  dm_scatterplot(data, m1, m2, m1.cutpoint, m2.cut, outcome, outcome.pos, outcome.neg)

  # dm_scatterplot
  data <- clin.bmk
  m1 <- "TMB"
  m2 <- "gepscore_CD.8.T.effector"
  outcome <- "binaryResponse"
  outcome.pos <- "CR/PR"
  outcome.neg <- "SD/PD"
  m1.cutpoint <- cutponit.num(x = data[[m1]], method = "roc", outcome = data[[outcome]],
                              outcome.pos = outcome.pos, outcome.neg = outcome.neg)
  m2.cutpoint <- cutponit.num(x = data[[m2]], method = "roc", outcome = data[[outcome]],
                              outcome.pos = outcome.pos, outcome.neg = outcome.neg)

  dm_scatterplot(data =data, m1=m1, m2=m2,  outcome = outcome,
                 m1.cutpoint=m1.cutpoint, m2.cutpoint=m2.cutpoint,
                 outcome.pos=outcome.pos, outcome.neg=outcome.neg)

  # dm_boxplot
  data <- clin.bmk
  m1 <- "mut_ARID1A"
  m1.cat.pos <- "YES"
  m1.cat.neg <- "NO"
  m2 <- "gep_CXCL13"
  outcome <- "binaryResponse"
  outcome.pos <- "CR/PR"
  outcome.neg <- "SD/PD"
  #m2.cut <- median(data[[m2]], na.rm=T)
  m2.cutpoint <- cutponit.num(x = data[[m2]], method = "roc", outcome = data[[outcome]],
                              outcome.pos = outcome.pos, outcome.neg = outcome.neg)
  m2.cutpoint <- cutponit.num(x = data[[m2]], method = "optimal_surv",
                              surv.time = data$os, surv.event = data$censOS)
  dm_boxplot(data = data, m1=m1, m2=m2,outcome=outcome,
             m1.cat.pos = m1.cat.pos, m1.cat.neg= m1.cat.neg,
             m2.cutpoint = m2.cutpoint,
             outcome.pos = outcome.pos,
             outcome.neg= outcome.neg)

  # dm_jitter
  dm_jitter(data = clin.bmk,
            m1 = "mut_ARID1A", m2 = "Sex", outcome="binaryResponse",
            m1.cat.pos = "YES", m1.cat.neg= "NO",
            m2.cat.pos = "F", m2.cat.neg = "M",
            outcome.pos = "CR/PR", outcome.neg="SD/PD")


  data <- iris
  outcome <- "Species"
  outcome.pos <- "setosa"
  outcome.neg <- "versicolor"
  marker1 <- "Sepal.Length"
  marker2 <- "Sepal.Width"
  dm_roc(data = iris, outcome ="Species", outcome.pos = "setosa",
         outcome.neg = "versicolor", marker1 = "Sepal.Length", marker2="Sepal.Width")

}
