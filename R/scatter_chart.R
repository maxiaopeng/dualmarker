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
