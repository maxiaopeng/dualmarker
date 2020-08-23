##' scatterplot
##' scatter plot with cutoff lines
##' @param data
##' @param marker1
##' @param marker2
##' @param m1.cutpoint
##' @param cut
##' @param outcome
##' @param outcome_pos
##' @param outcome_neg
##' @return ggplot object
.dm_scatterplot <- function(data, marker1, marker2,outcome,
                            m1.cutpoint, m2.cutpoint, outcome_pos, outcome_neg=NULL){
  # check input
  assertthat::assert_that(all(c(outcome, marker1, marker2) %in% colnames(data)),
              msg = paste(c(marker1, marker2, outcome, "not in your data")))
  if(is.null(outcome_neg)) outcome_neg <- setdiff(na.omit(unique(data[[outcome]])), outcome_pos)
  # prep data
  keep <- data[[outcome]] %in% c(outcome_pos, outcome_neg)
  if(any(!keep)){
    print(paste0("warning: ",sum(!keep), " records have no expected outcome, removed\n"))
    data <- data[keep,]
  }
  data$.outcome <- ifelse(data[[outcome]] %in% outcome_pos, "pos","neg") %>%
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
##' @param m1_cat_pos
##' @param m1_cat_neg
##' @param m2.cutpoint
##' @param outcome_pos
##' @param outcome_neg
##' @return ggplot object
.dm_stripplot <- function(data, marker1, marker2,outcome,
                       m1_cat_pos, m1_cat_neg=NULL,
                       m2.cutpoint,
                       outcome_pos, outcome_neg=NULL){
  # check input
  assertthat::assert_that(all(c(outcome, marker1, marker2) %in% colnames(data)),
              msg = paste(c(marker1, marker2, outcome, "not in your data")))
  if(is.null(outcome_neg)) outcome_neg <- setdiff(na.omit(unique(data[[outcome]])), outcome_pos)
  # prep data
  keep <- data[[outcome]] %in% c(outcome_pos, outcome_neg)
  if(any(!keep)){
    print(paste0("warning: ",sum(!keep), " records have no expected outcome, removed\n"))
    data <- data[keep,]
  }
  data$.outcome <- ifelse(data[[outcome]] %in% outcome_pos, "pos","neg") %>%
    factor(levels = c("pos","neg"))
  data %<>% mutate( region = case_when(!!sym(marker1) %in% m1_cat_pos & !!sym(marker2) >= m2.cutpoint ~ "R1",
                                       !!sym(marker1) %in% m1_cat_neg & !!sym(marker2) >= m2.cutpoint ~ "R2",
                                       !!sym(marker1) %in% m1_cat_neg & !!sym(marker2) < m2.cutpoint ~ "R3",
                                       !!sym(marker1) %in% m1_cat_pos & !!sym(marker2) < m2.cutpoint ~ "R4",
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
##' @param m1_cat_pos
##' @param m1_cat_neg
##' @param m2.cut
##' @param outcome_pos
##' @param outcome_neg
##' @return ggplot object
.dm_jitter <- function(data, marker1, marker2, outcome,
                      m1_cat_pos, m1_cat_neg=NULL,
                      m2_cat_pos, m2_cat_neg = NULL,
                      outcome_pos, outcome_neg=NULL){
  # check input
  assertthat::assert_that(all(c(outcome, marker1, marker2) %in% colnames(data)),
              msg = paste(c(marker1, marker2, outcome, "not in your data")))
  if(is.null(outcome_neg)) outcome_neg <- setdiff(na.omit(unique(data[[outcome]])), outcome_pos)
  # prep data
  keep <- data[[outcome]] %in% c(outcome_pos, outcome_neg)
  if(any(!keep)){
    print(paste0("warning: ",sum(!keep), " records have no expected outcome, removed\n"))
    data <- data[keep,]
  }
  data$.outcome <- ifelse(data[[outcome]] %in% outcome_pos, "pos","neg") %>%
    factor(levels = c("pos","neg"))
  data %<>% mutate( region = case_when(!!sym(marker1) %in% m1_cat_pos & !!sym(marker2) %in% m2_cat_pos ~ "R1",
                                       !!sym(marker1) %in% m1_cat_neg & !!sym(marker2) %in% m2_cat_pos ~ "R2",
                                       !!sym(marker1) %in% m1_cat_neg & !!sym(marker2) %in% m2_cat_neg ~ "R3",
                                       !!sym(marker1) %in% m1_cat_pos & !!sym(marker2) %in% m2_cat_neg ~ "R4",
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
dm_scatter_chart <- function(data, outcome, outcome_pos, outcome_neg=NULL,
                             marker1, marker2,
                             m1_datatype = "auto", m2_datatype = "auto",
                             num_cut_method = "median",
                             m1_cat_pos = NULL, m1_cat_neg = NULL,
                             m2_cat_pos = NULL, m2_cat_neg = NULL){
  if(m1_datatype == "auto"){
    m1_datatype <- datatype_num_cat(data[[marker1]])}
  if(m2_datatype == "auto"){
    m2_datatype <- datatype_num_cat(data[[marker2]])}
  if(m2_datatype == "num"){
    assertthat::assert_that(num_cut_method %in% c("median","roc"),
                msg = "num_cut_method should be ['median', 'roc'], not 'none'")
    m2.cutpoint <- cutpoint(x = data[[marker2]], method = num_cut_method,
                            outcome = data[[outcome]], outcome_pos = outcome_pos,
                            outcome_neg = outcome_neg)
    #print("m2.cutpoint", m2.cutpoint)
    if(m1_datatype == "num"){
      m1.cutpoint <- cutpoint(x = data[[marker1]], method = num_cut_method,
                              outcome = data[[outcome]], outcome_pos = outcome_pos,
                              outcome_neg = outcome_neg)
      .dm_scatterplot(data = data, marker1 = marker1,
                      marker2 = marker2,outcome = outcome,
                      m1.cutpoint = m1.cutpoint,
                      m2.cutpoint = m2.cutpoint,
                      outcome_pos = outcome_pos,
                      outcome_neg= outcome_neg)
    }else{
      assertthat::assert_that(!is.null(m1_cat_pos), msg = "m1_cat_pos should not be NULL")
      .dm_stripplot(data = data, marker1 = marker1, marker2 = marker2,outcome = outcome,
                    m1_cat_pos = m1_cat_pos, m1_cat_neg= m1_cat_neg,
                    m2.cutpoint = m2.cutpoint,
                    outcome_pos = outcome_pos, outcome_neg= outcome_neg)
    }
  }else if(m1_datatype == "num"){
    m1.cutpoint <- cutpoint(x = data[[marker1]], method = num_cut_method,
                            outcome = data[[outcome]], outcome_pos = outcome_pos,
                            outcome_neg = outcome_neg)
    assertthat::assert_that(!is.null(m2_cat_pos), msg = "m2_cat_pos should not be NULL")
    .dm_stripplot(data = data, marker1 = marker2, marker2 = marker1,outcome = outcome,
                  m1_cat_pos = m2_cat_pos, m1_cat_neg= m2_cat_neg,
                  m2.cutpoint = m1.cutpoint,
                  outcome_pos = outcome_pos, outcome_neg= outcome_neg)
  }else{
    assertthat::assert_that(!is.null(m1_cat_pos) && !is.null(m2_cat_pos),
                msg= "m1_cat_pos, m2_cat_pos should NOT be NULL")
    .dm_jitter(data = data, marker1 = marker1, marker2=marker2, outcome=outcome,
                           m1_cat_pos = m1_cat_pos, m1_cat_neg= m1_cat_neg,
                           m2_cat_pos = m2_cat_pos, m2_cat_neg = m2_cat_neg,
                           outcome_pos = outcome_pos , outcome_neg= outcome_neg)
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
  outcome_pos <- "setosa"
  outcome_neg <- "versicolor"
  m1.cutpoint <- median(data[[m1]])
  m2.cut <- median(data[[m2]])
  dm_scatterplot(data, m1, m2, m1.cutpoint, m2.cut, outcome, outcome_pos, outcome_neg)

  # dm_scatterplot
  data <- clin.bmk
  m1 <- "TMB"
  m2 <- "gepscore_CD.8.T.effector"
  outcome <- "binaryResponse"
  outcome_pos <- "CR/PR"
  outcome_neg <- "SD/PD"
  m1.cutpoint <- cutponit.num(x = data[[m1]], method = "roc", outcome = data[[outcome]],
                              outcome_pos = outcome_pos, outcome_neg = outcome_neg)
  m2.cutpoint <- cutponit.num(x = data[[m2]], method = "roc", outcome = data[[outcome]],
                              outcome_pos = outcome_pos, outcome_neg = outcome_neg)

  dm_scatterplot(data =data, m1=m1, m2=m2,  outcome = outcome,
                 m1.cutpoint=m1.cutpoint, m2.cutpoint=m2.cutpoint,
                 outcome_pos=outcome_pos, outcome_neg=outcome_neg)

  # dm_boxplot
  data <- clin.bmk
  m1 <- "mut_ARID1A"
  m1_cat_pos <- "YES"
  m1_cat_neg <- "NO"
  m2 <- "gep_CXCL13"
  outcome <- "binaryResponse"
  outcome_pos <- "CR/PR"
  outcome_neg <- "SD/PD"
  #m2.cut <- median(data[[m2]], na.rm=T)
  m2.cutpoint <- cutponit.num(x = data[[m2]], method = "roc", outcome = data[[outcome]],
                              outcome_pos = outcome_pos, outcome_neg = outcome_neg)
  m2.cutpoint <- cutponit.num(x = data[[m2]], method = "optimal_surv",
                              time = data$os, event = data$censOS)
  dm_boxplot(data = data, m1=m1, m2=m2,outcome=outcome,
             m1_cat_pos = m1_cat_pos, m1_cat_neg= m1_cat_neg,
             m2.cutpoint = m2.cutpoint,
             outcome_pos = outcome_pos,
             outcome_neg= outcome_neg)

  # dm_jitter
  dm_jitter(data = clin.bmk,
            m1 = "mut_ARID1A", m2 = "Sex", outcome="binaryResponse",
            m1_cat_pos = "YES", m1_cat_neg= "NO",
            m2_cat_pos = "F", m2_cat_neg = "M",
            outcome_pos = "CR/PR", outcome_neg="SD/PD")


  data <- iris
  outcome <- "Species"
  outcome_pos <- "setosa"
  outcome_neg <- "versicolor"
  marker1 <- "Sepal.Length"
  marker2 <- "Sepal.Width"
  dm_roc(data = iris, outcome ="Species", outcome_pos = "setosa",
         outcome_neg = "versicolor", marker1 = "Sepal.Length", marker2="Sepal.Width")

}
