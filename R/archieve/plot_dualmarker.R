##' scatterplot
##' scatter plot with cutoff lines
##' @param data
##' @param m1
##' @param m2
##' @param m1.cutpoint
##' @param cut
##' @param outcome
##' @param outcome_pos
##' @param outcome_neg
##' @return ggplot object
##' @export
dm_scatterplot <- function(data, m1, m2,outcome, m1.cutpoint, m2.cutpoint, outcome_pos, outcome_neg=NULL){
  # check input
  assertthat::assert_that(all(c(outcome, m1, m2) %in% colnames(data)),
              msg = paste(c(m1, m2, outcome, "not in your data")))
  if(is.null(outcome_neg)) outcome_neg <- setdiff(na.omit(unique(data[[outcome]])), outcome_pos)
  # prep data
  keep <- data[[outcome]] %in% c(outcome_pos, outcome_neg)
  if(any(!keep)){
    print(paste0("warning: ",sum(!keep), " records have no expected outcome, removed\n"))
    data <- data[keep,]
  }
  data$.outcome <- ifelse(data[[outcome]] %in% outcome_pos, "pos","neg") %>%
    factor(levels = c("pos","neg"))
  data$region <- case_when(m1 >= m1.cutpoint & m2 >= m2.cutpoint ~ "R1",
                           m1 < m1.cutpoint & m2 >= m2.cutpoint ~ "R2",
                           m1 < m1.cutpoint & m2 < m2.cutpoint ~ "R3",
                           m1 >= m1.cutpoint & m2 < m2.cutpoint ~ "R4",
                           TRUE ~ "others")
  data %<>% dplyr::filter(region %in% c("R1","R2","R3","R4"))
  # plot
  ggplot(data = data,
         aes_string(x = m1, y = m2, color = ".outcome"))+
    annotate("rect", xmin = m1.cutpoint, xmax = Inf, ymin = m2.cutpoint, ymax = Inf, fill = color.quadrant.1[1], alpha=0.3) +
    annotate("rect", xmin = -Inf, xmax = m1.cutpoint, ymin = m2.cutpoint, ymax = Inf, fill = color.quadrant.1[2], alpha=0.3) +
    annotate("rect", xmin = -Inf, xmax = m1.cutpoint, ymin = -Inf, ymax = m2.cutpoint, fill = color.quadrant.1[3], alpha=0.3) +
    annotate("rect", xmin = m1.cutpoint, xmax = Inf, ymin = -Inf, ymax = m2.cutpoint, fill = color.quadrant.1[4], alpha=0.3) +
    geom_point() +
    geom_hline(yintercept = m2.cutpoint, linetype="dashed", color = "skyblue")+
    geom_vline(xintercept = m1.cutpoint, linetype="dashed", color = "skyblue")+
    #   scale_color_manual(values=c("pos" = "#F8766D", "neg"="black"))+
    theme_bw()
}

##' dm_boxplot
##'
##' boxplot for marker1(categorical), marker2(number) and outcome
##'
##' @param data
##' @param m1
##' @param m2
##' @param outcome
##' @param m1.pos
##' @param m1.neg
##' @param m2.cutpoint
##' @param outcome_pos
##' @param outcome_neg
##' @return ggplot object
dm_boxplot <- function(data, m1, m2,outcome,
                       m1.pos, m1.neg=NULL,
                       m2.cutpoint,
                       outcome_pos, outcome_neg=NULL){
  # check input
  assertthat::assert_that(all(c(outcome, m1, m2) %in% colnames(data)),
              msg = paste(c(m1, m2, outcome, "not in your data")))
  if(is.null(outcome_neg)) outcome_neg <- setdiff(na.omit(unique(data[[outcome]])), outcome_pos)
  # prep data
  keep <- data[[outcome]] %in% c(outcome_pos, outcome_neg)
  if(any(!keep)){
    print(paste0("warning: ",sum(!keep), " records have no expected outcome, removed\n"))
    data <- data[keep,]
  }
  data$.outcome <- ifelse(data[[outcome]] %in% outcome_pos, "pos","neg") %>%
    factor(levels = c("pos","neg"))
  data %<>% mutate( region = case_when(!!sym(m1) %in% m1.pos & !!sym(m2) >= m2.cutpoint ~ "R1",
                                       !!sym(m1) %in% m1.neg & !!sym(m2) >= m2.cutpoint ~ "R2",
                                       !!sym(m1) %in% m1.neg & !!sym(m2) < m2.cutpoint ~ "R3",
                                       !!sym(m1) %in% m1.pos & !!sym(m2) < m2.cutpoint ~ "R4",
                                       TRUE ~ "others")) %>%
    dplyr::filter(region %in% c("R1","R2","R3","R4"))

  ggplot(data, aes_string(x = m1, y = m2))+
    geom_blank() +
    annotate("rect", xmin = 1.5, xmax = Inf, ymin = m2.cutpoint, ymax = Inf, fill = color.quadrant.1[1], alpha=0.3) +
    annotate("rect", xmin = -Inf, xmax = 1.5, ymin = m2.cutpoint, ymax = Inf, fill = color.quadrant.1[2], alpha=0.3) +
    annotate("rect", xmin = -Inf, xmax = 1.5, ymin = -Inf, ymax = m2.cutpoint, fill = color.quadrant.1[3], alpha=0.3) +
    annotate("rect", xmin = 1.5, xmax = Inf, ymin = -Inf, ymax = m2.cutpoint, fill = color.quadrant.1[4], alpha=0.3) +
    geom_boxplot(size=0.05, alpha=0.4, outlier.shape = "")+
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
##' @param m1
##' @param m2
##' @param outcome
##' @param m1.pos
##' @param m1.neg
##' @param m2.cut
##' @param outcome_pos
##' @param outcome_neg
##' @return ggplot object
dm_jitter <- function(data, m1, m2, outcome,
                       m1.pos, m1.neg=NULL,
                       m2.pos, m2.neg = NULL,
                       outcome_pos, outcome_neg=NULL){
  # check input
  assertthat::assert_that(all(c(outcome, m1, m2) %in% colnames(data)),
              msg = paste(c(m1, m2, outcome, "not in your data")))
  if(is.null(outcome_neg)) outcome_neg <- setdiff(na.omit(unique(data[[outcome]])), outcome_pos)
  # prep data
  keep <- data[[outcome]] %in% c(outcome_pos, outcome_neg)
  if(any(!keep)){
    print(paste0("warning: ",sum(!keep), " records have no expected outcome, removed\n"))
    data <- data[keep,]
  }
  data$.outcome <- ifelse(data[[outcome]] %in% outcome_pos, "pos","neg") %>%
    factor(levels = c("pos","neg"))
  data %<>% mutate( region = case_when(!!sym(m1) %in% m1.pos & !!sym(m2) %in% m2.pos ~ "R1",
                                       !!sym(m1) %in% m1.neg & !!sym(m2) %in% m2.pos ~ "R2",
                                       !!sym(m1) %in% m1.neg & !!sym(m2) %in% m2.neg ~ "R3",
                                       !!sym(m1) %in% m1.pos & !!sym(m2) %in% m2.neg ~ "R4",
                                       TRUE ~ "others")) %>%
    dplyr::filter(region %in% c("R1","R2","R3","R4"))

  ggplot(data, aes_string(x = m1, y = m2))+
    geom_blank() +
    annotate("rect", xmin = 1.5, xmax = Inf, ymin = 1.5, ymax = Inf, fill = color.quadrant.1[1], alpha=0.3) +
    annotate("rect", xmin = -Inf, xmax = 1.5, ymin = 1.5, ymax = Inf, fill = color.quadrant.1[2], alpha=0.3) +
    annotate("rect", xmin = -Inf, xmax = 1.5, ymin = -Inf, ymax = 1.5, fill = color.quadrant.1[3], alpha=0.3) +
    annotate("rect", xmin = 1.5, xmax = Inf, ymin = -Inf, ymax = 1.5, fill = color.quadrant.1[4], alpha=0.3) +
    geom_jitter(aes_string(color = ".outcome"), width = 0.3) +
    geom_hline(yintercept = 1.5, linetype="dashed", color = "skyblue")+
    geom_vline(xintercept = 1.5, linetype="dashed", color = "skyblue")+
    theme_bw()
}

##' plot survival of dual markers
dm_surv <- function(data, time, event, marker1, marker2, na.rm=T){
  if(na.rm){
    data %<>% drop_na(!!sym(marker1), !!sym(marker2))
  }
  data$.m.combined <- paste(data[[marker1]], data[[marker2]], sep = "_")
  survfit <- .survfit(data = data, var = ".m.combined",
                      time = time, event = event)
  .survplot(survfit = survfit,data= data)
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
  m1.pos <- "YES"
  m1.neg <- "NO"
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
             m1.pos = m1.pos, m1.neg= m1.neg,
             m2.cutpoint = m2.cutpoint,
             outcome_pos = outcome_pos,
             outcome_neg= outcome_neg)

  # dm_jitter
  dm_jitter(data = clin.bmk,
            m1 = "mut_ARID1A", m2 = "Sex", outcome="binaryResponse",
            m1.pos = "YES", m1.neg= "NO",
            m2.pos = "F", m2.neg = "M",
            outcome_pos = "CR/PR", outcome_neg="SD/PD")
}
