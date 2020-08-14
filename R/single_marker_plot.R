#############
## boxplot
#############
.sm_boxplot <- function(data, outcome, marker, title = "", ...){
  ggplot(data, aes_string( x = outcome, y = marker, fill = outcome))+
    geom_boxplot(outlier.shape = "")+
    geom_jitter(width = 0.2)+
    stat_compare_means(...)+
    theme_bw()+
    labs(title = title)
}


##############
## barplot
##############
.sm_barplot <- function(data, outcome, marker, percent = F, title = ""){
  d <- data %>%
    group_by(!!sym(outcome), !!sym(marker)) %>%
    dplyr::count(name = "Freq") %>%
    group_by(!!sym(marker)) %>%
    mutate( Percent = round(Freq / sum(Freq), 2))
  yvar <- ifelse(percent, "Percent", "Freq")
  g <- ggplot(d,
              aes_string( x = marker, y = yvar,
                         fill= outcome)) +
    geom_bar(stat="identity", color="black") +
    #theme(axis.text = element_text(angle = 90)) +
    #labs(x="")+
    theme_bw() +
    labs(title = title)
  g + geom_text(aes_string(label= yvar), angle=90,
                position = position_stack(vjust = 0.5))
}

#############
## integration
##############
dm_boxplot <- function(data, outcome, outcome.pos, outcome.neg=NULL,
                       marker1, marker2,
                       m1.datatype = "auto", m1.cat.pos = NULL, m1.cat.neg =NULL,
                       m2.datatype = "auto", m2.cat.pos = NULL, m2.cat.neg =NULL){
  if(m1.datatype == "auto"){
    m1.datatype <- datatype.num.cat(data[[marker1]])}
  if(m2.datatype == "auto"){
    m2.datatype <- datatype.num.cat(data[[marker2]])}
  data$.outcome <- binarize.cat(as.character(data[[outcome]]),
                                pos = outcome.pos,
                                neg = outcome.neg) %>%
    factor(levels = c("pos","neg"))
  data %<>% drop_na(.outcome)
  if(m1.datatype == "num"){
    g1 <- .sm_boxplot(data = data, outcome = ".outcome", marker = marker1, title = paste0("Marker1:", marker1))+
      theme(legend.position = "none")
  }else{
    data$.m1 <- binarize.cat( x = as.character(data[[marker1]]), pos = m1.cat.pos, neg = m1.cat.neg) %>%
      factor(levels = c("pos","neg"))
    g1 <- .sm_barplot(data = data, outcome = ".outcome", marker = ".m1", title = paste0("Marker1:", marker1))+
      theme(legend.position = "none")
  }
  if(m2.datatype == "num"){
    g2 <- .sm_boxplot(data = data, outcome = ".outcome", marker = marker2, title = paste0("Marker2:", marker2))+
      theme(legend.position = "none")
  }else{
    data$.m2 <- binarize.cat( x = as.character(data[[marker2]]), pos = m2.cat.pos, neg = m2.cat.neg) %>%
      factor(levels = c("pos","neg"))
    g2 <- .sm_barplot(data = data, outcome = ".outcome", marker = ".m2", title = paste0("Marker2:", marker2))+
      theme(legend.position = "none")
  }
  cowplot::plot_grid(g1,g2, align = "h", axis = "l", ncol = 2
                     #labels= list(paste0("marker1:", marker1), paste0("marker2:", marker2))
                     )
}

if(F){
  .sm_boxplot(data = iris, outcome = "Species", marker = "Sepal.Length")

  data <- iris %>% dplyr::filter(Species %in% c("setosa","versicolor"))
  data$Sepal.Length.level <- cut(data$Sepal.Length, breaks = 2, labels = c("low","high"))
  .sm_barplot(data = data, outcome = "Species",
              marker = "Sepal.Length.level", percent = T)

  dm_boxplot(data = iris, outcome = "Species",
             outcome.pos = "setosa",
             outcome.neg = "versicolor",
             marker1 = "Sepal.Length",
             marker2  = "Sepal.Width")

  dm_boxplot(data = mtcars, outcome = "vs",
             outcome.pos = "1",
             outcome.neg = "0",
             marker2 = "wt",
             marker1  = "gear", m1.datatype = "cat",
             m1.cat.pos = c("5"), m1.cat.neg = c("3","4"))
}



