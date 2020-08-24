#############
## boxplot
#############
.sm_boxplot <- function(data, response, marker, title = "", ...){
  ggplot(data, aes_string( x = response, y = marker, fill = response))+
    geom_boxplot(outlier.shape = "")+
    geom_jitter(width = 0.2)+
    stat_compare_means(...)+
    theme_bw()+
    labs(title = title)
}


##############
## barplot
##############
.sm_barplot <- function(data, response, marker, percent = F, title = ""){
  d <- data %>%
    dplyr::group_by(!!sym(response), !!sym(marker)) %>%
    dplyr::count(name = "Freq") %>%
    dplyr::group_by(!!sym(marker)) %>%
    mutate( Percent = round(Freq / sum(Freq), 2))
  yvar <- ifelse(percent, "Percent", "Freq")
  g <- ggplot(d,
              aes_string( x = marker, y = yvar,
                         fill= response)) +
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
dm_boxplot <- function(data, response, response_pos, response_neg=NULL,
                       marker1, marker2,
                       m1_datatype = "auto", m1_cat_pos = NULL, m1_cat_neg =NULL,
                       m2_datatype = "auto", m2_cat_pos = NULL, m2_cat_neg =NULL){
  if(m1_datatype == "auto"){
    m1_datatype <- datatype_num_cat(data[[marker1]])}
  if(m2_datatype == "auto"){
    m2_datatype <- datatype_num_cat(data[[marker2]])}
  data$.response <- binarize_cat(as.character(data[[response]]),
                                pos = response_pos,
                                neg = response_neg) %>%
    factor(levels = c("pos","neg"))
  data %<>% drop_na(.response)
  if(m1_datatype == "num"){
    g1 <- .sm_boxplot(data = data, response = ".response",
                      marker = marker1, title = paste0("Marker1:", marker1))+
      theme(legend.position = "none")
  }else{
    data$.m1 <- binarize_cat( x = as.character(data[[marker1]]),
                              pos = m1_cat_pos, neg = m1_cat_neg) %>%
      factor(levels = c("pos","neg"))
    g1 <- .sm_barplot(data = data, response = ".response", marker = ".m1",
                      title = paste0("Marker1:", marker1))+
      theme(legend.position = "none")
  }
  if(m2_datatype == "num"){
    g2 <- .sm_boxplot(data = data, response = ".response", marker = marker2,
                      title = paste0("Marker2:", marker2))+
      theme(legend.position = "none")
  }else{
    data$.m2 <- binarize_cat( x = as.character(data[[marker2]]),
                              pos = m2_cat_pos, neg = m2_cat_neg) %>%
      factor(levels = c("pos","neg"))
    g2 <- .sm_barplot(data = data, response = ".response", marker = ".m2",
                      title = paste0("Marker2:", marker2))+
      theme(legend.position = "none")
  }
  cowplot::plot_grid(g1,g2, align = "h", axis = "l", ncol = 2
                     #labels= list(paste0("marker1:", marker1), paste0("marker2:", marker2))
                     )
}



