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
#' boxplot of responder and non-responder
#'
#' @param data dataframe
#' @param response response variable
#' @param response.pos positive value(s) for response, e.g. "CR_PR"
#' @param response.neg  negative value(s) for response e.g. "SD_PD"
#' @param marker1 marker1
#' @param marker2 marker2
#' @param m1.datatype data type of marker1
#' @param m1.cat.pos positive value(s) for marker1 if marker1 is categorical, e.g. "MUT"
#' @param m1.cat.neg negative value(s) for marker1 if marker1 is categorical, e.g. "WT"
#' @param m2.datatype data type of marker2
#' @param m2.cat.pos positive value(s) for marker2 if marker2 is categorical, e.g. "MUT"
#' @param m2.cat.neg negative value(s) for marker2 if marker2 is categorical, e.g. "WT"
#'
#' @return panel of two ggplot object
dm_response_boxplot <- function(data, response, response.pos, response.neg=NULL,
                       marker1, marker2,
                       m1.datatype = "auto", m1.cat.pos = NULL, m1.cat.neg =NULL,
                       m2.datatype = "auto", m2.cat.pos = NULL, m2.cat.neg =NULL){
  if(m1.datatype == "auto"){
    m1.datatype <- datatype_num_cat(data[[marker1]])}
  if(m2.datatype == "auto"){
    m2.datatype <- datatype_num_cat(data[[marker2]])}
  data$.response <- binarize_cat(as.character(data[[response]]),
                                pos = response.pos,
                                neg = response.neg) %>%
    factor(levels = c("pos","neg"))
  data %<>% drop_na(.response)
  if(m1.datatype == "num"){
    g1 <- .sm_boxplot(data = data, response = ".response",
                      marker = marker1, title = paste0("Marker1:", marker1))+
      theme(legend.position = "none")
  }else{
    data$.m1 <- binarize_cat( x = as.character(data[[marker1]]),
                              pos = m1.cat.pos, neg = m1.cat.neg) %>%
      factor(levels = c("pos","neg"))
    g1 <- .sm_barplot(data = data, response = ".response", marker = ".m1",
                      title = paste0("Marker1:", marker1))+
      theme(legend.position = "none")
  }
  if(m2.datatype == "num"){
    g2 <- .sm_boxplot(data = data, response = ".response", marker = marker2,
                      title = paste0("Marker2:", marker2))+
      theme(legend.position = "none")
  }else{
    data$.m2 <- binarize_cat( x = as.character(data[[marker2]]),
                              pos = m2.cat.pos, neg = m2.cat.neg) %>%
      factor(levels = c("pos","neg"))
    g2 <- .sm_barplot(data = data, response = ".response", marker = ".m2",
                      title = paste0("Marker2:", marker2))+
      theme(legend.position = "none")
  }
  cowplot::plot_grid(g1,g2, align = "h", axis = "l", ncol = 2
                     #labels= list(paste0("marker1:", marker1), paste0("marker2:", marker2))
                     )
}



