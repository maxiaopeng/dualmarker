#############
## boxplot
#############
.sm_boxplot <- function(data, response, marker, title = "", ...){
  ggplot(data, aes_string( x = response, y = marker, color = response))+
    geom_boxplot(outlier.shape = "", na.rm=T)+
    geom_jitter(width = 0.2, alpha = 0.5, na.rm=T)+
    stat_compare_means(na.rm=T, ...)+
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
dm_response_boxplot <- function(data,
                                response,
                                response.pos,
                                response.neg=NULL,
                                marker1,
                                marker2,
                                m1.datatype = "auto",
                                m1.cat.pos = NULL,
                                m1.cat.neg =NULL,
                                m2.datatype = "auto",
                                m2.cat.pos = NULL,
                                m2.cat.neg =NULL,
                                label.m1 = marker1,
                                label.m2 = marker2,
                                palette = "default",
                                label.response.pos = "pos",
                                label.response.neg = "neg",
                                label.m1.pos = "pos",
                                label.m1.neg = "neg",
                                label.m2.pos = "pos",
                                label.m2.neg = "neg",
                                na.rm.response=T,
                                na.rm.marker = T){
  assertthat::assert_that(m1.datatype %in% c("auto","num","cat"),
                          msg = "m1.datatype should be ['auto','num','cat']")
  assertthat::assert_that(m2.datatype %in% c("auto","num","cat"),
                          msg = "m2.datatype should be ['auto','num','cat']")
  if(m1.datatype == "auto"){
    m1.datatype <- datatype_num_cat(data[[marker1]])
    }
  if(m2.datatype == "auto"){
    m2.datatype <- datatype_num_cat(data[[marker2]])
    }
  data$.response <- binarize_cat(
    x = as.character(data[[response]]),
    pos = response.pos,
    neg = response.neg,
    label.pos = label.response.pos,
    label.neg = label.response.neg) %>%
    factor(levels = c(label.response.pos,label.response.neg))
  if(na.rm.response){
    data %<>% tidyr::drop_na(.response)
  }else{
    data$.response %<>% forcats::fct_explicit_na(na_level = "NA")
  }

  if(m1.datatype == "num"){
    g1 <- .sm_boxplot(data = data,
                      response = ".response",
                      marker = marker1,
                      title = paste0("Marker1: ", label.m1))+
      theme(legend.position = "none") +
      labs(y = label.m1) +
      theme (plot.title = element_text (face = "bold"))
  }else{
    data$.m1 <- binarize_cat( x = as.character(data[[marker1]]),
                              pos = m1.cat.pos, neg = m1.cat.neg,
                              label.pos = label.m1.pos,
                              label.neg = label.m1.neg) %>%
      factor(levels = c(label.m1.pos, label.m1.neg))
    if(!na.rm.marker){
      data$.m1 %<>% forcats::fct_explicit_na(na_level = "NA")
    }
    g1 <- .sm_barplot(data = data %>% dplyr::filter(!is.na(.m1)),
                      response = ".response",
                      marker = ".m1",
                      title = paste0("Marker1: ", label.m1))+
      theme(legend.position = "none")+
      labs(x = label.m1) +
      theme (plot.title = element_text (face = "bold"))
  }
  if(m2.datatype == "num"){
    #print(label.m2)
    g2 <- .sm_boxplot(data = data,
                      response = ".response", marker = marker2,
                      title = paste0("Marker2: ", label.m2))+
      theme(legend.position = "none") +
      labs(y = label.m2) +
      theme (plot.title = element_text (face = "bold"))

  }else{
    data$.m2 <- binarize_cat( x = as.character(data[[marker2]]),
                              pos = m2.cat.pos, neg = m2.cat.neg,
                              label.pos = label.m2.pos,
                              label.neg = label.m2.neg) %>%
      factor(levels = c(label.m2.pos,label.m2.neg))
    if(!na.rm.marker){
      data$.m2 %<>% forcats::fct_explicit_na(na_level = "NA")
    }
    g2 <- .sm_barplot(data = data %>% dplyr::filter(!is.na(.m2)),
                      response = ".response",
                      marker = ".m2",
                      title = paste0("Marker2: ", label.m2))+
      theme(legend.position = "none") +
      labs(x = label.m2) +
      theme (plot.title = element_text (face = "bold"))
  }
  ggpubr::ggarrange(
    ggpubr::ggpar(g1, palette = palette),
    ggpubr::ggpar(g2, palette = palette),
    align = "hv",
    ncol = 2,
    legend = "right", common.legend = T)
}



