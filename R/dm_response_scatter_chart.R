#' scatterplot
#'
#' scatter plot with cutoff lines
#'
#' @param data data frame
#' @param marker1 marker1 variable
#' @param marker2 marker2 variable
#' @param m1.cutpoint cutpoint for marker1
#' @param m2.cutpoint cutpoint for marker2
#' @param response response variable
#' @param response.pos positive response value(s)
#' @param response.neg negative response value(s)
.dm_scatterplot <- function(data, marker1, marker2,
                            response,response.pos, response.neg = NULL,
                            m1.cutpoint, m2.cutpoint ) {
  # check input
  .assert_colname(data, c(marker1, marker2, response))
  data %<>% tidyr::drop_na(!!sym(marker1), !!sym(marker2), !!sym(response))
  data$.response <- binarize_cat(
    x = data[[response]], pos = response.pos, neg = response.neg) %>%
    factor(levels = c("pos", "neg"))
  data %<>% mutate(
    region = case_when(
      marker1 >= m1.cutpoint & marker2 >= m2.cutpoint ~ "R1",
      marker1 < m1.cutpoint & marker2 >= m2.cutpoint ~ "R2",
      marker1 < m1.cutpoint & marker2 < m2.cutpoint ~ "R3",
      marker1 >= m1.cutpoint & marker2 < m2.cutpoint ~ "R4",
      TRUE ~ "others")
    ) %>% dplyr::filter(region %in% c("R1", "R2", "R3", "R4"))
  # plot
  ggplot(data = data,
         aes_string(x = marker1, y = marker2, color = ".response")) +
    annotate( "rect", xmin = m1.cutpoint, xmax = Inf, ymin = m2.cutpoint, ymax = Inf, fill = color.quadrant.1[1], alpha = 0.05) +
    annotate( "rect", xmin = -Inf,xmax = m1.cutpoint, ymin = m2.cutpoint, ymax = Inf, fill = color.quadrant.1[2], alpha = 0.05) +
    annotate( "rect", xmin = -Inf, xmax = m1.cutpoint, ymin = -Inf, ymax = m2.cutpoint, fill = color.quadrant.1[3], alpha = 0.05) +
    annotate( "rect", xmin = m1.cutpoint, xmax = Inf, ymin = -Inf, ymax = m2.cutpoint, fill = color.quadrant.1[4], alpha = 0.05) +
    geom_point() +
    geom_hline(yintercept = m2.cutpoint, linetype = "dashed", color = "skyblue") +
    geom_vline(xintercept = m1.cutpoint, linetype = "dashed", color = "skyblue") +
    #   scale_color_manual(values=c("pos" = "#F8766D", "neg"="black"))+
    theme_bw()
}

#' dm_stripplot
#'
#' boxplot for marker1(categorical), marker2(number) and response
#'
#' @param data data.frame
#' @param marker1 marker1
#' @param marker2 marker2
#' @param response response variables
#' @param m1.cat.pos positive value(s) for marker1 if marker1 is categorical
#' @param m1.cat.neg negative value(s) for marker1 if marker1 is categorical
#' @param m2.cutpoint cutpoint for marker2
#' @param response.pos positive value(s) for response
#' @param response.neg negative value(s) for response
.dm_stripplot <- function(data,
                          marker1, marker2,
                          response, response.pos, response.neg = NULL,
                          m1.cat.pos, m1.cat.neg = NULL,
                          m2.cutpoint) {
  .assert_colname(data, c(marker1, marker2, response))
  # prepare data
  data$.response <- binarize_cat(x = data[[response]], pos = response.pos,
               neg = response.neg, label.pos = "pos", label.neg = "neg") %>%
    factor(., levels = c("pos", "neg"))
  data$.m1 <- binarize_cat(x = data[[marker1]], pos = m1.cat.pos,
                           neg = m1.cat.neg, label.pos = "pos", label.neg = "neg")
  data %<>% tidyr::drop_na(.response, .m1, !!sym(marker2))

  data %<>% mutate(
    region = case_when(
      .m1 %in% "pos" & !!sym(marker2) >= m2.cutpoint ~ "R1",
      .m1 %in% "neg" &  !!sym(marker2) >= m2.cutpoint ~ "R2",
      .m1 %in% "neg" &  !!sym(marker2) < m2.cutpoint ~ "R3",
      .m1 %in% "pos" & !!sym(marker2) < m2.cutpoint ~ "R4",
      TRUE ~ "others"
    )) %>% dplyr::filter(region %in% c("R1", "R2", "R3", "R4"))
  # plot
  ggplot(data, aes_string(x = ".m1", y = marker2)) +
    geom_blank() +
    annotate( "rect", xmin = 1.5, xmax = Inf, ymin = m2.cutpoint, ymax = Inf, fill = color.quadrant.1[1],alpha = 0.1) +
    annotate( "rect", xmin = -Inf, xmax = 1.5, ymin = m2.cutpoint, ymax = Inf, fill = color.quadrant.1[2], alpha = 0.1) +
    annotate( "rect", xmin = -Inf, xmax = 1.5, ymin = -Inf, ymax = m2.cutpoint, fill = color.quadrant.1[3], alpha = 0.1 ) +
    annotate( "rect", xmin = 1.5, xmax = Inf, ymin = -Inf, ymax = m2.cutpoint, fill = color.quadrant.1[4], alpha = 0.1) +
    #geom_boxplot(size=0.05, alpha=0.4, outlier.shape = "")+
    geom_jitter(aes_string(color = ".response"), width = 0.3) +
    #stat_signif(comparisons = list(c("YES","NO")))+
    geom_hline(yintercept = m2.cutpoint, linetype = "dashed",color = "skyblue") +
    geom_vline(xintercept = 1.5, linetype = "dashed", color = "skyblue") +
    #   scale_color_manual(values=c("pos" = "#F8766D", "neg"="black"))+
    labs(x = marker1, y = marker2)+
    theme_bw()
}

#' dm_jitter
#'
#' jitter for marker1(categorical), marker2(categorical) and response
#'
#' @param data data frame
#' @param marker1 marker1 variable
#' @param marker2 marker2 variable
#' @param response response variable
#' @param response.pos positive value(s) of response
#' @param response.neg negative value(s) of response
#' @param m1.cat.pos positive value(s) if marker1 is categorical
#' @param m1.cat.neg negative value(s) if marker1 is categorical
#' @param m2.cat.pos positive value(s) if marker2 is categorical
#' @param m2.cat.neg negative value(s) if marker2 is categorical
.dm_jitter <- function(data,
                       marker1,
                       marker2,
                       response,
                       response.pos,
                       response.neg = NULL,
                       m1.cat.pos, m1.cat.neg = NULL,
                       m2.cat.pos, m2.cat.neg = NULL) {
  .assert_colname(data, c(marker1, marker2, response))
  # prepare data
  data$.response <- binarize_cat(x = data[[response]], pos = response.pos,
                                 neg = response.neg, label.pos = "pos", label.neg = "neg") %>%
    factor(., levels = c("pos", "neg"))
  data$.m1 <- binarize_cat(x = data[[marker1]], pos = m1.cat.pos,
                           neg = m1.cat.neg, label.pos = "pos", label.neg = "neg")
  data$.m2 <- binarize_cat(x = data[[marker2]], pos = m2.cat.pos,
                           neg = m2.cat.neg, label.pos = "pos", label.neg = "neg")
  data %<>% tidyr::drop_na(.response, .m1, .m2)

  data %<>% mutate(
    region = case_when(
      .m1 %in% "pos" &  .m2 %in% "pos" ~ "R1",
      .m1 %in% "neg" &  .m2 %in% "pos" ~ "R2",
      .m1 %in% "neg" &  .m2 %in% "neg" ~ "R3",
      .m1 %in% "pos" &  .m2 %in% "neg" ~ "R4",
      TRUE ~ "others"
    )) %>% dplyr::filter(region %in% c("R1", "R2", "R3", "R4"))

  # plot
  ggplot(data, aes(x = .m1, y = .m2)) +
    geom_blank() +
    annotate( "rect", xmin = 1.5, xmax = Inf, ymin = 1.5, ymax = Inf, fill = color.quadrant.1[1], alpha = 0.1) +
    annotate( "rect", xmin = -Inf, xmax = 1.5, ymin = 1.5, ymax = Inf, fill = color.quadrant.1[2], alpha = 0.1) +
    annotate( "rect", xmin = -Inf, xmax = 1.5, ymin = -Inf, ymax = 1.5, fill = color.quadrant.1[3], alpha = 0.1) +
    annotate( "rect", xmin = 1.5, xmax = Inf, ymin = -Inf, ymax = 1.5, fill = color.quadrant.1[4], alpha = 0.1) +
    geom_jitter(aes_string(color = ".response"), width = 0.3) +
    geom_hline(yintercept = 1.5, linetype = "dashed",color = "skyblue") +
    geom_vline(xintercept = 1.5, linetype = "dashed", color = "skyblue") +
    labs(x = marker1, y = marker2)+
    theme_bw()
}

#' Scatter-chart
#'
#' scatter-like chart to show relationship between two markers. Every sample is
#' presented as one dot, whose color indicates the response/response if 'response'
#' is assigned. If both markers are continuous variable, show scatter-plot. If
#' one marker is continuous, another one is categorical, show strip-plot. If
#' both are categorical, show jitter-plot.
#'
#' @param data data.frame
#' @param response response/response variables
#' @param response.pos positive value(s) for response/response
#' @param response.neg negative value(s) for response/response
#' @param marker1 marker1 variable
#' @param marker2 marker2 variable
#' @param m1.num.cut cut method/value(s) if marker1 is numeric
#' @param m1.cat.pos positive value(s) if marker1 is categorical
#' @param m1.cat.neg negative value(s) if marker1 is categorical
#' @param m2.num.cut cut method/value(s) if marker2 is numeric
#' @param m2.cat.pos positive value(s) if marker2 is categorical
#' @param m2.cat.neg negative value(s) if marker2 is categorical
dm_scatter_chart <- function(  data, response,response.pos,response.neg = NULL,
                               marker1, marker2,
                               m1.num.cut = "median", m1.cat.pos = NULL,  m1.cat.neg = NULL,
                               m2.num.cut = "median", m2.cat.pos = NULL, m2.cat.neg = NULL) {
    m1.datatype <- datatype_num_cat(data[[marker1]])
    m2.datatype <- datatype_num_cat(data[[marker2]])
    if (m2.datatype == "num") {
      m2.cutpoint <- cutpoint(x = data[[marker2]], method = m2.num.cut)
      if (m1.datatype == "num") {
        m1.cutpoint <- cutpoint( x = data[[marker1]],  method = m1.num.cut )
        .dm_scatterplot(
          data = data,
          marker1 = marker1,
          marker2 = marker2,
          response = response,
          m1.cutpoint = m1.cutpoint,
          m2.cutpoint = m2.cutpoint,
          response.pos = response.pos,
          response.neg = response.neg
        )
      } else{
        assert_that(!is.null(m1.cat.pos), msg = "m1.cat.pos should not be NULL")
        .dm_stripplot(
          data = data,
          marker1 = marker1,
          marker2 = marker2,
          response = response,
          response.pos = response.pos,
          response.neg = response.neg,
          m1.cat.pos = m1.cat.pos,
          m1.cat.neg = m1.cat.neg,
          m2.cutpoint = m2.cutpoint
        )
      }
    } else if (m1.datatype == "num") {
      m1.cutpoint <- cutpoint( x = data[[marker1]],method = m1.num.cut )
      assert_that(!is.null(m2.cat.pos), msg = "m2.cat.pos should not be NULL")
      .dm_stripplot(
        data = data,
        marker1 = marker2,
        marker2 = marker1,
        response = response,
        m1.cat.pos = m2.cat.pos,
        m1.cat.neg = m2.cat.neg,
        m2.cutpoint = m1.cutpoint,
        response.pos = response.pos,
        response.neg = response.neg
      ) + coord_flip()
    } else{
      assert_that(!is.null(m1.cat.pos) && !is.null(m2.cat.pos),
                  msg = "m1.cat.pos, m2.cat.pos should NOT be NULL")
      .dm_jitter(
        data = data,
        marker1 = marker1,
        marker2 = marker2,
        response = response,
        m1.cat.pos = m1.cat.pos,
        m1.cat.neg = m1.cat.neg,
        m2.cat.pos = m2.cat.pos,
        m2.cat.neg = m2.cat.neg,
        response.pos = response.pos ,
        response.neg = response.neg
      )
    }
  }
