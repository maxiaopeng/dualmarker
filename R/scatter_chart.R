#' scatterplot
#'
#' scatter plot with cutoff lines
#'
#' @param data data frame
#' @param marker1 marker1 variable
#' @param marker2 marker2 variable
#' @param m1_cutpoint cutpoint for marker1
#' @param m2_cutpoint cutpoint for marker2
#' @param response response variable
#' @param response_pos positive response value(s)
#' @param response_neg negative response value(s)
.dm_scatterplot <- function(data,
                            marker1,
                            marker2,
                            response,
                            m1_cutpoint,
                            m2_cutpoint,
                            response_pos,
                            response_neg = NULL) {
  # check input
  assert_that(all(c(response, marker1, marker2) %in% colnames(data)),
              msg = paste(c(
                marker1, marker2, response, "not in your data"
              )))
  if (is.null(response_neg))
    response_neg <-
      setdiff(na.omit(unique(data[[response]])), response_pos)
  # prep data
  keep <- data[[response]] %in% c(response_pos, response_neg)
  if (any(!keep)) {
    print(paste0(
      "warning: ",
      sum(!keep),
      " records have no expected response, removed\n"
    ))
    data <- data[keep, ]
  }
  data$.response <-
    ifelse(data[[response]] %in% response_pos, "pos", "neg") %>%
    factor(levels = c("pos", "neg"))
  data$region <-
    case_when(
      marker1 >= m1_cutpoint & marker2 >= m2_cutpoint ~ "R1",
      marker1 < m1_cutpoint &
        marker2 >= m2_cutpoint ~ "R2",
      marker1 < m1_cutpoint &
        marker2 < m2_cutpoint ~ "R3",
      marker1 >= m1_cutpoint &
        marker2 < m2_cutpoint ~ "R4",
      TRUE ~ "others"
    )
  data %<>% dplyr::filter(region %in% c("R1", "R2", "R3", "R4"))
  # plot
  ggplot(data = data,
         aes_string(x = marker1, y = marker2, color = ".response")) +
    annotate(
      "rect",
      xmin = m1_cutpoint,
      xmax = Inf,
      ymin = m2_cutpoint,
      ymax = Inf,
      fill = color.quadrant.1[1],
      alpha = 0.05
    ) +
    annotate(
      "rect",
      xmin = -Inf,
      xmax = m1_cutpoint,
      ymin = m2_cutpoint,
      ymax = Inf,
      fill = color.quadrant.1[2],
      alpha = 0.05
    ) +
    annotate(
      "rect",
      xmin = -Inf,
      xmax = m1_cutpoint,
      ymin = -Inf,
      ymax = m2_cutpoint,
      fill = color.quadrant.1[3],
      alpha = 0.05
    ) +
    annotate(
      "rect",
      xmin = m1_cutpoint,
      xmax = Inf,
      ymin = -Inf,
      ymax = m2_cutpoint,
      fill = color.quadrant.1[4],
      alpha = 0.05
    ) +
    geom_point() +
    geom_hline(yintercept = m2_cutpoint,
               linetype = "dashed",
               color = "skyblue") +
    geom_vline(xintercept = m1_cutpoint,
               linetype = "dashed",
               color = "skyblue") +
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
#' @param m1_cat_pos positive value(s) for marker1 if marker1 is categorical
#' @param m1_cat_neg negative value(s) for marker1 if marker1 is categorical
#' @param m2_cutpoint cutpoint for marker2
#' @param response_pos positive value(s) for response
#' @param response_neg negative value(s) for response
.dm_stripplot <- function(data,
                          marker1,
                          marker2,
                          response,
                          response_pos,
                          response_neg = NULL,
                          m1_cat_pos,
                          m1_cat_neg = NULL,
                          m2_cutpoint) {
  # check input
  assert_that(all(c(response, marker1, marker2) %in% colnames(data)),
              msg = paste(c(
                marker1, marker2, response, "not in your data"
              )))
  if (is.null(response_neg))
    response_neg <-
      setdiff(na.omit(unique(data[[response]])), response_pos)
  # prep data
  keep <- data[[response]] %in% c(response_pos, response_neg)
  if (any(!keep)) {
    print(paste0(
      "warning: ",
      sum(!keep),
      " records have no expected response, removed\n"
    ))
    data <- data[keep, ]
  }
  data$.response <-
    ifelse(data[[response]] %in% response_pos, "pos", "neg") %>%
    factor(levels = c("pos", "neg"))
  data %<>% mutate(
    region = case_when(
      !!sym(marker1) %in% m1_cat_pos &
        !!sym(marker2) >= m2_cutpoint ~ "R1",!!sym(marker1) %in% m1_cat_neg &
        !!sym(marker2) >= m2_cutpoint ~ "R2",!!sym(marker1) %in% m1_cat_neg &
        !!sym(marker2) < m2_cutpoint ~ "R3",!!sym(marker1) %in% m1_cat_pos &
        !!sym(marker2) < m2_cutpoint ~ "R4",
      TRUE ~ "others"
    )
  ) %>%
    dplyr::filter(region %in% c("R1", "R2", "R3", "R4"))

  ggplot(data, aes_string(x = marker1, y = marker2)) +
    geom_blank() +
    annotate(
      "rect",
      xmin = 1.5,
      xmax = Inf,
      ymin = m2_cutpoint,
      ymax = Inf,
      fill = color.quadrant.1[1],
      alpha = 0.1
    ) +
    annotate(
      "rect",
      xmin = -Inf,
      xmax = 1.5,
      ymin = m2_cutpoint,
      ymax = Inf,
      fill = color.quadrant.1[2],
      alpha = 0.1
    ) +
    annotate(
      "rect",
      xmin = -Inf,
      xmax = 1.5,
      ymin = -Inf,
      ymax = m2_cutpoint,
      fill = color.quadrant.1[3],
      alpha = 0.1
    ) +
    annotate(
      "rect",
      xmin = 1.5,
      xmax = Inf,
      ymin = -Inf,
      ymax = m2_cutpoint,
      fill = color.quadrant.1[4],
      alpha = 0.1
    ) +
    #geom_boxplot(size=0.05, alpha=0.4, outlier.shape = "")+
    geom_jitter(aes_string(color = ".response"), width = 0.3) +
    #stat_signif(comparisons = list(c("YES","NO")))+
    geom_hline(yintercept = m2_cutpoint,
               linetype = "dashed",
               color = "skyblue") +
    geom_vline(xintercept = 1.5,
               linetype = "dashed",
               color = "skyblue") +
    #   scale_color_manual(values=c("pos" = "#F8766D", "neg"="black"))+
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
#' @param response_pos positive value(s) of response
#' @param response_neg negative value(s) of response
#' @param m1_cat_pos positive value(s) if marker1 is categorical
#' @param m1_cat_neg negative value(s) if marker1 is categorical
#' @param m2_cat_pos positive value(s) if marker2 is categorical
#' @param m2_cat_neg negative value(s) if marker2 is categorical
.dm_jitter <- function(data,
                       marker1,
                       marker2,
                       response,
                       response_pos,
                       response_neg = NULL,
                       m1_cat_pos,
                       m1_cat_neg = NULL,
                       m2_cat_pos,
                       m2_cat_neg = NULL) {
  # check input
  assert_that(all(c(response, marker1, marker2) %in% colnames(data)),
              msg = paste(c(
                marker1, marker2, response, "not in your data"
              )))
  if (is.null(response_neg))
    response_neg <-
      setdiff(na.omit(unique(data[[response]])), response_pos)
  # prep data
  keep <- data[[response]] %in% c(response_pos, response_neg)
  if (any(!keep)) {
    print(paste0(
      "warning: ",
      sum(!keep),
      " records have no expected response, removed\n"
    ))
    data <- data[keep, ]
  }
  data$.response <-
    ifelse(data[[response]] %in% response_pos, "pos", "neg") %>%
    factor(levels = c("pos", "neg"))
  data %<>% mutate(
    region = dplyr::case_when(
      !!sym(marker1) %in% m1_cat_pos &
        !!sym(marker2) %in% m2_cat_pos ~ "R1",!!sym(marker1) %in% m1_cat_neg &
        !!sym(marker2) %in% m2_cat_pos ~ "R2",!!sym(marker1) %in% m1_cat_neg &
        !!sym(marker2) %in% m2_cat_neg ~ "R3",!!sym(marker1) %in% m1_cat_pos &
        !!sym(marker2) %in% m2_cat_neg ~ "R4",
      TRUE ~ "others"
    )
  ) %>%
    dplyr::filter(region %in% c("R1", "R2", "R3", "R4"))

  ggplot(data, aes_string(x = marker1, y = marker2)) +
    geom_blank() +
    annotate(
      "rect",
      xmin = 1.5,
      xmax = Inf,
      ymin = 1.5,
      ymax = Inf,
      fill = color.quadrant.1[1],
      alpha = 0.1
    ) +
    annotate(
      "rect",
      xmin = -Inf,
      xmax = 1.5,
      ymin = 1.5,
      ymax = Inf,
      fill = color.quadrant.1[2],
      alpha = 0.1
    ) +
    annotate(
      "rect",
      xmin = -Inf,
      xmax = 1.5,
      ymin = -Inf,
      ymax = 1.5,
      fill = color.quadrant.1[3],
      alpha = 0.1
    ) +
    annotate(
      "rect",
      xmin = 1.5,
      xmax = Inf,
      ymin = -Inf,
      ymax = 1.5,
      fill = color.quadrant.1[4],
      alpha = 0.1
    ) +
    geom_jitter(aes_string(color = ".response"), width = 0.3) +
    geom_hline(yintercept = 1.5,
               linetype = "dashed",
               color = "skyblue") +
    geom_vline(xintercept = 1.5,
               linetype = "dashed",
               color = "skyblue") +
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
#' @param response_pos positive value(s) for response/response
#' @param response_neg negative value(s) for response/response
#' @param marker1 marker1 variable
#' @param marker2 marker2 variable
#' @param m1_datatype data type for marker1, 'num' or 'cat', default 'auto'
#' @param m2_datatype data type for marker1, 'num' or 'cat', default 'auto'
#' @param num_cut_method cut method if markers are numeric
#' @param m1_cat_pos positive value(s) if marker1 is categorical
#' @param m1_cat_neg negative value(s) if marker1 is categorical
#' @param m2_cat_pos positive value(s) if marker2 is categorical
#' @param m2_cat_neg negative value(s) if marker2 is categorical
#' @export
dm_scatter_chart <-
  function(data,
           response,
           response_pos,
           response_neg = NULL,
           marker1,
           marker2,
           m1_datatype = "auto",
           m2_datatype = "auto",
           num_cut_method = "median",
           m1_cat_pos = NULL,
           m1_cat_neg = NULL,
           m2_cat_pos = NULL,
           m2_cat_neg = NULL) {
    if (m1_datatype == "auto") {
      m1_datatype <- datatype_num_cat(data[[marker1]])
    }
    if (m2_datatype == "auto") {
      m2_datatype <- datatype_num_cat(data[[marker2]])
    }
    if (m2_datatype == "num") {
      assert_that(num_cut_method %in% c("median", "roc"),
                  msg = "num_cut_method should be ['median', 'roc'], not 'none'")
      m2_cutpoint <-
        cutpoint(
          x = data[[marker2]],
          method = num_cut_method,
          response = data[[response]],
          response_pos = response_pos,
          response_neg = response_neg
        )
      #print("m2_cutpoint", m2_cutpoint)
      if (m1_datatype == "num") {
        m1_cutpoint <-
          cutpoint(
            x = data[[marker1]],
            method = num_cut_method,
            response = data[[response]],
            response_pos = response_pos,
            response_neg = response_neg
          )
        .dm_scatterplot(
          data = data,
          marker1 = marker1,
          marker2 = marker2,
          response = response,
          m1_cutpoint = m1_cutpoint,
          m2_cutpoint = m2_cutpoint,
          response_pos = response_pos,
          response_neg = response_neg
        )
      } else{
        assert_that(!is.null(m1_cat_pos), msg = "m1_cat_pos should not be NULL")
        .dm_stripplot(
          data = data,
          marker1 = marker1,
          marker2 = marker2,
          response = response,
          m1_cat_pos = m1_cat_pos,
          m1_cat_neg = m1_cat_neg,
          m2_cutpoint = m2_cutpoint,
          response_pos = response_pos,
          response_neg = response_neg
        )
      }
    } else if (m1_datatype == "num") {
      m1_cutpoint <-
        cutpoint(
          x = data[[marker1]],
          method = num_cut_method,
          response = data[[response]],
          response_pos = response_pos,
          response_neg = response_neg
        )
      assert_that(!is.null(m2_cat_pos), msg = "m2_cat_pos should not be NULL")
      .dm_stripplot(
        data = data,
        marker1 = marker2,
        marker2 = marker1,
        response = response,
        m1_cat_pos = m2_cat_pos,
        m1_cat_neg = m2_cat_neg,
        m2_cutpoint = m1_cutpoint,
        response_pos = response_pos,
        response_neg = response_neg
      ) + coord_flip()
    } else{
      assert_that(!is.null(m1_cat_pos) && !is.null(m2_cat_pos),
                  msg = "m1_cat_pos, m2_cat_pos should NOT be NULL")
      .dm_jitter(
        data = data,
        marker1 = marker1,
        marker2 = marker2,
        response = response,
        m1_cat_pos = m1_cat_pos,
        m1_cat_neg = m1_cat_neg,
        m2_cat_pos = m2_cat_pos,
        m2_cat_neg = m2_cat_neg,
        response_pos = response_pos ,
        response_neg = response_neg
      )
    }
  }
