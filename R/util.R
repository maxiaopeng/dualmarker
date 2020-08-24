#' @import survival
#' @import ggplot2
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom magrittr set_colnames
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr as_data_frame
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr case_when
#' @importFrom dplyr arrange
#' @importFrom dplyr ungroup
#' @importFrom dplyr bind_rows bind_cols
#' @importFrom dplyr filter
#' @importFrom dplyr tibble
#' @importFrom dplyr everything
#' @importFrom dplyr left_join
#' @importFrom purrr map
#' @importFrom purrr walk
#' @importFrom purrr map_dfc
#' @importFrom purrr map_df
#' @importFrom purrr map2_df
#' @importFrom broom tidy
#' @importFrom rlang sym
#' @importFrom rlang !!
#' @importFrom tibble add_column
#' @importFrom tibble as_tibble
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr spread
#' @importFrom tidyr gather
#' @importFrom tidyr drop_na
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_sub
#' @importFrom survminer ggsurvplot
#' @importFrom survminer surv_cutpoint
#' @importFrom pROC coords
#' @importFrom pROC auc roc
#' @importFrom pROC ggroc
#' @importFrom ggpubr stat_compare_means
.load_library <- function(){}

#' get datatype "num" or "cat"
#'
#' @param x vector
#' @return "num" or "cat"
datatype_num_cat <- function(x) {
  if (class(x) %in% c("factor", "character")) {
    res <- "cat"
  } else if (class(x) %in% c("numeric", "integer", "float", "double")) {
    res <- "num"
  } else{
    stop("not cat or num, please check\n")
  }
  res
}

#' cutpoint
#'
#' get cutpoint
#' @param x vector
#' @param method median(default), roc, optimal_surv
#' @param response used if method='roc'
#' @param response_pos used if method='roc', positive response label
#' @param response_neg used if method='roc', negative response label, if NULL, use all other samples except response_pos
#' @param auc_best_method 'youden' or 'closest.topleft', used if method='roc'
#' @param time used if method='optimal_surv_cut', survival time used for
#' @param surv_minprop used if method='optimal_surv_cut', min proportion in high and low group
#' @return cutpoint
#' @export
cutpoint <- function(x,
                     method = "median",
                     response = NULL,
                     response_pos = NULL,
                     response_neg = NULL,
                     auc_best_method = "youden",
                     time = NULL,
                     event = NULL,
                     surv_minprop = 0.3) {
  assert_that(all(class(x) %in% c("numeric", "integer")),
              msg = "x should be numeric")
  min <- min(x, na.rm = T)
  max <- max(x, na.rm = T)
  cutpoint <- NA
  if (method == "median") {
    cutpoint <- median(x, na.rm = T)
  } else if (method %in% c("roc")) {
    assert_that(!is.null(response) && !is.null(response_pos),
                msg = "response and response_pos are required")
    df <- data.frame(predictor = x,
                     response = response)
    cutpoint <- auc_stats(
      data = df,
      response = "response",
      control = response_neg,
      case = response_pos,
      predictor = "predictor",
      best_method = auc_best_method
    )$threhold[1]
  } else if (method %in% c("optimal_surv")) {
    assert_that(!is.null(time) && !is.null(event),
                msg = "time and event are required")
    df <- data.frame(time = time,
                     event = event,
                     feature = x)
    surv.cutp <- do.call(
      "surv_cutpoint",
      list(
        data = df,
        time = "time",
        event = "event",
        variables = "feature",
        minprop = surv_minprop
      )
    )
    cutpoint <- surv.cutp$cutpoint[1, 1]
  } else{
    stop("method should be [median, roc, optimal_surv_cut]")
  }
  assert_that(cutpoint >= min && cutpoint <= max,
              msg = paste0("cut point should be [", min, ",", max, "]"))
  cutpoint
}

#' binarize numeric values
#'
#' binarize numeric values by predefined cutpoint
#'
#' @param x vector of numeric
#' @param cutpoint the cutpoint
#' @param response necessary if cut = "youden_ROC"
#' @param label_pos label for 'positive' group, default 'high'
#' @param label_neg label for 'negative' group, default 'low'
#' @return binary value
#' @export
binarize_num <- function(x,
                         cutpoint,
                         return_binary = F,
                         label_pos = "high",
                         label_neg = "low") {
  min <- min(x, na.rm = T)
  max <- max(x, na.rm = T)
  assert_that(cutpoint >= min && cutpoint <= max,
              msg = paste0("cut point should be [", min, ",", max, "]"))
  breaks <- c(min - 1, cutpoint, max + 1)
  if (return_binary) {
    out <- cut(x, breaks = breaks, labels = c(0, 1)) %>%
      as.character() %>% as.integer()
  } else{
    out <- cut(x,
               breaks = breaks,
               labels = c(label_neg, label_pos))
  }
  out
}

#' binarize categorical variable
#' @param x vector
#' @param pos positive values
#' @param neg negative values, if NULL, use all other values except 'pos', the non-pos,
#' non-neg values in x will be converted to NA
#' @param return_binary return the binary 0(negative), 1(positive), default FALSE
#' @param label_pos label for positive values, default 'pos'
#' @param label_neg label for negative values, default 'neg'
binarize_cat <- function(x,
                         pos,
                         neg = NULL,
                         return_binary = F,
                         label_pos = "pos",
                         label_neg = "neg") {
  assert_that(all(class(x) %in% c("factor", "character")), msg = "x should be factor/character")
  assert_that(!all(is.na(x)), msg = " all x is NA, can't be cutted")
  if ("factor" %in% class(x)) {
    x <- as.character(x)
  }
  all.val <- na.omit(unique(x))
  if (is.null(neg))
    neg <- setdiff(all.val, pos)
  assert_that(all(c(pos, neg) %in% all.val), msg = "pos or neg isn't in x")
  out <- base::rep(NA, times = length(x))
  if (return_binary) {
    out[x %in% pos] <- 1
    out[x %in% neg] <- 0
    out <- as.integer(out)
  } else{
    out[x %in% pos] <- label_pos
    out[x %in% neg] <- label_neg
    out <- factor(out, levels = c(label_neg, label_pos))
  }
  out
}


#' binarize categorical or numeric data
#'
#' @param x input vector
#' @param datatype "num" or "cat", default "auto", which will get by \code{\link{datatype_num_cat}(x)}
#' @param return_binary return the binary 0(negative), 1(positive), default FALSE
#' @param label_pos label for positive values, default 'pos'
#' @param label_neg label for negative values, default 'neg'
#' @param cat_pos if x is categorical, the positive group values
#' @param cat_neg if x is categorical, the negative group values
#' @param num_cut_method cut method for numeric variable
#' @param response response variable
#' @param response_pos positive values for response
#' @param response_neg negative values for response
#' @param auc_best_method best method using AUC analysis. Possible values include: "youden"(default), "closest.topleft"
#' @param time survival time
#' @param event survival event
#' @param surv_minprop optimal cutpoint based on survival, minimum proporal
#' @return list containing 'data' and 'cutpoint', 'data' is the binary response,
#' 'cutpoint' is NA if input isn't numeric variable
binarize_data <- function(x,
                          datatype = "auto",
                          return_binary = F,
                          label_pos = NULL,
                          label_neg = NULL,
                          cat_pos = NULL,
                          cat_neg = NULL,
                          num_cut_method = "median",
                          response = NULL,
                          response_pos = NULL,
                          response_neg = NULL,
                          auc_best_method = "youden",
                          time = NULL,
                          event = NULL,
                          surv_minprop = 0.3) {
  assert_that(datatype %in% c("auto", "num", "cat"),
              msg = " datatype should be [auto, num, cat]")
  cutpoint <- NA
  if (datatype == "auto") {
    datatype <- datatype_num_cat(x)
  }
  if (datatype == "num") {
    assert_that(num_cut_method %in% c("roc", "median", "optimal_surv"),
                msg = "num_cut_method should be [roc, median, optimal_surv]")
    cutpoint <- cutpoint(
      x = x,
      method = num_cut_method,
      response = response,
      response_pos = response_pos,
      response_neg = response_neg,
      auc_best_method = auc_best_method,
      time = time ,
      event = event,
      surv_minprop = surv_minprop
    )
    if (is.null(label_pos))
      label_pos <- "high"
    if (is.null(label_neg))
      label_neg <- "low"
    m <-
      binarize_num(
        x = x,
        cutpoint = cutpoint,
        return_binary = return_binary,
        label_neg = label_neg,
        label_pos = label_pos
      )
  } else{
    if (is.null(label_pos))
      label_pos <- "pos"
    if (is.null(label_neg))
      label_neg <- "neg"
    assert_that(!is.null(cat_pos),
                msg = "cat_pos should not be NULL")
    m <-
      binarize_cat(
        x = x,
        pos = cat_pos,
        neg = cat_neg,
        return_binary = return_binary,
        label_pos = label_pos,
        label_neg = label_neg
      )
  }
  list(data = m, cutpoint = cutpoint)
}

#' auc_stats
#'
#' get statistics of AUC
#'
#' @param data dataframe
#' @param response response variable
#' @export
auc_stats <-
  function(data,
           response,
           case,
           control = NULL,
           predictor,
           best_method = "youden") {
    assert_that(all(c(response, predictor) %in% colnames(data)),
                msg = paste(response, "or", predictor, "not in data"))
    if (is.null(control))
      control <- setdiff(na.omit(unique(data[[response]])), case)
    # remove non-case and non-control record
    keep <- data[[response]] %in% c(case, control)
    if (any(!keep)) {
      print(paste0(
        "warning: ",
        sum(!keep),
        " records have no expected response, removed\n"
      ))
      data <- data[keep, ]
    }
    data$.response <-
      ifelse(data[[response]] %in% case, "case", "control")
    # roc
    res.roc <- pROC::roc(
      data$.response,
      data[[predictor]],
      levels = c("control", "case"),
      ci = T,
      auc = T,
      plot = F
    )
    res.coords <- pROC::coords(
      res.roc,
      "best",
      ret = c(
        "threshold",
        "specificity",
        "sensitivity",
        "accuracy",
        "ppv",
        "npv"
      ),
      transpose = FALSE,
      best.method = best_method
    )
    tibble(
      control = toString(control),
      case = toString(case),
      direction = res.roc$direction,
      n.control = length(res.roc$control),
      n.case = length(res.roc$cases),
      n.all = length(res.roc$control) + length(res.roc$case),
      auc = round(res.roc$auc, digits = 3),
      lower.95 = res.roc$ci[1],
      upper.95 = res.roc$ci[3],
      threhold = res.coords$threshold,
      specificity.threhold = res.coords$specificity,
      sensitivity.threhold = res.coords$sensitivity,
      accuracy.threhold = res.coords$accuracy,
      ppv.threhold = res.coords$ppv,
      npv.threhold = res.coords$npv
    )
  }


# survfit from data.frame
.survfit <- function(data, var, time, event) {
  data[[event]] %<>% as.character() %>% as.integer()
  assert_that(all(data[[event]] %in% c(0, 1)),
              msg = "event should be 0 and 1")
  str.formula <- paste0("Surv(", time, ",", event, ") ~ ", var)
  survfit <- do.call("survfit",
                     list(
                       as.formula(str.formula),
                       data = data,
                       conf.type = 'log-log'
                     )) # log-log to get the same result with SAS
  survfit
}

# survplot using survminer
.survplot <- function(survfit, data, ...) {
  survminer::ggsurvplot(survfit,
                        data = data,
                        pval = T,
                        risk.table = T,
                        ...)
}

#' label quadrant
#'
#' label four quadrants R1, R2, R3, R4, by binary input of marker1 and marker2
#'
#' marker1, marker2 are binary factors, and first level as negative, 2nd level as positive
#' for (m1, m2):
#' (pos, pos) => R1, first quadrant
#' (neg, pos) => R2, 2nd quadrant
#' (neg, neg) => R3, 3rd quadrant
#' (pos, neg) => R4, 4th quadrant
#'
#' @param m1 binary factor, 1st level as negative, 2nd level as positive
#' @param m2 binary factor, 1st level as negative, 2nd level as positive
#' @return factor with 4 levels, R1, R2, R3, R4
.label_quadrant <- function(m1, m2) {
  assert_that(class(m1) == "factor" && nlevels(m1) == 2,
              msg = "marker1 should be factor with 2 levels")
  assert_that(class(m2) == "factor" && nlevels(m2) == 2,
              msg = "marker2 should be factor with 2 levels")
  assert_that(length(m1) == length(m2),
              msg = "marker1 and marker2 have different length")
  m1.neg <- levels(m1)[1]
  m1.pos <- levels(m1)[2]
  m2.neg <- levels(m2)[1]
  m2.pos <- levels(m2)[2]
  dplyr::case_when(
    m1 %in% m1.pos & m2 %in% m2.pos ~ "R1",
    m1 %in% m1.neg & m2 %in% m2.pos ~ "R2",
    m1 %in% m1.neg & m2 %in% m2.neg ~ "R3",
    m1 %in% m1.pos & m2 %in% m2.neg ~ "R4",
    TRUE ~ ""
  ) %>% factor(., levels = c("R1", "R2", "R3", "R4"))
}
