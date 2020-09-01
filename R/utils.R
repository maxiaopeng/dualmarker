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
#' @importFrom dplyr group_modify
#' @importFrom dplyr summarise
#' @importFrom dplyr case_when
#' @importFrom dplyr arrange
#' @importFrom dplyr ungroup
#' @importFrom dplyr bind_rows bind_cols
#' @importFrom dplyr filter
#' @importFrom dplyr tibble
#' @importFrom dplyr everything
#' @importFrom dplyr left_join
#' @importFrom dplyr progress_estimated
#' @importFrom purrr map
#' @importFrom purrr walk
#' @importFrom purrr map_dfc
#' @importFrom purrr map_dfr
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
#' @importFrom tidyr unite
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_sub
#' @importFrom stringr str_subset
#' @importFrom survminer ggsurvplot
#' @importFrom survminer surv_cutpoint
#' @importFrom survminer surv_pvalue
#' @importFrom pROC coords
#' @importFrom pROC auc roc
#' @importFrom pROC ggroc
#' @importFrom ggpubr stat_compare_means
#' @importFrom glue glue
#' @importFrom ggrepel geom_text_repel
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
#' @param method median(default), roc, mean, mean+sd, mean-sd, upper-tertile, lower-tertile
#' @param response used if method='roc'
#' @param response.pos used if method='roc', positive response label
#' @param response.neg used if method='roc', negative response label, if NULL, use all other samples except response.pos
#' @param auc_best_method 'youden' or 'closest.topleft', used if method='roc'
#' @return cutpoint
#' @export
cutpoint <- function(x, method = "median") {
  assert_that(all(class(x) %in% c("numeric", "integer")),
              msg = "x should be numeric")
  min <- min(x, na.rm = T)
  max <- max(x, na.rm = T)
  if(class(method) %in% c("numeric","integer")){
    cutpoint <- method
  } else if (method == "median") {
    cutpoint <- median(x, na.rm = T)
  } else if(method == "mean"){
    cutpoint <- mean(x, na.rm = T)
  } else if(method == "mean+sd"){
    cutpoint <- mean(x, na.rm=T) + sd(x, na.rm=T)
  } else if(method == "mean-sd"){
    cutpoint <- mean(x, na.rm=T) - sd(x, na.rm=T)
  } else if(method == "upper-tertile"){
    cutpoint <- quantile(x, probs = 2/3, na.rm=T)
  }  else if(method == "lower-tertile"){
    cutpoint <- quantile(x, probs = 1/3, na.rm=T)
  } else{
    stop("method should be [median,mean,mean+sd, mean-sd, upper-tertile, lower-tertile]")
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
#' @param label.pos label for 'positive' group, default 'high'
#' @param label.neg label for 'negative' group, default 'low'
#' @return binary value
#' @export
binarize_num <- function(x,
                         cutpoint,
                         return.binary = F,
                         label.pos = "high",
                         label.neg = "low") {
  min <- min(x, na.rm = T)
  max <- max(x, na.rm = T)
  assert_that(cutpoint >= min && cutpoint <= max,
              msg = paste0("cut point should be [", min, ",", max, "]"))
  breaks <- c(min - 1, cutpoint, max + 1)
  if (return.binary) {
    out <- cut(x, breaks = breaks, labels = c(0, 1)) %>%
      as.character() %>% as.integer()
  } else{
    out <- cut(x,
               breaks = breaks,
               labels = c(label.neg, label.pos))
  }
  out
}

#' binarize categorical variable
#' @param x vector
#' @param pos positive values
#' @param neg negative values, if NULL, use all other values except 'pos', the non-pos,
#' non-neg values in x will be converted to NA
#' @param return.binary return the binary 0(negative), 1(positive), default FALSE
#' @param label.pos label for positive values, default 'pos'
#' @param label.neg label for negative values, default 'neg'
binarize_cat <- function(x,
                         pos,
                         neg = NULL,
                         return.binary = F,
                         label.pos = "pos",
                         label.neg = "neg") {
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
  if (return.binary) {
    out[x %in% pos] <- 1
    out[x %in% neg] <- 0
    out <- as.integer(out)
  } else{
    out[x %in% pos] <- label.pos
    out[x %in% neg] <- label.neg
    out <- factor(out, levels = c(label.neg, label.pos))
  }
  out
}


#' binarize categorical or numeric data
#'
#' @param x input vector
#' @param datatype "num" or "cat", default "auto", which will get by \code{\link{datatype_num_cat}(x)}
#' @param return.binary return the binary 0(negative), 1(positive), default FALSE
#' @param label.pos label for positive values, default 'pos'
#' @param label.neg label for negative values, default 'neg'
#' @param cat.pos if x is categorical, the positive group values
#' @param cat.neg if x is categorical, the negative group values
#' @param num.cut cut method for numeric variable
#' @return list containing 'data' and 'cutpoint', 'data' is the binary response,
#' 'cutpoint' is NA if input isn't numeric variable
binarize_data <- function(x, datatype = "auto",
                          return.binary = F,
                          num.cut = "median",
                          cat.pos = NULL,
                          cat.neg = NULL,
                          label.pos = NULL,
                          label.neg = NULL) {
  assert_that(length(x)>0)
  assert_that(datatype %in% c("auto", "num", "cat"),
              msg = " datatype should be [auto, num, cat]")
  cutpoint <- NA
  if (datatype == "auto") {
    datatype <- datatype_num_cat(x)
  }
  if (datatype == "num") {
    cutpoint <- cutpoint( x = x, method = num.cut)
    if (is.null(label.pos)) label.pos <- "high"
    if (is.null(label.neg)) label.neg <- "low"
    m <-binarize_num( x = x,
        cutpoint = cutpoint, return.binary = return.binary,
        label.neg = label.neg, label.pos = label.pos)
  } else{
    if (is.null(label.pos)) label.pos <- "pos"
    if (is.null(label.neg)) label.neg <- "neg"
    assert_that(!is.null(cat.pos), msg = "cat.pos should not be NULL")
    m <- binarize_cat( x = x, pos = cat.pos, neg = cat.neg, return.binary = return.binary,
        label.pos = label.pos, label.neg = label.neg)
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
auc_stats <- function(data, response,
           case, control = NULL, predictor,
           best_method = "youden") {
    assert_that(all(c(response, predictor) %in% colnames(data)),
                msg = paste(response, "or", predictor, "not in data"))
    if (is.null(control))
      control <- setdiff(na.omit(unique(data[[response]])), case)
    # remove non-case and non-control record
    keep <- data[[response]] %in% c(case, control)
    if (any(!keep)) {
      #warning(paste0(sum(!keep)," records have no expected response, removed"))
      data <- data[keep, ]
    }
    data$.response <- ifelse(data[[response]] %in% case, "case", "control")
    # roc
    res.roc <- pROC::roc(
      data$.response, data[[predictor]],
      levels = c("control", "case"),
      ci = T, auc = T,plot = F)
    res.coords <- pROC::coords(
      res.roc,"best",
      ret = c( "threshold", "specificity","sensitivity",
        "accuracy", "ppv","npv"),
      transpose = FALSE,best.method = best_method )
    tibble(
      control = toString(control),
      case = toString(case),
      direction = res.roc$direction,
      n.control = length(res.roc$control),
      n.case = length(res.roc$cases),
      n.all = length(res.roc$control) + length(res.roc$case),
      auc = round(res.roc$auc, digits = 3),
      auc.lower95 = res.roc$ci[1],
      auc.upper95 = res.roc$ci[3],
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

#' check the variable in colnames
#'
#' @param data dataframe
#' @param var variable names
#'
.assert_colname <- function(data, var){
    var <- var[!is.null(var) & !is.na(var)]
    if(length(var)>0){
      keep <- var %in% colnames(data)
      assert_that(
        all(keep),
        msg = glue::glue(sum(!keep)," variables do not exist:", var[!keep]))
    }
}


#' collapse data.frame to one-row tibble
#'
#' @param df data frame
#' @param id_cols id cols, unique
#' @param var_cols var cols
#' @param sep separator between id and value
#' @return one-row of tibble, each column is the
.collapse_df <- function(df, id_cols, var_cols, sep="_"){
  df %>%
    dplyr::select(all_of(c(id_cols, var_cols))) %>%
    tidyr::pivot_longer(cols = all_of(var_cols),
                        names_to = ".var.name", values_to = ".value") %>%
    tidyr::pivot_wider(names_from = all_of(c(id_cols, ".var.name")),
                       values_from = ".value", names_sep = sep)
}


