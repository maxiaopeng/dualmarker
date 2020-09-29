#' quadrant stats survival
#'
#' statistics of survival time in each quadrant
#'
#' @param data dataframe
#' @param time survival time
#' @param event survival event
#' @param marker1 marker1
#' @param marker2 marker2
#' @return
.quadrant_survival_stats <- function(data, time, event,
                                    marker1, marker2){

  data %<>% tidyr::drop_na(!!sym(marker1), !!sym(marker2))
  res <- data %>% dplyr::group_by(!!sym(marker1), !!sym(marker2)) %>%
    dplyr::group_modify(.f = ~{
      tmp <- .survfit(data = .x, var = "1", time = time, event = event)
      summary(tmp)$table %>% t %>% as_tibble() %>%
        dplyr::select(records, events, median, `0.95LCL`, `0.95UCL`)
    }) %>% ungroup()
  res$.quadrant <- .label_quadrant(res[[marker1]], res[[marker2]])
  res %>% dplyr::select(.quadrant, dplyr::everything()) %>%
    dplyr::arrange(.quadrant)
}

#' Survival test for binary variables
#'
#' test using log-rank and cox-test for binary variable
#'
#' @param data dataframe
#' @param var variable
#' @param time survival time
#' @param event survival event
#' @return log-rank test and cox test result
.surv_test_binary_var <- function(data, var, time, event){
  .assert_colname(data, c(var, time, event))
  data[[event]] %<>% as.character() %>% as.integer()
  assert_that(all(data[[event]] %in% c(0,1)), msg = "event should be 0 and 1")
  data[[var]] %<>% base::droplevels()
  assert_that(class(data[[var]]) == "factor",
              nlevels(data[[var]]) == 2,
              msg = "var should be factors with 2 levels")

  fit <- .survfit(data = data, var = var, time = time, event = event)
  res.logrank <- survminer::surv_pvalue( fit = fit, data=data, method = "survdiff") %>%
    as_tibble() %>% dplyr::select(pval.logrank = pval)

  fml <- paste0("Surv(", time,",", event,") ~ ", var) %>% as.formula()
  res.coxph <- survival::coxph(formula = fml, data = data) %>%
    summary() %>% .$coefficients %>% as_tibble() %>%
    dplyr::select(HR= `exp(coef)`, pval.cox = `Pr(>|z|)`)
  dplyr::bind_cols(res.logrank, res.coxph)
}

#' quadrant survival test
#'
#' log-rank test to compare each quadrants
#'
#' @param data dataframe
#' @param time survival time
#' @param event survival event
#' @param marker1 marker1
#' @param marker2 marker2
#' @param na.rm remove NA
.quadrant_survival_test <- function(data, time, event, marker1, marker2, na.rm=T){
  # prep data
  if(na.rm){
    data %<>% drop_na(!!sym(marker1), !!sym(marker2))
  }
  data$.quadrant <- .label_quadrant(data[[marker1]], data[[marker2]])
  pairs <- list("R1_vs_R4" = c("R1", "R4"),
                "R2_vs_R3" = c("R2","R3"),
                "R1_vs_R2" = c("R1","R2"),
                "R3_vs_R4" = c("R3","R4"),
                "R1_vs_R3" = c("R1","R3"),
                "R2_vs_R4" = c("R2","R4"))
  purrr::map_dfr(pairs, .f = ~{
    keep <- data$.quadrant %in% .x
    d <- data[keep, ]
    out <- .surv_test_binary_var(data = d, var = ".quadrant", time = time, event = event)
    out$reference = sort(.x)[1]
    out
  }, .id = "comparison")
}

#' dual marker survival analysis for four quadrant(core)
#'
#' @param data dataframe
#' @param time survival time
#' @param event survival event
#' @param marker1 marker1
#' @param marker2 marker2
#' @return list of 'stats' and 'test'
.dm_survival_4quad_core <- function(data, time, event,
                                        marker1, marker2){
  # check input
  data[[event]] %<>% as.character() %>% as.integer()
  assert_that(all(data[[event]] %in% c(0,1)), msg = "event should be 0 and 1")
  assert_that(class(data[[marker1]]) == "factor" &&
                nlevels(data[[marker1]]) == 2,
              msg = "marker1 should be factors with 2 levels")
  assert_that(class(data[[marker2]]) == "factor" &&
                nlevels(data[[marker2]]) == 2,
              msg = "marker2 should be factors with 2 levels")
  # prep data
  stats <- .quadrant_survival_stats(data = data, time = time, event = event,
                          marker1 = marker1, marker2 = marker2)

  #test <- .quadrant_survival_test(data = data, time = time, event = event,
  #                        marker1 = marker1, marker2 = marker2)
  list(stats = stats)
}

#' dual marker survival analysis of four quadrants
#'
#' @param data dataframe
#' @param time survival time
#' @param event survival event
#' @param marker1 marker1
#' @param marker2 marker2
#' @param m1.datatype data type of marker1
#' @param m1.num.cut cut method/value(s) if marker1 is numeric
#' @param m1.cat.pos positive values for marker1 if it is categorical
#' @param m1.cat.neg negative values for marker1 if it is categorical
#' @param m2.datatype data type of marker2
#' @param m2.num.cut cut method/value(s) if marker2 is numeric
#' @param m2.cat.pos positive values for marker2 if it is categorical
#' @param m2.cat.neg negative values for marker2 if it is categorical
dm_survival_4quad <- function(data, time, event,
                              marker1, marker2,
                              m1.datatype = "auto",
                              m1.num.cut = "median", m1.cat.pos = NULL, m1.cat.neg = NULL,
                              m2.datatype = "auto",
                              m2.num.cut = "median", m2.cat.pos = NULL, m2.cat.neg = NULL){
  # prep .m1
  res <- binarize_data(x = data[[marker1]],
                       datatype = m1.datatype,
                       num.cut = m1.num.cut,
                       cat.pos = m1.cat.pos, cat.neg = m1.cat.neg)
  data$.m1 <- res$data
  cutpoint.m1 <- res$cutpoint
  if(m1.datatype=="auto"){m1.datatype <- datatype_num_cat(data[[marker1]])}
  # prep .m2
  res <- binarize_data(x = data[[marker2]], datatype = m2.datatype,
                       num.cut = m2.num.cut,
                       cat.pos = m2.cat.pos, cat.neg = m2.cat.neg)
  if(m2.datatype=="auto"){m2.datatype <- datatype_num_cat(data[[marker2]])}
  data$.m2 <- res$data
  cutpoint.m2 <- res$cutpoint
  # run 4quadrant analysis
  out <- .dm_survival_4quad_core(data = data, time = time,
                                     event = event, marker1=".m1", marker2=".m2")
  #out$data <- data
  out$param <- tibble(
    time = time, event = event,
    marker1=marker1, marker2 =marker2,
    cutpoint.m1 = cutpoint.m1, m1.cat.pos = toString(m1.cat.pos), m1.cat.neg = toString(m1.cat.neg),
    cutpoint.m2 = cutpoint.m2, m2.cat.pos = toString(m2.cat.pos), m2.cat.neg = toString(m2.cat.neg)
    )
  out
}

