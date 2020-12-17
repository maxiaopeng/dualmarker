#' search marker2 using logistic regression
#'
#' search marker2 to combine with marker1
#'
#' @param data dataframe
#' @param response response variable
#' @param response.pos positive value(s) for response variable
#' @param response.neg negative value(s) for response variable
#' @param marker1 marker1
#' @param m2.candidates candidates marker2
#' @param m1.binarize binarize marker1, default FALSE
#' @param m2.binarize binarize marker2, default FALSE
#' @param m1.num.cut cut method/values for numeric marker1
#' @param m2.num.cut cut method/values for numeric marker2
#' @param m1.cat.pos positive value for categorical marker1
#' @param m1.cat.neg negative value for categorical marker1
#' @param m2.cat.pos positive value for categorical marker2
#' @param m2.cat.neg negative value for categorical marker2
#' @param covariates confounding factor
#' @param auc report AUC, default FALSE
#' @export
dm_combM_logit <- function(data, response, response.pos, response.neg=NULL,
                        candidates, covariates=NULL,
                        m.binarize,
                        m.num.cut = "median", m.cat.pos = NULL, m.cat.neg = NULL,
                        auc=T, p.adjust.method = "BH"){
  .assert_colname(data, c(response, candidates, covariates))

  candidates.combn <- combn(x = candidates, m = 2, simplify = F)
  pb <- dplyr::progress_estimated(length(candidates.combn))
  res <- purrr::map(candidates.combn, .f = ~{
    try({
      pb$tick()$print()
      dm_logit(
        data = data,
        response = response, response.pos = response.pos, response.neg = response.neg,
        marker1 = .x[1], marker2 = .x[2],
        covariates= covariates,
        m1.binarize = m.binarize,
        m2.binarize = m.binarize,
        m1.num.cut = m.num.cut, m1.cat.pos = m.cat.pos, m1.cat.neg = m.cat.neg,
        m2.num.cut = m.num.cut, m2.cat.pos = m.cat.pos, m2.cat.neg = m.cat.neg,
        auc=auc)
    }, silent = T)
  }) %>% purrr::discard(.p = ~is(.x, "try-error"))

  assert_that(length(res)>0, msg = "fail to dm_combineM_logit")
  res <- dplyr::bind_rows(res)
  res %>%
    adjust_pvalue(p.col = "SM1_m1_p.value", output.col = "SM1_m1_p.adj",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "SM2_m2_p.value", output.col = "SM2_m2_p.adj",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "DM_m1_p.value", output.col = "DM_m1_p.adj",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "DM_m2_p.value", output.col = "DM_m2_p.adj",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "DMI_m1_p.value", output.col = "DMI_m1_p.adj",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "DMI_m2_p.value", output.col = "DMI_m2_p.adj",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "DMI_m1:m2_p.value", output.col = "DMI_m1:m2_p.adj",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "pval_SM1_vs_NULL", output.col = "padj_SM1_vs_NULL",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "pval_SM2_vs_NULL", output.col = "padj_SM2_vs_NULL",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "pval_SM1_vs_DM", output.col = "padj_SM1_vs_DM",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "pval_SM2_vs_DM", output.col = "padj_SM2_vs_DM",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "pval_SM1_vs_DMI", output.col = "padj_SM1_vs_DMI",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "pval_SM2_vs_DMI", output.col = "padj_SM2_vs_DMI",
                  method = p.adjust.method)
}


#' search M2 using Cox model
#'
#' search M2 to combine with M1 using Cox model
#'
#' @param data dataframe
#' @param time survival time
#' @param candidates marker candidates
#' @param covariates confounding factor
#' @param m.binarize binarize marker1, default FALSE
#' @param m.num.cut cut method/values for numeric marker1
#' @param m.cat.pos positive value for categorical marker1
#' @param m.cat.neg negative value for categorical marker2
#'
#' @export
dm_combM_cox <- function(data, time, event,
                              candidates, covariates=NULL,
                              m.binarize,
                              m.num.cut = "median", m.cat.pos = NULL, m.cat.neg = NULL,
                              p.adjust.method = "BH"){
  .assert_colname(data, c(time, event, candidates, covariates))
  candidates.combn <- combn(x = candidates, m = 2, simplify = F)
  pb <- dplyr::progress_estimated(length(candidates.combn))
  res <- purrr::map(candidates.combn, .f = ~{
    try({
      pb$tick()$print()
      dm_cox(data = data,
             time = time, event = event,
               marker1 = .x[1], marker2 = .x[2],
               covariates= covariates,
               m1.binarize = m.binarize,
               m2.binarize = m.binarize,
               m1.num.cut = m.num.cut, m1.cat.pos = m.cat.pos, m1.cat.neg = m.cat.neg,
               m2.num.cut = m.num.cut, m2.cat.pos = m.cat.pos, m2.cat.neg = m.cat.neg)
    }, silent = T)
  }) %>% purrr::discard(.p = ~is(.x, "try-error"))
  assert_that(length(res)>0, msg = "fail to dm_combineM_logit")
  res <- dplyr::bind_rows(res)
  res %>%
    adjust_pvalue(p.col = "SM1_m1_p.value", output.col = "SM1_m1_p.adj",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "SM2_m2_p.value", output.col = "SM2_m2_p.adj",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "DM_m1_p.value", output.col = "DM_m1_p.adj",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "DM_m2_p.value", output.col = "DM_m2_p.adj",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "DMI_m1_p.value", output.col = "DMI_m1_p.adj",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "DMI_m2_p.value", output.col = "DMI_m2_p.adj",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "DMI_m1:m2_p.value", output.col = "DMI_m1:m2_p.adj",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "pval_SM1_vs_NULL", output.col = "padj_SM1_vs_NULL",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "pval_SM2_vs_NULL", output.col = "padj_SM2_vs_NULL",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "pval_SM1_vs_DM", output.col = "padj_SM1_vs_DM",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "pval_SM2_vs_DM", output.col = "padj_SM2_vs_DM",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "pval_SM1_vs_DMI", output.col = "padj_SM1_vs_DMI",
                  method = p.adjust.method) %>%
    adjust_pvalue(p.col = "pval_SM2_vs_DMI", output.col = "padj_SM2_vs_DMI",
                  method = p.adjust.method)
}

