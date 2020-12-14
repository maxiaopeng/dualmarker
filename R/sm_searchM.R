#' Search single marker with logit model
#'
#' @param data data frame
#' @param response response variable
#' @param response.pos positive values for response
#' @param response.neg negative values for response
#' @param candidates candidate markers
#' @param covariates confounding factors
#' @param binarize to binarize marker, default FALSE
#' @param num.cut cut method/values for numeric variable
#' @param cat.pos positive values for marker if marker is categorical
#' @param cat.neg negative values for marker if marker is categorical
#' @param auc statistic AUC, default FALSE
#'
#' @return dataframe
sm_searchM_logit <- function(data, response, response.pos, response.neg=NULL,
                            candidates, covariates=NULL,
                            binarize = F,
                            num.cut = "none", cat.pos = NULL, cat.neg = NULL,
                            auc=F){
  .assert_colname(data, c(response))
  candidates <- base::intersect(candidates, colnames(data))
  assert_that(length(candidates)>0, msg = "target features don't exist")
  pb <- dplyr::progress_estimated(length(candidates))
  res <- purrr::map(candidates, .f = ~{
    try({
      pb$tick()$print()
      sm_logit(data = data,
              response = response,
              response.pos = response.pos,
              response.neg = response.neg,
              marker = .x,
              covariates= covariates,
              binarize = binarize,
              num.cut = num.cut, cat.pos = cat.pos, cat.neg = cat.neg,
               auc=auc)
    }, silent = T)
  }) %>% purrr::discard(.p = ~is(.x, "try-error"))
  assert_that(length(res)>0, msg = "fail to search_logit")
  dplyr::bind_rows(res)
}


#' search marker using Cox model
#'
#' @param data data frame
#' @param time survival time
#' @param event survival event
#' @param num.cut cut method/value for numeric variable
#' @param candidates candidates markers
#' @param binarize to binarize the data
#' @param cat.pos positive value(s)
#' @param cat.neg negative value(s)
#' @param covariates confounding factors
sm_searchM_cox <- function(data, time, event,
                            candidates,
                            covariates = NULL,
                            binarize=F,
                            num.cut = "none",
                            cat.pos=NULL, cat.neg=NULL){
  .assert_colname(data, c(time, event, covariates))
  candidates <- base::intersect(candidates, colnames(data))
  assert_that(length(candidates)>0, msg = "features don't exist")
  pb <- dplyr::progress_estimated(length(candidates))
  res <- purrr::map(candidates, .f = ~{
    try({
      pb$tick()$print()
      sm_cox(data = data,
             time = time,
             event = event,
             marker = .x,
             covariates = covariates,
             binarize = binarize,
             num.cut = num.cut, cat.pos = cat.pos, cat.neg = cat.neg)
    }, silent = T)
  }) %>% purrr::discard(.p =  ~ is(.x, "try-error"))
  assert_that(length(res)>0, msg = "fail to sm_searchM_cox")
  dplyr::bind_rows(res)
}

#' plot sm_searchM
#'
#' @param res.searchM result of sm_searchM
#' @param top.n label top-n
sm_searchM_topPlot <- function(res.searchM, top.n=20){
  # plot
  res.searchM %>%
    ggplot(aes(x = .m_estimate,y = -log10(.m_p.value)))+
    geom_point(na.rm=T) +
    ggrepel::geom_text_repel(
      aes(label = marker),
      data = res.searchM %>% dplyr::top_n(n=top.n, w = - .m_p.value)
      )+
    geom_hline(yintercept = -log10(c(0.01, 0.05)),
               color = "skyblue", linetype="dashed") +
    labs(x= "estimate",
         y = "log10Pval")
}
