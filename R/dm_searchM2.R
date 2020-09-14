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
#' @param na.rm remove NA, default TRUE
#' @param confound.factor confounding factor
#' @param auc report AUC, default FALSE
#' @export
dm_searchM2_logit <- function(data, response, response.pos, response.neg=NULL,
                        marker1, m2.candidates, confound.factor=NULL,
                        m1.binarize, m2.binarize,
                        m1.num.cut = "median", m1.cat.pos = NULL, m1.cat.neg = NULL,
                        m2.num.cut = "median", m2.cat.pos = NULL, m2.cat.neg = NULL,
                        na.rm=T, auc=F){
  .assert_colname(data, c(response, marker1, confound.factor))
  m2.candidates <- base::intersect(m2.candidates, colnames(data))
  assert_that(length(m2.candidates)>0, msg = "target features don't exist")
  pb <- dplyr::progress_estimated(length(m2.candidates))
  res <- purrr::map(m2.candidates, .f = ~{
    try({
      pb$tick()$print()
      dm_logit(data = data, response = response,
              response.pos = response.pos, response.neg = response.neg,
              marker1 = marker1, marker2 = .x,
              confound.factor= confound.factor,
              m1.binarize = m1.binarize,
              m2.binarize = m2.binarize,
              m1.num.cut = m1.num.cut, m1.cat.pos = m1.cat.pos, m1.cat.neg = m1.cat.neg,
              m2.num.cut = m2.num.cut, m2.cat.pos = m2.cat.pos, m2.cat.neg = m2.cat.neg,
              na.rm=na.rm, auc=auc)
    }, silent = T)
  }) %>% purrr::discard(.p = ~is(.x, "try-error"))
  assert_that(length(res)>0, msg = "fail to searchM2_logit")
  dplyr::bind_rows(res)
}


#' search M2 using Cox model
#'
#' search M2 to combine with M1 using Cox model
#'
#' @param data dataframe
#' @param time survival time
#' @param event survival event
#' @param marker1 marker1
#' @param m2.candidates marker2 candidates
#' @param na.rm remove na, default TRUE
#' @param confound.factor confounding factor
#' @param m1.binarize binarize marker1, default FALSE
#' @param m2.binarize binarize marker2, default FALSE
#' @param m1.num.cut cut method/values for numeric marker1
#' @param m2.num.cut cut method/values for numeric marker2
#' @param m1.cat.pos positive value for categorical marker1
#' @param m1.cat.neg negative value for categorical marker1
#' @param m2.cat.pos positive value for categorical marker2
#' @param m2.cat.neg negative value for categorical marker2
#'
#' @export
dm_searchM2_cox <- function(data, time, event,
                            marker1, m2.candidates,
                            confound.factor = NULL,
                            m1.binarize, m2.binarize,
                            m1.num.cut = "median", m1.cat.pos=NULL, m1.cat.neg=NULL,
                            m2.num.cut = "median", m2.cat.pos=NULL, m2.cat.neg=NULL,
                            na.rm=T){
  .assert_colname(data, c(time, event, marker1, confound.factor))
  # process m2.candidates
  m2.candidates <- base::intersect(m2.candidates, colnames(data))
  assert_that(length(m2.candidates)>0, msg = "features don't exist")
  names(m2.candidates) <- m2.candidates

  pb <- dplyr::progress_estimated(length(m2.candidates))
  res <- purrr::map(m2.candidates, .f = ~{
    try({
      pb$tick()$print()
      dm_cox(data = data,
            time = time,
            event = event,
            marker1 = marker1, marker2 = .x,
            confound.factor = confound.factor,
            m1.binarize = m1.binarize,
            m2.binarize = m2.binarize,
            m1.num.cut = m1.num.cut, m1.cat.pos = m1.cat.pos,m1.cat.neg = m1.cat.neg,
            m2.num.cut = m2.num.cut , m2.cat.pos = m2.cat.pos,m2.cat.neg = m2.cat.neg,
            na.rm=na.rm)
    }, silent = T)
    }) %>% purrr::discard(.p =  ~ is(.x, "try-error"))
  assert_that(length(res)>0, msg = "fail to searchM2_cox")
  dplyr::bind_rows(res)
}



#' plot search M2 result
#'
#' @param res_searchM2 result of search M2
#' @param max_n max M2s to show
#' @param pval_threh pvalue threhold
#' @return an ggplot object
#' @export
dm_searchM2_topPlot <- function(res_searchM2, top.n = 30, pval_threh = 0.01){
  max.label <- 30
  m1.name <- res_searchM2$m1[1]
  d <- res_searchM2 %>%
    mutate(
      m2 = stringr::str_sub(m2, end = max.label) %>% make.names(unique = T),
      sign.m1 = sign(m1_.m1_estimate),
      sign.m2 = sign(m2_.m2_estimate),
      sign.int = sign(`mdi_.m1:.m2_estimate`),
      pval.int = `mdi_.m1:.m2_p.value`,
      sign.pval.int = - log10(pval.int) * sign.m2,
      sign.pval.m1.vs.md = -log10(pval.m1.vs.md) * sign.m2,
      sign.pval.m1.vs.mdi = -log10(pval.m1.vs.mdi) * sign.m2,
      sign.pval.m2.vs.null = -log10(pval.m2.vs.null) * sign.m2,
      sign.pval.m2.vs.md = -log10(pval.m2.vs.md) * sign.m2,
      sign.pval.m2.vs.mdi = -log10(pval.m2.vs.mdi) * sign.m2,
      vs_M1 = -log10(pmin(pval.m1.vs.md, pval.m1.vs.mdi)),
      vs_M2 = -log10(pmin(pval.m2.vs.md, pval.m2.vs.mdi))) %>%
    dplyr::select(m2, sign.m2,sign.int, pval.int,
                  sign.pval.int, sign.pval.m1.vs.md,
                  sign.pval.m1.vs.mdi,
                  sign.pval.m2.vs.null,
                  sign.pval.m2.vs.md,
                  sign.pval.m2.vs.mdi,
                  vs_M1, vs_M2,
                  contains("AIC"))

  # dual marker vs. M1
  g.m2 <- d %>%
    dplyr::top_n(n=top.n, w = abs(pmax(sign.pval.m1.vs.md, sign.pval.m1.vs.mdi))) %>%
    mutate(m2 = reorder(m2, sign.pval.m1.vs.md)) %>%
    dplyr::select(m2,
                  M1_vs_dual = sign.pval.m1.vs.md,
                  M1_vs_dual_int = sign.pval.m1.vs.mdi) %>%
    gather(key = "model_comparison", value= "value", -m2) %>%
    ggplot(aes(x = m2, y = value, color = model_comparison))+
    geom_point()+
    geom_hline(yintercept = c(-log10(c(0.05, 0.01)),log10(c(0.05, 0.01))),
               linetype="dashed", color="skyblue")+
    theme_bw()+
    labs(y = "signed log10-pValue",
         x = "marker2", title = "Marker2's effect in dualmarker model")+
    coord_flip()
  # interaction terms
  g.int <- d %>%
    dplyr::top_n(n=top.n, w = abs(sign.pval.int)) %>%
    mutate(m2 = reorder(m2, sign.pval.int)) %>%
    ggplot(aes(x = m2, y = sign.pval.int))+
    geom_point()+
    geom_hline(yintercept = c(-log10(c(0.05, 0.01)),log10(c(0.05, 0.01))),
               linetype="dashed", color="skyblue")+
    theme_bw()+
    labs(y = "signed log10-pValue",
         x = "marker2", title = "Marker2's interaction with Marker1")+
    coord_flip()

  # # AIC show
  # g3 <-  d %>%
  #   dplyr::top_n(n=top.n,  w = pmin(md_AIC, mdi_AIC)) %>%
  #   mutate(m2 = reorder(m2, md_AIC)) %>%
  #   dplyr::select(m2,
  #                 dual = md_AIC,
  #                 dual_int = mdi_AIC) %>%
  #   gather(key = "model", value= "value", -m2) %>%
  #   ggplot(aes(x = m2, y = value, color = model))+
  #   geom_point()+
  #   theme_bw()+
  #  coord_flip()

  # scatterplot
  g.m1m2 <- d %>%
    ggplot(aes(x=vs_M1, y=vs_M2, text=m2))+
    geom_point() +
    ggrepel::geom_text_repel(aes(label = m2), data = d %>%
                               dplyr::filter(vs_M1 >1.3, vs_M2>1.3) %>%
                               dplyr::top_n(n=top.n,  w = pmax(vs_M1, vs_M2))) +
    geom_hline(yintercept = -log10(0.01), color="skyblue", linetype="dashed")+
    geom_vline(xintercept = -log10(0.01), color="skyblue", linetype = "dashed")+
    labs(x= "log10Pval(dual vs marker1)", y = "log10Pval(dual vs marker2)",
         title = "Dualmarker vs marker1/marker2")
  list(m2_effect = g.m2, interact = g.int, m1_m2_effect = g.m1m2)
}
