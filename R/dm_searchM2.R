#' search marker2 using logistic regression
#'
#' search marker2 to combine with marker1
#'
#' @param data data frame
#' @param response response variable
#' @param response.pos positive value(s) for response variable
#' @param response.neg negative value(s) for response variable
#' @param marker1 marker1
#' @param m2.candidates candidates marker2
#' @param m1.binarize binarize marker1, default FALSE
#' @param m2.binarize binarize marker2, default FALSE
#' @param m1.num.cut cut method/values for numeric marker1
#'   if m1.binarize is TRUE and marker1 is numeric
#' @param m2.num.cut cut method/values for numeric marker2
#'   if m1.binarize is TRUE and marker2 is numeric
#' @param m1.cat.pos positive value for categorical marker1
#'   if m1.binarize is TRUE and marker1 is categorical
#' @param m1.cat.neg negative value for categorical marker1
#'   if m1.binarize is TRUE and marker1 is categorical
#' @param m2.cat.pos positive value for categorical marker2
#'   if m1.binarize is TRUE and marker2 is categorical
#' @param m2.cat.neg negative value for categorical marker2
#'   if m1.binarize is TRUE and marker2 is categorical
#' @param covariates confounding factor
#' @param auc report AUC, default FALSE
#' @param p.adjust.method see also p.adjust.methods
#' @seealso \code{\link[stats]{p.adjust}}
#' @export
dm_searchM2_logit <- function(data,
                              response,
                              response.pos,
                              response.neg=NULL,
                              marker1,
                              m2.candidates,
                              covariates = NULL,
                              m1.binarize = F,
                              m2.binarize = F,
                              m1.num.cut = "median",
                              m1.cat.pos = NULL,
                              m1.cat.neg = NULL,
                              m2.num.cut = "median",
                              m2.cat.pos = NULL,
                              m2.cat.neg = NULL,
                              auc = T,
                              p.adjust.method = "BH")
{
  .assert_colname(data, c(response, marker1, covariates))
  m2.candidates <- base::intersect(m2.candidates, colnames(data))
  assert_that(length(m2.candidates)>0, msg = "target features don't exist")
  pb <- dplyr::progress_estimated(length(m2.candidates))
  res <- purrr::map(m2.candidates, .f = ~{
    try({
      pb$tick()$print()
      dm_logit(data = data, response = response,
              response.pos = response.pos,
              response.neg = response.neg,
              marker1 = marker1,
              marker2 = .x,
              covariates= covariates,
              m1.binarize = m1.binarize,
              m2.binarize = m2.binarize,
              m1.num.cut = m1.num.cut,
              m1.cat.pos = m1.cat.pos,
              m1.cat.neg = m1.cat.neg,
              m2.num.cut = m2.num.cut,
              m2.cat.pos = m2.cat.pos,
              m2.cat.neg = m2.cat.neg,
              auc=auc)
    }, silent = T)
  }) %>% purrr::discard(.p = ~is(.x, "try-error"))
  assert_that(length(res)>0, msg = "fail to searchM2_logit")
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
#' @param event survival event
#' @param marker1 marker1
#' @param m2.candidates marker2 candidates
#' @param covariates confounding factor
#' @param m1.binarize binarize marker1, default FALSE
#' @param m2.binarize binarize marker2, default FALSE
#' @param m1.num.cut cut method/values for numeric marker1
#' @param m2.num.cut cut method/values for numeric marker2
#' @param m1.cat.pos positive value for categorical marker1
#' @param m1.cat.neg negative value for categorical marker1
#' @param m2.cat.pos positive value for categorical marker2
#' @param m2.cat.neg negative value for categorical marker2
#' @param p.adjust.method see also p.adjust.methods
#' @export
dm_searchM2_cox <- function(data,
                            time,
                            event,
                            marker1,
                            m2.candidates,
                            covariates = NULL,
                            m1.binarize = F,
                            m2.binarize = F,
                            m1.num.cut = "median",
                            m1.cat.pos = NULL,
                            m1.cat.neg = NULL,
                            m2.num.cut = "median",
                            m2.cat.pos = NULL,
                            m2.cat.neg = NULL,
                            p.adjust.method = "BH"){
  .assert_colname(data, c(time, event, marker1, covariates))
  # process m2.candidates
  assert_that(all(m2.candidates %in% colnames(data)),
              msg = "features don't exist")
  names(m2.candidates) <- m2.candidates

  pb <- dplyr::progress_estimated(length(m2.candidates))
  res <- purrr::map(m2.candidates, .f = ~{
    try({
      pb$tick()$print()
      dm_cox(data = data,
            time = time,
            event = event,
            marker1 = marker1,
            marker2 = .x,
            covariates = covariates,
            m1.binarize = m1.binarize,
            m2.binarize = m2.binarize,
            m1.num.cut = m1.num.cut,
            m1.cat.pos = m1.cat.pos,
            m1.cat.neg = m1.cat.neg,
            m2.num.cut = m2.num.cut ,
            m2.cat.pos = m2.cat.pos,
            m2.cat.neg = m2.cat.neg
            )
    }, silent = T)
    }) %>% purrr::discard(.p =  ~ is(.x, "try-error"))
  assert_that(length(res)>0, msg = "fail to searchM2_cox")
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



stat_pval <- function(data, p.cols, pval.cut = c(0.01, 0.05, 0.1, 0.2)){
  .assert_colname(data, p.cols)
  data[p.cols] %>%
    tidyr::gather(key = "pval.name", value = "pval") %>%
    group_by(pval.name) %>%
    group_modify(.f = ~{
      n = sapply(pval.cut, function(a.cut) {sum(.x$pval < a.cut, na.rm=T)})
      data.frame(pval.cut = pval.cut, n = n)
    }) %>% ungroup()
}
plot_pval_stat <- function(data, p.cols, pval.cut = c(0.01, 0.05, 0.1, 0.2))
{
  .assert_colname(data, p.cols)
  stats <- stat_pval(data = data, p.cols = p.cols, pval.cut = pval.cut)
  stats$pval.name %<>% factor(., levels = p.cols)
  ggpubr::ggbarplot(data = stats, x = "pval.cut", y = "n",
                    fill = "pval.name", label = T,
                    position = position_dodge(width=0.7))
}

#' plot search M2 result
#'
#' @param res.searchM2 result of search M2
#' @param top.n top-N
#' @return an ggplot object
#' @export
dm_searchM2_topPlot <- function(res.searchM2,
                                top.n = 30,
                                show.padj =F,
                                line.sig = c(0.01, 0.05),
                                palette = "default"){
  label.max.len <- 30
#  m1.name <- res.searchM2$m1[1]
  is.cox <- "DM_CPE" %in% colnames(res.searchM2)

  if(show.padj){
    pval.int <- "DMI_m1:m2_p.adj"
    pval.m1.vs.md <- "padj_SM1_vs_DM"
    pval.m1.vs.mdi <- "padj_SM1_vs_DMI"
    pval.m2.vs.null <- "padj_SM2_vs_NULL"
    pval.m2.vs.md <- "padj_SM2_vs_DM"
    pval.m2.vs.mdi <- "padj_SM2_vs_DMI"
  }else{
    pval.int <- "DMI_m1:m2_p.value"
    pval.m1.vs.md <- "pval_SM1_vs_DM"
    pval.m1.vs.mdi <- "pval_SM1_vs_DMI"
    pval.m2.vs.null <- "pval_SM2_vs_NULL"
    pval.m2.vs.md <- "pval_SM2_vs_DM"
    pval.m2.vs.mdi <- "pval_SM2_vs_DMI"
  }

  d <- res.searchM2 %>%
    mutate(
      m2 = stringr::str_sub(m2, end = label.max.len) %>% make.names(unique = T),
      sign.m1 = sign(SM1_m1_estimate),
      sign.m2 = sign(SM2_m2_estimate),
      sign.int = sign(`DMI_m1:m2_estimate`),
      sign.pval.int = -log10(!!sym(pval.int)) * sign.m2,
      sign.pval.m1.vs.md = -log10(!!sym(pval.m1.vs.md)) * sign.m2,
      sign.pval.m1.vs.mdi = -log10(!!sym(pval.m1.vs.mdi)) * sign.m2,
      sign.pval.m2.vs.null = -log10(!!sym(pval.m2.vs.null)) * sign.m2,
      sign.pval.m2.vs.md = -log10(!!sym(pval.m2.vs.md)) * sign.m2,
      sign.pval.m2.vs.mdi = -log10(!!sym(pval.m2.vs.mdi)) * sign.m2,
      vs_M1 = -log10(pmin(!!sym(pval.m1.vs.md), !!sym(pval.m1.vs.mdi))),
      vs_M2 = -log10(pmin(!!sym(pval.m2.vs.md), !!sym(pval.m2.vs.mdi)))) %>%
    dplyr::select(m2, sign.m2,sign.int, !!sym(pval.int),
                  sign.pval.int, sign.pval.m1.vs.md,
                  sign.pval.m1.vs.mdi,
                  sign.pval.m2.vs.null,
                  sign.pval.m2.vs.md,
                  sign.pval.m2.vs.mdi,
                  vs_M1, vs_M2,
                  contains("AIC"),
                  contains("CPE"),
                  contains("auc"))

  # dual marker vs. M1
  p.m2 <- d %>%
    dplyr::top_n(n=top.n, w = abs(pmax(sign.pval.m1.vs.md, sign.pval.m1.vs.mdi))) %>%
    mutate(m2 = reorder(m2, sign.pval.m1.vs.md)) %>%
    dplyr::select(m2,
                  M1_vs_dual = sign.pval.m1.vs.md,
                  M1_vs_dual_int = sign.pval.m1.vs.mdi) %>%
    gather(key = "model_comparison", value= "value", -m2) %>%
    ggplot(aes(x = m2, y = value, color = model_comparison))+
    geom_segment(aes(xend = m2, yend = 0), color = "grey")+
    geom_point(na.rm=T)+
    geom_hline(yintercept = c(-log10(line.sig),log10(line.sig)),
               linetype="dashed", color="skyblue")+
    geom_hline(yintercept = 0, size=1, color ="grey")+
    theme_minimal()+
    labs(y = "signed log10-pValue",
         x = "marker2", title = "Marker2's effect in dualmarker model")+
    coord_flip()
  p.m2 <- ggpubr::ggpar(p.m2, palette = palette)

  # interaction terms
  p.int <- d %>%
    dplyr::top_n(n=top.n, w = abs(sign.pval.int)) %>%
    mutate(m2 = reorder(m2, sign.pval.int)) %>%
    ggplot(aes(x = m2, y = sign.pval.int))+
    geom_segment(aes(xend = m2, yend = 0), color = "grey")+
    geom_point(na.rm=T)+
    geom_hline(yintercept = c(-log10(line.sig),log10(line.sig)),
               linetype="dashed", color="skyblue")+
    geom_hline(yintercept = 0, size=1, color ="grey")+
    theme_bw()+
    labs(y = "signed log10-pValue",
         x = "marker2", title = "Interaction between two markers")+
    coord_flip()
  p.int <- ggpubr::ggpar(p.int, palette = palette)

  # AIC show
  # p.aic <-  d %>%
  #   dplyr::top_n(n=top.n,  w = pmin(DM_AIC, DMI_AIC)) %>%
  #   mutate(m2 = reorder(m2, DM_AIC)) %>%
  #   dplyr::select(m2,
  #                 dual = DM_AIC,
  #                 dual_int = DMI_AIC) %>%
  #   gather(key = "model", value= "value", -m2) %>%
  #   ggplot(aes(x = m2, y = value, color = model))+
  #   geom_point()+
  #   theme_bw()+
  #  coord_flip()

  # CPE
  if(is.cox){
    p.cpe <-  d %>%
      dplyr::top_n(n=top.n,  w = pmax(DM_CPE, DMI_CPE)) %>%
      mutate(m2 = reorder(m2, DM_CPE)) %>%
      dplyr::select(m2,
                    dual = DM_CPE,
                    dual_int = DMI_CPE) %>%
      gather(key = "model", value= "value", -m2) %>%
      ggplot(aes(x = m2, y = value, color = model))+
      geom_point()+
      theme_bw()+
      coord_flip()
    p.cpe <- ggpubr::ggpar(p.cpe, palette = palette)
  }else{
    p.auc <-  d %>%
      dplyr::top_n(n=top.n,  w = pmax(DM_auc, DMI_auc)) %>%
      mutate(m2 = reorder(m2, DM_auc)) %>%
      dplyr::select(m2,
                    dual = DM_auc,
                    dual_int = DMI_auc) %>%
      gather(key = "model", value= "value", -m2) %>%
      ggplot(aes(x = m2, y = value, color = model))+
      geom_point()+
      theme_bw()+
      coord_flip()
    p.auc <- ggpubr::ggpar(p.auc, palette = palette)
  }

  # scatterplot
  p.m1m2 <- d %>%
    ggplot(aes(x=vs_M1, y=vs_M2, text=m2))+
    geom_point(na.rm=T) +
    ggrepel::geom_text_repel(
      aes(label = m2),
      data = d %>% dplyr::filter(vs_M1 > 1.3 | vs_M2> 1.3) %>%
        dplyr::top_n(n=top.n,  w = pmax(vs_M1, vs_M2))) +
    geom_hline(yintercept = -log10(line.sig), color="skyblue", linetype="dashed")+
    geom_vline(xintercept = -log10(line.sig), color="skyblue", linetype = "dashed")+
    labs(x= "-log10Pval(dual vs marker1)", y = "-log10Pval(dual vs marker2)",
         title = "Dualmarker vs marker1/marker2")+
    theme_bw()
  #p.m1m2 <- ggpubr::ggpar(p.m1m2, palette = palette)

  out <- list(m2_effect = p.m2, interact = p.int, m1_m2_effect = p.m1m2)
  if(is.cox)  out$CPE <- p.cpe else out$auc = p.auc
  out
}


