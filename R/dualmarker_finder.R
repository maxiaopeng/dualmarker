##' dm_searchM2_logit
##' search marker2 to combine with marker1
##' @param data
##' @param response
##' @param response_pos
##' @param response_neg
##' @param marker1
##' @param targets
dm_searchM2_logit <- function(data, response, response_pos, response_neg=NULL,
                        marker1, targets,
                        binarization = F,
                        num_cut_method = "none",
                        m1_cat_pos = NULL, m1_cat_neg = NULL,
                        na.rm=T){
  targets <- base::intersect(targets, colnames(data))
  assert_that(length(targets)>0, msg = "target features don't exist")
  names(targets) <- targets
  purrr::map_dfr(targets, .f = ~{
    dm_logit(data = data, response = response,
             response_pos = response_pos, response_neg = response_neg,
             marker1 = marker1, marker2 = .x,
             binarization = binarization,
             num_cut_method = num_cut_method,
             m1_cat_pos = m1_cat_pos, m1_cat_neg = m1_cat_neg,
             na.rm=na.rm)
  })
}


dm_searchM2_logit_plot <- function(res.searchM2, max.n = 50, pval.threh = 0.01){
  max.label <- 30
  m1.name <- res.searchM2$m1[1]
  # filter M2
  col.pval <- c('pval.vsM1_md','pval.vsM1_md.int','pval.vsMd_md.int')
  pval.min <- do.call(base::pmin, c(as.list(res.searchM2[,col.pval]), na.rm=T))
  keep <- pval.min <= min(pval.threh, sort(pval.min)[max.n])
  res.searchM2.sig <- res.searchM2[keep,]

  d <- res.searchM2.sig %>%
    mutate(sign_m2 = sign(estimate.m2_m2)) %>%
    dplyr::select(m2, sign_m2,
                  M2_vs_Null = pval.vsNull_m2,
                  Md_vs_M1 = pval.vsM1_md,
                  Md.int_vs_M1 = pval.vsM1_md.int,
                  Md_vs_M2 = pval.vsM2_md)

  .f <- function(pval, sign){
    - sign * log10(pval) }
  dd <- mutate_at(.tbl = d, .vars = vars(contains("_vs_")),
                  .funs = .f, sign = d$sign_m2) %>%
    mutate(m2 = str_sub(m2, end=max.label) %>% make.names(unique=T)) %>%
    mutate(m2 = reorder(m2, Md_vs_M1))

  dd %>%
    tidyr::gather(key = "comparison", value="pval", contains("_vs_")) %>%
    ggplot(aes(y = pval, x = m2, color = comparison))+
    geom_point()+
    geom_hline(yintercept = c(-log10(c(0.05, 0.01)),log10(c(0.05, 0.01))),
               linetype="dashed", color="grey")+
    theme_bw()+
    labs(y = "worse response <- signed log10-pValue -> better response", x = "marker2",
         title = paste0("Significant marker2 to combine with ", m1.name))+
    coord_flip()
}

##' @description
##' find dual marker by four-quadrant analysis
##' @export
dm_searchM2_4quadrant <- function(data, response, response_pos, response_neg=NULL,
                                  marker1, targets,
                                  num_cut_method = "none",
                                  m1_cat_pos = NULL, m1_cat_neg = NULL
                                  ){
  targets <- base::intersect(targets, colnames(data))
  assert_that(length(targets)>0, msg = "target features don't exist")
  names(targets) <- targets
  purrr::map_dfr(targets, .f = ~{
    res.quad <- dm_4quadrant(data = data, response = response,
                 response_pos = response_pos, response_neg = response_neg,
                 marker1 = marker1,
                 marker2 = .x,
                 num_cut_method = num_cut_method,
                 m1_cat_pos = m1_cat_pos, m1_cat_neg = m1_cat_neg,
                 na.rm = T)
    out.basic <- tibble(m1 = marker1, m2 = .x, num_cut_method=num_cut_method)
    out.4quad.count <- res.quad$stats %>% dplyr::select(region, n.total, n.pos, pct.pos) %>%
      tidyr::gather(key = "key", value ="value", -region) %>%
      unite(., col = ".id", region, key) %>%
      tidyr::spread(key = ".id", value="value")
    out.4quad.test <- res.quad$test %>% dplyr::select(comparison, p.value) %>%
      mutate(comparison = paste0("pval.", comparison)) %>%
      tidyr::spread(key = "comparison", value="p.value")
    estimate.m2 <- res.quad$test %>%
      dplyr::filter(comparison %in% "R12_vs_R34") %>%
      pull(estimate) %>% .[1]
    out.4quad.test$sign.m2 <- ifelse(estimate.m2>1, 1, -1)
    bind_cols(out.basic, out.4quad.count, out.4quad.test)
  })
}


dm_searchM2_4quadrant_plot <- function(res.searchM2, max.n = 30, pval.threh = 0.01){
  max.label <- 30
  m1.name <- res.searchM2$m1[1]


  # filter M2
  col.pval <- c('pval.R1_vs_R4','pval.R2_vs_R3')
  pval.min <- do.call(base::pmin, c(as.list(res.searchM2[,col.pval]), na.rm=T))
  keep <- pval.min <= min(pval.threh, sort(pval.min)[max.n])
  res.searchM2.sig <- res.searchM2[keep,]

  d <- res.searchM2.sig %>%
    dplyr::select(m2,
                  sign.m2,
                  R12_vs_R34 = pval.R12_vs_R34,
                  R1_vs_R4 = pval.R1_vs_R4,
                  R2_vs_R3 = pval.R2_vs_R3)

  .f <- function(pval, sign){ - sign * log10(pval) }
  dd <- mutate_at(.tbl = d, .vars = vars(contains("_vs_")),
                  .funs = .f, sign = d$sign.m2) %>%
    mutate(m2 = str_sub(m2, end=max.label) %>% make.names(unique=T)) %>%
    mutate(m2 = reorder(m2, R12_vs_R34))

  dd %>%
    tidyr::gather(key = "comparison", value="pval", contains("_vs_")) %>%
    ggplot(aes(y = pval, x = m2, color = comparison))+
    geom_point()+
    geom_hline(yintercept = c(-log10(c(0.05, 0.01)),log10(c(0.05, 0.01))),
               linetype="dashed", color="grey")+
    theme_bw()+
    labs(y = "negative correlation <- signed log10-pValue -> positive correlation", x = "marker2",
         title = paste0("Significant marker2 to combine with ", m1.name))+
    coord_flip()

}

##' find marker2 to combine with M1 for survival prediction
##' @param data
##' @param time
##' @param event
##' @param marker1
##' @export
dm_searchM2_cox <- function(data, time, event,
                            marker1, targets, binarization=F,
                            num_cut_method = "none",
                            m1_cat_pos=NULL, m1_cat_neg=NULL,
                            na.rm=T){
  targets <- base::intersect(targets, colnames(data))
  assert_that(length(targets)>0, msg = "features don't exist")
  names(targets) <- targets
  purrr::map_dfr(targets, .f = ~{
    dm_cox(data = data,
           time = time,
           event = event,
           marker1 = marker1, marker2 = .x,
           binarization = binarization,
           num_cut_method = num_cut_method,
           m1_cat_pos = m1_cat_pos, m1_cat_neg = m1_cat_neg,
           na.rm=na.rm)
  })
}

##' plot of most significant marker2
##'
dm_searchM2_cox_plot <- function(res.searchM2, padj=F, max.n = 30, pval.threh = 0.01){
  max.label <- 30
  m1.name <- res.searchM2$m1[1]
  # filter Marker2
  col.pval <- c('pval.vsM1_md','pval.vsM1_md.int','pval.vsMd_md.int')
  pval.min <- do.call(base::pmin, c(as.list(res.searchM2[,col.pval]), na.rm=T))
  keep <- pval.min <= min(pval.threh, sort(pval.min)[max.n])
  res.searchM2.sig <- res.searchM2[keep,]

  d <- res.searchM2.sig %>%
    mutate(sign_m2 = sign(1 - HR_m2)) %>%
    dplyr::select(m2, sign_m2,
                  M1_vs_dual = pval.vsM1_md,
                  M1_vs_dual.int = pval.vsM1_md.int,
                  M2_vs_dual = pval.vsM2_md,
                  M2_vs_dual.int = pval.vsM2_md.int)

  .f <- function(pval, sign){ - sign * log10(pval) }
  dd <- mutate_at(.tbl = d, .vars = vars(contains("_vs_")),
                  .funs = .f, sign = d$sign_m2) %>%
    mutate(m2 = str_sub(m2, end= max.label) %>% make.names(unique=T)) %>%
    mutate(m2 = reorder(m2, M1_vs_dual))

  dd %>%
    tidyr::gather(key = "comparison", value="pval", contains("_vs_")) %>%
    ggplot(aes(y = pval, x = m2, color = comparison))+
    geom_point()+
    geom_hline(yintercept = c(-log10(c(0.05, 0.01)),log10(c(0.05, 0.01))),
               linetype="dashed", color="grey")+
    theme_bw()+
    labs(y = "inferior survival <- signed log10-pValue -> superior survival",
         x = "marker2",
         title = paste0("Significant marker2 to combine with ", m1.name))+
    coord_flip()
}

