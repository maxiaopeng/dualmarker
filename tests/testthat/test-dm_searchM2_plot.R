
test_that("searchM2 cox model", {
  m2.candidates <- stringr::str_subset(colnames(clin_bmk_IMvigor210),"gep_")
  res.m2.cox <- dm_searchM2_cox(
    data = clin_bmk_IMvigor210,
    # survival
    time = "os",
    event = "censOS",
    # marker1
    marker1 = "mut_ARID1A",
    m1.binarize = T,
    m1.cat.pos = "YES",
    m1.cat.neg = "NO",
    # marker2
    m2.candidates = m2.candidates,
    m2.binarize = F,
    p.adjust.method = "BH"
  )

  df.meta <- res.m2.cox %>%
    dplyr::select(
      feature = m2,
      m1_.m1_estimate, m1_.m1_p.value,
      m2_.m2_estimate, m2_.m2_p.value,
      md_.m1_estimate, md_.m1_p.value, md_.m2_estimate, md_.m2_p.value,
      mdi_.m1_estimate, mdi_.m1_p.value, mdi_.m2_estimate, mdi_.m2_p.value, `mdi_.m1:.m2_estimate`,`mdi_.m1:.m2_p.value`) %>%
    tidyr::pivot_longer(cols = -feature, names_to = c("model","element","class"), names_sep = "_", values_to = "value") %>%
    tidyr::unite(data=., study, model,element) %>%
    tidyr::pivot_wider(data = ., names_from = "class", values_from = "value")
  df.meta$assoc.orient <- sign(df.meta$estimate)
  df.meta %>% head(210) %>%
    fun.meta.heatmap(
    df.meta = ., study="study", feature= "feature", pval = "p.value", assoc.orient = "assoc.orient", discretize.pval = T)

  fun.meta.heatmap <- function(df.meta, study, feature,
                               pval, assoc.orient,
                               feature.selected = NULL,
                               discretize.pval=F,
                               display.numbers =F){
    if(discretize.pval){
      d <- df.meta %>%
        mutate(pval2 = base::cut(!!sym(pval), breaks = c(0, 0.001, 0.01, 0.05, 0.1,1),
                                 labels = c("4","3","2","1","0")) %>% as.character %>% as.integer())
    }else{
      d <- df.meta %>%
        mutate(pval2 = -log10(!!sym(pval)))
    }
    m <- d %>%
      mutate(signed.pval = !!sym(assoc.orient) * pval2) %>%
      tidyr::pivot_wider(id_cols = all_of(feature),
                  names_from = all_of(study),
                  values_from = "signed.pval",
                  values_fill = list("signed.pval"=0)) %>%
      as.data.frame() %>%
      column_to_rownames(feature) %>%
      as.matrix()
    if(!is.null(feature.selected)){
      keep <- rownames(m) %in% feature.selected
      assert_that(any(keep), msg = "No features are selected")
      m <- m[keep,, drop=F]
    }
    if(discretize.pval){
      col1 <- colorRampPalette(c("blue", "white"))(6)
      col2 <- colorRampPalette(c("red", "white"))(6) %>% rev
      col <- c(col1[1:5], "#DDDDDD",col2[2:6])

      pheatmap::pheatmap(m, scale="none", display_numbers = display.numbers,
               number_format = "%d",
               breaks = seq(-5, 5, by=1)-0.5, color = col
               #legend_breaks = seq(-4, 4, by=1),
               #legend_labels = c("0.001","0.01","0.05","0.1","1", "0.1","0.05","0.01","0.001")
      )
    }else{
      pheatmap(m, scale="none")
    }
  }

})
