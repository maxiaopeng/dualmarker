if(F){
  library(readr)
  test_that("searchM2 works", {
    marker1 <- "TMB"
    m2.candidates <- clin_bmk_IMvigor210 %>%
      dplyr::select(contains("gepscore")) %>%
      colnames()
    res.searchM2.logit <- dm_searchM2_logit(data = clin_bmk_IMvigor210,
                                             response = "binaryResponse",
                                             response.pos="CR/PR",
                                             response.neg="SD/PD",
                                             marker1 = marker1,
                                             m2.candidates = m2.candidates,
                                             m1.binarize = F, m2.binarize = F,
                                             na.rm=T)
    dm_searchM2_topPlot(res.searchM2.logit)
  })


  test_that("searchM2 cox model", {
    marker1 <- "TMB"
    m2.candidates <- clin_bmk_IMvigor210 %>%
      dplyr::select(contains("gepscore")) %>%
      colnames()
    res.searchM2.cox <- dm_searchM2_cox(
        data=clin_bmk_IMvigor210,
        time = "os",
        event = "censOS",
        marker1 = marker1,
        m2.candidates = m2.candidates,
        m1.binarize = F, m2.binarize = F)
    dm_searchM2_topPlot(res.searchM2.cox)
  })

  test_that("searchM2 cox model", {
    file.clin.info <- system.file("extdata","TCGA_melanoma_meta_info.csv", package = "dualmarker")
    file.gepscore <- system.file("extdata","TCGA_melanoma_GEPscore.csv", package = "dualmarker")
    file.gep <- system.file("extdata","TCGA_melanoma_GEP.csv", package = "dualmarker")

    clin <- readr::read_csv(file.clin.info)
    gep <- readr::read_csv(file.gep) %>%
      as.data.frame() %>% column_to_rownames(".id") %>% t %>% as.data.frame %>% rownames_to_column(".id")
    colnames(gep)[-1] %<>% paste0("gep.", .)
    gepscore <- readr::read_csv(file.gepscore) %>%
      as.data.frame() %>% column_to_rownames(".id") %>% t %>% as.data.frame %>% rownames_to_column(".id")
    colnames(gepscore)[-1] %<>% paste0("gepscore.", .)
    clin.bmk <- plyr::join_all(list(clin, gep, gepscore), by = ".id")

    m2.candidates <- colnames(gep)[-1]
    res.searchM2.cox <- dm_searchM2_cox(data = clin.bmk %>% dplyr::filter(OS %in% c(0,1)),
                           time = "OS.time", event="OS",
                           marker1 = "gepscore.CTL",
                           m2.candidates = m2.candidates, m1.binarize = F, m2.binarize = T,
                           num.cut = "mean+sd")
    dm_searchM2_topPlot(res.searchM2.cox)
    dm_pair(data = clin.bmk %>% filter(OS %in% c(0,1)),
            marker1 = "gepscore.CTL", marker2 = "gep.AXL", time = "OS.time", event = "OS")
  })
  #
  test_that("searchM2 cox model", {
    file.clin.info <- system.file("extdata","TCGA_melanoma_meta_info.csv", package = "dualmarker")
    file.gepscore <- system.file("extdata","TCGA_melanoma_GEPscore.csv", package = "dualmarker")
    file.gep <- system.file("extdata","TCGA_melanoma_GEP.csv", package = "dualmarker")

    clin <- readr::read_csv(file.clin.info)
    gep <- readr::read_csv(file.gep) %>%
      as.data.frame() %>% column_to_rownames(".id") %>% t %>% as.data.frame %>% rownames_to_column(".id")
    colnames(gep)[-1] %<>% paste0("gep.", .)
    gepscore <- readr::read_csv(file.gepscore) %>%
      as.data.frame() %>% column_to_rownames(".id") %>% t %>% as.data.frame %>% rownames_to_column(".id")
    colnames(gepscore)[-1] %<>% paste0("gepscore.", .)
    clin.bmk <- plyr::join_all(list(clin, gep, gepscore), by = ".id")

    m2.candidates <- colnames(gep)[-1]
    res.searchM2.cox <- dm_searchM2_cox(data = clin.bmk %>% dplyr::filter(OS %in% c(0,1)),
                                        time = "OS.time", event="OS",
                                        marker1 = "gepscore.CTL",
                                        m2.candidates = m2.candidates, m1.binarize = F, m2.binarize = T,
                                        num.cut = "mean+sd")
    dm_searchM2_topPlot(res.searchM2.cox)
    dm_pair(data = clin.bmk %>% filter(OS %in% c(0,1)),
            marker1 = "gepscore.CTL", marker2 = "gep.AXL", time = "OS.time", event = "OS")
  })

}
