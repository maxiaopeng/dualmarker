##########################
# mut_ARID1A + gepscore
##########################
source("R/load_all_functions.R")

# dm_search_1
marker1 <- "mut_ARID1A"
targets <- clin.bmk %>% dplyr::select(contains("gep")) %>%
  colnames() %>% setdiff(., "gep_sample_id")

out <- dm_searchM2_logit(data = clin.bmk,
                         outcome = "binaryResponse",
                         outcome.pos="CR/PR",
                         outcome.neg="SD/PD",
                         marker1 = marker1,
                         m1.cat.pos = "YES", m1.cat.neg = "NO",
                         targets = targets,
                         binarization = F, na.rm=T)
#View(out)
write.csv(out, "test/logit_mutARID1A_vs_GEPscore.csv")
dm_searchM2_logit_plot(out)

####################
# cox model
####################
targets <- clin.bmk %>% dplyr::select(contains("gep_"),"TMB") %>%
  colnames() %>% setdiff(., "gep_sample_id")
res.searchM2 <- dm_searchM2_cox(data=clin.bmk, surv.time = "os",
                       surv.event = "censOS", marker1 = "mut_ARID1A",
                       m1.cat.pos = "YES", m1.cat.neg = "NO",
                       targets = targets, binarization = F)
write.csv(res.searchM2, "test/cox_mutARID1A_vs_GEP_all.csv")
res.searchM2 <- read_csv("test/cox_mutARID1A_vs_GEP_all.csv")

goi <- fun.read.gmt("inst/extdata/panel_gene_list.txt")$HTG_PIPIO
goi %<>% paste0("gep_",.)

res.searchM2 %>%
  filter(m2 %in% c("TMB",goi)) %>%
  dm_searchM2_cox_plot(., pval.threh = 0.01, max.n = 50)

goi <- res.searchM2 %>% filter(HR_m2 < 1, pval.vsM1_md < 0.001) %>% pull(m2)
length(goi)
corr <- cor(clin.bmk[, goi], use = "complete.obs")
pheatmap::pheatmap(corr)



d <- res.searchM2 %>%
  filter(m2 %in% c("TMB",goi)) %>%
  dplyr::mutate(
  m2_single = -log10(pval.waldtest_m2),
  md.int_vsM1 = -log10(pval.vsM1_md.int),
  md.int_vsM2 = -log10(pval.vsM2_md.int),
  md_vsM2 = -log10(pval.vsM2_md),
  md_vsM1 = -log10(pval.vsM1_md),
  vs_M1 = -log10(pmin(pval.vsM1_md, pval.vsM1_md.int)),
  vs_M2 = -log10(pmin(pval.vsM2_md, pval.vsM2_md.int)))

g <- d %>%
  ggplot(aes(x=vs_M1, y=vs_M2, text=m2))+
  geom_point()
g <- g + geom_hline(yintercept = -log10(0.01), color="blue")+
  geom_vline(xintercept = -log10(0.01), color="blue")
g
ggplotly(g)
#######################
# 4 quadrant
#######################
targets <- clin.bmk %>% dplyr::select(contains("gep_"),"TMB") %>%
  colnames() %>% setdiff(., "gep_sample_id")
res.searchM2 <- dm_searchM2_4quadrant(data = clin.bmk,outcome = "binaryResponse",
                      outcome.pos="CR/PR", outcome.neg="SD/PD",
                      marker1 = "mut_ARID1A", targets = targets,
                      num.cut.method = "median",
                      m1.cat.pos = "YES", m1.cat.neg = "NO")

write.csv(res.searchM2,"test/fourquadrant_mutARID1A_vs_GEPscore.csv")
dm_searchM2_4quadrant_plot(res.searchM2, pval.threh = 0.5, max.n = 30)
