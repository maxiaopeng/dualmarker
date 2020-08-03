##########################
# mut_ARID1A + gepscore
##########################
source("R/load_all_functions.R")

# dm_search_1
marker1 <- "mut_ARID1A"
targets <- clin.bmk %>% dplyr::select(contains("gepscore")) %>%
  colnames() %>% setdiff(., "gep_sample_id")

out <- dm_searchM2_logit(data = clin.bmk,
                         outcome = "binaryResponse",
                         outcome.pos="CR/PR",
                         outcome.neg="SD/PD",
                         marker1 = marker1,
                         m1.cat.pos = "YES", m1.cat.neg = "NO",
                         targets = targets,
                         binarization = F, na.rm=T)
View(out)
write.csv(out, "test/logit_mutARID1A_vs_GEPscore.csv")
dm_searchM2_logit_plot(out)

####################
# cox model
####################
targets <- clin.bmk %>% dplyr::select(contains("gepscore_"),"TMB") %>%
  colnames() %>% setdiff(., "gep_sample_id")
out <- dm_search_cox_1(data=clin.bmk, surv.time = "os",
                       surv.event = "censOS", marker1 = "mut_ARID1A",
                       m1.cat.pos = "YES", m1.cat.neg = "NO",
                       targets = targets, binarization = F)
write.csv(out, "test/cox_mutARID1A_vs_GEP.csv")
dm_searchM2_cox_plot(out)

#######################
# 4 quadrant
#######################
targets <- clin.bmk %>% dplyr::select(contains("gepscore_"),"TMB") %>%
  colnames() %>% setdiff(., "gep_sample_id")
res.searchM2 <- dm_searchM2_4quadrant(data = clin.bmk,outcome = "binaryResponse",
                      outcome.pos="CR/PR", outcome.neg="SD/PD",
                      marker1 = "mut_ARID1A", targets = targets,
                      num.cut.method = "median",
                      m1.cat.pos = "YES", m1.cat.neg = "NO")

write.csv(res.searchM2,"test/fourquadrant_mutARID1A_vs_GEPscore.csv")
dm_searchM2_4quadrant_plot(res.searchM2, pval.threh = 0.5, max.n = 30)
