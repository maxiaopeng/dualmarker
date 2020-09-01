##########################
# TMB + GEP/gepscore
##########################

source("R/load_all_functions.R")

# dm_search_1
marker1 <- "TMB"
targets <- clin.bmk %>% dplyr::select(contains("gepscore")) %>%
  colnames() %>% setdiff(., "gep_sample_id")

res.searchM2 <- dm_searchM2_logit(data = clin.bmk,  outcome = "binaryResponse",
                   outcome.pos="CR/PR",
                   outcome.neg="SD/PD",
                   marker1 = marker1,
                   targets = targets,
                   binarization = F, na.rm=T)
#View(out)
write.csv(out, "test/logit_TMB_vs_GEPscore.csv")
dm_searchM2_logit_plot(res.searchM2, pval.threh = 0.05)

####################
# cox model
####################
targets <- clin.bmk %>% dplyr::select(contains("gepscore")) %>%
  colnames() %>% setdiff(., "gep_sample_id")
res.searchM2 <- dm_searchM2_cox(data=clin.bmk, surv.time = "os",
                       surv.event = "censOS", marker1 = "TMB",
                       targets = targets, binarization = F)
write.csv(out, "test/cox_TMB_vs_GEPscore.csv")

dm_searchM2_cox_plot(res.searchM2, pval.threh = 0.05)
####################
# by 4quadrant
###################
targets <- clin.bmk %>% dplyr::select(contains("gepscore_")) %>%
  colnames() %>% setdiff(., "gep_sample_id")
res.searchM2 <- dm_searchM2_4quadrant(data = clin.bmk,
                             outcome = "binaryResponse",
                      outcome.pos="CR/PR",
                      outcome.neg="SD/PD",
                      marker1 = "TMB",
                      targets = targets,
                      num.cut.method = "median")
write.csv(res.searchM2,"test/fourquadrant_TMB_vs_GEPscore.csv")
dm_searchM2_4quadrant_plot(res.searchM2, pval.threh = 0.5, max.n = 30)
