##########################
# TMB + GEP/gepscore
##########################

source("R/load_all_functions.R")

# dm_search_1
marker1 <- "TMB"
targets <- clin.bmk %>% dplyr::select(contains("gepscore")) %>%
  colnames() %>% setdiff(., "gep_sample_id")

out <- dm_searchM2_logit(data = clin.bmk,  outcome = "binaryResponse",
                   outcome.pos="CR/PR",
                   outcome.neg="SD/PD",
                   marker1 = marker1,
                   targets = targets,
                   binarization = F, na.rm=T)
View(out)
write.csv(out, "test/logit_TMB_vs_GEPscore.csv")

# filter M2s
pval.threh <- 0.03
col.pval <- c('pval.vsM1_md','pval.vsM1_md.int','pval.vsMd_md.int')
keep <- rowSums(out[,col.pval] <= pval.threh)>0
out.sig <- out[keep,]
dim(out.sig)
out.sig$m2

d <- out.sig %>%
  mutate(sign_m2 = sign(estimate.m2_m2)) %>%
  dplyr::select(m2, sign_m2, pval.vsNull_m2, pval.vsM1_md, pval.vsM1_md.int, pval.vsMd_md.int)

.f <- function(pval, sign){
  - sign * log10(pval) }
dd <- mutate_at(.tbl = d, .vars = vars(contains("pval")),
                .funs = .f, sign = d$sign_m2) %>%
  mutate(m2 = str_sub(m2, end=30) %>% make.names(unique=T)) %>%
  mutate(m2 = reorder(m2, pval.vsM1_md))


dd %>%
  gather(key = "comparison", value="pval", contains("pval")) %>%
  ggplot(aes(y = pval, x = m2, color = comparison, shape = factor(sign_m2)))+
  geom_point()+
  geom_hline(yintercept = c(-log10(c(0.05, 0.01)),log10(c(0.05, 0.01))), linetype="dashed", color="grey")+
  theme_bw()+
  labs(y = "signed log10-pValue", x = "marker2", title = "Significant marker to combine with TMB")+
  theme(axis.text = element_text(angle = 90))

####################
# cox model
####################
targets <- clin.bmk %>% dplyr::select(contains("gepscore")) %>%
  colnames() %>% setdiff(., "gep_sample_id")
out <- dm_searchM2_cox(data=clin.bmk, surv.time = "os",
                       surv.event = "censOS", marker1 = "TMB",
                       targets = targets, binarization = F)
write.csv(out, "test/cox_TMB_vs_GEPscore.csv")

# filter M2s
pval.threh <- 0.01
col.pval <- c('pval.vsM1_md','pval.vsM1_md.int','pval.vsMd_md.int')
keep <- rowSums(out[,col.pval] <= pval.threh)>0
out.sig <- out[keep,]
dim(out.sig)
out.sig$m2

d <- out.sig %>%
  mutate(sign_m2 = sign(1 - HR_m2)) %>%
  dplyr::select(m2, sign_m2, pval.waldtest_m2, pval.vsM1_md, pval.vsM1_md.int, pval.vsMd_md.int)

.f <- function(pval, sign){ - sign * log10(pval) }
dd <- mutate_at(.tbl = d, .vars = vars(contains("pval")),
                .funs = .f, sign = d$sign_m2) %>%
  mutate(m2 = str_sub(m2, end=30) %>% make.names(unique=T)) %>%
  mutate(m2 = reorder(m2, pval.vsM1_md))

dd %>%
  gather(key = "comparison", value="pval", contains("pval")) %>%
  ggplot(aes(y = pval, x = m2, color = comparison, shape = factor(sign_m2)))+
  geom_point()+
  geom_hline(yintercept = c(-log10(c(0.05, 0.01)),log10(c(0.05, 0.01))), linetype="dashed", color="grey")+
  theme_bw()+
  labs(y = "signed log10-pValue", x = "marker2", title = "Significant marker to combine with TMB")+
  theme(axis.text = element_text(angle = 90))

####################
# by 4quadrant
###################
marker1 <- "TMB"
targets <- clin.bmk %>% dplyr::select(contains("gepscore_")) %>%
  colnames() %>% setdiff(., "gep_sample_id")
res.searchM2 <- dm_searchM2_4quadrant(data = clin.bmk,
                             outcome = "binaryResponse",
                      outcome.pos="CR/PR",
                      outcome.neg="SD/PD",
                      marker1 = marker1,
                      targets = targets,
                      num.cut.method = "median")
write.csv(res.searchM2,"test/fourquadrant_TMB_vs_GEPscore.csv")
dm_searchM2_4quadrant_plot(res.searchM2, pval.threh = 0.5, max.n = 30)
