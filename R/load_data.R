library(IMvigor210CoreBiologies)
library(tidyverse)
library(magrittr)
source("~/working/Biomarker_integrated_reporter/src/my_functions.R")

#######################
### clin info
#######################
# clin.info
data("cds")
data("fmone")
clin.info.gep <- cds %>% pData() %>%
  dplyr::rename(TMB = "FMOne mutation burden per MB") %>%
  rownames_to_column("gep_sample_id") %>%
  as_tibble()
clin.info.gep$ANONPT_ID %<>% as.integer()

# clin.info.mut
clin.info.mut <- pData(fmone) %>%
  dplyr::rename(TMB = "FMOne mutation burden per MB") %>%
  rownames_to_column("mut_sample_id") %>%
  as_tibble()
clin.info.mut$ANONPT_ID %<>% as.integer()
clin.info.mut$TMB %<>% as.numeric() %>% round(0)

# merge
fun.stat.overlap(clin.info.gep$ANONPT_ID, clin.info.mut$ANONPT_ID)
clin.info.all <- full_join(mutate(clin.info.gep, is_gep = "YES"),
                           mutate(clin.info.mut, is_mut = "YES"),
                           by = "ANONPT_ID")
# check BOR (all are consistent)
table(clin.info.all$`Best Confirmed Overall Response.x`,
      clin.info.all$`Best Confirmed Overall Response.y`, useNA="ifany")
clin.info.all %>% dplyr::select(contains("IC Level")) %>% table(useNA="ifany")
clin.info.all %>% dplyr::select(contains("TC Level")) %>% table(useNA="ifany")
clin.info.all %>% dplyr::select(contains("TMB"))
#
clin.info.all %<>%
  mutate(BOR = ifelse(!is.na(`Best Confirmed Overall Response.x`),
                      `Best Confirmed Overall Response.x`,
                      `Best Confirmed Overall Response.y`)) %>%
  mutate(IC_level = ifelse(!is.na(`IC Level.x`), "IC Level.x", "IC Level.y")) %>%
  mutate(TC_level = ifelse(!is.na(`TC Level.x`), "TC Level.x", "TC Level.y")) %>%
  mutate(TMB      = ifelse(!is.na(TMB.x), TMB.x,TMB.y)) %>%
  dplyr::select(-contains(".x"), -contains(".y"))


#####################
### GEP
#####################
data(cds)

# 1. gep
cds2 <- cds
# 1.1 sample_id
cds2$sample_id <- paste0("P",cds2$ANONPT_ID)
#keep <- !duplicated(cds2$sample_id)
#cds2 <- cds2[,keep]
colnames(cds2) <- make.names(cds2$sample_id, unique = T)

# 1.2 gene name
geneNames <- setNames(fData(cds2)$Symbol,
                      as.character(rownames(fData(cds2))))
expr.voom <- IMvigor210CoreBiologies::filterNvoom(counts(cds2),
                                              minSamples=ncol(counts(cds2))/10,minCpm=0.25)$E
expr.voom <- t(scale( t( expr.voom ), center=TRUE, scale=TRUE))
rownames(expr.voom) <- geneNames[rownames(expr.voom)] %>% base::unname()
keep <- "" != rownames(expr.voom) & !is.na(rownames(expr.voom)) & !duplicated(rownames(expr.voom))
expr.voom <- expr.voom[keep,]
# 1.3 es
es.gep <- fun.make.eSet(m.expr =expr.voom, sample.info = pData(cds2))

# 2. signature
data(human_gene_signatures)
#genesets <- fun.read.gmt("inst/extdata/BGsigDB_v2.0.gmt")
genesets <- fun.read.gmt("inst/extdata/h.all.v7.0.symbols.gmt")
names(genesets) %<>% make.names()
human_gene_signatures <- c(human_gene_signatures, genesets)
human_gene_signatures %<>% purrr::keep(.p = ~{length(intersect(.x, rownames(es.gep)))>0})

sig.score <- map_dfr(human_gene_signatures, .f = ~{
  goi <- intersect(.x, rownames(es.gep))
  IMvigor210CoreBiologies::gsScore(gm = exprs(es.gep[goi,]))
}, .id = "sig_name")

es.gepscore <- fun.make.eSet(m.expr = sig.score %>%
                               column_to_rownames("sig_name") %>% as.matrix(),
                             sample.info = pData(cds2))

##########################
# mutation by FMI
##########################
ls.str(assayData(fmone))

#assayData(fmone)$known_short
#assayData(fmone)$likely_short
#assayData(fmone)$amplification
#assayData(fmone)$deletion
#assayData(fmone)$gain
# save
purrr::walk(names(assayData(fmone)), .f = ~{
  assayData(fmone)[[.x]] %>%
    as.data.frame() %>%
    rownames_to_column("gene") %>%
    write_tsv(., str_c("inst/extdata/IMvigor210CoreBiologies_fmone_",.x,".tsv"))
})

# aggregate all mutation
x <- names(assayData(fmone))
names(x) <- x
mut.long <- purrr::map_dfr(x,
                           .f= ~{
                             assayData(fmone)[[.x]] %>%
                               as.data.frame() %>%
                               rownames_to_column("gene") %>%
                               gather(key = "mut_sample_id", value="var_detail", -gene) %>%
                               dplyr::filter(""!=var_detail)
                           },.id = "fmi_var_type")
mut.long <-  tidyr::separate_rows(mut.long, var_detail, sep=";")
mut.long$var_detail %<>% str_trim()
mut.long %<>% ungroup()

mut.long %<>%
  left_join(., clin.info.mut %>% dplyr::select(mut_sample_id, ANONPT_ID),
            by = "mut_sample_id")
mut.wide <- mut.long %>%
  mutate(pts_id = paste0("P", ANONPT_ID)) %>%
  dplyr::group_by(gene, pts_id) %>%
  summarise(var_detail = paste0(var_detail, collapse = ";")) %>%
  spread(key = "gene", value="var_detail")
colnames(mut.wide)[-1] %<>% paste0("mut_",.)

##########################
# combine all data
##########################
tmp.clin <- clin.info.gep %>%
  mutate(pts_id = paste0("P", ANONPT_ID) %>% make.names(unique = T)) %>%
  setNames(colnames(.) %>% make.names)


#goi <- c("CXCL13","HLA-DRA","B2M","CD163","CD68","CD80","CD86","NCR1","FCGR1A","FCGR1B","FCGR2A","FCGR2B","FCGR3A","FCGR3B","ADORA2A","CD226","HAVCR2","ENTPD1","CD274","NT5E","PDCD1","PVR","PVRL2","TIGIT","TNFRSF4","TNFSF4","VTCN1","PDCD1LG2","PIK3CD","CD4","CD8A","FOXP3","GZMB","IFNG","LAG3","NECTIN2","CD276","MAP4K1","IL2RA","IL2RB","TGFB1","TLR7","TLR8","TLR9","TMEM173","KLRC1","KLRC2","KLRD1","HLA-E","HLA-G","HLA-F","LILRB1","LILRB2","TIGIT","CD27","CD8A","PDCD1LG2","LAG3","CD274","CXCR6","CMKLR1","NKG7","CCL5","PSMB10","IDO1","CXCL9","HLA-DQA1","CD276","STAT1","HLA-DRB1","HLA-E")
genesets <- fun.read.gmt("inst/extdata/panel_gene_list.txt")
goi <- genesets$HTG_PIPIO
goi <- rownames(es.gep)
tmp.gep <- fun.subset.es(es = es.gep, subset.row = unique(goi)) %>%
  exprs() %>% t %>% as.data.frame() %>%
  setNames(paste0("gep_", make.names(colnames(.)))) %>%
  rownames_to_column("pts_id")

tmp.gepscore <- es.gepscore %>%
  exprs() %>% t %>% as.data.frame() %>%
  setNames(paste0("gepscore_", make.names(colnames(.)))) %>%
  rownames_to_column("pts_id")

tmp.mut <- mut.wide
tmp.mut[,-1] %<>% map_df(., ~ ifelse(is.na(.x), "NO","YES"))

clin.bmk <- join_all(dfs = list(tmp.clin, tmp.gep, tmp.gepscore, tmp.mut),
                     by="pts_id", type = "left")

# check ARID1A mut + CXCL13 GEP
table(!is.na(clin.bmk$mut_ARID1A), !is.na(clin.bmk$gep_CXCL13)& clin.bmk$os>0)

# save
save(clin.bmk, clin.info.gep, clin.info.mut, es.gep, es.gepscore, mut.wide, file = "inst/extdata/my_IMvigor210_data.RData")
