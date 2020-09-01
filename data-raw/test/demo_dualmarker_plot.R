devtools::load_all()

data <- clin.bmk
outcome = "binaryResponse"
outcome.pos="CR/PR"
outcome.neg="SD/PD"
marker1 = "TMB"
marker2 <- "gepscore_TGFb.19gene"
num.cut.method <- "median"
m1.datatype <- "auto"
m2.datatype <- "auto"
time <- "os"
event <- "censOS"


##################
## TMB + TGFb signature/gepscore_Mastcells.Danaher.IO
##################
# 1. cut by median
res <- dm_pair(
  data = clin.bmk,
  outcome = "binaryResponse",
  outcome.pos="CR/PR",
  outcome.neg="SD/PD",
  marker1 = "TMB",
  marker2 = "gepscore_TGFb.19gene",
  num.cut.method = "median",
  m1.cat.pos = NULL,
  m1.cat.neg = NULL,
  m2.cat.pos = NULL,
  m2.cat.neg = NULL,
  surv.time = "os",
  surv.event = "censOS")

pdf("test/Imvigor210_TMB_gepscoreTGFb_cutByMedian.pdf")
walk(res$plot, print)
dev.off()

# 2. cut by roc
res <- dm_pair(
  data = clin.bmk,
  outcome = "binaryResponse",
  outcome.pos="CR/PR",
  outcome.neg="SD/PD",
  marker1 = "TMB",
  marker2 = "gepscore_TGFb.19gene",
  num.cut.method = "roc",
  m1.cat.pos = NULL,
  m1.cat.neg = NULL,
  m2.cat.pos = NULL,
  m2.cat.neg = NULL,
  surv.time = "os",
  surv.event = "censOS")

pdf("test/Imvigor210_TMB_gepscoreTGFb_cutByRoc.pdf")
walk(res$plot, print)
dev.off()


# 3. gepscore_Mastcells.Danaher.IO
res <- dm_pair(
  data = clin.bmk,
  outcome = "binaryResponse",
  outcome.pos="CR/PR",
  outcome.neg="SD/PD",
  marker1 = "TMB",
  marker2 = "gepscore_Mastcells.Danaher.IO",
  num.cut.method = "median",
  m1.cat.pos = NULL,
  m1.cat.neg = NULL,
  m2.cat.pos = NULL,
  m2.cat.neg = NULL,
  surv.time = "os",
  surv.event = "censOS")

pdf("test/Imvigor210_TMB_gepscoreMasterCell_cutByMedian.pdf")
walk(res$plot, print)
dev.off()

# feature of interest
res <- dm_pair(
  data = clin.bmk,
  outcome = "binaryResponse",
  outcome.pos="CR/PR",
  outcome.neg="SD/PD",
  marker1 = "TMB",
  marker2 = "gepscore_PARPi.response",
  num.cut.method = "median",
  m1.cat.pos = NULL,
  m1.cat.neg = NULL,
  m2.cat.pos = NULL,
  m2.cat.neg = NULL,
  surv.time = "os",
  surv.event = "censOS")
pdf("test/Imvigor210_TMB_gepscore_PARPi.response_cutByMedian.pdf")
walk(res$plot, print)
dev.off()

##################
## mut_ARID1A + gep_CXCL13 signature
##################
# 1. cut by median
res <- dm_pair(
  data = clin.bmk,
  outcome = "binaryResponse",
  outcome.pos="CR/PR",
  outcome.neg="SD/PD",
  marker1 = "mut_ARID1A",
  marker2 = "gep_CXCL13",
  num.cut.method = "median",
  m1.cat.pos = "YES",
  m1.cat.neg = "NO",
  m2.cat.pos = NULL,
  m2.cat.neg = NULL,
  surv.time = "os",
  surv.event = "censOS")

pdf("test/Imvigor210_mutARID1A_gepCXCL13_cutByMedian.pdf")
walk(res$plot, print)
dev.off()

# ARID1A + TMB
res <- dm_pair(
  data = clin.bmk,
  outcome = "binaryResponse",
  outcome.pos="CR/PR",
  outcome.neg="SD/PD",
  marker1 = "mut_ARID1A",
  marker2 = "TMB",
  num.cut.method = "median",
  m1.cat.pos = "YES",
  m1.cat.neg = "NO",
  m2.cat.pos = NULL,
  m2.cat.neg = NULL,
  surv.time = "os",
  surv.event = "censOS")

pdf("test/Imvigor210_mutARID1A_TMB_cutByMedian.pdf")
walk(res$plot, print)
dev.off()

# 2. feature of interest
res <- dm_pair(
  data = clin.bmk,
  outcome = "binaryResponse",
  outcome.pos="CR/PR",
  outcome.neg="SD/PD",
  marker1 = "mut_ARID1A",
  marker2 = "gep_MUC1",
  num.cut.method = "median",
  m1.cat.pos = "YES",
  m1.cat.neg = "NO",
  m2.cat.pos = NULL,
  m2.cat.neg = NULL,
  surv.time = "os",
  surv.event = "censOS")

pdf("test/Imvigor210_mutARID1A_gepMUC1_cutByMedian.pdf")
walk(res$plot, print)
dev.off()

##################
## mut_ARID1A + IC.Level signature
##################
# 1. cut by median
res <- dm_pair(
  data = clin.bmk,
  outcome = "binaryResponse",
  outcome.pos="CR/PR",
  outcome.neg="SD/PD",
  marker1 = "mut_ARID1A",
  marker2 = "IC.Level",
  num.cut.method = "none",
  m1.cat.pos = "YES",
  m1.cat.neg = "NO",
  m2.cat.pos = "IC2+",
  m2.cat.neg = c("IC0","IC1"),
  surv.time = "os",
  surv.event = "censOS")

pdf("test/Imvigor210_mutARID1A_IClevel.pdf")
walk(res$plot, print)
dev.off()

