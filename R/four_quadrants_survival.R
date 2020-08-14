
##' @description statistics of survival time in each quadrant
.quadrant.stats.survival <- function(data, time, event,
                                    marker1, marker2){

  data %<>% drop_na(!!sym(marker1), !!sym(marker2))
  res <- data %>% group_by(!!sym(marker1), !!sym(marker2)) %>%
    group_modify(.f = ~{
      res <- .survfit(data = .x, var = "1", time = time, event = event)
      summary(res)$table %>% t %>% as_tibble()
    }) %>% ungroup()
  res$.quadrant <- .label.quadrant(res[[marker1]], res[[marker2]]) %>%
    as.factor()
  res %>% dplyr::select(.quadrant, everything()) %>%
    dplyr::arrange(.quadrant)
}

##' @description test using log-rank or cox-test for binary variable
.surv.test.binaryVar <- function(data, var, time, event){

  data[[event]] %<>% as.character() %>% as.integer()
  assert_that(all(data[[event]] %in% c(0,1)), msg = "event should be 0 and 1")
  data[[var]] %<>% base::droplevels()
  assert_that(class(data[[var]]) == "factor" &&
                nlevels(data[[var]]) == 2,
              msg = "var should be factors with 2 levels")

  fit <- .survfit(data = data, var = var, time = time, event = event)
  res.logrank <- surv_pvalue( fit = fit, data=data, method = "survdiff") %>%
    as_tibble() %>% dplyr::select(pval.logrank = pval)

  fml <- paste0("Surv(", time,",", event,") ~ ", var) %>% as.formula()
  res.coxph <- coxph(formula = fml, data = data) %>% summary() %>%
    .$coefficients %>% as_tibble() %>%
    dplyr::select(HR= `exp(coef)`, pval.cox = `Pr(>|z|)`)
  bind_cols(res.logrank, res.coxph)
}

##' @description  log-rank test to compare each quadrants
.quadrant.test.survival <- function(data, time, event, marker1, marker2, na.rm=T){
  # prep data
  if(na.rm){
    data %<>% drop_na(!!sym(marker1), !!sym(marker2))
  }
  data$.quadrant <- .label.quadrant(data[[marker1]], data[[marker2]])
  pairs <- list("R1_vs_R4" = c("R1", "R4"),
                "R2_vs_R3" = c("R2","R3"),
                "R1_vs_R2" = c("R1","R2"),
                "R3_vs_R4" = c("R3","R4"),
                "R1_vs_R3" = c("R1","R3"),
                "R2_vs_R4" = c("R2","R4"))
  map_dfr(pairs, .f = ~{
    keep <- data$.quadrant %in% .x
    d <- data[keep, ]
    out <- .surv.test.binaryVar(data = d, var = ".quadrant", time = time, event = event)
    out$reference = sort(.x)[1]
    out
  }, .id = "comparison")
}

.dm_4quadrant_survival_core <- function(data, time, event,
                                        marker1, marker2){
  # check input
  data[[event]] %<>% as.character() %>% as.integer()
  assert_that(all(data[[event]] %in% c(0,1)), msg = "event should be 0 and 1")
  assert_that(class(data[[marker1]]) == "factor" &&
                nlevels(data[[marker1]]) == 2,
              msg = "marker1 should be factors with 2 levels")
  assert_that(class(data[[marker2]]) == "factor" &&
                nlevels(data[[marker2]]) == 2,
              msg = "marker2 should be factors with 2 levels")
  # prep data
  stats <- .quadrant.stats.survival(data = data, time = time, event = event,
                          marker1 = marker1, marker2 = marker2)

  test <- .quadrant.test.survival(data = data, time = time, event = event,
                          marker1 = marker1, marker2 = marker2)
  list(stats = stats, test =test)
}

##' @description
##' four quadrant analysis of survival
dm_4quadrant_survival <- function(data, time, event,
                                  marker1, marker2, num.cut.method="none",
                                  m1.datatype = "auto",
                                  m1.cat.pos = NULL, m1.cat.neg = NULL,
                                  m2.datatype = "auto",
                                  m2.cat.pos = NULL, m2.cat.neg = NULL){
  # prep .m1
  res <- binarize.data(x = data[[marker1]],
                       datatype = m1.datatype,
                       num.cut.method = num.cut.method,
                       outcome = data[[outcome]],
                       outcome.pos = outcome.pos, outcome.neg =outcome.neg,
                       cat.pos = m1.cat.pos, cat.neg = m1.cat.neg)
  data$.m1 <- res$data
  cutpoint.m1 <- res$cutpoint
  if(m1.datatype=="auto"){m1.datatype <- datatype.num.cat(data[[marker1]])}
  # prep .m2
  res <- binarize.data(x = data[[marker2]], datatype = m2.datatype,
                       num.cut.method = num.cut.method,
                       outcome = data[[outcome]],
                       outcome.pos = outcome.pos, outcome.neg =outcome.neg,
                       cat.pos = m2.cat.pos, cat.neg = m2.cat.neg)
  if(m2.datatype=="auto"){m2.datatype <- datatype.num.cat(data[[marker2]])}
  data$.m2 <- res$data
  cutpoint.m2 <- res$cutpoint
  # run 4quadrant analysis
  out <- .dm_4quadrant_survival_core(data = data, time = time,
                                     event = event, marker1=".m1", marker2=".m2")
  #out$data <- data
  out$param <- tibble(time = time,
                      event = event,
                      m1=marker1,
                      m2 =marker2,
                      marker.cut.method = num.cut.method,
                      m1.datatype = m1.datatype,
                      m2.datatype = m2.datatype,
                      cutpoint.m1 = cutpoint.m1,
                      cutpoint.m2 = cutpoint.m2)
  out
}


if(F){
  data <- clin.bmk
  outcome = "binaryResponse"
  outcome.pos="CR/PR"
  outcome.neg="SD/PD"
  marker1 = "TMB"
  marker2 <- "gepscore_gene19"
  num.cut.method <- "median"
  m1.datatype <- "auto"
  m2.datatype <- "auto"
  surv.time <- "os"
  surv.event <- "censOS"
  dm_4quadrant_survival(data = clin.bmk, time = "os", event = "censOS", marker1 = "TMB",
                        marker2 = "gepscore_gene19", num.cut.method = "median")
  dm_4quadrant_survival(data = clin.bmk, time = "os", event = "censOS", marker1 = "mut_ARID1A",
                        marker2 = "gep_CXCL13", num.cut.method = "median", m1.cat.pos = "YES", m1.cat.neg = "NO")
  res <- dm_4quadrant_survival(data = clin.bmk, time = "os", event = "censOS", marker1 = "mut_ARID1A",
                        marker2 = "IC.Level", num.cut.method = "median",
                        m1.cat.pos = "YES", m1.cat.neg = "NO",
                        m2.cat.pos = c("IC0","IC1"), m2.cat.neg = c("IC2+"))
}
