.survfit <- function(data, var, time, event){
  data[[event]] %<>% as.character() %>% as.integer()
  assert_that(all(data[[event]] %in% c(0,1)), msg = "event should be 0 and 1")
  str.formula <- paste0("Surv(", time,",", event,") ~ ", var)
  survfit <- do.call("survfit",
                     list(as.formula(str.formula),data=data))
  survfit
}

.survplot <- function(survfit, data, ...){
  ggsurvplot(survfit, data=data,
             pval = T,risk.table = T,
             ...)
}

##' @descrption plot survival of dual markers
##' @param marker1 categorical variables with 2 levels
##' @param marker2 categorical variables with 2 levels
##' @param na.rm remove NA from marker1 and marker2
.dm_KMplot_core <- function(data, surv.time, surv.event, marker1, marker2,
                    label.m1 = marker1, label.m2 = marker2, na.rm=T){
  label.max.len <- 40
  assert_that(class(data[[marker1]]) == "factor" && nlevels(data[[marker1]]) == 2,
              msg = "marker1 should be factor with 2 levels")
  assert_that(class(data[[marker2]]) == "factor" && nlevels(data[[marker2]]) == 2,
              msg = "marker2 should be factor with 2 levels")

  # marker1
  label.m1 <- str_sub(label.m1, end=label.max.len)
  data[[label.m1]] <- data[[marker1]]
  survfit.m1 <-  .survfit(data = data, var = label.m1, time = surv.time, event = surv.event)
  km.m1 <- .survplot(survfit = survfit.m1,data= data)

  # marker2
  label.m2 <- str_sub(label.m2, end=label.max.len)
  assert_that(label.m1 != label.m2, msg= "label.m1 should NOT equal to label.m2")
  data[[label.m2]] <- data[[marker2]]
  survfit.m2 <- .survfit(data = data, var = label.m2, time = surv.time, event = surv.event)
  km.m2 <- .survplot(survfit = survfit.m2,data= data)

  # dual marker
  if(na.rm){
    data %<>% drop_na(!!sym(marker1), !!sym(marker2))
  }
  data$.region <- .label.quadrant(data[[marker1]], data[[marker2]])
  data$.group <- paste(data[[marker1]], data[[marker2]], sep = "_")
  mapper <- dplyr::select(data, .region, .group) %>% unique()

  tmp <- as.data.frame(color.quadrant.2) %>%
    rownames_to_column(".region") %>%
    setNames(c(".region","color")) %>%
    left_join(mapper, . , by = ".region") %>%
    arrange(.group)

  survfit.md <- .survfit(data = data, var = ".group", time = surv.time, event = surv.event)
  km.md <- .survplot(survfit = survfit.md,data= data, palette = as.character(tmp$color))
  list(marker1 = km.m1, marker2 = km.m2, dualmarker = km.md)
}

dm_KMplot <- function(data, surv.time, surv.event, marker1, marker2,
                      num.cut.method="median",
                      outcome=NULL, outcome.pos=NULL, outcome.neg=NULL,
                      m1.datatype="auto", m1.label.pos="pos", m1.label.neg="neg",
                      m1.cat.pos = NULL, m1.cat.neg = NULL,
                      m2.datatype="auto", m2.label.pos="pos", m2.label.neg = "neg",
                      m2.cat.pos = NULL, m2.cat.neg = NULL,
                      na.rm=T){
  assert_that(all(c(surv.time, surv.event, marker1, marker2) %in%  colnames(data)),
              msg = "surv.time, surv.event, marker1, marker2 are required")
  # marker1
  res <- binarize.data(x = data[[marker1]], datatype = m1.datatype, num.cut.method = num.cut.method, return.binary = F,
              label.pos = m1.label.pos, label.neg = m1.label.neg, cat.pos = m1.cat.pos, cat.neg = m1.cat.neg,
              outcome = data[[outcome]], outcome.pos = outcome.pos, outcome.neg = outcome.neg)
  data$.m1 <- res$data
  # marker2
  res <- binarize.data(x = data[[marker2]], datatype = m2.datatype, num.cut.method = num.cut.method, return.binary = F,
                     label.pos = m2.label.pos, label.neg = m2.label.neg, cat.pos = m2.cat.pos, cat.neg = m2.cat.neg,
                     outcome = data[[outcome]], outcome.pos = outcome.pos, outcome.neg = outcome.neg)
  data$.m2 <- res$data
  .dm_KMplot_core(data = data, surv.time= surv.time, surv.event=surv.event,
                  marker1 = ".m1", marker2 = ".m2",
                  label.m1 = marker1, label.m2 = marker2, na.rm=na.rm)
}
