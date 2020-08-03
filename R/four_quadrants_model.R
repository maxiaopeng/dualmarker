################################
### statistics of four quadrant
################################

##' convert positive number and total numbers in each quadrant to dataframe
##' @param x positive numbers in each quadrant
##' @param n total numbers in each quadrant
##' @example
##' x <- c(5,3,12,9)
##' n <- c(8, 10, 20, 18)
##' @return dataframe with n.total, n.pos, n.neg, pct.pos, pos.lower95, pos.upper95, region, .x1.level, .x2.level
.quadrant.transform <- function(x, n, combined.quadrant=F){
  x <- x[1:4]
  n <- n[1:4]
  names(x) <- names(n) <- c("R1","R2","R3","R4")
  out.region <- tibble(
    region = c("R1","R2","R3","R4"),
    .x1.level = c("pos","neg","neg","pos"),
    .x2.level = c("pos","pos","neg","neg")
  )
  if(combined.quadrant){
    x["R12"] <- x["R1"] + x["R2"]
    x["R34"] <- x["R3"] + x["R4"]
    x["R14"] <- x["R1"] + x["R4"]
    x["R23"] <- x["R2"] + x["R3"]
    x["R1234"] <- x["R1"] + x["R2"] + x["R3"] + x["R4"]
    n["R12"] <- n["R1"] + n["R2"]
    n["R34"] <- n["R3"] + n["R4"]
    n["R14"] <- n["R1"] + n["R4"]
    n["R23"] <- n["R2"] + n["R3"]
    n["R1234"] <- n["R1"] + n["R2"] + n["R3"] + n["R4"]
    out.region.new <- tibble(
      region = c("R12","R34","R14","R23","R1234"),
      .x1.level = c(NA,NA,"pos","neg", NA),
      .x2.level = c("pos","neg",NA,NA,NA)
    )
    out.region <- bind_rows(out.region, out.region.new)
  }
  out.count <- map2_df(.x = x, .y = n, .f = ~{
    res <- binom.test(.x, .y);
    tibble(
      n.total = .y,
      n.pos = .x,
      n.neg = .y-.x,
      pct.pos = res$estimate,
      pos.lower95 = res$conf.int[1],
      pos.upper95 = res$conf.int[2])
  })
  bind_cols(out.region, out.count)
}

##' @description compare response rate between quadrants
##' @details R1 vs R4, R2 vs R3, R1 vs R2, R3 vs R4, R1 vs R3, R2 vs R4,
##' R14(R1+R4) vs R24(R2+R4), R12(R1+R2) vs R34(R3 + R4)
.quadrant.test <- function(x,n, method="fisher.test", ...){
  stats <- .quadrant.transform(x,n, combined.quadrant = T) %>%
    as.data.frame() %>%
    column_to_rownames("region")
  pairs <- list("R1_vs_R4" = c("R1", "R4"),
                "R2_vs_R3" = c("R2","R3"),
                "R1_vs_R2" = c("R1","R2"),
                "R3_vs_R4" = c("R3","R4"),
                "R1_vs_R3" = c("R1","R3"),
                "R2_vs_R4" = c("R2","R4"),
                "R14vs_R23" = c("R14","R23"),
                "R12_vs_R34" = c("R12","R34"))
  map_dfr(pairs, .f = ~{
    d <- stats[.x, c("n.pos", "n.neg")]
    out1 <- tibble(group1 = toString(d[1,,drop=T]),
                   group2 = toString(d[2,,drop=T]))
    if(method=="fisher.test"){
      out2 <-  fisher.test(d, ...) %>% broom::tidy()
    }else if(method == "chisq.test"){
      out2 <- chisq.test(d, ...) %>% broom::tidy()
    }else{
      assert_that(method %in% c("fisher.test","chisq.test"),
                  msg= "method should be [fisher.test, chisq.test]")
    }
    bind_cols(out1, out2)
  }, .id = "comparison")
}

##' the first level as negative, 2nd level as positve
##' for (x1, x2):
##' (pos, pos) => R1, first quadrant
##' (neg, pos) => R2, 2nd quadrant
##' (neg, neg) => R3, 3rd quadrant
##' (pos, neg) => R4, 4th quadrant
##' @param m1 factor with 2 levels
##' @param m2 factor with 2 levels
##'
.label.quadrant <- function(m1, m2){
  assert_that(class(m1) == "factor" && nlevels(m1) == 2,
              msg = "marker1 should be factor with 2 levels")
  assert_that(class(m2) == "factor" && nlevels(m2) == 2,
              msg = "marker2 should be factor with 2 levels")
  m1.neg <- levels(m1)[1]
  m1.pos <- levels(m1)[2]
  m2.neg <- levels(m2)[1]
  m2.pos <- levels(m2)[2]
  case_when(m1 %in% m1.pos & m2 %in% m2.pos ~ "R1",
            m1 %in% m1.neg & m2 %in% m2.pos ~ "R2",
            m1 %in% m1.neg & m2 %in% m2.neg ~ "R3",
            m1 %in% m1.pos & m2 %in% m2.neg ~ "R4",
            TRUE ~ ""
  )
}

##' @param data
##' @param outcome should be [ pos, neg ]
##' @param marker1 should be factor with 2 levels, 1st level will be treated as negative;
##' @param marker2 should be factor with 2 levels, 1st level will be treated as negative;
.dm_4quadrant_core <- function(data, outcome=".outcome", marker1=".m1", marker2=".m2",na.rm=T){
  # check input
  assert_that(class(data[[outcome]]) == "factor" &&
                nlevels(data[[outcome]]) == 2,
              msg = "outcome should be factors with 2 levels")
  assert_that(all(levels(data[[outcome]]) == c("neg","pos")),
              msg = "outcome level should be ['neg','pos']")
  assert_that(class(data[[marker1]]) == "factor" &&
                nlevels(data[[marker1]]) == 2,
              msg = "marker1 should be factors with 2 levels")
  assert_that(class(data[[marker2]]) == "factor" &&
                nlevels(data[[marker2]]) == 2,
              msg = "marker2 should be factors with 2 levels")
  # prep data
  if(na.rm){
    data %<>% drop_na( !!sym(outcome), !!sym(marker1), !!sym(marker2))
  }
  data$.quadrant <- .label.quadrant(data[[marker1]], data[[marker2]])
  total.n <- data %>% group_by(.quadrant) %>% dplyr::count() %>% pull(n)
  names(total.n) <- c("R1","R2","R3","R4")
  pos.n <- data %>% dplyr::filter(!!sym(outcome) %in% "pos") %>%
    group_by(.quadrant) %>% count() %>% pull(n)
  names(pos.n) <- c("R1","R2","R3","R4")
  # stats
  stats <- .quadrant.transform(pos.n, total.n)
  # test
  test <- .quadrant.test(x =pos.n, n = total.n)
  list(pos.n= pos.n, total.n = total.n, stats = stats, test =test)
}

##' dm_4quadrant
##' evaluate the logistic regression of dual marker
##' @param data
##' @param outcome
##' @param outcome.pos
##' @param outcome.neg
##' @param marker1
##' @param marker2
##' @param num.cut.method marker cut method, [none, roc, median]
##' @return dual marker logistic regression summary
dm_4quadrant <- function(data, outcome, outcome.pos, outcome.neg=NULL,
                         marker1, marker2, num.cut.method="none",
                         m1.cat.pos = NULL, m1.cat.neg = NULL,
                         m2.cat.pos = NULL, m2.cat.neg = NULL,
                         na.rm=T){
  # prep .outcome
  data$.outcome <- binarize.cat(x = data[[outcome]],
                                pos = outcome.pos, neg = outcome.neg)
  # prep .m1
  res <- binarize.data(x = data[[marker1]],
                     datatype = "auto",
                     num.cut.method = num.cut.method,
                     outcome = data[[outcome]],
                     outcome.pos = outcome.pos, outcome.neg =outcome.neg,
                     cat.pos = m1.cat.pos, cat.neg = m1.cat.neg)
  data$.m1 <- res$data
  cutpoint.m1 <- res$cutpoint

  # prep .m2
  res <- binarize.data(x = data[[marker2]], datatype = "auto",
                     num.cut.method = num.cut.method,
                     outcome = data[[outcome]],
                     outcome.pos = outcome.pos, outcome.neg =outcome.neg,
                     cat.pos = m2.cat.pos, cat.neg = m2.cat.neg)
  data$.m2 <- res$data
  cutpoint.m2 <- res$cutpoint
  # run 4quadrant analysis
  out <- .dm_4quadrant_core(data, outcome=".outcome",
                             marker1=".m1", marker2=".m2",na.rm=T)
  out$data <- data
  out$param <- tibble(outcome = outcome,
                      outcome.pos = toString(outcome.pos),
                      outcome.neg = toString(outcome.neg),
                      m1=marker1, m2 =marker2,
                      marker.cut.method = num.cut.method,
                      cutpoint.m1 = cutpoint.m1,
                      cutpoint.m2 = cutpoint.m2)
  out
}


