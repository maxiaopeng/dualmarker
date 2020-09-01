################
## statistics of four quadrant
################

#' statistic of response rate in each quadrant
#'
#' @param x positive number in each quadrant, length(x)
#' @param n total number in each quadrant
#' @return dataframe with n.total, n.pos, n.neg, pct.pos, pos.lower95,
#'   pos.upper95, region, .m1.level, .m2.level
.quadrant_stats_response <- function(x, n, combined_quadrant=F){
  assert_that(all(x<=n),
              msg = "positive number should not greater than total number in each quadrant")
  assert_that(length(x) == 4, msg = "length of x is not 4")
  assert_that(length(n) == 4, msg = "length of n is not 4")
  x <- x[1:4]
  n <- n[1:4]
  names(x) <- names(n) <- c("R1","R2","R3","R4")
  out.region <- tibble(
    region = c("R1","R2","R3","R4"),
    .m1.level = c("pos","neg","neg","pos"),
    .m2.level = c("pos","pos","neg","neg")
  )
  if(combined_quadrant){
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
      .m1.level = c(NA,NA,"pos","neg", NA),
      .m2.level = c("pos","neg",NA,NA,NA)
    )
    out.region <- bind_rows(out.region, out.region.new)
  }
  out.count <- purrr::map2_df(.x = x, .y = n, .f = ~{
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

#' test of response rate in different quadrant
#'
#' @description compare response rate between quadrants
#' @details R1 vs R4, R2 vs R3, R1 vs R2, R3 vs R4, R1 vs R3, R2 vs R4,
#' R14(R1+R4) vs R24(R2+R4), R12(R1+R2) vs R34(R3 + R4)
.quadrant_test_response <- function(x,n, method="fisher.test", ...){
  assert_that(all(x<=n), msg = "positive number should not greater than total number in each quadrant")
  stats <- .quadrant_stats_response(x,n, combined_quadrant = T) %>%
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
  purrr::map_dfr(pairs, .f = ~{
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

#' four quadrant analysis for response(core)
#'
#' @param data data frame
#' @param response response variable, factor with level of 'neg' and 'pos'
#' @param marker1 marker1 variable, factor with 2 levels and 1st level is negative
#' @param marker2 marker2 variable, factor with 2 levels and 1st level is negative
.dm_reponse_4quad_core <- function(data, response, marker1, marker2,na.rm=T){
  # check input
  assert_that(class(data[[response]]) == "factor",
              nlevels(data[[response]]) == 2,
              msg = "response should be factors with 2 levels")
  assert_that(all(levels(data[[response]]) == c("neg","pos")),
              msg = "response level should be ['neg','pos']")
  assert_that(class(data[[marker1]]) == "factor",
              nlevels(data[[marker1]]) == 2,
              msg = "marker1 should be factors with 2 levels")
  assert_that(class(data[[marker2]]) == "factor",
              nlevels(data[[marker2]]) == 2,
              msg = "marker2 should be factors with 2 levels")
  # prep data
  if(na.rm){
    data %<>% drop_na(!!sym(response), !!sym(marker1), !!sym(marker2))
  }
  data$.quadrant <- .label_quadrant(data[[marker1]], data[[marker2]])
  total.n <- data %>% dplyr::group_by(.quadrant, .drop=F) %>% dplyr::count() %>% pull(n)
  names(total.n) <- c("R1","R2","R3","R4")
  pos.n <- data %>% dplyr::filter(!!sym(response) %in% "pos") %>%
    dplyr::group_by(.quadrant, .drop=F) %>% dplyr::count() %>% dplyr::pull(n)
  names(pos.n) <- c("R1","R2","R3","R4")
  # stats
  stats.4quad <- .quadrant_stats_response(pos.n, total.n)
  # test
  #test.4quad <- .quadrant_test_response(x =pos.n, n = total.n)
  list(pos.n= pos.n, total.n = total.n,
       stats.4quad = stats.4quad)
       #test.4quad =test.4quad)
}

#' Four quadrant analysis for response
#'
#' evaluate the logistic regression of dual marker
#'
#' @param data data frame
#' @param response response variable
#' @param response.pos positive value(s) of response variable
#' @param response.neg negative value(s) of response variable, default NULL, i.e. all values except positive value(s)
#' @param marker1 marker1 variable
#' @param marker2 marker2 variable
#' @param m1.num.cut cut method/value(s) if marker1 is numeric
#' @param m1.cat.pos positive value(s) if marker1 is categorical variable
#' @param m1.cat.neg negative value(s) if marker1 is categorical variable
#' @param m2.num.cut cut method/value(s) if marker2 is numeric
#' @param m2.cat.pos positive value(s) if marker2 is categorical variable
#' @param m2.cat.neg negative value(s) if marker2 is categorical variable
#' @param na.rm remove NA, default TRUE
#' @return list of 'pos.n', 'total.n', 'stats', 'test', 'param'
dm_response_4quad <- function(data, response, response.pos, response.neg=NULL,
                         marker1, marker2,
                         m1.num.cut = "median", m1.cat.pos = NULL, m1.cat.neg = NULL,
                         m2.num.cut = "median", m2.cat.pos = NULL, m2.cat.neg = NULL,
                         na.rm=T){
  # prep .response
  data$.response <- binarize_cat(x = data[[response]],
                                pos = response.pos, neg = response.neg)
  # prep .m1
  res <- binarize_data(x = data[[marker1]],
                     num.cut = m1.num.cut,
                     cat.pos = m1.cat.pos, cat.neg = m1.cat.neg)
  data$.m1 <- res$data
  cutpoint.m1 <- res$cutpoint

  # prep .m2
  res <- binarize_data(x = data[[marker2]],
                       num.cut = m2.num.cut,
                      cat.pos = m2.cat.pos, cat.neg = m2.cat.neg)
  data$.m2 <- res$data
  cutpoint.m2 <- res$cutpoint
  # run 4quadrant analysis
  out <- .dm_reponse_4quad_core(data, response=".response",
                             marker1=".m1", marker2=".m2",na.rm= na.rm)
  #out$data <- data
  out$param <- tibble(response = response,
                      response.pos = toString(response.pos),
                      response.neg = toString(response.neg),
                      m1=marker1, m2 =marker2,
                      cutpoint.m1 = cutpoint.m1,
                      cutpoint.m2 = cutpoint.m2)
  out
}


