% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dm_response_logistic.R
\name{dm_logit}
\alias{dm_logit}
\title{Logistic regression for dual marker}
\usage{
dm_logit(
  data,
  response,
  response.pos,
  response.neg = NULL,
  marker1,
  marker2,
  covariates = NULL,
  m1.binarize,
  m2.binarize,
  m1.num.cut = "median",
  m1.cat.pos = NULL,
  m1.cat.neg = NULL,
  m2.num.cut = "median",
  m2.cat.pos = NULL,
  m2.cat.neg = NULL,
  auc = T
)
}
\arguments{
\item{data}{data.frame}

\item{response}{response variables}

\item{response.pos}{positive values of response}

\item{response.neg}{negative values of response}

\item{marker1}{marker1 variable}

\item{marker2}{marker2 variable}

\item{m1.num.cut}{cut method/value(s) if marker1 is numeric}

\item{m1.cat.pos}{positive value(s) if marker1 is categorical}

\item{m1.cat.neg}{negative value(s) if marker1 is categorical}

\item{m2.num.cut}{cut method/value(s) if marker2 is numeric}

\item{m2.cat.pos}{positive value(s) if marker2 is categorical}

\item{m2.cat.neg}{negative value(s) if marker2 is categorical}
}
\value{
summary of dual marker logistic regression
}
\description{
Evaluate the logistic regression of dual marker
}
