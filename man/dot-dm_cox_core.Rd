% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dm_survival_cox.R
\name{.dm_cox_core}
\alias{.dm_cox_core}
\title{dm_cox_core}
\usage{
.dm_cox_core(
  data,
  time,
  event,
  marker1,
  marker2,
  covariates = NULL,
  cindex = T
)
}
\arguments{
\item{data}{dataframe}

\item{marker1}{marker1}

\item{marker2}{marker2}
}
\value{
stats
}
\description{
Evaluate the logistic regression of dual marker
}
