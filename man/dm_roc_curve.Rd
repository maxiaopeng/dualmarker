% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dm_response_roc.R
\name{dm_roc_curve}
\alias{dm_roc_curve}
\title{plot ROC curve for dual markers}
\usage{
dm_roc_curve(
  data,
  response,
  response.pos,
  response.neg = NULL,
  marker1,
  marker2,
  m1.cat.pos = NULL,
  m1.cat.neg = NULL,
  m2.cat.pos = NULL,
  m2.cat.neg = NULL,
  logit.reg = TRUE,
  logit.reg.int = FALSE,
  palette = "default"
)
}
\arguments{
\item{data}{data.frame}

\item{response}{response}

\item{response.pos}{positive values of response}

\item{response.neg}{negative values of response, default NULL, i.e.g all values except response.pos}

\item{marker1}{marker1}

\item{marker2}{marker2}

\item{m1.cat.pos}{if m1 is categorical variable, the positve values}

\item{m1.cat.neg}{if m1 is categorical variable, the negative values}

\item{m2.cat.pos}{if m2 is categorical variable, the positve values}

\item{m2.cat.neg}{if m2 is categorical variable, the negative values}

\item{logit.reg}{add logistic regression curve, no interaction, default TRUE}

\item{logit.reg.int}{add logistic regression(with interaction) curve, default FALSE}
}
\value{
ROC curve, ggplot object
}
\description{
draw ROC curve for marker1, marker2 and fitted values by logistic regression w/ or w/o interaction between two markers
}
