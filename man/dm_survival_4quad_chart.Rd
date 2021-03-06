% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dm_survival_4quadrants_chart.R
\name{dm_survival_4quad_chart}
\alias{dm_survival_4quad_chart}
\title{survival plot of 4 quadrants}
\usage{
dm_survival_4quad_chart(
  data,
  time,
  event,
  marker1,
  marker2,
  m1.datatype = "auto",
  m1.num.cut = "median",
  label.m1 = marker1,
  m1.cat.pos = NULL,
  m1.cat.neg = NULL,
  label.m1.pos = NULL,
  label.m1.neg = NULL,
  label.m2 = marker2,
  m2.datatype = "auto",
  m2.num.cut = "median",
  label.m2.pos = NULL,
  label.m2.neg = NULL,
  m2.cat.pos = NULL,
  m2.cat.neg = NULL,
  palette.4quadrant = "default",
  title = ""
)
}
\arguments{
\item{data}{dataframe}

\item{time}{survival time}

\item{event}{survival event}

\item{marker1}{marker1}

\item{marker2}{marker2}

\item{m1.datatype}{data type of marker1}

\item{m1.num.cut}{cut method/value for numeric marker1}

\item{m1.cat.pos}{positive value(s) if marker1 is categorical}

\item{m1.cat.neg}{negative value(s) if marker1 is categorical}

\item{m2.datatype}{data type of marker1}

\item{m2.num.cut}{cut method/value for numeric marker2}

\item{m2.cat.pos}{positive value(s) if marker2 is categorical}

\item{m2.cat.neg}{negative value(s) if marker2 is categorical}
}
\value{
panel of plot
}
\description{
survival plot of 4 quadrants
}
