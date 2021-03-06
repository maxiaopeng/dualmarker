% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dm_response_4quadrants_chart.R
\name{dm_response_4quad_chart}
\alias{dm_response_4quad_chart}
\title{Quadrant response chart}
\usage{
dm_response_4quad_chart(
  x,
  n,
  title = "",
  label.m1.pos = "pos",
  label.m1.neg = "neg",
  label.m2.pos = "pos",
  label.m2.neg = "neg",
  label.m1 = "Marker1",
  label.m2 = "Marker2",
  palette = "default",
  palette.4quadrant = "default"
)
}
\arguments{
\item{x}{positive number in 1st,2nd,3rd and 4th quadrant}

\item{n}{total number in 1st,2nd,3rd and 4th quadrant}
}
\value{
Panel of 4 ggplot plots
}
\description{
panel of plots, showing per quadrant: 1) sample size using proportion chart,
2) response rate using matrix chart and 3) donut chart, response difference
between quadrant using 4) line chart
}
