% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finance.R
\name{solar_potential_fun}
\alias{solar_potential_fun}
\title{solar_potential_fun}
\usage{
solar_potential_fun(
  day,
  latitude = 53,
  K_max = 0.42,
  K_min = 0.3,
  azimuth_angle = "South",
  phase_K = 0
)
}
\arguments{
\item{day}{day of year}

\item{latitude}{latitude}

\item{K_max}{max value of clearness index default}

\item{K_min}{min value of clearness index default}

\item{azimuth_angle}{roof aspect compass point}

\item{phase_K}{0 of max value occurs in summer, pi/2 if it occurs in winter (default 0)}
}
\value{
hours
}
\description{
solar_potential_fun
}
\examples{
solar_potential_fun(30,53,0.45,0.3)
}
