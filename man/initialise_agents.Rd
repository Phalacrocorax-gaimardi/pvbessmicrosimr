% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abm.R
\name{initialise_agents}
\alias{initialise_agents}
\title{initialise_agents}
\usage{
initialise_agents(sD, yeartime, cal_run)
}
\arguments{
\item{sD}{scenario (usable_roof_fraction only)}

\item{yeartime}{start year (default 2010)}

\item{cal_run}{calibration run number between 1 and 100}
}
\value{
a dataframe with columns
}
\description{
creates the agent initial state including model weights, randomised available rooftop areas for solar PV and a mapping of survey agents to CER dataset. It is assumed that solar pv systems are absent at yeartime e.g. 2010
}
