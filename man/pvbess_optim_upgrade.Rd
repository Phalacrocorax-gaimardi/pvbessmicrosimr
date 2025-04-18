% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finance.R
\name{pvbess_optim_upgrade}
\alias{pvbess_optim_upgrade}
\title{pvbess_optim_upgrade}
\usage{
pvbess_optim_upgrade(
  S1_old,
  S2_old,
  B_old,
  capex_old,
  aspect,
  solar_constraint_1,
  solar_constraint_2,
  shading_factor_1,
  shading_factor_2,
  D_max,
  D_min,
  params,
  tariff_plan = "night_saver"
)
}
\arguments{
\item{S1_old}{old primary roof capacity}

\item{S2_old}{old secondary roof capacity}

\item{B_old}{old BESS}

\item{capex_old}{the existing annualised investment cost before the upgrade}

\item{aspect}{aspect}

\item{solar_constraint_1}{additional primary roof capacity}

\item{solar_constraint_2}{additional secondary roof capacity}

\item{shading_factor_1}{shading factor 1}

\item{shading_factor_2}{shading factor 2}

\item{D_max}{winter average daily max}

\item{D_min}{summer average daily min}

\item{params}{params}

\item{tariff_plan}{tariff plan, default "night-saver"}
}
\value{
data frame
}
\description{
pvbess_optim_upgrade
}
