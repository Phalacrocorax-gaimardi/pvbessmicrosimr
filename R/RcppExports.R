# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' demand_cpp
#'
#' calculate daily expected demand
#'
#' @param day day of year
#' @param D_max max demand in winter
#' @param D_min min demand in winter
#' @param lag_D lag in days default 30
NULL

#' day_length_cpp
#'
#' the length of a day
#'
#' @param day day of year
#' @param latitude latitude
NULL

#' @name seai_grant_cpp
#' @title seai_grant_cpp
#' @description The fast version of seai_grant
#'
#' @param params fast scenario parameters
#' @param s solar capacity in kW
#' @param b battery capacity in kWh
#' @return grant amount in euros
#' @export
#'
#' @examples
NULL

demand_cpp <- function(day, D_max, D_min, lag_D = 30.0) {
    .Call(`_pvbessmicrosimr_demand_cpp`, day, D_max, D_min, lag_D)
}

day_length_cpp <- function(day, latitude) {
    .Call(`_pvbessmicrosimr_day_length_cpp`, day, latitude)
}

daylight_usage_cpp <- function(day, rho_solstice) {
    .Call(`_pvbessmicrosimr_daylight_usage_cpp`, day, rho_solstice)
}

clearness_index_cpp <- function(day, K_max, K_min, phase_K = 0, exponent = 4) {
    .Call(`_pvbessmicrosimr_clearness_index_cpp`, day, K_max, K_min, phase_K, exponent)
}

solar_potential_cpp <- function(day, latitude = 53, K_max = 0.42, K_min = 0.3, azimuth_angle = "South", phase_K = 0) {
    .Call(`_pvbessmicrosimr_solar_potential_cpp`, day, latitude, K_max, K_min, azimuth_angle, phase_K)
}

energy_flows_cpp <- function(S_1, S_2, aspect, shading_factor_1, shading_factor_2, B, D_max, D_min, params) {
    .Call(`_pvbessmicrosimr_energy_flows_cpp`, S_1, S_2, aspect, shading_factor_1, shading_factor_2, B, D_max, D_min, params)
}

seai_grant_cpp <- function(params, s, b) {
    .Call(`_pvbessmicrosimr_seai_grant_cpp`, params, s, b)
}

