
#sD <- readxl::read_xlsx("~/Policy/CAMG/SolarPVReport/PVBESS_microsimr/scenario_parameters.xlsx", sheet="scenario_BASE")
#seai_elec <- read_csv("~/Policy/CAMG/SolarPVReport/PVBESS_microsimr/electricity_household_demand_price_SEAI.csv")
#solar_potentials <- read_csv("~/Policy/CAMG/SolarPVReport/PVBESS_microsimr/solar_potentials_30degreepitch_pvwatts.csv")


#' scenario_params
#'
#' builds the complete parameter set at yeartime from scenario sD
#'
#' @param sD scenario parameters e.g. scenario_0
#' @param yeartime decimal time
#'
#' @return long form dataframe containing parameter names and values
#' @export
#'
#' @examples
scenario_params <- function(sD,yeartime){
  #fast params
  scen <- tibble::tibble(parameter="yeartime", value=  yeartime)
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="battery_cost", value=  bess_cost_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="battery_install_cost", value=  bess_install_cost_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="pv_cost", value=  pv_cost_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="pv_install_cost", value=  pv_install_cost_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="pvbess_cost_synergy",  value=dplyr::filter(sD, parameter=="pvbess_cost_synergy")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="day_tariff", value =  day_tariff_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="evening_tariff", value =  evening_tariff_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="night_tariff", value =  night_tariff_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="e_price_inflation", value =  electricity_price_inflation_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="fit_inflation", value =  fit_inflation_fun(sD,yeartime)))
  #scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="fit_price_inflation", value =  0))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="standing_charge", value =  standing_charge_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="fit", value =  fit_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="fit_tax_threshold", value =  fit_tax_threshold_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="resilience_premium", value =  resilience_premium_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="marginal_tax_rate", value =  dplyr::filter(sD, parameter=="marginal_tax_rate")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="marginal_tax_rate", value =  dplyr::filter(sD, parameter=="marginal_tax_rate")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="finance_rate", value =  finance_rate_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="term_of_loan", value =  dplyr::filter(sD, parameter=="term_of_loan")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="discount_rate", value =  dplyr::filter(sD, parameter=="discount_rate")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="system_lifetime", value =  dplyr::filter(sD, parameter=="system_lifetime")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="e_demand_factor", value =  electricity_demand_factor_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="sol_lower_threshold", value =  dplyr::filter(sD, parameter=="sol_lower_threshold")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="sol_upper_threshold", value =  dplyr::filter(sD, parameter=="sol_upper_threshold")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="sol_lower_grant", value =  dplyr::filter(sD, parameter=="sol_lower_grant")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="sol_upper_grant", value =  dplyr::filter(sD, parameter=="sol_upper_grant")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="sol_lower_threshold", value =  dplyr::filter(sD, parameter=="sol_lower_threshold")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="grant_introduction_date", value =  dplyr::filter(sD, parameter=="grant_introduction_date")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="grant_removal_date", value =  dplyr::filter(sD, parameter=="grant_removal_date")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="bat_threshold", value =  dplyr::filter(sD, parameter=="bat_threshold")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="bat_grant", value =  dplyr::filter(sD, parameter=="bat_grant")$value))
  #scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="self_sufficiency_effect", value =  self_sufficiency_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="kWp_per_m2", value =  kWp_per_m2_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="usable_roof_fraction", value =  dplyr::filter(sD, parameter=="usable_roof_fraction")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="mean_shading_factor", value =  dplyr::filter(sD, parameter=="mean_shading_factor")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="latitude", value =  dplyr::filter(sD, parameter=="latitude")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="K_max", value =  dplyr::filter(sD, parameter=="K_max")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="K_min", value =  dplyr::filter(sD, parameter=="K_min")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="lag_D", value =  dplyr::filter(sD, parameter=="lag_D")$value))
 # scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="rho_solstice", value =  dplyr::filter(sD, parameter=="rho_solstice")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="acceleration_factor", value =  acceleration_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="beta.", value =  dplyr::filter(sD, parameter=="beta.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="lambda.", value =  dplyr::filter(sD, parameter=="lambda.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="p.", value =  dplyr::filter(sD, parameter=="p.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="nu.", value =  dplyr::filter(sD, parameter=="nu.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="rho.", value =  dplyr::filter(sD, parameter=="rho.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="delta.", value =  dplyr::filter(sD, parameter=="delta.")$value))


  #return(scen)
  return(scen %>% fast_params())
}

#' fast_params
#'
#' helper function to convert a long format dataframe to an environment object, used for fast access to scenario parameters
#'
#' @param params_long long format dataframe with columns "parameter" and "value"
#'
#' @return environment object
#' @export
#'
#' @examples
fast_params <- function(params_long){

  test <- as.list(params_long$value)
  names(test) <- params_long$parameter
  test <- list2env(test)
  return(test)
}

#' gen_roofsection_solar_area
#'
#' generate a half rooftop solar area potential stochastically (log normal distribution). This is calibrated to BER data up to 2023 separately for
#' bungalows and two-storey dwellings. Bungalows have a higher solar potential because of their relatively larger footprint. This function uses
#' a detailed model based on house location (region), housing_type (q1) and number of storeys (1 or 2).
#'
#' @param region region 1=Dublin 2= rest of Leinster 3= Munster 4= Ulster/Connaught
#' @param house_type 1=flat 2= terraced 3= semi 4= detached 5=other
#' @param stories number of stories
#' @param bedrooms number of bedrooms
#' @param house_period construction year integer
#' @param roof_floor_ratio 2/sqrt(3) for 30 degree pitch
#' @param usable_roof_fraction fraction of rooftop usable for solar PV default 0.7
#'
#' @return potential half-roof area for solar pv in m2
#' @export
gen_roofsection_solar_area <- function (region=1,house_type=1,stories=1,bedrooms=2,house_period=6, roof_floor_ratio = 2/sqrt(3), usable_roof_fraction = 0.7) {
  #
  house_period <- ifelse(house_period==7,sample(c(1,2,3),1),house_period) #don't knows are likely to be older houses

  q1_0 <- pv_qanda %>% dplyr::filter(question_code=="q1",response_code==house_type) %>% dplyr::pull(response)
  qc2_0 <- pv_qanda %>% dplyr::filter(question_code=="qc2",response_code==region) %>% dplyr::pull(response)

  df <- area_model_parameters %>% dplyr::filter(q1==q1_0,q2==stories,qc2==qc2_0,q5==house_period,q3==bedrooms)
  if(nrow(df)==0) df <- df <- area_model_parameters %>% dplyr::filter(q1==q1_0,q2==stories,qc2==qc2_0,q5==5,q3==bedrooms)
  mu <- df  %>% dplyr::pull(mu)
  sigma <- df %>% dplyr::pull(sigma)
  ceiling_area <- exp(rnorm(1,mean=mu,sd=sigma))
  solar_area <- ceiling_area * roof_floor_ratio * usable_roof_fraction/2
  return(solar_area)
}

#' gen_microcalibration_ensemble
#'
#' generates a calibrated abm N-member ensemble (model empirical utils and weights) based on functions defined
#' in pvbesscalibrater. The utilities and weights are indexed according to the calibration run.
#'
#'
#' @param N number of independent calibration runs
#' @param regularisation parameter controlling presence of negative weights 1= full, 0= none
#' @param epsilon initial hypothetical bias correction,1= no bias correction, default = 0.7
#'
#' @returns a two-component list of the empirical utilities and weights
#' @export
#'
#' @examples
gen_microcalibration_ensemble <- function(N,regularisation=1,epsilon=0.7){

  pv_data <- pvbesscalibrater::feature_select(pv_survey_oo,recode_bills=F,drop_lowestbills = T)
  pv_data <- pvbesscalibrater::transform_to_utils(pv_data,epsilon=0.7)

  utils_oo <- tibble::tibble()
  model_weights <- tibble::tibble()

  for(i in 1:N){

    bst <- pvbesscalibrater::get_boosted_tree_model(pv_data)
    shap_scores_long <- pvbesscalibrater::get_shap_scores(pv_data,bst)

    empirical_util <- pvbesscalibrater::get_empirical_partial_utilities(shap_scores_long, regularisation = 1)
    empirical_util$calibration <- i
    utils_oo <- utils_oo %>% dplyr::bind_rows(empirical_util)

    weights <- pvbesscalibrater::get_model_weights(shap_scores_long, regularisation = 1)
    weights$calibration <- i
    model_weights <- model_weights %>% dplyr::bind_rows(weights)

  }

  return(list(utils_oo,model_weights))

}

#test <- gen_model_ensemble(100)
#test[[1]] %>% filter(question_code != "theta") %>% ggplot(aes(response_code,du_average,colour=factor(calibration)))+geom_point()+geom_line() + facet_wrap(.~question_code,scales="free_x")




#' acceleration_fun
#'
#' accelerated solar PV "ideation" multiplier for agents. The 2010-2022 calibrated rate p. is replaced p. * acceleration_fun to allow for the impact of advertisinga campaigns etc.
#' where acceleration = 1 for yeartime < 2023.
#'
#' @param sD scenario
#' @param yeartime decimal time
#'
#' @return
#' @export
#'
#' @examples
acceleration_fun <- function(sD,yeartime){

  value_2025 <- sD %>% dplyr::filter(parameter=="acceleration_factor_2025") %>% dplyr::pull(value)
  value_2030 <- sD %>% dplyr::filter(parameter=="acceleration_factor_2030") %>% dplyr::pull(value)
  value <- approx(x=c(2022.5,2025.5,2030.5), y=c(1,value_2025,value_2030),xout=yeartime,rule=2)$y
  return(value)
}

#' roundr
#'
#' stochastic round
#'
#' @param x real number to be rounded up or down
#'
#' @return integer
#' @export
#'
#' @examples
roundr <- function(x){
  x1 <- trunc(x)
  weights = c(1+x1-x,x-x1)
  return(sample(c(x1,x1+1),size=1,prob=weights))
}




#' solar_round_down
#'
#' round solar installation capacity down to increment of panel capacity
#'
#' @param x optiumum S
#' @param solar_increment default 0.25
#'
#' @returns capacity in kWh
#' @export
#'
#' @examples
solar_round_down <- function(x,solar_increment=0.25) {
  floor(x / solar_increment) * solar_increment
}

#' bess_round
#'
#' round to nearest multiple of bess_increment
#'
#' @param x optimum storage
#' @param bess_increment minimum bess increment
#'
#' @returns
#' @export
#'
#' @examples
bess_round  <- function(x, bess_increment=2.5) {
  round(x / bess_increment) * bess_increment
}

