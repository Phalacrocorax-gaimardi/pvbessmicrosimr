#pv_survey_oo <- read_csv("~/Policy/CAMG/SolarPVReport/PVBESS_calibrater/pv_survey_oo.csv")
#pv_survey <- read_csv("~/Policy/CAMG/SolarPVReport/PVBESS_calibrater/pv_survey.csv")
#bill_values <- read_csv("~/Policy/CAMG/SolarPVReport/PVBESS_calibrater/bills.csv")
#pv_qanda <- read_csv("~/Policy/CAMG/SolarPVReport/PVBESS_calibrater/pv_qanda.csv")
#pv_questions <- read_csv("~/Policy/CAMG/SolarPVReport/PVBESS_calibrater/pv_questions.csv")
#area_model_parameters <- read_csv("~/Policy/CAMG/SolarPVReport/PVBESS_calibrater/area_model_parameters.csv")


#' bess_cost_fun
#'
#' solar battery cost in euros/kWh
#'
#' @param sD scenario parameters dataframe
#' @param yeartime decimal time
#'
#' @return scalar cost in euros/kWh
#' @export
#'
#' @examples
bess_cost_fun <- function(sD,yeartime){

  cost_2010 <- sD %>% dplyr::filter(parameter=="bess_cost_2010") %>% dplyr::pull(value)
  cost_2015 <- sD %>% dplyr::filter(parameter=="bess_cost_2015") %>% dplyr::pull(value)

  cost_2022 <- sD %>% dplyr::filter(parameter=="bess_cost_2022") %>% dplyr::pull(value)
  cost_2025 <- sD %>% dplyr::filter(parameter=="bess_cost_2025") %>% dplyr::pull(value)
  cost_2030 <- sD %>% dplyr::filter(parameter=="bess_cost_2030") %>% dplyr::pull(value)
  cost_2050 <- sD %>% dplyr::filter(parameter=="bess_cost_2050") %>% dplyr::pull(value)
  dplyr::if_else(identical(cost_2025,numeric(0)),
         cost <- approx(x=c(2010.5,2015.5,2022.5,2030.5,2050.5), y=c(cost_2010,cost_2015,cost_2022,cost_2030,cost_2050),xout=yeartime,rule=2)$y,
         cost <- approx(x=c(2010.5,2015.5,2022.5,2025.5,2030.5,2050.5), y=c(cost_2010,cost_2015,cost_2022,cost_2025,cost_2030,cost_2050),xout=yeartime,rule=2)$y
  )
  return(cost)
}

#' bess_labour_cost_fun
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return scalar cost in euros
#' @export
#'
#' @examples bess_labour_cost_fun(sD,2025)
bess_labour_cost_fun <- function(sD,yeartime){

  values <- sD %>% dplyr::filter(parameter %in% c("bess_labour_cost_2015","bess_labour_cost_2025","bess_labour_cost_2030")) %>% dplyr::pull(value)
  approx(x=c(2015.5,2025.5,2030.5), y=values,xout=yeartime,rule=2)$y %>% return()
}

#' pv_cost_fun
#'
#' solar pv cost in euros/kWp
#'
#' @param sD scenario parameters dataframe
#' @param yeartime decimal time
#'
#' @return scalar cost in euros/kWh
#' @export
#'
#' @examples pv_cost_fun(sD,2026)
pv_cost_fun <- function(sD,yeartime){

  values <- sD %>% dplyr::filter(parameter %in% c("pv_cost_2010","pv_cost_2015","pv_cost_2022","pv_cost_2025","pv_cost_2030","pv_cost_2050")) %>% dplyr::pull(value)
  approx(x=c(2010.5,2015.5,2022.5,2025.5,2030.5,2050.5), y=values,xout=yeartime,rule=2)$y %>% return()
}

#' pv_install_cost_fun
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return scalar cost in euros
#' @export
#'
#' @examples
pv_install_cost_fun <- function(sD,yeartime){

  cost_2010 <- sD %>% dplyr::filter(parameter=="pv_install_cost_2010") %>% dplyr::pull(value)
  cost_2015 <- sD %>% dplyr::filter(parameter=="pv_install_cost_2015") %>% dplyr::pull(value) #inverter cost add more costs here if known

  cost_2022 <- sD %>% dplyr::filter(parameter=="pv_install_cost_2022") %>% dplyr::pull(value)
  cost_2030 <- sD %>% dplyr::filter(parameter=="pv_install_cost_2030") %>% dplyr::pull(value)
  cost_2050 <- sD %>% dplyr::filter(parameter=="pv_install_cost_2050") %>% dplyr::pull(value)
  cost <- approx(x=c(2010.5,2015.5,2022.5,2030.5,2050.5), y=c(cost_2010,cost_2015,cost_2022,cost_2030,cost_2050),xout=yeartime,rule=2)$y
  return(cost)
}

#' evening_tariff_fun
#'
#' actual (currenly to mid 2023) and projected path of electricity prices. Data from seai_elec. For inflation expectations see electricity_price_inflation_fun.
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return price per kWh in euros
#' @export
#'
#' @examples evening_tariff_fun(sD,2029)
evening_tariff_fun <- function(sD,yeartime){

  seai_elec1 <- seai_elec %>% dplyr::filter(year >=2008) #add more costs here if known
  #cost_2022 <- sD %>% dplyr::filter(parameter=="electricity_price_2022") %>% dplyr::pull(value)
  values <- sD %>% dplyr::filter(parameter %in% c("evening_tariff_2025","evening_tariff_2030","evening_tariff_2050")) %>% dplyr::pull(value)
  approx(x=c(seai_elec1$year,2025,2030,2050)+0.5, y=c(seai_elec1$price/100,values),xout=yeartime,rule=2)$y %>% return()
}

#' day_tariff_fun
#'
#' actual (currenly to mid 2023) and projected path of electricity prices. Data from seai_elec. For inflation expectations see electricity_price_inflation_fun.
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return price per kWh in euros
#' @export
#'
#' @examples day_tariff_fun(sD,2030)
day_tariff_fun <- function(sD,yeartime){

  seai_elec1 <- seai_elec %>% dplyr::filter(year >=2008) #add more costs here if known
  #cost_2022 <- sD %>% dplyr::filter(parameter=="electricity_price_2022") %>% dplyr::pull(value)
  values <- sD %>% dplyr::filter(parameter %in% c("day_tariff_2025","day_tariff_2030","day_tariff_2050")) %>% dplyr::pull(value)
  approx(x=c(seai_elec1$year+0.5,2025.5,2030.5,2050.5), y=c(seai_elec1$price/100,values),xout=yeartime,rule=2)$y %>% return()
}


#' night_tariff_fun
#'
#' Night tariffs. Historical rates are assumed to be 45% of the day rate
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return price per kWh in euros
#' @export
#'
#' @examples
night_tariff_fun <- function(sD,yeartime){

  seai_elec1 <- seai_elec %>% dplyr::filter(year >=2008) #add more costs here if known
  values <- sD %>% dplyr::filter(parameter %in% c("night_tariff_2025","night_tariff_2030","night_tariff_2050")) %>% dplyr::pull(value)
  approx(x=c(seai_elec1$year+0.5,2025.5,2030.5,2050.5), y=c(0.45*seai_elec1$price/100,values),xout=yeartime,rule=2)$y %>% return()
}


#' electricity_price_inflation_fun
#'
#' inflation expectations in decimal units. This is used in NPV calculations.
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return electricity inflation expectation in decimal units
#' @export
#'
#' @examples
electricity_price_inflation_fun <- function(sD,yeartime){
  #
  inflate_2010 <- sD %>% dplyr::filter(parameter=="electricity_price_inflation_2010") %>% dplyr::pull(value)
  inflate_2022 <- sD %>% dplyr::filter(parameter=="electricity_price_inflation_2022") %>% dplyr::pull(value)
  inflate_2030 <- sD %>% dplyr::filter(parameter=="electricity_price_inflation_2030") %>% dplyr::pull(value)
  inflate_2050 <- sD %>% dplyr::filter(parameter=="electricity_price_inflation_2050") %>% dplyr::pull(value)
  cost <- approx(c(2010.5,2022.5,2030.5,2050.5), y=c(inflate_2010,inflate_2022,inflate_2030,inflate_2050),xout=yeartime,rule=2)$y
  return(cost)
}

#' FiT_inflation_fun
#'
#' feed-in tariff (ceg) price inflation expectations in decimal units. This is used in NPV calculations.
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return fit inflation expectation in decimal units
#' @export
#'
#' @examples
fit_inflation_fun <- function(sD,yeartime){
  #
  fit_inflate_2022 <- sD %>% dplyr::filter(parameter=="fit_inflation_expectation_2022") %>% dplyr::pull(value)
  fit_inflate_2030 <- sD %>% dplyr::filter(parameter=="fit_inflation_expectation_2030") %>% dplyr::pull(value)
  fit_inflate_2050 <- sD %>% dplyr::filter(parameter=="fit_inflation_expectation_2050") %>% dplyr::pull(value)
  cost <- approx(c(2022.5,2030.5,2050.5), y=c(fit_inflate_2022,fit_inflate_2030,fit_inflate_2050),xout=yeartime,rule=2)$y
  return(0)

}

#' standing_charge_fun
#'
#' household standing charge (expectations & historical)
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return standing charge
#' @export
#'
#' @examples standing_charge_fun(sD,2026)
standing_charge_fun <- function(sD,yeartime){

  values <- sD %>% dplyr::filter(parameter %in% c("standing_charge_2015","standing_charge_2022","standing_charge_2025","standing_charge_2030","standing_charge_2050")) %>% dplyr::pull(value) #add more costs here if known
  approx(x=c(2015.5,2022.5,2025.5,2030.5,2050.5), y=values,xout=yeartime,rule=2)$y %>% return()


}


#' finance_rate_fun
#'
#' historical and future solar PV finance rates. Allows for low interest load subvention.
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return decimal rate (scalar)
#' @export
#'
#' @examples
finance_rate_fun <- function(sD,yeartime){

  fin_2015 <- 0.07 #add more costs here if known
  fin_2021 <- 0.07

  fin_2022 <- sD %>% dplyr::filter(parameter=="finance_rate_2022") %>% dplyr::pull(value)
  fin_2030 <- sD %>% dplyr::filter(parameter=="finance_rate_2030") %>% dplyr::pull(value)
  fin_2050 <- sD %>% dplyr::filter(parameter=="finance_rate_2050") %>% dplyr::pull(value)
  fin <- approx(x=c(2015.5,2021.5,2022.5,2030.5,2050.5), y=c(fin_2015,fin_2021,fin_2022,fin_2030,fin_2050),xout=yeartime,rule=2)$y
  return(fin)


}


#' fit_fun
#'
#' clean export guarantee price euro/kWh to 2022-2050
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return euro/kWh
#' @export
#'
#' @examples
fit_fun <- function(sD,yeartime){

  values <- sD %>% dplyr::filter(parameter %in% c("fit_2022","fit_2025","fit_2030","fit_2050")) %>% dplyr::pull(value)
  approx(x=c(2022.5,2025.5,2030.5,2050.5), y=values,xout=yeartime,rule=2,yleft=0)$y %>% return()
}


#' fit_tax_threshold_fun
#'
#' FiT tax threshold in euros before tax is liable (tax disregard)
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return
#' @export
#'
#' @examples
fit_tax_threshold_fun <- function(sD,yeartime){

  values <- sD %>% dplyr::filter(parameter %in% c("fit_tax_threshold_2025","fit_tax_threshold_2030","fit_tax_threshold_2050")) %>% dplyr::pull(value)
  approx(x=c(2025.5,2030.5,2050.5), y=values,xout=yeartime,rule=2)$y %>% return()
}


#' resilience_premium_fun
#'
#' annual resilience premium (willingness-to-pay) for 1 day of battery storage
#'
#' @param sD scenario
#' @param yeartime year fraction
#'
#' @returns
#' @export
#'
#' @examples resilience_premium_fun(sD,2024)
resilience_premium_fun <- function(sD,yeartime){

  values <- sD %>% dplyr::filter(parameter %in% c("resilience_premium_2025","resilience_premium_2030")) %>% dplyr::pull(value)
  approx(x=c(2025.5,2030.5), y=values,xout=yeartime,rule=2)$y %>% return()
}


#' electricity_demand_factor_fun
#'
#' mean household demand compared to 2010 (year of CER dataset).
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return demand factor
#' @export
#'
#' @examples
electricity_demand_factor_fun <- function(sD,yeartime){

  seai_elec1 <- seai_elec %>% dplyr::filter(year <= 2020) #add more costs here if known
  seai_elec1 <- seai_elec1 %>% dplyr::mutate(factor=kWh/5302)

  fact_2021 <- sD %>% dplyr::filter(parameter=="electricity_demand_factor_2021") %>% dplyr::pull(value)
  fact_2030 <- sD %>% dplyr::filter(parameter=="electricity_demand_factor_2030") %>% dplyr::pull(value)
  fact_2050 <- sD %>% dplyr::filter(parameter=="electricity_demand_factor_2050") %>% dplyr::pull(value)
  fact <- approx(x=c(seai_elec1$year+0.5,2021.5,2030.5,2050.5), y=c(seai_elec1$factor,fact_2021,fact_2030,fact_2050),xout=yeartime,rule=2)$y
  return(fact)

}

#' self_sufficiency_fun
#'
#' self-sufficiency premium vs survey value (< 1)
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return reduction factor (between 0 & 1)
#' @export
#'
#' @examples
self_sufficiency_fun <- function(sD,yeartime){

  fall_year <- sD %>% dplyr::filter(parameter=="self_suff_premium_fall") %>% dplyr::pull(value)
  vanish_year <- sD %>% dplyr::filter(parameter=="self_suff_premium_vanish") %>% dplyr::pull(value)
  fact <- approx(x=c(fall_year,vanish_year), y=c(1,0),xout=yeartime,rule=2)$y
  return(fact)
}


#' seai_grant
#'
#' seai grant for domestic solar installation 2022
#'
#' @param params fast scenario parameters
#' @param s solar capacity in kW
#' @param b battery capacity in kWh
#'
#' @return grant amount in euros
#' @export
#'
#' @examples
seai_grant <- function(params,s,b){

  sol_lower_threshold <- params$sol_lower_threshold
  sol_upper_threshold <- params$sol_upper_threshold
  sol_lower_grant <- params$sol_lower_grant
  sol_upper_grant <- params$sol_upper_grant
  bat_threshold <- params$bess_threshold
  bat_grant <- params$bat_grant

  max_sol_grant <- sol_lower_threshold*sol_lower_grant + (sol_upper_threshold-sol_lower_threshold)*sol_upper_grant

  grant <- dplyr::case_when(s <= sol_lower_threshold~sol_lower_grant*s,
                            s >= sol_upper_threshold & b < bat_threshold~max_sol_grant,
                            s >=sol_upper_threshold & b >= bat_threshold~max_sol_grant + bat_grant,
                            s > sol_lower_threshold & s < sol_upper_threshold~sol_lower_threshold*sol_lower_grant+(s-sol_lower_threshold)*sol_upper_grant)

  grant <- dplyr::case_when( ((params$yeartime >= params$grant_introduction_date) & (params$yeartime <= params$grant_removal_date))~grant,
                             params$yeartime < params$grant_introduction_date~0,
                             params$yeartime > params$grant_removal_date~0)
  return(grant)
}

#' seai_grant_fast
#'
#' The fast version of seai_grant
#'
#' @param params fast scenario parameters
#' @param s solar capacity in kW
#' @param b battery capacity in kWh
#'
#' @return grant amount in euros
#' @export
#'
#' @examples
seai_grant_fast <- function(params, s, b) {
  # Extract parameters
  sol_lower_threshold <- params$sol_lower_threshold
  sol_upper_threshold <- params$sol_upper_threshold
  sol_lower_grant <- params$sol_lower_grant
  sol_upper_grant <- params$sol_upper_grant
  max_sol_grant <- sol_lower_threshold * sol_lower_grant + (sol_upper_threshold - sol_lower_threshold) * sol_upper_grant

  # Precompute time-based conditions
  is_grant_active_pv <- (params$yeartime >= params$grant_introduction_date) & (params$yeartime <= params$pv_grant_removal_date)
  is_grant_active_bess <- (params$yeartime >= params$grant_introduction_date) & (params$yeartime <= params$bess_grant_removal_date)

  # Calculate PV grant using vectorized operations
  pv_grant <- ifelse(s <= sol_lower_threshold, sol_lower_grant * s,
                     ifelse(s > sol_upper_threshold, max_sol_grant,
                            sol_lower_threshold * sol_lower_grant + (s - sol_lower_threshold) * sol_upper_grant))
  pv_grant <- pv_grant * is_grant_active_pv  # Apply time-based condition

  # Calculate BESS grant using vectorized operations
  bess_grant <- ifelse(b >= params$bess_threshold, params$bess_grant, 0)
  bess_grant <- bess_grant * is_grant_active_bess  # Apply time-based condition

  # Return the total grant
  return(pv_grant + bess_grant)
}


pv_system_efficiency <- function(sD,yeartime){

  shockley_quisser <- 0.3316
  inverter_eff <- 0.85
  efficiency_2015 <- 0.16
  eta <- 0.5
  lambda <- (shockley_quisser*inverter_eff/efficiency_2015-1)*(1/(2015-1990))^(-eta)

  shockley_quisser*inverter_eff/(1+lambda*(yeartime-1990)^(-eta))

}

#' kWp_per_m2_fun
#'
#' solar panel efficiency (peak power per m2) at time yeartime
#'
#' @param sD scenario
#' @param yeartime decimal time
#'
#' @return  kWp per m2
#' @export
#'
#' @examples
kWp_per_m2_fun <- function(sD,yeartime){

  values <- sD %>% dplyr::filter(parameter %in% c("kWp_per_m2_2015","kWp_per_m2_2023","kWp_per_m2_2030","kWp_per_m2_2050")) %>% dplyr::pull(value)
  approx(x=c(2015.5,2023.5,2030.5,2050.5), y=values,xout=yeartime,rule=2)$y %>% return()
}


#' survey_bills_to_kwh
#'
#' a function that converts highest and lower bi-monthly bills from survey to daily D_max and D_min assuming
#' a seasonal demand lag_D. D_max and D_min are used to estimate the financial return on pv bess investment.
#' Missing data are imputed by default. An issue that needs to be addressed is "level pay"
#'
#' @param pv_data_in survey data e.g. pv_survey_oo
#' @param lag_D seasonal lag in demand, default 30 days.
#'
#' @returns
#' @export
#'
#' @examples
survey_bills_to_kwh <- function(pv_data_in, lag_D=30){
  #
  #impute missing bills
  pv_data_in$ID <- 1:nrow(pv_data_in)

  complete_data <- pv_data_in %>% dplyr::filter(q14!=13,q15 != 13)
  complete_data <- complete_data %>% dplyr::inner_join(bill_values %>% dplyr::rename("q15"=response_code,"lowest_bill"=bill))
  complete_data <- complete_data %>% dplyr::inner_join(bill_values %>% dplyr::rename("q14"=response_code,"highest_bill"=bill))
  complete_data <- complete_data %>% dplyr::mutate(dplyr::across(dplyr::everything(),as.numeric))
  # regression model relating high and low bills
  high_model <- nls(highest_bill ~ a * lowest_bill + exp(b), start = list(a = 2, b = 1),
                   algorithm = "port", lower = c(0, -Inf), upper = c(5, Inf),data=complete_data)
  low_model <- nls(lowest_bill ~ a * highest_bill + exp(b), start = list(a = 2, b = 1),
                    algorithm = "port", lower = c(0, -Inf), upper = c(5, Inf),data=complete_data)


  #q14 missing but not q15
  missing_high_data <- pv_data_in %>% dplyr::filter(q14==13,q15 != 13)
  missing_high_data <- missing_high_data %>% dplyr::inner_join(bill_values %>% dplyr::rename("q15"=response_code,"lowest_bill"=bill))
  missing_high_data$lowest_bill <- as.numeric(missing_high_data$lowest_bill)
  coefs <- summary(high_model)$coefficients[, "Estimate"]
  std_errors <- summary(high_model)$coefficients[, "Std. Error"]
  missing_high_data$a <- rnorm(nrow(missing_high_data), mean=coefs[1],sd=std_errors[1])
  missing_high_data$b <- rnorm(nrow(missing_high_data), mean=coefs[2],sd=std_errors[2])
  missing_high_data <- missing_high_data %>% dplyr::mutate(highest_bill = a*lowest_bill + exp(b))
  #q15 missing but not q15
  missing_low_data <- pv_data_in %>% dplyr::filter(q14!=13,q15 == 13)
  missing_low_data <- missing_low_data %>% dplyr::inner_join(bill_values %>% dplyr::rename("q14"=response_code,"highest_bill"=bill))
  missing_low_data$highest_bill <- as.numeric(missing_low_data$highest_bill)
  coefs <- summary(low_model)$coefficients[, "Estimate"]
  std_errors <- summary(low_model)$coefficients[, "Std. Error"]
  missing_low_data$a <- rnorm(nrow(missing_low_data), mean=coefs[1],sd=std_errors[1])
  missing_low_data$b <- rnorm(nrow(missing_low_data), mean=coefs[2],sd=std_errors[2])
  missing_low_data <- missing_low_data %>% dplyr::mutate(lowest_bill = a*highest_bill + exp(b))

  #########################################
  #both high and low missing model
  #generate q14 as lognormally distributed
  ###########################################
  missing_both_data <- pv_data_in %>% dplyr::filter(q14==13,q15 == 13) #143 rows
  #model by household profile
  logparams <- complete_data %>% dplyr::group_by(qi) %>% dplyr::summarise(logmean=mean(log(highest_bill)),logsd=sd(log(highest_bill)))
  missing_both_data <- missing_both_data %>% dplyr::inner_join(logparams) %>% dplyr::rowwise() %>% dplyr::mutate(highest_bill=rlnorm(1,logmean,logsd))
  #missing_both_data$highest_bill <- rlnorm(nrow(missing_both_data),logmean,sdmean)
  coefs <- summary(low_model)$coefficients[, "Estimate"]
  std_errors <- summary(low_model)$coefficients[, "Std. Error"]
  missing_both_data$a <- rnorm(nrow(missing_both_data), mean=coefs[1],sd=std_errors[1])
  missing_both_data$b <- rnorm(nrow(missing_both_data), mean=coefs[2],sd=std_errors[2])
  missing_both_data <- missing_both_data %>% dplyr::mutate(lowest_bill = a*highest_bill + exp(b)) %>% dplyr::select(-logmean,-logsd)

  complete_data <- complete_data %>% dplyr::bind_rows(missing_low_data,missing_high_data,missing_both_data) %>% dplyr::select(-a,-b)
  complete_data <- complete_data %>% dplyr::arrange(ID)
  #flip bills of miscreants where highest_bill < lowest_bill
  complete_data <- complete_data %>% dplyr::mutate(temp_high= dplyr::if_else(lowest_bill > highest_bill, lowest_bill,highest_bill))
  complete_data <- complete_data %>% dplyr::mutate(temp_low = dplyr::if_else(lowest_bill > highest_bill, highest_bill,lowest_bill))
  complete_data <- complete_data %>% dplyr::select(-highest_bill,-lowest_bill) %>% dplyr::rename("highest_bill" = temp_high, "lowest_bill" = temp_low)

  e_price_2023 <- seai_elec %>% dplyr::filter(year==2023) %>% dplyr::pull(price)/100*1.15 #15% correction for credits
  complete_data <- complete_data %>% dplyr::mutate(lowest_kwh=lowest_bill/e_price_2023,highest_kwh=highest_bill/e_price_2023)

  complete_data <- complete_data %>% tidyr::drop_na() %>% dplyr::rowwise() %>% dplyr::mutate(params = list(get_demand_params(highest_kwh, lowest_kwh,lag_D))) %>%
    dplyr::ungroup() %>% tidyr::unnest_wider(params)
  #model annual demand
  complete_data <- complete_data %>% dplyr::rowwise() %>% dplyr::mutate(annual_kwh=sum(demand_fun(1:365,D_max,D_min)))

  complete_data %>% dplyr::arrange(ID) %>% return()
}


#' get_demand_params
#'
#' utility function used by
#'
#' @param highest_kwh highest kWh bi-monthly usage inferred from 2023 bills
#' @param lowest_kwh lowest kWh bi-monthly usage inferred from 2023 bills
#' @param lag_D demand seasonal lag default 30 days
#'
#' @returns
#' @export
#'
#' @examples
get_demand_params <- function(highest_kwh,lowest_kwh,lag_D = 30){

  #if(lowest_kwh > highest_kwh) stop("lowest kWh is greater than highest kWh")
  max_kwh <- function(D) sum(demand_fun(1:61,D_max=D[1],D_min=D[2],lag_D)) #winter
  min_kwh <- function(D) sum(demand_fun(183:243,D_max=D[1],D_min=D[2],lag_D)) #summer
  obj_fun <- function(D)(lowest_kwh-min_kwh(D))^2 + (highest_kwh-max_kwh(D))^2
  #optim(c(10,10), obj_fun,method="L-BFGS-B", lower=c(0,0),upper=c(Inf,Inf))
  solution <- nloptr::nloptr(c(20,20),obj_fun, lb=c(0,0), ub=c(100,100),opts=list(algorithm="NLOPT_LN_BOBYQA",maxeval=1000))$solution
  names(solution) <- c("D_max","D_min")
  solution %>% return()
  }

#get_demand_params_from_survey <- Vectorize(get_demand_params_from_survey, c(lowest_kwh,highest_kwh))

pv_labour_cost_fun <- function(sD, yeartime) {
  # Filter once and extract values
  costs <- sD %>% dplyr::filter(parameter %in% c("pv_labour_2015", "pv_labour_2025", "pv_labour_2030")) %>% dplyr::pull(value)

  # Perform interpolation
  approx(x = c(2015.5, 2025.5, 2030.5),
         y = costs,
         xout = yeartime,
         rule = 2)$y
}

pv_inverter_cost_fun <- function(sD, yeartime) {
  # Filter once and extract values
  costs <- sD %>% dplyr::filter(parameter %in% c("pv_inverter_2015", "pv_inverter_2025", "pv_inverter_2030")) %>% dplyr::pull(value)

  # Perform interpolation
  approx(x = c(2015.5, 2025.5, 2030.5),
         y = costs,
         xout = yeartime,
         rule = 2)$y
}
