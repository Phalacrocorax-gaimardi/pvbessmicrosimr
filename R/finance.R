#' clearness_index_fun
#'
#' describes the reduction in solar potential to the atmosphere and cloudiness as a function of day of year
#'
#' @param day day of year 1..366
#' @param K_max max value of clearness index 0 < K_max < 1
#' @param K_min K_min < K_max
#' @param phase_K phase 0 if K_max occurs in mid summer, pi/2 if K_max occurs in mid winter
#' @param exponent flatness of clearness index in clear months, >= 2. For Dublin airport exponent is about 4.
#'
#' @return real number
#' @export
#'
#' @examples clearness_index_fun(30,0.45,0.3)
clearness_index_fun <- function(day,K_max, K_min,phase_K=0,exponent=4){
  #
  sigma <- dplyr::if_else(K_min < K_max,-1/log(K_min/K_max),Inf)

  K_max*exp(-abs((cos(pi*(day/365)-phase_K))^exponent)/(sigma))

}


#' solar_potential_fun
#'
#' @param day day of year
#' @param latitude latitude
#' @param K_max max value of clearness index default
#' @param K_min min value of clearness index default
#' @param azimuth_angle roof aspect compass point
#' @param phase_K 0 of max value occurs in summer, pi/2 if it occurs in winter (default 0)
#'
#' @return hours
#' @export
#'
#' @examples solar_potential_fun(30,53,0.45,0.3)
solar_potential_fun <- function(day,latitude=53,K_max=0.42,K_min=0.3,azimuth_angle="South",phase_K=0){
  #
  declination <- 23.44*sin(2*pi/365*(day-81))
  cos_psi <- -tan(2*pi*latitude/360)*tan(declination*2*pi/360)
  cos_psi <- dplyr::if_else(abs(cos_psi) > 1,sign(cos_psi),cos_psi)
  day_length <- 24*acos(-tan(2*pi*latitude/360)*tan(declination*2*pi/360))/pi
  daily_irradiance <- 1/pi*cos(2*pi*latitude/360)*cos(2*pi*declination/360)*sqrt(1-cos_psi^2)+(1/pi)*acos(cos_psi)*sin(2*pi*latitude/360)*sin(2*pi*declination/360)
  #total daily irradiance in J/m2
  aspect_factor <- solar_potentials %>% dplyr::filter(azimuth==azimuth_angle) %>% dplyr::pull(potential)/2.5
  return(aspect_factor*24*1367*daily_irradiance/1000*clearness_index_fun(day,K_max,K_min,phase_K,exponent=4))

}


#' demand_fun
#'
#' A simple sinusoidal model of mean daily demand
#'
#' @param day day of year
#' @param D_max maxiumum demand in kWh
#' @param D_min minimum demand in kWh
#' @param lag_D lag (in days) default 30
#'
#' @return daily kWh demand
#' @export
#'
#' @examples demand_fun(30,14,11,30 )
demand_fun <- function(day, D_max,D_min,lag_D=30){

  #the demand function peaks in winter months
  phase_D=lag_D/360*2*pi

  (D_max + D_min)/2 + (D_max-D_min)/2*cos(2*pi*day/365-phase_D) %>% return()
}



#' pvbess_cost_fun
#'
#' annualised capital and operational cost of a household energy system including grid, pvbess and finance costs
#'
#' @param S_1 installed PV capacity primary roof section
#' @param S_2 installed PV capacity secondary roof section
#' @param aspect orientation of building
#' @param shading_factor_1 primary shading factor
#' @param shading_factor_2 secondary shading factor
#' @param B storage capacity
#' @param D_max max demand (winter)
#' @param D_min min demand (summer)
#' @param params parameter object created by scenario_params(sD, yeartime)
#' @param tariff_plan if "24hr" the night rate is set equal to the day  rate
#'
#' @return an annualised cost in euros
#' @export
#'
#' @examples pvbess_cost_fun(5,5,"SW-NE",1,1,5,18,11,scenario_params(sD,2025),"night_saver")
pvbess_cost_fun <- function(S_1,S_2,aspect,shading_factor_1,shading_factor_2,B,D_max,D_min,params, tariff_plan ="night_saver"){
  #
  if(!(tariff_plan %in% c("night_saver","24hr"))) stop("bad tariff plan")
  p_n <- dplyr::if_else(tariff_plan=="24hr",params$day_tariff,params$night_tariff)

  #parameter checks
  #if(p_n > pmin(p_d,p_e)) stop("night rate is too high. Assumption of night charging from grid is violated")
  #if(p_f > pmin(p_d,p_e)) stop("FiT is too high. Self-consumption of solar energy assumption is violated")
  #if(p_d > p_e) stop("unlikely that evening rate is lower than daylight rate")

  df <- energy_flows_cpp(S_1,S_2,aspect,shading_factor_1,shading_factor_2,B,D_max,D_min,mget(ls(params),params) %>% unlist()) %>% tibble::as_tibble()
  operating_cost <- params$evening_tariff*sum(df$evening_imports) + params$day_tariff*sum(df$day_imports) + p_n*sum(df$E_n1+df$E_n2)
  #include fit revenues
  fit_revenue <- params$fit*sum(df$day_exports)
  fit_revenue <- dplyr::if_else(fit_revenue > params$fit_tax_threshold, params$fit_tax_threshold +(1-params$marginal_tax_rate)*(fit_revenue-params$fit_tax_threshold),fit_revenue)
  operating_cost <- operating_cost - fit_revenue
  operating_cost + annualised_system_cost(S_1+S_2,B,params) - params$resilience_premium*B/D_max + params$standing_charge  %>% return()
}

#' pvbess_opex_cost_fun
#'
#' annual operation cost i.e. grid costs (usage + standing charge), FiT, resilience premium
#'
#' @param S_1 installed PV capacity primary roof section
#' @param S_2 installed PV capacity secondary roof section
#' @param aspect orientation of building
#' @param shading_factor_1 primary shading factor
#' @param shading_factor_2 secondary shading factor
#' @param B storage capacity
#' @param D_max max demand (winter)
#' @param D_min min demand (summer)
#' @param params parameter object created by scenario_params(sD, yeartime)
#' @param tariff_plan if "24hr" the night rate is set equal to the day  rate
#'
#' @returns scalar (euros)
#' @export
#'
#' @examples
pvbess_opex_cost_fun  <- function(S_1,S_2,aspect,shading_factor_1,shading_factor_2,B,D_max,D_min,params, tariff_plan ="night_saver"){
  #
  if(!(tariff_plan %in% c("night_saver","24hr"))) stop("bad tariff plan")
  p_n <- dplyr::if_else(tariff_plan=="24hr",params$day_tariff,params$night_tariff)

  df <- energy_flows_cpp(S_1,S_2,aspect,shading_factor_1,shading_factor_2,B,D_max,D_min,mget(ls(params),params) %>% unlist()) %>% tibble::as_tibble()
  operating_cost <- params$evening_tariff*sum(df$evening_imports) + params$day_tariff*sum(df$day_imports) + p_n*sum(df$E_n1+df$E_n2)
  #include fit revenues
  fit_revenue <- params$fit*sum(df$day_exports)
  fit_revenue <- dplyr::if_else(fit_revenue > params$fit_tax_threshold, params$fit_tax_threshold +(1-params$marginal_tax_rate)*(fit_revenue-params$fit_tax_threshold),fit_revenue)
  operating_cost <- operating_cost - fit_revenue
  operating_cost - params$resilience_premium*B/D_max + params$standing_charge  %>% return()
}


#' pvbess_exports_fun
#'
#' annual exports to grid in kWh
#'
#' @param S_1 installed PV capacity primary roof section
#' @param S_2 installed PV capacity secondary roof section
#' @param aspect orientation of building
#' @param shading_factor_1 primary shading factor
#' @param shading_factor_2 secondary shading factor
#' @param B storage capacity
#' @param D_max max demand (winter)
#' @param D_min min demand (summer)
#' @param params parameter object created by scenario_params(sD, yeartime)
#' @param tariff_plan if "24hr" the night rate is set equal to the day  rate
#'
#' @returns scalar (kWh)
#' @export
#'
#' @examples
pvbess_exports_fun  <- function(S_1,S_2,aspect,shading_factor_1,shading_factor_2,B,D_max,D_min,params, tariff_plan ="night_saver"){
  #
  if(!(tariff_plan %in% c("night_saver","24hr"))) stop("bad tariff plan")
  p_n <- dplyr::if_else(tariff_plan=="24hr",params$day_tariff,params$night_tariff)

  df <- energy_flows_cpp(S_1,S_2,aspect,shading_factor_1,shading_factor_2,B,D_max,D_min,mget(ls(params),params) %>% unlist()) %>% tibble::as_tibble()
  sum(df$day_exports) %>% return()
}

#' pvbess_imports_fun
#'
#' annual imports from grid in kWh
#'
#' @param S_1 installed PV capacity primary roof section
#' @param S_2 installed PV capacity secondary roof section
#' @param aspect orientation of building
#' @param shading_factor_1 primary shading factor
#' @param shading_factor_2 secondary shading factor
#' @param B storage capacity
#' @param D_max max demand (winter)
#' @param D_min min demand (summer)
#' @param params parameter object created by scenario_params(sD, yeartime)
#' @param tariff_plan if "24hr" the night rate is set equal to the day  rate
#'
#' @returns scalar (kWh)
#' @export
#'
#' @examples
pvbess_imports_fun  <- function(S_1,S_2,aspect,shading_factor_1,shading_factor_2,B,D_max,D_min,params, tariff_plan ="night_saver"){
  #
  if(!(tariff_plan %in% c("night_saver","24hr"))) stop("bad tariff plan")
  p_n <- dplyr::if_else(tariff_plan=="24hr",params$day_tariff,params$night_tariff)

  df <- energy_flows_cpp(S_1,S_2,aspect,shading_factor_1,shading_factor_2,B,D_max,D_min,mget(ls(params),params) %>% unlist()) %>% tibble::as_tibble()
  sum(df$day_imports) %>% return()
}

#' annualised_system_cost
#'
#' this includes SEAI grant but excludes any resilience premium
#'
#' @param S total installed solar capacity
#' @param B storage capacity
#' @param params parameters created by scenario_params(sD,yeartime)
#' @param upgrade whether this is a pv system upgrade or an initial installation
#'
#' @return annual cost in euros
#' @export
#'
#' @examples
annualised_system_cost <- function(S, B, params, upgrade = FALSE) {

  #
  pv_cost <- ifelse(S > 0, S * (params$pv_margin + params$pv_sbos + params$pv_cost + params$pv_labour), 0)
  bess_cost <- ifelse(B > 0, params$bess_labour_cost + B * params$bess_cost, 0)
  #invert sized by S or 0.5*B whichever is bigger. This assumes a C-rate for Li-ion batteries of 0.5
  inverter_cost <- pmax(S,0.5*B)*params$pv_inverter
  overhead_cost <- ifelse(S > 0 | B > 0, params$overhead,0)

  if (!upgrade) {
    # Grant and synergy for non-upgrade case
    grant <- ifelse(S == 0 & B == 0, 0, seai_grant_fast(params, S, B))
    synergy <- ifelse((S > 0) & (B > 0), params$pvbess_cost_synergy, 0) #probably set to zero
  } else {
    # Adjust costs for upgrade case
    bess_cost <- ifelse(B > 0, B * params$bess_cost + B * ifelse(S > 0, 0, params$pv_inverter), 0)
    grant <- 0
    synergy <- 0
  }

  # Amortize the total cost
  amort(r = params$delta., term = params$system_lifetime) * (pv_cost + bess_cost + inverter_cost + overhead_cost - grant - synergy)
}

#' energy_flows_fast
#'
#' @param S_1 installed solar capacity primary roof
#' @param S_2 installed solar capacity secondary roof
#' @param aspect house orientation
#' @param shading_factor_1 shading factor primary roof section
#' @param shading_factor_2 shading factor secondary roof section
#' @param B installed battery capacity
#' @param D_max max demand
#' @param D_min min demand
#' @param params parameters object created by scenario_params()
#'
#' @returns a tibble (converted from a data table)
#' @export
#'
#' @examples
energy_flows_fast <- function(S_1,S_2, aspect,shading_factor_1=1, shading_factor_2=1,B, D_max = 18, D_min = 11,params) {
  # Create day vector

  if(!(aspect %in% c("South-North","East-West","SW-NE","SE-NW"))) stop("bad house orientation label")
  day <- 1:365
  # Calculate demand, solar potential, and rho
  demand <- demand_cpp(day, D_max, D_min,lag_D = params$lag_D)
  solar_potential_1 <- shading_factor_1*solar_potential_cpp(day, params$latitude, params$K_max, params$K_min,azimuth_angle=stringr::str_split(aspect,"-")[[1]][1])
  solar_potential_2 <- shading_factor_2*solar_potential_cpp(day, params$latitude, params$K_max, params$K_min,azimuth_angle=stringr::str_split(aspect,"-")[[1]][2])

  rho <- daylight_usage_cpp(day, rho_solstice=params$rho.)


  # Create data.table
  dt <- data.table(
    day = day,
    demand = demand,
    solar_potential_1 = solar_potential_1,
    solar_potential_2 = solar_potential_2,
    rho = rho
  )

  # Add new columns using :=
  dt[, d_tilde := (1 - rho) * demand]
  dt[, sh_tilde := S_1 * solar_potential_1 + S_2 * solar_potential_2 - rho * demand]
  dt[, E_s := pmax(0, pmin(B, sh_tilde, d_tilde))]
  dt[, E_n1 := pmax(0, pmin(B, -sh_tilde))]
  dt[, E_n2 := data.table::fifelse(
    sh_tilde < 0,
    pmin(B - E_n1, d_tilde),
    pmin(B, d_tilde) - E_s
  )]

  # Add flag columns using :=
  dt[, f1 := as.integer(d_tilde - E_s - E_n2 > 0)]
  dt[, f2 := as.integer(-sh_tilde - E_n1 > 0)]
  dt[, f3 := as.integer(sh_tilde - E_s > 0)]

  # Add import/export columns using :=
  dt[, evening_imports := (d_tilde - E_s - E_n2) * f1]
  dt[, day_imports := (-sh_tilde - E_n1) * f2]
  dt[, day_exports := (sh_tilde - E_s) * f3]

  # Return as tibble
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("The 'tibble' package is required but not installed.")
  }
  return(tibble::as_tibble(dt))
}

energy_flows_faster <- function(S_1, S_2, aspect, shading_factor_1 = 1, shading_factor_2 = 1, B, D_max = 18, D_min = 11, params) {
  if (!(aspect %in% c("South-North", "East-West", "SW-NE", "SE-NW")))
    stop("bad house orientation label")

  # Extract azimuth angles
  azimuth_angles <- unlist(strsplit(aspect, "-"))

  # Create day vector
  day <- 1:365

  # Calculate demand, solar potential, and rho
  demand <- demand_fun(day, D_max, D_min, lag_D = params$lag_D)
  solar_potential_1 <- shading_factor_1 * solar_potential_fun(day, params$latitude, params$K_max, params$K_min, azimuth_angle = azimuth_angles[1])
  solar_potential_2 <- shading_factor_2 * solar_potential_fun(day, params$latitude, params$K_max, params$K_min, azimuth_angle = azimuth_angles[2])
  rho <- daylight_usage_fun(day, rho_solstice = params$rho.)

  # Create data.table
  dt <- data.table(day, demand, solar_potential_1, solar_potential_2, rho)

  # Compute sh_tilde and d_tilde first
  dt[, sh_tilde := S_1 * solar_potential_1 + S_2 * solar_potential_2 - rho * demand]
  dt[, d_tilde := (1 - rho) * demand]

  # Compute energy flows separately
  dt[, E_s := pmax(0, pmin(B, sh_tilde, d_tilde))]
  dt[, E_n1 := pmax(0, pmin(B, -sh_tilde))]
  dt[, E_n2 := data.table::fifelse(sh_tilde < 0, pmin(B - E_n1, d_tilde), pmin(B, d_tilde) - E_s)]

  # Compute flags separately
  dt[, f1 := as.integer(d_tilde - E_s - E_n2 > 0)]
  dt[, f2 := as.integer(-sh_tilde - E_n1 > 0)]
  dt[, f3 := as.integer(sh_tilde - E_s > 0)]

  # Compute imports/exports separately
  dt[, evening_imports := (d_tilde - E_s - E_n2) * f1]
  dt[, day_imports := (-sh_tilde - E_n1) * f2]
  dt[, day_exports := (sh_tilde - E_s) * f3]

  # Return as tibble
  return(tibble::as_tibble(dt))
}


#' lcoe_pv
#'
#' levelised cost of pvbess based on annualised system cost calculated using annualised_system_cost
#'
#' @param S installed capacity
#' @param mean_daily_potential annual average solar potentials (hours)
#' @param mu annualised capital cost of solar per kW installed (may include grant)
#'
#' @return euros/kWh
#' @export
#'
#' @examples lcoe_pv(4,2.5,40)
lcoe_pv <- function(S,mean_daily_potential,mu){

  annual_generation <- S*365*mean_daily_potential #kWh
  cost_per_kWh <- mu/annual_generation
  return(cost_per_kWh)
}

#' lcos_bess
#'
#' levelised cost of storage
#'
#' This is function of B
#'
#' @param B bess capacity (kWh)
#' @param lambda annualised cost (B kWh)
#'
#' @return
#' @export
#'
#' @examples
lcos_bess <- function(B,lambda){

  annual_storage <- B*365 #annual kWh stored by B kWh battery charged daily
  cost_per_kWh <- lambda/annual_storage #euro/kWh
  return(cost_per_kWh)
}

#' day_length_fun
#'
#' daylight hours as a function of day of year and latitude
#'
#' @param day day of year (1 .. 365)
#' @param latitude latitude angle in degrees
#'
#' @return hours
#' @export
#'
#' @examples day_length_fun(1,53)
day_length_fun <- function(day,latitude){

  declination <- 23.44 * sin(2 * pi/365 * (day - 81))
  day_length <- 24 * acos(-tan(2 * pi * latitude/360) * tan(declination *
                                                              2 * pi/360))/pi
  day_length %>% return()

}


#' amort
#'
#' @param r interest rate
#' @param term term
#'
#' @return annual amortisation payment for unit principal
#' @export
#'
#' @examples amort(0.07,20)
amort <- function(r,term){
    (r*(1+r)^term)/((1+r)^term-1)
  }


#' daylight_usage_fun
#'
#'
#' @param day day of year
#' @param rho_solstice daylight usage on solstice (12 hours)
#'
#' @return a number between 0 and 1
#' @export
#'
#' @examples daylight_usage_fun(80,0.25)
daylight_usage_fun <- function(day,rho_solstice){

  latitude <- 53
  pmin(1,rho_solstice*day_length_fun(day,53)/12)

}


#' pvbess_optim_complex
#'
#' @param aspect house orientation
#' @param solar_constraint_1 primary roof capacity constraint
#' @param solar_constraint_2 secondary roof capacity constraint
#' @param shading_factor_1 primary roof shading in 0 to 1
#' @param shading_factor_2 secondary roof shading
#' @param D_max winter demand max
#' @param D_min summer demand min
#' @param params scenario parameters
#' @param tariff_plan tariff plan "night-saver" or "24hr"
#'
#' @returns tibble with optimium system, annualised cost savings and other parameters
#' @export
#'
#' @examples
pvbess_optim_complex <- function(aspect,solar_constraint_1,solar_constraint_2,shading_factor_1,shading_factor_2,D_max,D_min,params, tariff_plan="night_saver"){

  if(!(tariff_plan %in% c("night_saver","24hr"))) stop("bad tariff plan")
  p_n <- dplyr::if_else(tariff_plan=="24hr",params$day_tariff,params$night_tariff)


  fun_sb <- function(capacities){
    #capacities = c(S_1,S_2,B)
    pvbess_cost_fun(capacities[1],capacities[2],aspect,shading_factor_1,shading_factor_2,capacities[3],D_max,D_min,params, tariff_plan)
  }

  fun_s <- function(capacities){
    pvbess_cost_fun(capacities[1],capacities[2],aspect,shading_factor_1,shading_factor_2,0,D_max,D_min,params, tariff_plan)
  }

  fun_b <- function(B){
    #params[1] is solar params[2] is battery
    pvbess_cost_fun(0,0,aspect,shading_factor_1,shading_factor_2,B,D_max,D_min,params, tariff_plan)
  }

  #result_sb <- optim(c(0.9*S_constraint,0.5*D_max),fun_sb, method="L-BFGS-B", lower=c(0,0), upper=c(S_constraint,D_max))
  result_sb <- nloptr::nloptr(c(0.9*solar_constraint_1,0.9*solar_constraint_2,0.9*D_max),fun_sb, lb=c(0,0,0), ub=c(solar_constraint_1,solar_constraint_2,D_max),
                              opts=list(algorithm="NLOPT_LN_BOBYQA",maxeval=1000))

  #result_s <- optim(0.9*S_constraint,fun_s, method="L-BFGS-B", lower=0, upper=S_constraint)
  result_s <- nloptr::nloptr(c(0.9*solar_constraint_1,0.9*solar_constraint_2),fun_s, lb=c(0,0), ub=c(solar_constraint_1,solar_constraint_2),
                             opts=list(algorithm="NLOPT_LN_BOBYQA",maxeval=1000))

  #result_b <- optim(0.9*D_max,fun_b, method="L-BFGS-B", lower=0, upper=D_max)
  result_b <- nloptr::nloptr(0.9*D_max,fun_b, lb=0, ub=D_max,
                             opts=list(algorithm="NLOPT_LN_BOBYQA",maxeval=1000))


  base_cost <- fun_sb(c(0,0,0))
  #compare four solutions
  #res <- c(base_cost,result_s$value,result_b$value,result_sb$value)
  res <- c(base_cost,result_s$objective,result_b$objective,result_sb$objective)

  i <- which.min(res)

  #tib0 <- tibble::tibble(aspect,S1_constraint=solar_constraint_1,S2_constraint=solar_constraint_2,D_max=D_max,D_min=D_min,p_d=params$day_tariff,p_e=params$evening_tariff,p_n=p_n,p_f=params$fit,pv_unit_cost=params$pv_cost,storage_unit_cost=params$battery_cost,cost_synergy = params$pvbess_cost_synergy,resilience_premium=params$resilience_premium)

  if(i==4) tib1 <- tibble::tibble(S_1=result_sb$solution[1],S_2=result_sb$solution[2],B=result_sb$solution[3],cost_optimal=result_sb$objective)
  if(i==1) tib1 <- tibble::tibble(S_1=0,S_2=0,B=0,cost_optimal=base_cost)
  if(i==2) tib1 <- tibble::tibble(S_1=result_s$solution[1],S_2=result_s$solution[2],B=0,cost_optimal=result_s$objective)
  if(i==3) tib1 <- tibble::tibble(S_1=0,S_2=0,B=result_b$solution[1],cost_optimal=result_b$objective)

  tib1 <- tib1 %>% dplyr::mutate(base_cost=base_cost,savings=(cost_optimal-base_cost)/base_cost,grant=seai_grant_fast(params,S_1+S_2,B))
  #tib1 %>% dplyr::bind_cols(tib0) %>% return()
  tib1 %>% return()
}


#' pvbess_optim_upgrade
#'
#' @param S1_old old primary roof capacity
#' @param S2_old old secondary roof capacity
#' @param B_old old BESS
#' @param capex_old the existing annualised investment cost before the upgrade
#' @param aspect aspect
#' @param solar_constraint_1 additional primary roof capacity
#' @param solar_constraint_2 additional secondary roof capacity
#' @param shading_factor_1 shading factor 1
#' @param shading_factor_2 shading factor 2
#' @param D_max winter average daily max
#' @param D_min summer average daily min
#' @param params params
#' @param tariff_plan tariff plan, default "night-saver"
#'
#' @returns data frame
#' @export
#'
#' @examples
pvbess_optim_upgrade <- function(S1_old,S2_old,B_old,capex_old,aspect,solar_constraint_1,solar_constraint_2,shading_factor_1,shading_factor_2,D_max,D_min,params, tariff_plan="night_saver"){

  if(!(tariff_plan %in% c("night_saver","24hr"))) stop("bad tariff plan")
  p_n <- dplyr::if_else(tariff_plan=="24hr",params$day_tariff,params$night_tariff)
  #annualised capex
  #capex_old <- annualised_system_cost(S1_old+S2_old,B_old,params)
  is_upgrade <- dplyr::if_else(S1_old > 0 | S2_old > 0 | B_old > 0, TRUE,FALSE)
  #the cost function to be minimised
  fun_sb <- function(add_capacities){
    #capacities = c(S_1,S_2,B)
    opex_new <- pvbess_opex_cost_fun(S1_old+add_capacities[1],S2_old+add_capacities[2],aspect,shading_factor_1,shading_factor_2,B_old+add_capacities[3],D_max,D_min,params, tariff_plan)
    opex_new +annualised_system_cost(add_capacities[1]+add_capacities[2],add_capacities[3],params, upgrade=is_upgrade)
    }

  fun_s <- function(add_capacities){
    opex_new <- pvbess_opex_cost_fun(S1_old+add_capacities[1],S2_old+add_capacities[2],aspect,shading_factor_1,shading_factor_2,B_old,D_max,D_min,params, tariff_plan)
    opex_new+annualised_system_cost(add_capacities[1]+add_capacities[2],0,params, upgrade=is_upgrade)
  }

  fun_b <- function(B){
    #params[1] is solar params[2] is battery
    opex_new <- pvbess_opex_cost_fun(S1_old,S2_old,aspect,shading_factor_1,shading_factor_2,B_old+B,D_max,D_min,params, tariff_plan)
    opex_new+annualised_system_cost(0,B,params, upgrade=is_upgrade)
    }

  #result_sb <- optim(c(0.9*S_constraint,0.5*D_max),fun_sb, method="L-BFGS-B", lower=c(0,0), upper=c(S_constraint,D_max))
  result_sb <- nloptr::nloptr(c(0.9*solar_constraint_1,0.9*solar_constraint_2,0.9*D_max),fun_sb, lb=c(0,0,0), ub=c(solar_constraint_1,solar_constraint_2,D_max),
                              opts=list(algorithm="NLOPT_LN_BOBYQA",maxeval=1000))

  #result_s <- optim(0.9*S_constraint,fun_s, method="L-BFGS-B", lower=0, upper=S_constraint)
  result_s <- nloptr::nloptr(c(0.9*solar_constraint_1,0.9*solar_constraint_2),fun_s, lb=c(0,0), ub=c(solar_constraint_1,solar_constraint_2),
                             opts=list(algorithm="NLOPT_LN_BOBYQA",maxeval=1000))

  #result_b <- optim(0.9*D_max,fun_b, method="L-BFGS-B", lower=0, upper=D_max)
  result_b <- nloptr::nloptr(0.9*D_max,fun_b, lb=0, ub=D_max,
                             opts=list(algorithm="NLOPT_LN_BOBYQA",maxeval=1000))


  base_cost <- fun_sb(c(0,0,0))
  #base_cost <- opex_old+annualised_system_cost(S1_old+S2_old,B_old,params)
  #base_cost <- opex_old + annualised_system_cost(S1_old+S2_old,B = params,upgrade=F)
  #compare four solutions
  #res <- c(base_cost,result_s$value,result_b$value,result_sb$value)
  res <- c(base_cost,result_s$objective,result_b$objective,result_sb$objective)

  i <- which.min(res)

  #tib0 <- tibble::tibble(aspect,S1_constraint=solar_constraint_1,S2_constraint=solar_constraint_2,D_max=D_max,D_min=D_min,p_d=params$day_tariff,p_e=params$evening_tariff,p_n=p_n,p_f=params$fit,pv_unit_cost=params$pv_cost,storage_unit_cost=params$battery_cost,cost_synergy = params$pvbess_cost_synergy,resilience_premium=params$resilience_premium)

  if(i==4) tib1 <- tibble::tibble(dS_1=result_sb$solution[1],dS_2=result_sb$solution[2],dB=result_sb$solution[3])
  if(i==1) tib1 <- tibble::tibble(dS_1=0,dS_2=0,dB=0)
  if(i==2) tib1 <- tibble::tibble(dS_1=result_s$solution[1],dS_2=result_s$solution[2],dB=0)
  if(i==3) tib1 <- tibble::tibble(dS_1=0,dS_2=0,dB=result_b$solution[1])
  opex_old <- pvbess_opex_cost_fun(S1_old,S2_old,aspect,shading_factor_1,shading_factor_2,B_old,D_max,D_min,params, tariff_plan)

  tib1 <- tib1 %>% dplyr::mutate(capex_add=annualised_system_cost(dS_1+dS_2,dB,params,upgrade=is_upgrade),capex_new=capex_old+capex_add)
  tib1 <- tib1 %>% dplyr::mutate(opex_new = pvbess_opex_cost_fun(S1_old+dS_1,S2_old+dS_2,aspect,shading_factor_1,shading_factor_2,B_old+dB,D_max,D_min,params, tariff_plan)
  ,cost_old=capex_old+opex_old, cost_optimal=capex_old+capex_add+opex_new)
  tib1 <- tib1 %>% dplyr::mutate(savings = (cost_optimal-cost_old)/cost_old)
  tib1 <- tib1 %>% dplyr::rowwise() %>% dplyr::mutate(grant = dplyr::if_else(is_upgrade,0,seai_grant_fast(params,dS_1+dS_2, dB)))

  #tib1 %>% dplyr::bind_cols(tib0) %>% return()
  tib1 %>% return()
}



