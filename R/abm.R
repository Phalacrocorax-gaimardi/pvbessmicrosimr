
#' initialise_agents
#'
#' creates the agent initial state including model weights, randomised available rooftop areas for solar PV and a mapping of survey agents to CER dataset. It is assumed that solar pv systems are absent at yeartime e.g. 2010
#'
#' @param sD scenario (usable_roof_fraction only)
#' @param yeartime start year (default 2010)
#' @param cal_run calibration run number between 1 and 100
#'
#' @return a dataframe with columns
#' @export
#'
#' @examples
initialise_agents <- function(sD,yeartime,cal_run){

  #initialise to 2010
  params <- scenario_params(sD,yeartime)
  agents <- abm_weights_oo %>% dplyr::filter(calibration==cal_run) %>% dplyr::select(-calibration)
  #test <- map_survey_to_cer(params,lambda)
  #owner occupier non-apartment
  #test <- test %>% dplyr::filter(q1 %in% 2:5,q3 %in% 1:2)
  pv_surv <- pv_survey_oo %>% dplyr::select(qc2,q1,q2,q3,q5,q14,q15,q45)
  pv_surv$ID <- 1:nrow(pv_surv)
  #agents <- agents %>% dplyr::inner_join(pv_survey_oo %>% dplyr::select(ID,housecode,region,q1))
  agents <- agents %>% dplyr::inner_join(pv_surv)
  #agents <- agents %>% dplyr::inner_join(cer_demand)
  agents <- agents  %>% dplyr::rowwise() %>% dplyr::mutate(area_1=gen_roofsection_solar_area(qc2,q1,q2,q3,q5,roof_floor_ratio=2/sqrt(3), usable_roof_fraction = params$nu.))
  agents <- agents %>% dplyr::mutate(area_2 = area_1)
  #remove non-stochastic features q1 and region than can be restored later using pv_survey_oo based on ID
  agents <- agents %>% dplyr::select(-qc2,-q1,-q2,-q3,-q5)
  #primitive shading model
  agents <- agents  %>% dplyr::rowwise() %>% dplyr::mutate(shading1=rbeta(1,params$mean_shading_factor/(1-params$mean_shading_factor),1), shading2 = rbeta(1,params$mean_shading_factor/(1-params$mean_shading_factor),1))
  agents <- agents %>% dplyr::rowwise() %>% dplyr::mutate(aspect=sample(c("South-North","SW-NE","SE-NW","East-West"),size=1))
  #add demand parameters
  agents <- agents %>% dplyr::inner_join(survey_bills_to_kwh(pv_survey_oo) %>% dplyr::select(ID,D_max,D_min,annual_kwh))

  #agents <- agents[,c(1,7:14,2:6)]
  #agents$imports_old <- agents$annual_kwh
  #agents$exports_old <- 0
  agents$S1_old <- 0
  agents$S2_old <- 0
  agents$B_old <- 0
  #agents$imports_new <- agents$annual_kwh
  #agents$exports_new <- 0
  agents$S1_new <- 0
  agents$S2_new <- 0
  agents$B_new <- 0
  #initial capex and opex costs
  agents$capex_old <- 0
  agents$capex_new <- 0
  agents <- agents %>% dplyr::mutate(opex_old=pvbessmicrosimr::pvbess_opex_cost_fun(0,0,"South-North",1,1,0,D_max,D_min,params))
  agents <- agents %>% dplyr::mutate(opex_new=opex_old)

  #set all social terms to zero
  agents$q45 <- 1
  return(agents %>% dplyr::ungroup())
}


#' update_agents
#'
#' micro-simulation time-step updater
#'
#' The workhorse ABM function.Within a scenario, does a single month update of the agent characteristics. A random sample of agents evaluates their economic and social
#' utilities. If these exceed their individual threshold an optimal PV_BESS system is adopted.
#'
#'
#' @param sD  scenario dataframe
#' @param yeartime decimal time
#' @param agents_in input agent dataframe
#' @param social_network artifical social network
#' @param ignore_social option to ignore social effects. Default is FALSE.
#' @param cal_run microcalibration run index between 1 and 100
#'
#' @return updated agent dataframe
#' @export
#' @examples
update_agents <- function(sD,yeartime,agents_in, social_network,ignore_social=F,cal_run){

  #
  #beta. <- 0.2532785
  #params at yeartime
  params <- scenario_params(sD,yeartime)
  #solar panel efficiency
  kWpm2 <- params$kWp_per_m2
  #treat rho_solstice as a parameter
  #params$rho_solstice <- rho.

  empirical_u <- empirical_utils_oo %>% dplyr::filter(calibration==cal_run) %>% dplyr::select(-calibration)

  du_social <- dplyr::filter(empirical_u,question_code=="q45")$du_average
  theta <- dplyr::filter(empirical_u,question_code=="theta")$du_average


  a_s <- agents_in
  a_s$transaction <- F
  a_s$savings <- NA
  a_s <- dplyr::ungroup(a_s)
  #update definitions of old and new for all agents
  a_s <- a_s %>% dplyr::mutate(S1_old=S1_new,S2_old = S2_new,B_old=B_new)
  a_s <- a_s %>% dplyr::mutate(capex_old=capex_new,opex_old=opex_new)
  #this subsample of agents decide to look at rooftop pv
  b_s <- dplyr::slice_sample(a_s,n=roundr(dim(a_s)[1]*params$p.*params$acceleration_factor))
  #remove households that have already adopted solar PV or battery storage i.e. no system upgrades

  b_s <- b_s %>% dplyr::select(ID,D_max,D_min,annual_kwh,aspect,area_1,area_2,shading1,shading2,w_q14,w_q45,w_theta,q14,q15,q45,S1_old,S2_old,B_old,capex_old,opex_old,transaction)
  #potential first adopters

  b_s <- b_s %>% dplyr::rowwise() %>% dplyr::mutate(result = list(pvbess_optim_upgrade(S1_old,S2_old,B_old,capex_old,aspect,area_1*params$kWp_per_m2,area_2*params$kWp_per_m2,
                                                                                       shading1,shading2,D_max,D_min,params,tariff_plan="night_saver"))) %>% tidyr::unnest_wider(result)
  #identify any potential upgrades
  b_s <- b_s %>% dplyr::mutate(is_upgrade = !(S1_old==0 & S2_old==0 & B_old ==0) )

  #adoption increments of 250W and 2.5kWh hard-wired

  #bs_a <- bs_a %>% dplyr::rowwise() %>% dplyr::mutate(S_1 = solar_round_down(S_1,0.25),S_2=solar_round_down(S_2,0.25), B= bess_round(B,2.5))
  b_s <- b_s %>% dplyr::rowwise() %>% dplyr::mutate(dS_1 = solar_round_down(dS_1,0.25),dS_2=solar_round_down(dS_2,0.25), dB= bess_round(dB,2.5))
  #re-compute savings for actual system installed after rounding)
  b_s <- b_s %>% dplyr::rowwise() %>% dplyr::mutate(opex_new = pvbess_opex_cost_fun(S1_old+dS_1,S2_old+dS_2,aspect,shading1,shading2,B_old+dB,D_max,D_min,params,tariff_plan="night_saver"))
  b_s <- b_s %>% dplyr::rowwise() %>% dplyr::mutate(capex_new = capex_old+annualised_system_cost(dS_1+dS_2,dB,params,upgrade=is_upgrade))
  b_s <- b_s %>% dplyr::rowwise() %>% dplyr::mutate(savings = (capex_new+opex_new-cost_old)/cost_old)
  #
  b_s <- b_s %>% dplyr::mutate(du_fin = -params$beta.*w_q14*savings)
  b_s <- b_s %>% dplyr::mutate(du_social = dplyr::if_else(is_upgrade,0,w_q45*du_social[q45]))
  b_s <- b_s %>% dplyr::mutate(du_theta = dplyr::if_else(is_upgrade,0,w_theta*theta))
  #sum and inude hypothetical bias correction
  b_s <- b_s %>% dplyr::mutate(du_tot = du_fin+du_social+du_theta + params$lambda.)
  #
  #identify transactions
  b_s <- b_s %>% dplyr::mutate(transaction = dplyr::if_else(du_tot > 0,TRUE,FALSE))
  #identify upgrades
  b_s <- b_s %>% dplyr::mutate(is_upgrade = dplyr::if_else(du_tot > 0 & is_upgrade,TRUE,FALSE))
  #reject updates that did not occur
  b_s <- b_s %>% dplyr::mutate(dS_1 = dplyr::if_else(transaction, dS_1, 0))
  b_s <- b_s %>% dplyr::mutate(dS_2 = dplyr::if_else(transaction, dS_2, 0))
  b_s <- b_s %>% dplyr::mutate(dB = dplyr::if_else(transaction, dB, 0))
  #update areas for agents who transacted
  if(dim(b_s %>% dplyr::filter(transaction))[1] == 0) {
    print(paste("time", round(yeartime,1), "no PV-BESS adopters"))
    #print(paste("PV system augmenters because selected roofs already at capacity"))
  }
  b_s <- b_s %>% dplyr::mutate(area_1 = dplyr::if_else(transaction,area_1 - dS_1/kWpm2, area_1))
  b_s <- b_s %>% dplyr::mutate(area_2 = dplyr::if_else(transaction,area_2 - dS_2/kWpm2, area_2))


  #remove unwanted columns and replace S1_old+dS_1 by S1_new etc
  b_s <- b_s %>% dplyr::mutate(S1_new=S1_old+dS_1,S2_new=S2_old + dS_2,B_new=B_old+dB)
  b_s <- b_s %>% dplyr::select(-dS_1,-dS_2,-dB,-du_fin,-du_social,-du_theta,-capex_add,-cost_old,-is_upgrade,-cost_optimal)

  a_s <- dplyr::filter(a_s, !(ID %in% b_s$ID))
  a_s <- dplyr::bind_rows(a_s,b_s) %>% dplyr::arrange(as.numeric(ID))
  a_s <- a_s %>% dplyr::mutate(S1_new = tidyr::replace_na(S1_new,0), new_solar2 = tidyr::replace_na(S2_new,0),B_new = tidyr::replace_na(B_new,0))

    #recompute social variable
    ma <- igraph::as_adjacency_matrix(social_network)
    g <- social_network %>% tidygraph::activate(nodes) %>% dplyr::left_join(a_s,by="ID")
    #social network conformity effect
    adopter_nodes <- igraph::V(g)$S1_new > 0 | igraph::V(g)$S2_new | igraph::V(g)$B_new > 0
    a_s$q45 <- as.numeric(ma %*% adopter_nodes) #social reinforcement 0 no adoption 1 adoption
    if(ignore_social) a_s$qsp45 <- 0 #no pvs assumed present in local network
    a_s <- a_s %>% dplyr::rowwise() %>% dplyr::mutate(q45 = min(q45+1,4)) #qsp21 encoding 1,2,3
    #agents_out <- a_s
    #a_s <- a_s %>% dplyr::select(-du_tot)
    print(paste("time", round(yeartime,1), "PV system adopters",dim(a_s %>% dplyr::filter( (S1_new > 0 & S1_old==0) | (S2_new > 0 & S2_old==0)))[1]))
    print(paste("PV system augmentors",dim(a_s %>% dplyr::filter((S1_old > 0 & S1_new > S1_old) | (S2_old > 0 & S2_new > S2_old) | (S1_old > 0 & B_new > B_old) || (S2_old > 0 & B_new > B_old) ))[1]))
    return(dplyr::ungroup(a_s))
}

#agents_init <- initialise_agents(sD,2010,cal_run=1) #cal run is irrelevant here


#' get_financial_utility_scale
#'
#' @param agents_in agent chacteristics e.g. output by initialise_agents()
#' @param cal_run calibration run in 1 to 100
#'
#' @returns a scalar (beta.)
#' @export
#'
#' @examples
get_financial_utility_scale <- function(agents_in,cal_run){

  gen_optimised_pvbess <- function(agents_in,n_sample=nrow(pv_survey_oo),tariff_plan="night_saver",no_grant = FALSE){

    survey_time <- 2024
    params <- scenario_params(sD,survey_time)
    if(no_grant) params$grant_removal_date <- yeartime -1 #remove grant
    #empirical_u <- empirical_utils_oo %>% dplyr::filter(calibration==cal_run)
    agents_in <- agents_in %>% dplyr::slice_sample(n=n_sample) %>% dplyr::rowwise() %>% dplyr::mutate(result = list(pvbess_optim_complex(aspect,round(area_1*params$kWp_per_m2),round(area_2*params$kWp_per_m2),shading1,shading2,D_max,D_min,params,tariff_plan=tariff_plan))) %>% tidyr::unnest_wider(result)
    agents_in %>% dplyr::select(ID,q14,q15,D_max,D_min,aspect,shading1,shading2,S_1,S_2,B,savings) %>% return()
  }

  agents_in <- gen_optimised_pvbess(agents_in)
  survey_u <- agents_in %>% dplyr::group_by(q14) %>% dplyr::summarise(savings=-median(savings))
  empirical_u <- empirical_utils_oo %>% dplyr::filter(calibration==cal_run) %>% dplyr::filter(question_code=="q14")
  empirical_u <- empirical_u %>% dplyr::select(response_code,du_average) %>% dplyr::rename("q14"=response_code)
  survey_u <- survey_u %>% dplyr::inner_join(empirical_u)
  #coef(lm(du_average~savings,survey_u))[2] %>% return()
  #IQR(survey_u$du_average)/IQR(survey_u$savings) %>% return()
  mad(survey_u$du_average)/mad(survey_u$savings) %>% return() #mean absolute deviation relative to median
}


#' runABM
#'
#' Runs pv system adoption simulation on artificial society of 752 agents.
#' Each run is performed on an independently generated social network with randomisation from initialise_agents() and
#' with a random micro_calibration run (index 1..100)
#'
#' Bi-monthly timesteps.
#'
#' @param sD scenario set-up dataframe, typically read with readxlxs(...,sheet=scenario)
#' @param Nrun integer, number runs
#' @param simulation_end the final year of simulation of early termination is required
#' @param resample_society if TRUE resample pv_society with replacement to capture additional variability
#' @param n_unused_cores number of cores left unused in parallel/foreach. Recommended values 2 or 1.
#' @param use_parallel if TRUE uses multiple cores. Use FALSE for diagnostic runs
#' @param ignore_social if TRUE ignore social network effects. Default is FALSE
#'
#' @return a three component list - simulation output, scenario setup, meta-parameters
#' @export
#' @importFrom magrittr %>%
#' @importFrom lubridate %m+%
#' @importFrom foreach %dopar%
#'
runABM <- function(sD, Nrun=1,simulation_end=2030,resample_society=F,n_unused_cores=2, use_parallel=T,ignore_social=F){
  #
  year_zero <- 2015
  #calibration params:: MOVED TO SYSTDATA WHEN CALIBRATION COMPLETE
  p <- sD %>% dplyr::filter(parameter=="p.") %>% dplyr::pull(value)
  lambda <- sD %>% dplyr::filter(parameter=="lambda.") %>% dplyr::pull(value)
  beta <- sD %>% dplyr::filter(parameter=="beta.") %>% dplyr::pull(value)
  nu <- sD %>% dplyr::filter(parameter=="nu.") %>% dplyr::pull(value)

  print(paste("p.=",p,"lambda=",lambda,"beta.=",beta))
  seai_elec <- pvbessmicrosimr::seai_elec
  #bi-monthly runs
  Nt <- round((simulation_end-year_zero+1)*6)
  #annual runs
  #Nt <- round((simulation_end-year_zero+1))
  #agents0 <- agents_i
  #cal_run <- 40
  #u_empirical <- empirical_utils_oo %>% dplyr::filter(calibration==cal_run) %>% dplyr::select(-calibration)
  #
  if(use_parallel){

    number_of_cores <- parallel::detectCores() - n_unused_cores
    doParallel::registerDoParallel(number_of_cores)

    abm <- foreach::foreach(j = 1:Nrun, .packages = "dplyr", .combine=dplyr::bind_rows,.export = c("initialise_agents","update_agents","make_artificial_society","pvbess_opex_cost_fun","pvbess_optim_upgrade","pvbess_optim_complex")) %dopar% {
      #abm <- foreach::foreach(j = 1:Nrun, .errorhandling = "pass",.export = c("initialise_agents","update_agents4")) %dopar% {

      #create a new artificial society for each run
      print(paste("Generating network for run",j,"...."))
      if(!resample_society) social <- make_artificial_society(pvbessmicrosimr::pv_society_oo,pvbessmicrosimr::homophily,5)
      if(resample_society){
        agent_resample <- sample(1:dim(pvbessmicrosimr::pv_society_oo)[1],replace=T)
        society_new <- society[agent_resample,]
        society_new$ID <- 1:dim(pvbessmicrosimr::pv_society_oo)[1]
        social <- make_artificial_society(society_new,pvbessmicrosimr::homophily,4.5)
      }
      #randomiise ICEV emissions assignment
      #choose segments
      microcal_run <- sample(1:100,1)
      agents_in <- initialise_agents(sD,year_zero,microcal_run)
      u_empirical <- empirical_utils_oo %>% dplyr::filter(calibration==micro_cal_run) %>% dplyr::select(-calibration)
      #no transactions
      agents_in$transaction <- FALSE
      agent_ts<- vector("list",Nt)
      agent_ts[[1]] <- agents_in #agent parameters with regularized weights



      for(t in seq(2,Nt)){
        #bi-monthly
        yeartime <- year_zero+(t-1)/6
        agent_ts[[t]] <- update_agents(sD,yeartime,agent_ts[[t-1]],social_network=social,ignore_social,cal_run=microcal_run) #static social network, everything else static
      }

      for(t in 1:Nt) agent_ts[[t]]$t <- t
      agent_ts <- tibble::as_tibble(data.table::rbindlist(agent_ts,fill=T))
      agent_ts$simulation <- j
      #add vertex degree
      degrees <- tibble::tibble(ID=1:dim(pvbessmicrosimr::pv_society_oo)[1],degree=igraph::degree(social))
      agent_ts <- agent_ts %>% dplyr::inner_join(degrees)
      agent_ts
    }

    meta <- tibble::tibble(parameter=c("Nrun","end_year","beta.","lambda.","p."),value=c(Nrun,simulation_end,beta,lambda,p))
    abm <- abm %>% dplyr::mutate(date=lubridate::ymd(paste(year_zero,"-02-01",sep="")) %m+% months((t-1)*2)) %>% dplyr::arrange(simulation,date) %>% dplyr::select(-t)
    return(list("abm"=abm,"scenario"=sD,"system"=meta))
  }

  #don't use parallel
  #comment in next two lines for parallel
  if(!use_parallel){

    abm <- tibble::tibble()
    #number_of_cores <- parallel::detectCores() - n_unused_cores
    #doParallel::registerDoParallel(number_of_cores)
    #comment out next line for parallel
    for(j in 1:Nrun){
      #comment in next line for parallel
      #abm <- foreach::foreach(j = 1:Nrun, .combine=dplyr::bind_rows,.export = c("initialise_segments","update_agents4")) %dopar% {
      #create a new artificial society for each run
      print(paste("Generating network for run",j,"...."))
      #u_empirical <- empirical_utils_oo %>% dplyr::select(calibration=)
      if(!resample_society) social <- make_artificial_society(pv_society_oo,homophily,5)
      if(resample_society){
        agent_resample <- sample(1:dim(pv_society_oo)[1],replace=T)
        society_new <- pv_society_oo[agent_resample,]
        society_new$ID <- 1:dim(pv_society_oo)[1]
        social <- make_artificial_society(society_new,homophily,5)

      }
      #randomise ICEV emissions assignment
      #choose market segment for each agent
      microcal_run <- sample(1:100,1)
      u_empirical <- empirical_utils_oo %>% dplyr::filter(calibration==micro_cal_run) %>% dplyr::select(-calibration)
      agents_in <- initialise_agents(sD,year_zero,microcal_run)
      #no transactions
      agents_in$transaction <- FALSE
      agent_ts <- vector("list",Nt)
      agent_ts[[1]] <- agents_in #agent parameters with regularized weights

      for(t in seq(2,Nt)){
        #
        #yeartime <- year_zero+(t-1)
        yeartime <- year_zero+(t-1)/6
        agent_ts[[t]] <- update_agents(sD,yeartime,agent_ts[[t-1]],social_network=social,ignore_social,cal_run=microcal_run) #static social network, everything else static
        #agent_ts[[t]] <- tibble::tibble(t=t)
      }

      for(t in 1:Nt) agent_ts[[t]]$t <- t
      agent_ts <- tibble::as_tibble(data.table::rbindlist(agent_ts,fill=T))
      agent_ts$simulation <- j
      #network degree
      degrees <- tibble::tibble(ID=1:dim(pv_society_oo)[1],degree=igraph::degree(social))
      agent_ts <- agent_ts %>% dplyr::inner_join(degrees)
      abm <- dplyr::bind_rows(abm,agent_ts)
      #comment in next line for parallel
      #agent_ts
    }
    meta <- tibble::tibble(parameter=c("Nrun","end_year","beta.","lambda.","p."),value=c(Nrun,simulation_end,beta,lambda,p))
    #replace "t" with dates
    abm <- abm %>% dplyr::mutate(date=lubridate::ymd(paste(year_zero,"-02-01",sep="")) %m+% months((t-1)*2)) %>% dplyr::arrange(simulation,date) %>% dplyr::select(-t)
    return(list("abm"=abm,"scenario"=sD,"system"=meta))
  }

}



#' calABM
#'
#' macro-calibration of pvbess
#'
#' return uptake (number of households and installed capacity) in june 2023 and 2024 for comparison with ISEA reports
#'
#' @param sD base scenario (historical)
#' @param Nrun number of runs
#' @param n_unused_cores unsued cores default 2
#' @param use_parallel TRUE or FALSE
#' @param beta financial utility scale (drawn from financial_utility_scale)
#' @param lambda bias correction parameter
#' @param p rate parameter default 0.085
#' @param nu usable roof fraction for solar
#' @param rho rho_solstice parameter
#'
#' @returns
#' @export
#'
#' @examples
calABM <- function(sD, Nrun=4,n_unused_cores=2, use_parallel=T, beta,lambda,p,nu,rho){
  #
  year_zero <- 2015
  simulation_end <- 2024
  resample_society=F
  ignore_social=F
  #the calibration parameters
  sD_cal <- sD
  sD_cal[sD_cal$parameter=="beta.","value"] <- beta #financial partial utility scale
  sD_cal[sD_cal$parameter=="lambda.","value"] <- lambda #additional hypothetical bias correction
  sD_cal[sD_cal$parameter=="nu.","value"] <- nu #usable roof fraction for solar
  sD_cal[sD_cal$parameter=="rho_solstice","value"] <- rho

  #calibration params:: MOVED TO SYSTDATA WHEN CALIBRATION COMPLETE
  print(paste("beta.=",beta,"lambda.=",lambda,"p.=",p,"nu.=",nu,"rho_solstice=",rho))
  seai_elec <- pvbessmicrosimr::seai_elec
  #bi-monthly runs
  Nt <- round((simulation_end-year_zero+1)*6)
  #annual runs
  #Nt <- round((simulation_end-year_zero+1))
  #agents0 <- agents_i
  #cal_run <- sample(1:100,1)
  #u_empirical <- empirical_utils_oo %>% dplyr::filter(calibration==cal_run) %>% dplyr::select(-calibration)
  #
  if(use_parallel){

    number_of_cores <- parallel::detectCores() - n_unused_cores
    cl <- parallel::makeCluster(number_of_cores)
    doParallel::registerDoParallel(cl)

    abm <- foreach::foreach(j = 1:Nrun, .packages = "dplyr", .final = function(x) { parallel::stopCluster(cl); x },.combine=dplyr::bind_rows,.export = c("initialise_agents","update_agents","make_artificial_society","pvbess_opex_cost_fun","pvbess_optim_upgrade","pvbess_optim_complex")) %dopar% {
      #abm <- foreach::foreach(j = 1:Nrun, .errorhandling = "pass",.export = c("initialise_agents","update_agents4")) %dopar% {

      #create a new artificial society for each run
      print(paste("Generating network for run",j,"...."))
      if(!resample_society) social <- make_artificial_society(pvbessmicrosimr::pv_society_oo,pvbessmicrosimr::homophily,5)
      if(resample_society){
        agent_resample <- sample(1:dim(pvbessmicrosimr::pv_society_oo)[1],replace=T)
        society_new <- society[agent_resample,]
        society_new$ID <- 1:dim(pvbessmicrosimr::pv_society_oo)[1]
        social <- make_artificial_society(society_new,pvbessmicrosimr::homophily,4.5)
      }
      #randomiise ICEV emissions assignment
      #choose segments
      microcal_run <- sample(1:100,1)
      u_empirical <- empirical_utils_oo %>% dplyr::filter(calibration==microcal_run) %>% dplyr::select(-calibration)

      agents_in <- initialise_agents(sD_cal,year_zero,microcal_run)
      #no transactions
      agents_in$transaction <- FALSE
      agent_ts<- vector("list",Nt)
      agent_ts[[1]] <- agents_in #agent parameters with regularized weights

      for(t in seq(2,Nt)){
        #bi-monthly
        yeartime <- year_zero+(t-1)/6
        agent_ts[[t]] <- update_agents(sD_cal,yeartime,agent_ts[[t-1]],social_network=social,ignore_social,cal_run=microcal_run) #static social network, everything else static
      }

      for(t in 1:Nt) agent_ts[[t]]$t <- t
      agent_ts <- tibble::as_tibble(data.table::rbindlist(agent_ts,fill=T))
      agent_ts$simulation <- j
      #add vertex degree
      degrees <- tibble::tibble(ID=1:dim(pvbessmicrosimr::pv_society_oo)[1],degree=igraph::degree(social))
      agent_ts <- agent_ts %>% dplyr::inner_join(degrees)
      agent_ts
    }

    meta <- tibble::tibble(parameter=c("Nrun","end_year","beta.","lambda.","p."),value=c(Nrun,simulation_end,beta,lambda,p))
    abm <- abm %>% dplyr::mutate(date=lubridate::ymd(paste(year_zero,"-02-01",sep="")) %m+% months((t-1)*2)) %>% dplyr::arrange(simulation,date) %>% dplyr::select(-t)
    #return(list("abm"=abm,"scenario"=sD,"system"=meta))
  }

  #don't use parallel
  #comment in next two lines for parallel
  if(!use_parallel){

    abm <- tibble::tibble()
    #number_of_cores <- parallel::detectCores() - n_unused_cores
    #doParallel::registerDoParallel(number_of_cores)
    #comment out next line for parallel
    for(j in 1:Nrun){
      #comment in next line for parallel
      #abm <- foreach::foreach(j = 1:Nrun, .combine=dplyr::bind_rows,.export = c("initialise_segments","update_agents4")) %dopar% {
      #create a new artificial society for each run
      print(paste("Generating network for run",j,"...."))
      #u_empirical <- empirical_utils_oo %>% dplyr::select(calibration=)
      if(!resample_society) social <- make_artificial_society(pv_society_oo,homophily,5)
      if(resample_society){
        agent_resample <- sample(1:dim(pv_society_oo)[1],replace=T)
        society_new <- pv_society_oo[agent_resample,]
        society_new$ID <- 1:dim(pv_society_oo)[1]
        social <- make_artificial_society(society_new,homophily,5)

      }
      microcal_run <- sample(1:100,1)
      u_empirical <- empirical_utils_oo %>% dplyr::filter(calibration==microcal_run) %>% dplyr::select(-calibration)
      agents_in <- initialise_agents(sD_cal,year_zero,microcal_run)
      #no transactions
      agents_in$transaction <- FALSE
      agent_ts <- vector("list",Nt)
      agent_ts[[1]] <- agents_in #agent parameters with regularized weights

      for(t in seq(2,Nt)){
        #
        #yeartime <- year_zero+(t-1)
        yeartime <- year_zero+(t-1)/6
        agent_ts[[t]] <- update_agents(sD_cal,yeartime,agent_ts[[t-1]],social_network=social,ignore_social,cal_run=microcal_run) #static social network, everything else static
        #agent_ts[[t]] <- tibble::tibble(t=t)
      }

      for(t in 1:Nt) agent_ts[[t]]$t <- t
      agent_ts <- tibble::as_tibble(data.table::rbindlist(agent_ts,fill=T))
      agent_ts$simulation <- j
      #network degree
      degrees <- tibble::tibble(ID=1:dim(pv_society_oo)[1],degree=igraph::degree(social))
      agent_ts <- agent_ts %>% dplyr::inner_join(degrees)
      abm <- dplyr::bind_rows(abm,agent_ts)
      #comment in next line for parallel
      #agent_ts
    }
    meta <- tibble::tibble(parameter=c("Nrun","end_year","beta.","lambda.","p."),value=c(Nrun,simulation_end,beta,lambda,p))
    #replace "t" with dates
    abm <- abm %>% dplyr::mutate(date=lubridate::ymd(paste(year_zero,"-02-01",sep="")) %m+% months((t-1)*2)) %>% dplyr::arrange(simulation,date) %>% dplyr::select(-t)
    #
  }
  isea_dates <- pv_retrofit_uptake %>% dplyr::filter(lubridate::year(date)>= 2016) %>% dplyr::pull(date) #scale of solar and census dates
  cal <- abm %>% dplyr::filter(date %in% isea_dates) %>% dplyr::group_by(simulation,date) %>% dplyr::summarise(S=sum(S1_new+S2_new),adopted=sum(S1_new > 0 | S2_new>0,na.rm=T))
  cal <- cal %>% dplyr::ungroup() %>% dplyr::group_by(date) %>% dplyr::summarise(MW =1.21e+3/752*mean(S), n= 1.21e+6/752*mean(adopted))
  tibble::tibble(beta.=beta,lambda.=lambda,p.=p,nu.=nu) %>% dplyr::bind_cols(cal) %>% return()
  #observations 2023 60,000 households 208 MW 2024 94,000 households 373 MW
}



