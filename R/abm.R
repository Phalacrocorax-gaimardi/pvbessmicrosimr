
#' initialise_agents
#'
#' creates the agent initial state including model weights, randomised available rooftop areas for solar PV and a mapping of survey agents to CER dataset. It is assumed that solar pv systems are absent at yeartime e.g. 2010
#'
#' @param sD scenario (usable_roof_fraction only)
#' @param yeartime start year (default 2010)
#' @param cal_run calibration run number between 1 and 100
#' @param lambda demand matching parameter. Large lambda is more strict.
#'
#' @return a dataframe with columns
#' @export
#'
#' @examples
initialise_agents <- function(sD,yeartime,cal_run,lambda=2){

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
  agents <- agents  %>% dplyr::rowwise() %>% dplyr::mutate(area_1=gen_roofsection_solar_area(qc2,q1,q2,q3,q5,roof_floor_ratio=2/sqrt(3), usable_roof_fraction = params$usable_roof_fraction))
  agents <- agents %>% dplyr::mutate(area_2 = area_1)
  #remove non-stochastic features q1 and region than can be restored later using pv_survey_oo based on ID
  agents <- agents %>% dplyr::select(-qc2,-q1,-q2,-q3,-q5)
  #primitive shading model
  agents <- agents  %>% dplyr::rowwise() %>% dplyr::mutate(shading1=rbeta(1,params$mean_shading_factor/(1-params$mean_shading_factor),1), shading2 = rbeta(1,params$mean_shading_factor/(1-params$mean_shading_factor),1))
  agents <- agents %>% dplyr::rowwise() %>% dplyr::mutate(aspect=sample(c("South-North","SW-NE","SE-NW","East-West"),size=1))
  #add demand parameters
  agents <- agents %>% dplyr::inner_join(survey_bills_to_kwh(pv_survey_oo) %>% dplyr::select(ID,D_max,D_min,annual_kwh))

  #agents <- agents[,c(1,7:14,2:6)]
  agents$old_imports <- agents$annual_kwh
  agents$old_exports <- 0
  agents$old_solar1 <- 0
  agents$old_solar2 <- 0
  agents$old_battery <- 0
  agents$new_imports <- agents$annual_kwh
  agents$new_exports <- 0
  agents$new_solar1 <- 0
  agents$new_solar2 <- 0
  agents$new_battery <- 0
  #add initial rooftop capacity excluding apartments !
  #cer_survey1 <- cer_survey %>% dplyr::filter(housing_type != 1) %>% dplyr::rowwise() %>% dplyr::mutate(rooftop_capacity = get_rooftop_solar_potential(floor_area,housing_type,bedrooms))
  #missing rooftop capacity values replaced with mean for housing_type
  #roof <- cer_survey1 %>% dplyr::group_by(housing_type) %>% dplyr::mutate(rooftop_capacity = ifelse(is.na(rooftop_capacity), mean(rooftop_capacity,na.rm=T),rooftop_capacity))
  #roof <- roof %>% dplyr::ungroup() %>% dplyr::select(housecode,rooftop_capacity)
  #agents <- agents %>% dplyr::inner_join(roof)
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
#' @param micro_cal micro calibration run number
#' @param p. speed parameter (fixed in macro-calibration)
#' @param lambda. bias (fixed in macro-calibration)
#'
#' @return updated agent dataframe
#' @export
#' @examples
update_agents <- function(sD,yeartime,agents_in, social_network,ignore_social=F, micro_cal, p.=0.0085,lambda.=0.105){

  #
  #params at yeartime
  params <- scenario_params(sD,yeartime)
  #solar panel efficiency
  kWpm2 <- params$kWp_per_m2

  empirical_u <- empirical_utils_oo %>% dplyr::filter(calibration==1) %>% dplyr::select(-calibration)

  du_social <- dplyr::filter(empirical_u,question_code=="q45")$du_average
  theta <- dplyr::filter(empirical_u,question_code=="theta")$du_average

  a_s <- agents_in
  a_s$transaction <- F
  a_s <- dplyr::ungroup(a_s)
  #update definitions of old and new for all agents
  a_s <- a_s %>% dplyr::mutate(old_imports=new_imports,old_exports=new_exports,old_solar1=new_solar1,old_solar2 = new_solar2,old_battery=new_battery)
  #this subsample of agents decide to look at rooftop pv
  b_s <- dplyr::slice_sample(a_s,n=roundr(dim(a_s)[1]*p.*params$acceleration_factor))
  b_s <- b_s %>% dplyr::mutate(transaction=T) %>% dplyr::select(ID,D_max,D_min,aspect,area_1,area_2,shading1,shading2,w_q14,w_q45,w_theta,q14,q45,old_imports, old_exports,old_solar1,old_solar2,old_battery)

  get_sys_optimal <- function(b_s){


    b_s1 <- b_s %>% dplyr::rowwise() %>% dplyr::mutate(result = list(pvbess_optim_complex(aspect,round(area_1*params$kWp_per_m2),round(area_2*params$kWp_per_m2),
                                 shading1,shading2,D_max,D_min,params,tariff_plan="night_saver"))) %>% tidyr::unnest_wider(result)


  }


 get_sys_optimal <- function(params, b_s){

    #pv rooftop capacity constrained finacial utilities corresponding to costs in params
    #find current (old) values of imports and exports
    #cer_sys <- b_s %>% dplyr::left_join(dplyr::bind_rows(pvmicrosimr::cer_systems1,pvmicrosimr::cer_systems2,
     #                                                    pvmicrosimr::cer_systems3,pvmicrosimr::cer_systems4))
    #cer_sys <- pvmicrosimr::get_shaded_sys(cer_sys)
    #cer_sys <- b_s %>% dplyr::left_join(cer_systems)
    #new system is an enhancement
    #area1,2 is the remaining area for solar
    #restruct the search to available areas
    cer_sys <- cer_sys %>% dplyr::filter(solar1 <= old_solar1+kWpm2*area1,solar2 <= old_solar2+kWpm2*area2, solar1 >= old_solar1, solar2 >= old_solar2, battery >= old_battery)
    #calculate utilities: the slow part
    params_vec <- mget(ls(params),params) %>% unlist()
    #consider vectorising get_sys_util_c
    cer_sys <- cer_sys %>% dplyr::rowwise() %>% dplyr::mutate(du=get_sys_util_c(params_vec,demand,old_imports,old_exports,old_solar1,old_solar2,old_battery,imports,exports,solar1-old_solar1,solar2-old_solar2,battery-old_battery))
    #cer_sys %>% dplyr::mutate(du=get_sys_util_0(params,demand,old_imports,old_exports,old_solar1,old_solar2,old_battery,imports,exports,solar1-old_solar1,solar2-old_solar2,battery-old_battery)) %>% system.time()
    #use data.table?
    #cer_sys <- data.table::data.table(cer_sys)
    #cer_sys[][,du:=get_sys_util_0(params,demand,old_imports,old_exports,old_solar1,old_solar2,old_battery,imports,exports,solar1-old_solar1,solar2-old_solar2,battery-old_battery)] %>% system.time()
    #optimal
    if(dim(cer_sys)[1]==0) return(cer_sys)
    if(dim(cer_sys)[1] > 0){
      #cer_sys_opt <- cer_sys %>% dplyr::group_by(housecode) %>% dplyr::filter(du==max(du))
      cer_sys_opt <- cer_sys %>% dplyr::group_by(ID) %>% dplyr::filter(du==max(du))
      #reduce available area by area of new pv
      #THIS HAS TO BE REVERSED IF TRANSACTION DOES NOT OCCUR
      #cer_sys_opt <- cer_sys_opt %>% dplyr::mutate(area1 = area1 - (solar1-old_solar1)/kWpm2, area2 = area2 - (solar2-old_solar2)/kWpm2)
      cer_sys_opt <- cer_sys_opt %>% dplyr::rename(new_solar1=solar1,new_solar2 = solar2,new_battery=battery,new_imports=imports,new_exports=exports)
      return(cer_sys_opt)
    }
    #return(b_s %>% dplyr::inner_join(cer_sys_opt))
  }
  #financially optimal solar pv system
  b_s1 <- get_sys_optimal(params,b_s)
  #if no transactions are possible (all roofs in b_s are at capacity) then just return a_s unchanged.
  if(dim(b_s1)[1] == 0) {
    print(paste("time", round(yeartime,1), "no PV system adopters because poor economics or selected roofs at capacity"))
    #print(paste("PV system augmenters because selected roofs already at capacity"))
    return(a_s)
  }
  #if there are potential transactions
  if(dim(b_s1)[1] > 0) {
    #add in self-sufficiency/aversion effect
    # and scale factor from calibration
    #add parial utilities

    b_s1 <- b_s1 %>% dplyr::mutate(du_fin=beta.*w_q9_1*du-averse[q9_1])
    b_s1 <- b_s1 %>% dplyr::mutate(du_social = dplyr::case_when((old_solar1 > 0 | old_solar2 > 0)~0,(old_solar1==0 & old_solar2==0)~w_qsp21*du_social[qsp21]))
    #+lambda. or +w_theta*lambda.?
    b_s1 <- b_s1 %>% dplyr::mutate(du_theta = dplyr::case_when((old_solar1 > 0 | old_solar2 > 0)~0,(old_solar1==0 & old_solar2==0)~w_theta*theta+ lambda.))
    b_s1 <- b_s1 %>% dplyr::mutate(du_tot = du_fin+du_social+du_theta)
    #some agents do not transact even when du_fin > 0
    b_s_transact <- b_s1 %>% dplyr::filter(du_tot > 0)
    b_s_notransact <- b_s1 %>% dplyr::filter(du_tot <= 0)
    #update areas for agents who transacted
    b_s_transact <- b_s_transact %>% dplyr::mutate(area1 = area1 - (new_solar1-old_solar1)/kWpm2, area2 = area2 - (new_solar2-old_solar2)/kWpm2)
    b_s_transact$transaction <- T
    b_s_notransact$transaction <- F
    #this line reverses transactions did not occur because du_tot was negative
    b_s_notransact <- b_s_notransact %>% dplyr::mutate(new_solar1=old_solar1,new_solar2=old_solar2,new_battery=old_battery,new_imports=old_imports,new_exports=old_exports)
    #b_s <- update_cars(b_s,params)
    #
    a_s <- dplyr::filter(a_s, !(ID %in% c(b_s_notransact$ID,b_s_transact$ID)))
    a_s <- dplyr::bind_rows(a_s,b_s_notransact,b_s_transact) %>% dplyr::arrange(as.numeric(ID))
    a_s <- a_s %>% dplyr::mutate(new_solar1 = tidyr::replace_na(new_solar1,0), new_solar2 = tidyr::replace_na(new_solar2,0),new_battery = tidyr::replace_na(new_battery,0))
    #recompute social variable
    ma <- igraph::get.adjacency(social_network)
    g <- social_network %>% tidygraph::activate(nodes) %>% dplyr::left_join(a_s,by="ID")
    #social network conformity effect
    #
    #fossil_nodes  <- igraph::V(g)$fuel == "fossil"
    #adopter_nodes <- igraph::V(g)$old_solar == 0 & igraph::V(g)$new_solar > 0
    adopter_nodes <- igraph::V(g)$new_solar1 > 0 | igraph::V(g)$new_solar2 > 0
    a_s$qsp21 <- as.numeric(ma %*% adopter_nodes) #social reinforcement
    if(ignore_social) a_s$qsp21 <- 0 #no pvs assumed present in local network
    a_s <- a_s %>% dplyr::rowwise() %>% dplyr::mutate(qsp21 = min(qsp21+1,3)) #qsp21 encoding 1,2,3
    #agents_out <- a_s
    a_s <- a_s %>% dplyr::select(-du,-du_fin,-du_social,-du_theta,-du_tot)
    print(paste("time", round(yeartime,1), "PV system adopters",dim(a_s %>% dplyr::filter( (new_solar1 > 0 & old_solar1==0) | (new_solar2 > 0 & old_solar2==0)))[1]))
    print(paste("PV system augmentors",dim(a_s %>% dplyr::filter((old_solar1 > 0 & new_solar1 > old_solar1) | (old_solar2 > 0 & new_solar2 > old_solar2) | (old_solar1 > 0 & new_battery > old_battery) || (old_solar2 > 0 & new_battery > old_battery) ))[1]))
    return(dplyr::ungroup(a_s))
  }
}

#agents_init <- initialise_agents(sD,2010,cal_run=1) #cal run is irrelevant here


#' get_financial_utility_scale
#'
#' @param agents_in agent chacteristics e.g. output by initialise_agents()
#' @param cal_run calibration run in 1 to 100
#'
#' @returns a scalar
#' @export
#'
#' @examples
get_financial_utility_scale <- function(agents_in,cal_run){

  gen_optimised_pvbess <- function(agents_in,n_sample=nrow(pv_survey_oo),tariff_plan="night_saver",no_grant = FALSE){

    survey_time <- 2024
    params <- scenario_params(sD,yeartime)
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
  IQR(survey_u$du_average)/IQR(survey_u$savings) %>% return()

}


