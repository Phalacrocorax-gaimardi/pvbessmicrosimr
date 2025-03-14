

#' make_artificial_society
#'
#' Creates a homophilous social influence network
#'
#' @param society tibble containing society
#' @param homophily tibble with homophily parameters for society
#' @param nu gower distance exponent
#'
#' @return a tidygraph object
#' @export
#' @importFrom magrittr %>%
#' @examples
make_artificial_society <- function(society=society,homophily=homophily,nu=4.5){
  #create a random homophilous social network
  #social distance measure=gower distance
  #nu gives the social distance decay exponentlarger mu higher assortativity
  #agents with degree zero remain degree zero but there may be additional nodes with degree zero
  society <- society %>% dplyr::mutate(degree = dplyr::if_else(is.na(degree), sample(c(0,1:5,15,25),size=1,prob = c(0.51016949,0.03389831,0.10338983,0.09322034,0.08644068,0.09152542,0.06101695,0.02033898)),degree))
  society_factor <- unclass(society %>% dplyr::mutate_if(is.character,as.factor)) %>% as.data.frame()
  society_factor1 <- dplyr::filter(society_factor,degree != 0)
  society1 <- dplyr::filter(society, degree != 0)

  N_society1 <- nrow(society1)


  zeronodes <- dplyr::filter(society,degree==0)$ID #nodes with no influencers

  dist_mat <- cluster::daisy(society_factor1[,seq(2,dim(society)[2])], metric ="gower", weights=homophily$weights) %>% as.matrix()
  prob_mat <- (1-dist_mat)^nu
  prob_mat1 <- 1.1*prob_mat %*% diag(society_factor1$degree/apply(prob_mat,2,sum)) #adjust this parameter

  nodes <- tidyr::tibble(ID=society1$ID)
  edges <- tidyr::expand_grid(from=1:N_society1,to=1:N_society1)
  edges <- dplyr::filter(edges, from < to) #avoid loops
  #edges <- filter(edges, !(from %in% zeronodes)) #no edges frsociom zeronodes
  edges <- edges %>% dplyr::rowwise() %>% dplyr::mutate(p=prob_mat1[from,to])
  edges <- edges %>% dplyr::rowwise() %>% dplyr::mutate(keep=ifelse(stats::runif(1)< p,T,F))
  edges <- edges %>% dplyr::filter(keep)

  edges <- edges[,1:2]
  edges$from <- nodes$ID[edges$from] #relable to orginal ids
  edges$to <- nodes$ID[edges$to]
  #restore zero nodes
  nodes <- dplyr::bind_rows(nodes,tidyr::tibble(ID=zeronodes)) %>% dplyr::arrange(ID)
  #restore
  g <- tidygraph::tbl_graph(nodes=nodes,edges=edges,directed=F) %>% dplyr::inner_join(society,by="ID")
  return(g)

}

#g <- make_artificial_society(pv_society_oo,homophily,4.5)
#pv_soc <- pv_society_oo %>% dplyr::mutate(degree = dplyr::if_else(is.na(degree), sample(c(0,1:5,15,25),size=1,prob = c(0.51016949,0.03389831,0.10338983,0.09322034,0.08644068,0.09152542,0.06101695,0.02033898)),degree))

#cor(igraph::degree(g),pv_soc$degree) #correlation is 73%
#plot(igraph::degree(g), pv_soc$degree)


#' get_network_characteristics
#'
#' @param society society used to construct homophilous network
#' @param g homophilous social network
#'
#' @return table
#' @export
#'
#' @examples
get_network_characteristics <- function(society,g){
  homophily1 <- homophily %>% dplyr::rowwise() %>% dplyr::mutate( assortativity= igraph::assortativity_nominal(g,as.integer(factor(dplyr::pull(society,variable)))))
  knitr::kable(homophily1 %>% dplyr::filter(variable != "degree")) %>% kableExtra::kable_styling()
}

#get_network_characteristics(pv_society_oo,g)

#igraph::transitivity(g,type="global") #low transitivity ... need clique models

#social influence network compatible with

#df <- tibble()
#for(nu in seq(0.1,12,by=0.1)){
#
#   g <- make_artificial_society(pv_society_oo,homophily,nu=nu)
#   df <- bind_rows(df, tibble(nu=nu, deg = mean(igraph::degree(g))))#, simil_areatype = assortativity(g,factor(society$area_type)),simil_education = assortativity(g,factor(society$education))))
#}

#df_0 %>% ggplot( aes(deg,nu)) + geom_point() + geom_vline(xintercept=mean(pv_society_oo$degree)) + geom_smooth()+scale_x_continuous(breaks=0:12)
# observed degrees fixes nu=4.5
#df %>% select(-deg) %>% pivot_longer(-nu) %>% ggplot( aes(nu,value,colour=name)) + geom_point()

#pv_society_oo
#zet_survey_lab <- readxl::read_xlsx("~/Policy/SurveyDataAndAnalysis/Data/ZET_survey_2024_data_labels.xlsx",sheet=1)
#zet_survey <- readxl::read_xlsx("~/Policy/SurveyDataAndAnalysis/Data/ZET_survey_2024_values.xlsx",sheet=1)

#zet1 <- zet_survey_lab %>% dplyr::filter(serial %in% pv_survey_oo$serial)
#zet1$ID <- 1:nrow(zet1)
#society_codes <- c("ID","qb","qa","qc2","qd","qf","qg","q29")
#zet1 <- zet1 %>% dplyr::select(all_of(society_codes))
#names(zet1) <- c("ID","age","gender","region","class","education","area_type","degree")
#recode_degree <- function(char){

 # dplyr::case_when(char==1~1,char=="10-19"~15,char=="2"~2,char=="20+"~25,char=="3"~3,char=="4"~4,char=="5"~5,char=="Don't know"~NA,char=="None"~0)
#}
#recode_eduation <- function(n){

 # qanda %>% filter(question_code == "qf")
#  n_new <- case_when( n %in% c(1:4,9)~1, !(n %in% c(1:4,9))~n-3)
#  return(n_new)
#}
#recode_income <- function(n){

  #qanda %>% filter(question_code == "qh")
#  n_new <- case_when( n %in% c(1:2)~1, n %in% 3:4~2, n %in% 5:6~3, n %in% 7:8~4, n %in% 9:11~5, n==12~6)
#  return(n_new)
#}

#zet1 <- zet1 %>% dplyr::mutate(degree=recode_degree(degree))
#pv_survey <- zet_survey %>% dplyr::select(pv_questions$question_code)
#pv_society_oo <- zet1



