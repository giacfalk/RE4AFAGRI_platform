# Estimate the yearly electric demand from healthcare and education facilities
# define consumption of facility types (kWh/facility/year)
clusters <- st_as_sf(clusters)

clusters$beds_1 <- lengths(st_intersects(clusters_voronoi, health %>% filter(Tier==1)))
clusters$beds_2 <- lengths(st_intersects(clusters_voronoi, health %>% filter(Tier==2)))
clusters$beds_3 <- lengths(st_intersects(clusters_voronoi, health %>% filter(Tier==3)))
clusters$beds_4 <- lengths(st_intersects(clusters_voronoi, health %>% filter(Tier==4)))

clusters$beds_1 <- clusters$beds_1 * 1  
clusters$beds_2 <- clusters$beds_2 * beds_tier2
clusters$beds_3 <- clusters$beds_3 * beds_tier3
clusters$beds_4 <- clusters$beds_4 * beds_tier4

clusters$schools <- lengths(st_intersects(clusters_voronoi, primaryschools)) * pupils_per_school

for (m in 1:12){
  for (i in 1:24){
    
    aa <- clusters
    aa$geom=NULL
    aa$geometry=NULL
    
    clusters = mutate(clusters, !!paste0('er_hc_' , as.character(m) , "_" , as.character(i)) := (pull(!!as.name(paste0('health1', "_" , as.character(m))))[i] * clusters$beds_1 + pull(!!as.name(paste0('health2', "_" , as.character(m))))[i] * clusters$beds_2 + pull(!!as.name(paste0('health3', "_" , as.character(m))))[i] * clusters$beds_3 + pull(!!as.name(paste0('health4', "_" , as.character(m))))[i] * clusters$beds_4) * el_access_share_target * (match(scenarios$planning_year[scenario], planning_year) / length(planning_year)))
    
    aa <- clusters
    aa$geom=NULL

    clusters = mutate(clusters, !!paste0('er_sch_' , as.character(m) , "_" , as.character(i)) := ( pull(!!as.name(paste0('edu', "_" , as.character(m))))[i] / pupils_per_school * clusters$schools) * el_access_share_target * (match(scenarios$planning_year[scenario], planning_year) / length(planning_year)) )
    
    aa <- clusters
    aa$geom=NULL
    aa$geometry=NULL
    
  }}

for (m in 1:12){
  
  aa <- clusters
  aa$geom=NULL
  aa$geometry=NULL
  
  out = aa %>% dplyr::select(starts_with(paste0("er_hc_", as.character(m), "_"))) %>% rowSums(.)
  clusters[paste0('er_hc_tt' ,"_monthly_" , as.character(m))] = out
  
}

aa <- clusters
aa$geom=NULL
aa$geometry=NULL

# Generate variable for total daily demand and variables as shares of the daily demand
out = aa %>% dplyr::select(starts_with("er_hc_tt_monthly_")) %>% rowSums(.)
clusters$er_hc_tt = out

for (m in 1:12){
  
  aa <- clusters
  aa$geom=NULL
  aa$geometry=NULL
  
  out = aa %>% dplyr::select(starts_with(paste0("er_sch_", as.character(m), "_"))) %>% rowSums(.)
  clusters[paste0('er_sch_tt' ,"_monthly_" , as.character(m))] = out
  
}


aa <- clusters
aa$geom=NULL
aa$geometry=NULL

out = aa %>% dplyr::select(starts_with("er_sch_tt_monthly_")) %>% rowSums(.)
clusters$er_sch_tt = out

if (output_hourly_resolution==F){
  
  ### remove the hourly fields
  
}


save.image(paste0("results/", countrystudy, "/clusters_healthedu.Rdata"))
