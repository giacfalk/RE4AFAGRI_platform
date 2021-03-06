
## This R-script:
##      1) estimates electricity demand from schools and healthcare facilities in each cluster to meet RAMP-generated appliance-based loads based on the predicted tier/size of each facility at each time step

##

# Estimate the yearly electric demand from healthcare and education facilities
# define consumption of facility types (kWh/facility/year)
clusters <- st_as_sf(clusters)

clusters$beds_1 <- lengths(st_intersects(clusters_voronoi, health %>% filter(Tier==1)))
clusters$beds_2 <- lengths(st_intersects(clusters_voronoi, health %>% filter(Tier==2)))
clusters$beds_3 <- lengths(st_intersects(clusters_voronoi, health %>% filter(Tier==3)))
clusters$beds_4 <- lengths(st_intersects(clusters_voronoi, health %>% filter(Tier==4)))

# adjust to fill (potential) missing data

clusters$beds_1 <- ifelse(clusters$population>50 & clusters$beds_1==0, 1, clusters$beds_1)
clusters$beds_2 <- ifelse(clusters$population>100 & clusters$beds_2==0, 1, clusters$beds_2)
clusters$beds_3 <- ifelse(clusters$population>500 & clusters$beds_3==0 & clusters$isurban==1, 1, clusters$beds_3)
clusters$beds_4 <- ifelse(clusters$population>1000 & clusters$beds_4==0 & clusters$isurban==1, 1, clusters$beds_4)

#

clusters$beds_1 <- clusters$beds_1 * 1  
clusters$beds_2 <- clusters$beds_2 * beds_tier2
clusters$beds_3 <- clusters$beds_3 * beds_tier3
clusters$beds_4 <- clusters$beds_4 * beds_tier4

clusters$schools <- lengths(st_intersects(clusters_voronoi, primaryschools)) 

# adjust to fill (potential) missing data

clusters$schools <- ifelse(clusters$population>100 & clusters$schools==0, 1, clusters$schools)

clusters$schools <- clusters$schools * pupils_per_school

#

for (timestep in planning_year){
  

for (m in 1:12){
  for (i in 1:24){
    
    aa <- clusters
    aa$geom=NULL
    aa$geometry=NULL
    
    clusters = mutate(clusters, !!paste0('er_hc_' , as.character(m) , "_" , as.character(i)) := (pull(!!as.name(paste0('health1', "_" , as.character(m))))[i] * clusters$beds_1 + pull(!!as.name(paste0('health2', "_" , as.character(m))))[i] * clusters$beds_2 + pull(!!as.name(paste0('health3', "_" , as.character(m))))[i] * clusters$beds_3 + pull(!!as.name(paste0('health4', "_" , as.character(m))))[i] * clusters$beds_4) * el_access_share_target * (match(timestep, planning_year) / length(planning_year))) 
    
    aa <- clusters
    aa$geom=NULL

    clusters = mutate(clusters, !!paste0('er_sch_' , as.character(m) , "_" , as.character(i)) := ( pull(!!as.name(paste0('edu', "_" , as.character(m))))[i] / pupils_per_school * clusters$schools) * el_access_share_target * (match(timestep, planning_year) / length(planning_year))) 
    
    aa <- clusters
    aa$geom=NULL
    aa$geometry=NULL
    
  }}

for (m in 1:12){
  
  aa <- clusters
  aa$geom=NULL
  aa$geometry=NULL
  
  out = aa %>% dplyr::select(starts_with(paste0("er_hc_", as.character(m), "_"))) %>% rowSums(.)
  clusters[paste0('er_hc_tt' ,"_monthly_" , as.character(m), "_", timestep)] = out
  
}

aa <- clusters
aa$geom=NULL
aa$geometry=NULL

# Generate variable for total daily demand and variables as shares of the daily demand
out = aa %>% dplyr::select(starts_with("er_hc_tt_monthly_") & contains(as.character(timestep))) %>% rowSums(.)
clusters[paste0('er_hc_tt_', timestep)] = as.numeric(out)

for (m in 1:12){
  
  aa <- clusters
  aa$geom=NULL
  aa$geometry=NULL
  
  out = aa %>% dplyr::select(starts_with(paste0("er_sch_", as.character(m), "_"))) %>% rowSums(.)
  clusters[paste0('er_sch_tt' ,"_monthly_" , as.character(m), "_", timestep)] = out
  
}


aa <- clusters
aa$geom=NULL
aa$geometry=NULL

out = aa %>% dplyr::select(starts_with("er_sch_tt_monthly_") & contains(as.character(timestep))) %>% rowSums(.)
clusters[paste0('er_sch_tt_', timestep)] = out

}



if (output_hourly_resolution==F){
  
  ### remove the hourly fields
  
}


save.image(paste0(processed_folder, "clusters_healthedu.Rdata"))
