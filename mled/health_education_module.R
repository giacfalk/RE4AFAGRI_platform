# Estimate the yearly electric demand from healthcare and education facilities
# define consumption of facility types (kWh/facility/year)

clusters$beds_1 <- lengths(st_intersects(clusters, health %>% filter(Tier==1 & Country==countryname)))
clusters$beds_2 <- lengths(st_intersects(clusters, health %>% filter(Tier==2 & Country==countryname)))
clusters$beds_3 <- lengths(st_intersects(clusters, health %>% filter(Tier==3 & Country==countryname)))
clusters$beds_4 <- lengths(st_intersects(clusters, health %>% filter(Tier==4 & Country==countryname)))

clusters$beds_1 <- clusters$beds_1 * 1  
clusters$beds_2 <- clusters$beds_2 * 45
clusters$beds_3 <- clusters$beds_3 * 150
clusters$beds_4 <- clusters$beds_4 * 450

clusters$schools <- lengths(st_intersects(clusters, primaryschools)) * 700

clusters$schools <- clusters$schools * 700


for (m in 1:12){
  for (i in 1:24){
    
    aa <- clusters
    aa$geometry=NULL
    aa$geom=NULL
    
    clusters = mutate(clusters, !!paste0('er_hc_' , as.character(m) , "_" , as.character(i)) := pull(!!as.name(paste0('health1', "_" , as.character(m))))[i] * clusters$beds_1 + pull(!!as.name(paste0('health2', "_" , as.character(m))))[i] * clusters$beds_2 + pull(!!as.name(paste0('health3', "_" , as.character(m))))[i] * clusters$beds_3 + pull(!!as.name(paste0('health4', "_" , as.character(m))))[i] * clusters$beds_4)
    
    aa <- clusters
    aa$geometry=NULL
    aa$geom=NULL
    
    clusters = mutate(clusters, !!paste0('er_sch_' , as.character(m) , "_" , as.character(i)) := pull(!!as.name(paste0('edu', "_" , as.character(m))))[i] /700 * clusters$schools)
    
    aa <- clusters
    aa$geometry=NULL
    aa$geom=NULL
    
    # Schools and healthcare facilities are assumed to be already electrified in the total electricity access level in the cluster is > 0.75
    clusters[paste0('er_hc_' , as.character(m) , "_" , as.character(i))] = ifelse(clusters$elrate > 0.75, 0,  pull(aa[paste0('er_hc_' , as.character(m) , "_" , as.character(i))]))
    
    clusters[paste0('er_sch_' , as.character(m) , "_" , as.character(i))] = ifelse(clusters$elrate > 0.75, 0,  pull(aa[paste0('er_sch_' , as.character(m) , "_" , as.character(i))]))
    
  }}

for (m in 1:12){
  
  aa <- clusters
  aa$geometry=NULL
  aa$geom=NULL
  
  out = aa %>% dplyr::select(starts_with(paste0("er_hc_", as.character(m), "_"))) %>% rowSums(.)
  clusters[paste0('er_hc_tt' ,"_monthly_" , as.character(m))] = out
  
}

aa <- clusters
aa$geometry=NULL
aa$geom=NULL

# Generate variable for total daily demand and variables as shares of the daily demand
out = aa %>% dplyr::select(starts_with("er_hc_tt_monthly_")) %>% rowSums(.)
clusters$er_hc_tt = out

for (m in 1:12){
  
  aa <- clusters
  aa$geometry=NULL
  aa$geom=NULL
  
  out = aa %>% dplyr::select(starts_with(paste0("er_sch_", as.character(m), "_"))) %>% rowSums(.)
  clusters[paste0('er_sch_tt' ,"_monthly_" , as.character(m))] = out
  
}


aa <- clusters
aa$geometry=NULL
aa$geom=NULL

out = aa %>% dplyr::select(starts_with("er_sch_tt_monthly_")) %>% rowSums(.)
clusters$er_sch_tt = out
