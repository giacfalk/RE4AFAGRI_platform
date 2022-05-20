# extract nighttime lights above mining sites

replacement = ee$Image(0)
replacement_top = ee$Image(100)

noise_floor <- 0.25
noise_floor_top <- 100

nl =  ee$Image("users/giacomofalchetta/ntl_payne_2021")$subtract(0.125)
nl = nl$where(nl$lt(noise_floor), replacement)
nl = nl$where(nl$gt(noise_floor_top), replacement_top)

mining_sites_nl <- (nl$reduceRegions(reducer = ee$Reducer$sum(), collection=sf_as_ee(mining_sites), scale=450) %>% ee_as_sf() %>% dplyr::select(sum) %>% st_set_geometry(NULL))$sum

mining_sites$ntl <- mining_sites_nl

#

mining_sites <- filter(mining_sites, ntl>0)

mining_sites$mining_kwh_tt <- zambia_industry_final_demand_tot * (mining_sites$ntl / sum(mining_sites$ntl))

clusters_mining <- mining_sites

clusters_mining_r <- fasterize(clusters_mining, population_baseline, "mining_kwh_tt")

clusters$mining_kwh_tt <- exact_extract(clusters_mining_r, clusters, "mean")

rm(clusters_mining_r)

clusters$mining_kwh_tt <- ifelse(is.na(clusters$mining_kwh_tt), 0, clusters$mining_kwh_tt)

clusters$mining_kwh_tt <- clusters$mining_kwh_tt * (zambia_industry_final_demand_tot / sum(clusters$mining_kwh_tt , na.rm=T))


for (timestep in planning_year){
  
  clusters[paste0("mining_kwh_tt_" , timestep)] <-  clusters$mining_kwh_tt* (1 + ((pull(aa[paste0("gdp_capita_", timestep)]) - clusters$gdp_capita_2020) / clusters$gdp_capita_2020))

}

save.image(paste0(processed_folder, "clusters_mining_module.Rdata"))
