# extract nighttime lights above mining sites

replacement = ee$Image(0)
noise_floor <- 0.25
nl =  ee$ImageCollection("NOAA/VIIRS/DNB/MONTHLY_V1/VCMCFG")$filterDate('2021-01-01', '2022-01-01')$select('avg_rad')$median()$subtract(0.125)
nl = nl$where(nl$lt(noise_floor), replacement)
mining_sites_nl <- (nl$reduceRegions(reducer = ee$Reducer$sum(), collection=sf_as_ee(mining_sites), scale=450) %>% ee_as_sf() %>% dplyr::select(sum) %>% st_set_geometry(NULL))$sum

mining_sites$ntl <- mining_sites_nl

#

mining_sites$AREA <- ifelse(mining_sites$ntl==0, 0, mining_sites$AREA)

mining_sites$mining_kwh_tt <- zambia_industry_final_demand_tot * (mining_sites$AREA / sum(mining_sites$AREA))

clusters_mining <- mining_sites

clusters_mining_r <- fasterize(clusters_mining, population_baseline, "mining_kwh_tt")

clusters$mining_kwh_tt <- exact_extract(clusters_mining_r, clusters, "mean")

clusters$mining_kwh_tt <- ifelse(is.na(clusters$mining_kwh_tt), 0, clusters$mining_kwh_tt)

clusters$mining_kwh_tt <- clusters$mining_kwh_tt * (zambia_industry_final_demand_tot / sum(clusters$mining_kwh_tt , na.rm=T))
