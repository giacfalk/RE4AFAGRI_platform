mining_sites$mining_kwh_tt <- zambia_industry_final_demand_tot / mining_sites$AREA

clusters_mining <- mining_sites

clusters_mining_r <- fasterize(clusters_mining, population_baseline, "mining_kwh_tt")

clusters$mining_kwh_tt <- exact_extract(clusters_mining_r, clusters, "mean")

clusters$mining_kwh_tt <- ifelse(is.na(clusters$mining_kwh_tt), 0, clusters$mining_kwh_tt)

clusters$mining_kwh_tt <- clusters$mining_kwh_tt * (sum(mining_sites$mining_kwh_tt, na.rm=T) / sum(clusters$mining_kwh_tt , na.rm=T))
