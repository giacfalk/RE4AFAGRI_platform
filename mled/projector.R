####

# gridded population 
population_fut <- stack(find_it("population_ssp2soc_0p5deg_annual_2006-2100.nc"))[[-c(1:14)]]
population_fut <- exact_extract(population_fut, gadm2, "sum")

colnames(population_fut) <- paste0("X", 2020:2100)

population_fut_gr <- population_fut

for (i in 2:ncol(population_fut)){
  
  population_fut_gr[,i] <- ( population_fut[,i] -  population_fut[,i-1]) / population_fut[,i-1]
  
}

population_fut_gr[,1] <- NULL
colnames(population_fut_gr) <- paste0("X", 2021:2100)

gadm2 <- bind_cols(gadm2, population_fut, population_fut_gr)

######################################

# gridded gdp (planning year)
gdp <- stack(find_it(paste0("gdp_", scenario, "soc_10km_2010-2100.nc")))[[-2]]

######################################

# gdp capita

gdp_fut <- exact_extract(gdp, gadm2, "sum")
gdp_capita <- gdp_fut[,1:4] / population_fut[,c("X2020", "X2030", "X2040", "X2050")]

gdp_capita_gr <- gdp_capita

for (i in 2:ncol(gdp_capita)){
  
  gdp_capita_gr[,i] <- ( gdp_capita[,i] -  gdp_capita[,i-1]) / gdp_capita[,i-1]
  
}

gdp_capita_gr[,1] <- NULL
colnames(gdp_capita_gr) <- paste0("X", seq(2030, 2050, 10))

gadm2 <- bind_cols(gadm2, gdp_capita, gdp_capita_gr)

# gridded urbanisation (growth)

urbanization_baseline <- clamp(urbanization_baseline, 20, useValue=TRUE)
urbanization_baseline <- mask_raster_to_polygon(urbanization_baseline, st_as_sfc(st_bbox(clusters)))
population_baseline_agg <- aggregate(population_baseline, fact=10, fun="sum")
population_baseline_agg <- projectRaster(population_baseline_agg, urbanization_baseline)
values(population_baseline_agg)[is.na(values(urbanization_baseline))] <- NA

# future groundwater recharge

s_future <- find_it(stack(paste0("lpjml_gfdl-esm2m_ewembi_rcp26_", rcp, "soc_co2_qr_global_monthly_2006_2099.nc4")))

                    
