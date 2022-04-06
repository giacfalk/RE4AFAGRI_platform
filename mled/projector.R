# gridded population 
population_fut <- stack(find_it("population_ssp2soc_0p5deg_annual_2006-2100.nc"))[[-c(1:14)]]
population_fut <- exact_extract(population_fut, gadm2, "sum")

colnames(population_fut) <- paste0("pop_", 2020:planning_year)

population_fut_gr <- population_fut

for (i in 2:ncol(population_fut)){
  
  population_fut_gr[,i] <- ( population_fut[,i] -  population_fut[,i-1]) / population_fut[,i-1]
  
}

population_fut_gr[,1] <- NULL
colnames(population_fut_gr) <- paste0("pop_gr_", 2021:planning_year)

population_fut_gr$geometry <- gadm2$geometry
population_fut_gr <- st_as_sf(population_fut_gr)

# fasterize all layers and extract them into clusters !

template <- stack(find_it("population_ssp2soc_0p5deg_annual_2006-2100.nc"))[[1]]

out <- list()
j = 0

list_cols <- colnames(population_fut_gr)

for (i in list_cols[1:30]){

  j = j+1
  
  out[[j]] <- fasterize(population_fut_gr, template, i, "first")
  
}

population_fut_gr <- exact_extract(stack(out), clusters, "mean")
colnames(population_fut_gr) <- list_cols[1:30]

population_fut_gr <- population_fut_gr %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) 

clusters <- bind_cols(clusters, population_fut_gr)

######################################

# gridded gdp (planning year)
gdp_future <- stack(find_it(paste0("gdp_", scenario, "soc_10km_2010-2100.nc")))[[2:5]]
gdp_fut <- exact_extract(gdp_future, gadm2, "sum")

colnames(gdp_fut) <- paste0("gdp_", seq(2020, planning_year, 10))

gdp_fut_gr <- gdp_fut

population_fut <- dplyr::select(population_fut, matches("2020|2030|2040|2050"))

for (i in 2:ncol(gdp_fut)){
  
  gdp_fut_gr[,i] <- ( (gdp_fut[,i] / population_fut[,i]) -  (gdp_fut[,i-1] / population_fut[,i-1])) / (gdp_fut[,i-1] / population_fut[,i-1])
  
}

gdp_fut_gr[,1] <- NULL
colnames(gdp_fut_gr) <- paste0("gdp_gr_", seq(2030, planning_year, 10))

#

gdp_fut_gr$geometry <- gadm2$geometry
gdp_fut_gr <- st_as_sf(gdp_fut_gr)

#

wealth_baseline_r <- rasterize(wealth_baseline, gdp_future[[1]], "awi")
wealth_baseline <- exact_extract(wealth_baseline_r, clusters, "mean")

gdp_fut_gr_r <- list()

for (i in 1:(ncol(gdp_fut_gr)-1)){
  
  gdp_fut_gr_r[[i]] <- rasterize(gdp_fut_gr, gdp_future[[1]],  colnames(gdp_fut_gr)[i])
  
}

gdp_fut_gr_r <- stack(gdp_fut_gr_r)
gdp_fut_gr_r <- exact_extract(gdp_fut_gr_r, clusters, "mean")
gdp_fut_gr_r$geometry<-NULL

colnames(gdp_fut_gr_r) <- paste0("gdp_gr_", seq(2030, planning_year, 10))

gdp_fut_gr_r <- gdp_fut_gr_r %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) 

wealth_baseline <- data.frame(gdp_capita_2020 = wealth_baseline)
wealth_baseline <- wealth_baseline %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) 

for (year in seq(2030, planning_year, 10)){

wealth_baseline[paste0("gdp_capita_", year)] <- wealth_baseline[paste0("gdp_capita_", (year - 10))] * (1+(gdp_fut_gr_r[paste0("gdp_gr_", year)]))

}

clusters <- bind_cols(clusters, wealth_baseline)

######################################

# gridded urbanisation

clusters$isurban <- ifelse(clusters$isurban>0, 1, 0)

urban_future<- list.files(path=paste0(input_folder, "UrbanFraction_1km_GEOTIFF_Projections_SSPs1-5_2010-2100_v1"), recursive = T, pattern=scenario, full.names = T)

urban_future <- stack(lapply(urban_future, raster))
urban_future <- urban_future[[grep(planning_year, names(urban_future))]]
clusters$isurban_future <- ifelse(exact_extract(urban_future, clusters, "max")>0.01, 1, 0)
clusters$isurban_future <- ifelse(clusters$isurban==1, 1, clusters$isurban_future)

######################################

# future groundwater recharge

s_future <- stack(find_it(paste0("lpjml_gfdl-esm2m_ewembi_", rcp, "_", rcp, "soc_co2_qr_global_monthly_2006_2099.nc4")))[[which.min(abs(seq(2006, planning_year, 1) - planning_year))]]

