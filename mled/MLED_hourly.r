# MLED - Multi-sectoral Latent Electricity Demand assessment platform
# v1.1 (LEAP_RE)
# 11/05/2022

####
# system parameters

setwd("C:/Users/falchetta/Documents/GitHub/mled") # path of the cloned M-LED repository

db_folder = 'H:/My Drive/MLED_database' # path where to download the M-LED database

email<- "giacomo.falchetta@gmail.com" # NB; previously enabled to use Google Earth Engine via https://signup.earthengine.google.com

download_data <- F # flag: download the M-LED database? F if you already have done so previously.

downscale_cropland <- F # flag: downscale the MapSPAM cropland data (10 km resolution) using the Digital Earth Africa crop mask (10 m resolution)?

######################
# country and year

countrystudy <- "zambia" # country to run M-LED on
exclude_countries <- paste("rwanda", "nigeria", "zimbabwe", sep="|") # countries in the database files to exclude from the current run

planning_year = seq(2020, 2050, 10) # time steps and horizon year to make projections

######################
# scenarios

el_access_share_target <- 1 # target share of population with electricity in the planning year
irrigated_cropland_share_target <- 1  # target share of irrigation water demand met in the planning year
crop_processed_share_target <-  1  #target share of crop yield locally processed in the planning year
    
ssp <- c("ssp2") # list SSP scenarios to run
rcp <- c("rcp26") # list RCP scenarios to run

scenarios <- expand.grid(planning_year=planning_year, ssp = ssp, rcp = rcp, el_access_share_target=el_access_share_target, irrigated_cropland_share_target=irrigated_cropland_share_target, crop_processed_share_target=crop_processed_share_target, stringsAsFactors = F)
  
######################
# options and constriants 

output_hourly_resolution <- F  # produce hourly load curves for each month. if false, produce just monthly and yearly totals

no_productive_demand_in_small_clusters <- T

groundwater_sustainability_contraint <- T # impose limit on groundwater pumping based on monthly recharge

buffers_cropland_distance <- T # do not include agricultural loads from cropland distant more than n km (customisable in scenario file) from cluster centroid 

field_size_contraint <- T # consider only small farmland patches (smallholder farming)

process_already_irrigated_crops <- F # crop processing: include energy demand to process yield in already irrigated land

water_tank_storage <- T # water storage is possible

VAT_import_costs <- T # include VAT and import costs on PV and batteries in the solar pumps analysis

instalments_business_model <- T # upfront lump sum spread over lifetime for solar PV pumps costing

######################
# run the analysis

scenario <- 1

timestamp()
source("backend.R")

for (scenario in 1:nrow(scenarios)){

print(paste("Running ", paste(scenarios[scenario,], collapse = "_")))
    
# Load the country and scenario-specific data
timestamp()
source(paste0("scenario_", countrystudy, ".R"))

# Estimate electricity access levels and downscale current consumption level at each cluster
timestamp()
source("electricity_access.R")

# Create catchment areas to link agricultural land to population clusters
timestamp()
source("create_clusters_voronoi.R")

# Irrigation demand
timestamp()
source("crop_module.R")

# Water pumping to energy
timestamp()
source("pumping_module.R")

# Mining
timestamp()
source("mining_module.R")

# Residential energy demand
timestamp()
source("residential.R")

# Health and education demand 
timestamp()
source("health_education_module.R")

# Crop processing and storage
timestamp()
source("crop_processing_catchment_areas.R")

timestamp()
source("crop_processing.R")

# Other productive: SMEs
timestamp()
source("other_productive.R")

# Clean output
timestamp()
source("cleaner.R")

####

# Write output for soft-linking into OnSSET and NEST and for online visualisation

demand_fields <- c("PerHHD_tt", "residual_productive_tt", "er_hc_tt", "er_sch_tt", "er_kwh_tt", "kwh_cp_tt", "mining_kwh_tt")

clusters_onsset <- dplyr::select(clusters, id, starts_with("pop"), contains("isurban"), starts_with("gdp"), all_of(demand_fields))

colnames(clusters_onsset)[match(tail(colnames(clusters_onsset), 8), colnames(clusters_onsset))] <- c("residential", "smes", "healthcare", "schools", "irrigation", "crop_processing", "mining", "geometry")

clusters_onsset[is.na(clusters_onsset)] <- 0

write_sf(clusters_onsset, paste0("results/", countrystudy, "/onsset_clusters_with_mled_loads_", paste(scenarios[scenario,], collapse = "_"), ".gpkg"))

}

write_sf(clusters_voronoi %>% dplyr::select(id), paste0("results/", countrystudy, "/onsset_clusters_voronoi.gpkg"))

############

id <- fasterize(clusters_nest, rainfed[[1]], "OBJECTID")

clusters_onsset$OBJECTID <- exact_extract(id, clusters_onsset, "majority")
clusters_onsset$geom <- NULL
clusters_onsset <- group_by(clusters_onsset, OBJECTID) %>% summarise_all(., sum, na.rm=T)

clusters_nest <- merge(clusters_nest, clusters_onsset, "OBJECTID")

write_sf(clusters_nest, paste0("results/", countrystudy, "/nest_clusters_with_mled_loads_", paste(scenarios[scenario,], collapse = "_"), ".gpkg"))


#

gadm2$id <- 1:nrow(gadm2)
id <- fasterize(gadm2, diesel_price, "id")

clusters_onsset <- dplyr::select(clusters, all_of(demand_fields))
clusters_onsset$id <- exact_extract(id, clusters_onsset, "majority")
clusters_onsset$geom <- NULL
clusters_onsset <- group_by(clusters_onsset, id) %>% summarise_all(., sum, na.rm=T)

gadm2 <- merge(gadm2, clusters_onsset, "id")

write_sf(gadm2, paste0("results/", countrystudy, "/gadm2_with_mled_loads_", paste(scenarios[scenario,], collapse = "_"), ".gpkg"))

#################
# Welfare analysis

# Estimate pumps installation costs
timestamp()
source("pumps_installation_costs.R")

# Estimate cost of meeting demand with solar pumps
timestamp()
source("process_energy_costs.R")

# Estimate revenues and carry out an economic analysis
timestamp()
source("estimate_economic_revenues.R")

# Analyse the potential food security implications
timestamp()
source("food_security_implications.R")

#################
# Plotting and tables

timestamp()
source("generate_output_figures_tables.R")
