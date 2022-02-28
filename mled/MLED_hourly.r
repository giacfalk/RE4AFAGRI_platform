# MLED - Multisectoral Latent Electricity Demand assessment platform
# v2 (LEAP_RE adaptation)
# 28/02/2022

####
# system parameters

setwd("D:/OneDrive - IIASA/RE4AFAGRI_platform/mled") # path of the cloned M-LED repository

db_folder = 'F:/MLED_database' # path where to download the M-LED database

email<- "giacomo.falchetta@gmail.com" # previously enabled to use Google Earth Engine

download_data <- F # flag: download the M-LED database? F if you already have done so previously.

######################
# country

countrystudy <- "zambia" # country to run M-LED on
exclude_countries <- paste("rwanda", sep="|") # countries to exclude the database files from the current run

######################
# scenarios

planning_year = 2050 # horizon year to make projections
scenarios <-scenario <- c("ssp2") # list SSP scenario to run
rcp <- c("rcp26") # list RCP scenario to run

######################
# options and constriants 

groundwater_sustainability_contraint <- T # impose limit on water pumping based on groundwater recharge
field_size_contraint <- T # only consider small farmland patches (smallholder farming)
VAT_import_costs <- T # include VAT and import costs on PV and batteries in the analysis
instalments_business_model <- T # upfront lump sum required for PV & appliances
process_already_irrigated_crops <- T # crop processing: also include energy demand to process yield in currently irrigated land

######################
# run the analysis
 
timestamp()
source("backend.R")

for (scenario in scenarios){
  
# Select the scenario to be operated
timestamp()
source(paste0(input_country_specific, "scenario_", countrystudy, ".R"))

# Create catchment areas to link agricultural land to population clusters
timestamp()
source("create_clusters_voronoi.R")

# Irrigation demand
timestamp()
source("crop_module.R")

# Water pumping to energy
timestamp()
source("pumping_module.R")

# Residential energy demand
timestamp()
source("residential.R")

# Health and education demand 
timestamp()
source("health_education_module.R")

# Crop processing and storage
timestamp()
source("crop_processing.R")

# Other productive: SMEs
timestamp()
source("other_productive.R")


####

# Write output for soft-linking into OnSSET and NEST and for online visualisation

demand_fields <- c("PerHHD_tt", "residual_productive_tt", "er_hc_tt", "er_sch_tt", "er_kwh_tt", "kwh_cropproc_tt")

clusters_onsset <- dplyr::select(clusters, demand_fields)
write_sf(clusters_onsset, paste0(countrystudy, "_onsset_clusters_with_mled_loads.gpkg"))

id <- fasterize(clusters_nest, rainfed[[1]], "OBJECTID")

clusters_onsset$OBJECTID <- exact_extract(id, clusters_onsset, "majority")
clusters_onsset$geometry <- NULL
clusters_onsset <- group_by(clusters_onsset, OBJECTID) %>% summarise_all(., sum, na.rm=T)

clusters_nest <- merge(clusters_nest, clusters_onsset, "OBJECTID")

write_sf(clusters_nest, paste0(countrystudy, "_nest_clusters_with_mled_loads.gpkg"))

write_sf(clusters, paste0(countrystudy, "_clusters_for_dashboard.geojson"))


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
# Plotting



}


