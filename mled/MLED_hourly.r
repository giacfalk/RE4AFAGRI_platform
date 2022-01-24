# MLED - Multisectoral Latent Electricity Demand assessment platform
# v2 (LEAP_RE adaptation)
# 18/01/2022

####
# system parameters

setwd("D:/OneDrive - IIASA/RE4AFAGRI_platform/mled") # path of the cloned M-LED repository

db_folder = 'F:/MLED_database' # path where to download the M-LED database

email<- "giacomo.falchetta@gmail.com" # previously enabled to use Google Earth Engine

download_data <- F # flag: download the M-LED database? F if you already have done so previously.

######################
# scenarios

countrystudy <- "zambia" # country to run M-LED on

scenarios <-c("baseline")# list scenarios to run , "SSP2RCP45") 

######################
# options and constriants 

groundwater_sustainability_contraint <- T # impose limit on water pumping based on groundwater recharge
field_size_contraint <- T # only consider small farmland patches (smallholder farming)
VAT_import_costs <- T # include VAT and import costs on PV and batteries in the analysis
instalments_business_model <- T # upfront lump sum required for PV & appliances

######################
# run the analysis

timestamp()
source("backend.R", echo = T)

for (scenario in scenarios){
  
# Select the scenario to be operated
timestamp()
source(paste0(input_country_specific, "scenario_", countrystudy, ".R"), echo = T)

# Estimate electrification
timestamp()
source("electrification_estimation.R", echo = T)
#

# Cropland and irrigation demand
timestamp()
source("crop_module.R", echo = T)


# Water pumping to energy
timestamp()
source("groundwater_module.R", echo = T)


# Residential energy demand
timestamp()
source("residential.R", echo = T)


# Health and education demand 
timestamp()
source("health_education_module.R", echo = T)


# Other productive
timestamp()
source("other_productive.R", echo = T)


####

# Write output for soft-linking into OnSSET and NEST




#################
# Economic analysis

# Estimate pumps installation costs
timestamp()
source("pumps_installation_costs.R", echo = T)

# Estimate cost of meeting demand with solar pumps
timestamp()
source("process_energy_costs.R", echo = T)

# Estimate revenues and carry out an economic analysis
timestamp()
source("estimate_economic_revenues.R", echo = T)

# Analyse the potential food security implications
timestamp()
source("food_security_implications.R", echo = T)

#################
# Plotting



}


