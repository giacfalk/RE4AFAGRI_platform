#####################
# Parameters
#####################

# Country parameters
countryname = 'Rwanda' 
countryiso3 = 'RWA' # ISO3
national_official_population = 12950000 # number of people # REF:
national_official_elrate = 0.41 # national electricity access rate
national_official_population_without_access = national_official_population- (national_official_population*national_official_elrate) # number of people without access

# Planning horizon parameters
today = 2022
planning_year = 2040
planning_horizon = planning_year - today
discount_rate = 0.15 

# Energy efficiency improvement factors
eff_impr_rur1= 0.05
eff_impr_rur2= 0.075
eff_impr_rur3= 0.1
eff_impr_rur4= 0.125
eff_impr_rur5= 0.15

eff_impr_urb1= 0.05
eff_impr_urb2= 0.075
eff_impr_urb3= 0.1
eff_impr_urb4= 0.125
eff_impr_urb5= 0.15

# Groundwater pump technical parameters
rho = 1000 # density of water (1000 kg / m3)
g = 9.81 # gravitational constant (m / s2)
c = 3.6 * (10^6) # differential pressure, (Pa)

# Surface water parameters
water_speed = 2 #m/s, https://www.engineeringtoolbox.com/flow-velocity-water-pipes-d_385.html
water_viscosity = 0.00089 #https://www.engineersedge.com/physics/water__density_viscosity_specific_weight_13146.htm
pipe_diameter = 0.8 # m

#Threshold parameters
threshold_surfacewater_distance = 5000 # (m): distance threshold which discriminates if groundwater pumping is necessary or a surface pump is enough # REF:
threshold_groundwater_pumping = 15 # (m): maximum depth at which the model allows for water pumping: beyond it, no chance to install the pump # REF:

#number of hours of pumping, 3 during the morning and 3 during the evening
nhours_irr = 6
eta_pump = 0.75

# Transportation costs
fuel_consumption = 15 # (l/h) # REF: OnSSET
fuel_cost = 1 # (USD/l) # baseline, then adapted based on distance to city
truck_bearing_t = 15 # (tons) # REF: https://en.wikipedia.org/wiki/Dump_truck

#Pumo economic parameters
LCOE_for_pumping = 0.08 # USD/kWh # REF:
lifetimepump = 15

# import local crop prices csv.  Structure: X, Y, City, cropname1, cropname 2, ...
prices <- read.csv(paste0(input_country_specific, "FAOSTAT_data_8-11-2021.csv"))

prices <- prices %>% filter(Area==countryname) %>% group_by(Area, Item) %>% dplyr::summarise(Value=last(Value)) %>% ungroup()

parser <- read.csv(paste0(input_country_specific, "parser.csv"))

prices <- merge(prices, parser, all.x=T, by="Item")
prices <- na.exclude(prices)
prices <- prices %>% group_by(spam, Area) %>% summarise(Value=mean(Value, na.rm=T)) %>% ungroup()

for(i in 3:ncol(prices)){
  prices[,i][is.na(prices[,i])] <- mean(pull(prices[,i]), na.rm = TRUE)
}

# Appliances cost, household (check ./ramp/ramp/RAMP_households/Appliances cost.xlsx for inputs)
rur1_app_cost=154
rur2_app_cost=171
rur3_app_cost=278
rur4_app_cost=958
rur5_app_cost=1905

urb1_app_cost=113
urb2_app_cost=307
urb3_app_cost=902
urb4_app_cost=1464
urb5_app_cost=2994

# Appliances cost, schools (check ./ramp/ramp/RAMP_social/Appliances_schools.xlsx for inputs)
sch_1_app_cost = 60
sch_2_app_cost = 360
sch_3_app_cost = 1590
sch_4_app_cost = 2550
sch_5_app_cost = 3220

# Appliances cost, healtchare  (check ./ramp/ramp/RAMP_social/Appliances_healthcare.xlsx for inputs)
hc_1_app_cost = 110
hc_2_app_cost = 4710
hc_3_app_cost = 95060
hc_4_app_cost = 305660
hc_5_app_cost = 611450

#####################
# Input data
#####################

#
#clusters <- read_sf(paste0(home_repo_folder , 'clusters_final.gpkg'))

# Country and provinces shapefiles

gadm0 = read_rds(paste0(input_country_specific , 'gadm36_' , countryiso3 , '_0_sf.rds'))
gadm1 = read_rds(paste0(input_country_specific , 'gadm36_' , countryiso3 , '_1_sf.rds'))
gadm2 = read_rds(paste0(input_country_specific , 'gadm36_' , countryiso3 , '_2_sf.rds'))

provinces <- gadm1

# Define extent of country analysed
ext = extent(gadm0)

field_size <- raster(paste0(spam_folder, "global_field_size/field_size_10_40_cropland.img"))
field_size <- mask_raster_to_polygon(field_size, gadm0)

# maxflow of a pump in m3/s 
maxflow_boundaries <- c(0.000277778, 0.00277778) #i.e. 1-10 m3/h
maxflow <- field_size
values(maxflow) <- scales::rescale(values(maxflow), to = maxflow_boundaries)    
#https://assets.rikolto.org/paragraph/attachments/potential_irrigation_technologies_for_smallholder_farmers_in_tanzania_0.pdf

# Import cropland extent (Default dataset used: GFSAD30CE)
cropland_extent = lapply(list.files(path=spam_folder, pattern = "GFSAD30", full.names = T, recursive = T), raster)
extents <- lapply(cropland_extent, extent)
extent_country <- extent(gadm0)
extents <- which(unlist(lapply(lapply(extents, function(X){intersect(extent_country, X)}), is.null)) == FALSE)

cropland_extent <- cropland_extent[extents]

# Import climatezones (Default datasets used: GAEZ soil classes)
climatezones = raster(paste0(input_folder , 'GAEZ_climatezones.tif'))
climatezones <- mask_raster_to_polygon(climatezones, gadm0)

# Import diesel price layer (In each pixel: 2015 prices baseline , cost per transporting it from large cities)
diesel_price = raster(paste0(input_folder , 'diesel_price_baseline_countryspecific.tif'))
diesel_price <- mask_raster_to_polygon(diesel_price, gadm0)

#
crops = readxl::read_xlsx(paste0(input_country_specific , 'crops_cfs_ndays_months.xlsx'))

# Import csv of energy consumption by crop 
energy_crops = read.csv(paste0(input_country_specific,'crop_processing.csv'))

#
roads<-raster(paste0(input_folder, 'grip4_total_dens_m_km2.asc'), crs="+proj=longlat +datum=WGS84")
roads <- mask_raster_to_polygon(roads, gadm0)

dhs <- read_sf(paste0(input_folder, 'statcompiler_subnational_data_2020-03-17/shps/sdr_subnational_data_dhs_2015.shp'))
dhs <- filter(dhs, dhs$ISO==countrycode(countryiso3, "iso3c", "iso2c"))

empl_wealth<- read_sf(paste0(input_folder, 'sdr_subnational_data_dhs_2014.shp'))
empl_wealth <- filter(empl_wealth, empl_wealth$ISO==countrycode(countryiso3, "iso3c", "iso2c"))

traveltime <- raster(paste0(input_folder, 'travel.tif'))
traveltime <- mask_raster_to_polygon(traveltime, gadm0)

#
DepthToGroundwater = read.delim(paste0(input_folder , 'DepthToGroundwater/xyzASCII_dtwmap_v1.txt'), sep='\t')
GroundwaterStorage = read.delim(paste0(input_folder , 'GroundwaterStorage/xyzASCII_gwstor_v1.txt'), sep='\t')
GroundwaterProductivity = read.delim(paste0(input_folder , 'GroundwaterProductivity/xyzASCII_gwprod_v1.txt'), sep='\t')

#
urbrur <- raster(paste0(input_folder , 'GHSL_settlement_type.tif'))
urbrur <- mask_raster_to_polygon(urbrur, gadm0)

#
raster_tiers = raster(paste0(input_folder , 'tiersofaccess_SSA_2018.nc'))
raster_tiers <- mask_raster_to_polygon(raster_tiers, gadm0)

#

if (scenario == "baseline"){
  
  population <- raster(paste0(input_folder, "GHS_POP_E2015_GLOBE_R2019A_4326_30ss_V1_0.tif"))
  # irrigation
  population <- rgis::mask_raster_to_polygon(population, gadm0)
  
  #
  pet = stack(paste0(input_folder , "TerraClimate/TerraClimate_pet_2015.nc"))
  ppt = stack(paste0(input_folder , "TerraClimate/TerraClimate_ppt_2015.tif"))
  soil = stack(paste0(input_folder , "TerraClimate/TerraClimate_soil_2015.nc"))
  
  #
  
}

# Classifying schools and healthcare facilities
health = read_sf(paste0(input_country_specific , 'health.geojson'))

#Import primaryschools
primaryschools = read_sf(paste0(input_country_specific , 'schools.geojson'))

#####################
# Assumed load curves
#####################

# crop processing
load_curve_cp = c(0, 0, 0, 0, 0, 0, 0.0833, 0.0833, 0.0833, 0.0833, 0.0833, 0.0833, 0.0833, 0.0833, 0.0833, 0.0833, 0.0833, 0.0833, 0, 0, 0, 0, 0, 0)

# irrigation
load_curve_irrig = c(0, 0, 0, 0, 0, 0.166, 0.166, 0.166, 0.166, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.166, 0.166)
load_curve_irr = load_curve_irrig

# import load curve of productive activities
load_curve_prod_act <- read.csv(paste0(input_folder, 'productive profile.csv'))
