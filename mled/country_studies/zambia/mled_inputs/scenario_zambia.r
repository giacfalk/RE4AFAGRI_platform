if (download_data==T){
  
  wd_bk <- getwd()
  
  setwd(input_folder)
  
  #folder link to id
  jp_folder = "https://drive.google.com/drive/folders/13usehjrqngsAwbXeT60uLpvP2HkdhRm9"
  folder_id = drive_get(as_id(jp_folder))
  
  #find files in folder
  files = drive_ls(folder_id)
  
  #loop dirs and download files inside them
  for (i in seq_along(files$name)) {
    #list files
    i_dir = drive_ls(files[i, ])
    
    #mkdir
    dir.create(files$name[i])
    
    #download files
    for (file_i in seq_along(i_dir$name)) {
      #fails if already exists
      try({
        drive_download(
          as_id(i_dir$id[file_i]),
          path = str_c(files$name[i], "/", i_dir$name[file_i])
        )
      })
    }
  }
  
  
  setwd(wd_bk)
  
}

#####################
# Parameters
#####################

# Country parameters
countryname = 'Zambia' 
countryiso3 = 'ZMB' # ISO3
national_official_population = 18400000 # number of people
national_official_elrate = 0.43 # national residential electricity access rate
national_official_population_without_access = national_official_population- (national_official_population*national_official_elrate) # headcount of people without access

# Planning horizon parameters
today = 2022
planning_year = 2050
planning_horizon = planning_year - today
discount_rate = 0.15 

#Threshold parameters
threshold_surfacewater_distance = 5000 # (m): distance threshold which discriminates if groundwater pumping is necessary or a surface pump is enough # REF:
threshold_groundwater_pumping = 50 # (m): maximum depth at which the model allows for water pumping: beyond it, no chance to install the pump # REF:

# maxflow of a pump in m3/s
maxflow_boundaries <- c(0.000277778, 0.00277778) #i.e. 1-10 m3/h

# Energy efficiency improvement factors for appliances (% per year)
eff_impr_rur1= 0.05 / planning_horizon
eff_impr_rur2= 0.075 / planning_horizon
eff_impr_rur3= 0.1 / planning_horizon
eff_impr_rur4= 0.125 / planning_horizon
eff_impr_rur5= 0.15 / planning_horizon

eff_impr_urb1= 0.05 / planning_horizon
eff_impr_urb2= 0.075 / planning_horizon
eff_impr_urb3= 0.1 / planning_horizon
eff_impr_urb4= 0.125 / planning_horizon
eff_impr_urb5= 0.15 / planning_horizon

eff_impr_crop_proc <- 0.25 / planning_horizon

eff_impr_irrig <- 0.25 / planning_horizon

# efficiency of the water pump
eta_pump = 0.75

# lifetime of the pump
lifetimepump = 20

# Groundwater pump technical parameters
rho = 1000 # density of water (1000 kg / m3)
g = 9.81 # gravitational constant (m / s2)
c = 3.6 * (10^6) # differential pressure, (Pa)

# Surface water parameters
water_speed = 2 #m/s, https://www.engineeringtoolbox.com/flow-velocity-water-pipes-d_385.html
water_viscosity = 0.00089 #https://www.engineersedge.com/physics/water__density_viscosity_specific_weight_13146.htm
pipe_diameter = 0.8 # m

# Transportation costs
fuel_consumption = 15 # (l/h) # REF: OnSSET
fuel_cost = 1 # (USD/l) # baseline, then adapted based on distance to city
truck_bearing_t = 15 # (tons) # REF: https://en.wikipedia.org/wiki/Dump_truck

#####################
# Assumed load curves
#####################

# crop processing
load_curve_cp = c(0, 0, 0, 0, 0, 0, 0.0833, 0.0833, 0.0833, 0.0833, 0.0833, 0.0833, 0.0833, 0.0833, 0.0833, 0.0833, 0.0833, 0.0833, 0, 0, 0, 0, 0, 0)

# irrigation
load_curve_irrig = load_curve_irr = c(0, 0, 0, 0, 0, 0.166, 0.166, 0.166, 0.166, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.166, 0.166)

#number of hours of pumping, 3 during the morning and 3 during the evening
nhours_irr = sum(load_curve_irrig!=0)

# import load curve of productive activities
load_curve_prod_act <- read.csv(find_it('productive profile.csv'))

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

##########
# Country-specific data

clusters <- read_sf(find_it("clusters.shp"))

clusters_nest <- read_sf(find_it("Zambia_NEST_delineation.shp"))

# Country and provinces shapefiles
gadm0 = read_rds(find_it(paste0('gadm36_' , countryiso3 , '_0_sf.rds')))
gadm1 = read_rds(find_it(paste0('gadm36_' , countryiso3 , '_1_sf.rds')))
gadm2 = read_rds(find_it(paste0('gadm36_' , countryiso3 , '_2_sf.rds')))

# Define extent of country analysed
ext = extent(gadm0)

if (scenario == "baseline"){
  
    # gridded population (source: GHS)
  population <- raster(find_it("GHS_POP_E2015_GLOBE_R2019A_4326_30ss_V1_0.tif"))
  population <- rgis::mask_raster_to_polygon(population, gadm0)
  
  # climate data (source: TerraClimate)
  pet = stack(find_it("TerraClimate_pet_2015.nc"))
  ppt = stack(find_it("TerraClimate_ppt_2015.tif"))
  soil = stack(find_it("TerraClimate_soil_2015.nc"))
  
  # groundwater recharge (source: soft-link from WaterCROP)
  rainfed <- list.files(paste0(input_folder, "20211122_irrigation"), full.names = T, pattern = "rainfed", recursive=T)
  
  # groundwater recharge (source: ISIMIP)
    s <- stack(paste0(input_folder, "lpjml_gfdl-esm2m_ewembi_historical_histsoc_co2_qr_global_monthly_1861_2005.nc4"))
    
    # residential appliances ownership and usage 
    
    for (i in 1:12){
      assign(paste0('rur1' , "_" , as.character(i)), read.csv(paste0(home_repo_folder , 'ramp/RAMP_households/Rural/Outputs/Tier-1/output_file_' , as.character(i) , '.csv')) %>% rename(values = X0, minutes = X) %>% mutate(hour=minutes%/%60%%24) %>% group_by(hour) %>% summarise(values=mean(values)/1000)) 
      
      assign(paste0('rur2' , "_" , as.character(i)), read.csv(paste0(home_repo_folder , 'ramp/RAMP_households/Rural/Outputs/Tier-2/output_file_' , as.character(i) , '.csv')) %>% rename(values = X0, minutes = X) %>% mutate(hour=minutes%/%60%%24) %>% group_by(hour) %>% summarise(values=mean(values)/1000)) 
      
      assign(paste0('rur3' , "_" , as.character(i)), read.csv(paste0(home_repo_folder , 'ramp/RAMP_households/Rural/Outputs/Tier-3/output_file_' , as.character(i) , '.csv')) %>% rename(values = X0, minutes = X) %>% mutate(hour=minutes%/%60%%24) %>% group_by(hour) %>% summarise(values=mean(values)/1000)) 
      
      assign(paste0('rur4' , "_" , as.character(i)), read.csv(paste0(home_repo_folder , 'ramp/RAMP_households/Rural/Outputs/Tier-4/output_file_' , as.character(i) , '.csv')) %>% rename(values = X0, minutes = X) %>% mutate(hour=minutes%/%60%%24) %>% group_by(hour) %>% summarise(values=mean(values)/1000)) 
      
      assign(paste0('rur5' , "_" , as.character(i)), read.csv(paste0(home_repo_folder , 'ramp/RAMP_households/Rural/Outputs/Tier-5/output_file_' , as.character(i) , '.csv')) %>% rename(values = X0, minutes = X) %>% mutate(hour=minutes%/%60%%24) %>% group_by(hour) %>% summarise(values=mean(values)/1000)) 
      
    }
    
    for (i in 1:12){
      assign(paste0('urb1' , "_" , as.character(i)), read.csv(paste0(home_repo_folder , 'ramp/RAMP_households/Urban/Outputs/Tier-1/output_file_' , as.character(i) , '.csv')) %>% rename(values = X0, minutes = X) %>% mutate(hour=minutes%/%60%%24) %>% group_by(hour) %>% summarise(values=mean(values)/1000)) 
      
      assign(paste0('urb2' , "_" , as.character(i)), read.csv(paste0(home_repo_folder , 'ramp/RAMP_households/Urban/Outputs/Tier-2/output_file_' , as.character(i) , '.csv')) %>% rename(values = X0, minutes = X) %>% mutate(hour=minutes%/%60%%24) %>% group_by(hour) %>% summarise(values=mean(values)/1000)) 
      
      assign(paste0('urb3' , "_" , as.character(i)), read.csv(paste0(home_repo_folder , 'ramp/RAMP_households/Urban/Outputs/Tier-3/output_file_' , as.character(i) , '.csv')) %>% rename(values = X0, minutes = X) %>% mutate(hour=minutes%/%60%%24) %>% group_by(hour) %>% summarise(values=mean(values)/1000)) 
      
      assign(paste0('urb4' , "_" , as.character(i)), read.csv(paste0(home_repo_folder , 'ramp/RAMP_households/Urban/Outputs/Tier-4/output_file_' , as.character(i) , '.csv')) %>% rename(values = X0, minutes = X) %>% mutate(hour=minutes%/%60%%24) %>% group_by(hour) %>% summarise(values=mean(values)/1000)) 
      
      assign(paste0('urb5' , "_" , as.character(i)), read.csv(paste0(home_repo_folder , 'ramp/RAMP_households/Urban/Outputs/Tier-5/output_file_' , as.character(i) , '.csv')) %>% rename(values = X0, minutes = X) %>% mutate(hour=minutes%/%60%%24) %>% group_by(hour) %>% summarise(values=mean(values)/1000)) 
    
    # healthcare and education appliances ownership and usage 
  
      for (i in 1:12){
        assign(paste0('health1' , "_" , as.character(i)), read.csv(paste0(home_repo_folder , 'ramp/RAMP_services/1.Health/Dispensary/Outputs/output_file_' , as.character(i) , '.csv')) %>% rename(values = X0, minutes = X) %>% mutate(hour=minutes%/%60%%24) %>% group_by(hour) %>% summarise(values=mean(values)/1000)) 
        
        assign(paste0('health2' , "_" , as.character(i)), read.csv(paste0(home_repo_folder , 'ramp/RAMP_services/1.Health/HealthCentre/Outputs/output_file_' , as.character(i) , '.csv')) %>% rename(values = X0, minutes = X) %>% mutate(hour=minutes%/%60%%24) %>% group_by(hour) %>% summarise(values=mean(values)/1000)) 
        
        assign(paste0('health3' , "_" , as.character(i)), read.csv(paste0(home_repo_folder , 'ramp/RAMP_services/1.Health/SubCountyH/Outputs/output_file_' , as.character(i) , '.csv')) %>% rename(values = X0, minutes = X) %>% mutate(hour=minutes%/%60%%24) %>% group_by(hour) %>% summarise(values=mean(values)/1000)) 
        
        assign(paste0('health4' , "_" , as.character(i)), read.csv(paste0(home_repo_folder , 'ramp/RAMP_services/1.Health/SubCountyH/Outputs/output_file_' , as.character(i) , '.csv')) %>% rename(values = X0, minutes = X) %>% mutate(hour=minutes%/%60%%24) %>% group_by(hour) %>% summarise(values=mean(values)*1.3/1000)) 
        
        assign(paste0('health5' , "_" , as.character(i)), read.csv(paste0(home_repo_folder , 'ramp/RAMP_services/1.Health/SubCountyH/Outputs/output_file_' , as.character(i) , '.csv')) %>% rename(values = X0, minutes = X) %>% mutate(hour=minutes%/%60%%24) %>% group_by(hour) %>% summarise(values=mean(values)*1.6/1000)) 
        
      }
      
      
      for (i in 1:12){
        assign(paste0('edu' , "_" , as.character(i)), read.csv(paste0(home_repo_folder , 'ramp/RAMP_services/2.School/Output/output_file_' , as.character(i) , '.csv')) %>% rename(values = X0, minutes = X) %>% mutate(hour=minutes%/%60%%24) %>% group_by(hour) %>% summarise(values=mean(values)/1000)) #/10 schools simulated 
        
      }
      
            
    }
    
  
}

# Taxes on PV equipment
vat_import <- read.csv(find_it("vat_import.csv"), stringsAsFactors = F)
vat_import$ISO3 <- countrycode::countrycode(vat_import[,1], 'country.name', 'iso3c')

crops = readxl::read_xlsx(find_it('crops_cfs_ndays_months.xlsx'))

# Import csv of energy consumption by crop 
energy_crops = read.csv(find_it('crop_processing.csv'))

# Survey data
dhs <- read_sf(find_it('sdr_subnational_data_dhs_2015.shp'))
dhs <- filter(dhs, dhs$ISO==countrycode(countryiso3, "iso3c", "iso2c"))

empl_wealth<- read_sf(find_it('sdr_subnational_data_dhs_2014.shp'))
empl_wealth <- filter(empl_wealth, empl_wealth$ISO==countrycode(countryiso3, "iso3c", "iso2c"))

# Classifying schools and healthcare facilities
health = read_sf(find_it('health.geojson'))

#Import primaryschools
primaryschools = read_sf(find_it('schools.geojson'))

##########
# SSA-wide data

field_size <- raster(find_it("field_size_10_40_cropland.img"))
field_size <- mask_raster_to_polygon(field_size, st_as_sfc(st_bbox(clusters)))
gc()

maxflow <- field_size
gc()
v <- scales::rescale(values(maxflow), to = maxflow_boundaries)
values(maxflow) <- v
rm(v); gc()

traveltime_market = ee$Image("Oxford/MAP/accessibility_to_cities_2015_v1_0")

# Import diesel price layer (In each pixel: 2015 prices baseline , cost per transporting it from large cities)
diesel_price = raster(find_it('diesel_price_baseline_countryspecific.tif'))
diesel_price <- mask_raster_to_polygon(diesel_price, gadm0)

DepthToGroundwater = read.delim(find_it('xyzASCII_dtwmap_v1.txt'), sep='\t')
GroundwaterStorage = read.delim(find_it('xyzASCII_gwstor_v1.txt'), sep='\t')
GroundwaterProductivity = read.delim(find_it('xyzASCII_gwprod_v1.txt'), sep='\t')

# mask for areas near to existing LV grid
# lv_grid_density <- raster(find_it("targets.tif"))
# lv_grid_density <- mask_raster_to_polygon(lv_grid_density, st_as_sfc(st_bbox(clusters)))
# lv_grid_density <- terra::aggregate(lv_grid_density, fun=sum, fact=20)
# writeRaster(lv_grid_density, file=find_it("targets_10km.tif"), overwrite=T)
lv_grid_density <- raster(find_it("targets_10km.tif"))
crs(lv_grid_density) <- crs(population)
lv_grid_density <- lv_grid_density>=1

# Import cropland extent (Default dataset used: GFSAD30CE)
cropland_extent = lapply(list.files(path=input_folder, pattern = "GFSAD30AFCE_", full.names = T, recursive = T), raster)
extents <- lapply(cropland_extent, extent)
extent_country <- extent(gadm0)
extents <- which(unlist(lapply(lapply(extents, function(X){intersect(extent_country, X)}), is.null)) == FALSE)
cropland_extent <- cropland_extent[extents]

# Import climatezones (Default datasets used: GAEZ soil classes)
climatezones = raster(find_it('GAEZ_climatezones.tif'))
climatezones <- mask_raster_to_polygon(climatezones, gadm0)

roads<-raster(find_it('grip4_total_dens_m_km2.asc'), crs="+proj=longlat +datum=WGS84")
roads <- mask_raster_to_polygon(roads, gadm0)

traveltime <- raster(find_it('travel.tif'))
traveltime <- mask_raster_to_polygon(traveltime, gadm0)

urbrur <- raster(find_it('GHSL_settlement_type.tif'))
urbrur <- mask_raster_to_polygon(urbrur, gadm0)

raster_tiers = raster(find_it('tiersofaccess_SSA_2018.nc'))
raster_tiers <- mask_raster_to_polygon(raster_tiers, gadm0)
