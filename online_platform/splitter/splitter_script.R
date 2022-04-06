library(sf)
library(tidyverse)
library(googledrive)

#

#gadm3 <- read_sf("D:/OneDrive - IIASA/RE4AFAGRI_platform/online_platform/splitter/gadm404.gpkg")

load("D:/OneDrive - IIASA/RE4AFAGRI_platform/mled/results/zambia/clusters_other_productive.Rdata")

gadm3 <- read_sf("D:/OneDrive - IIASA/RE4AFAGRI_platform/mled/country_studies/zambia/mled_inputs/gadm40_ZMB_shp/gadm40_ZMB_2.shp") %>% dplyr::select(NAME_2)

sf <- clusters

# select columns of interest

sf <- dplyr::select(sf, c("PerHHD_tt", "residual_productive_tt", "er_hc_tt", "er_sch_tt", "er_kwh_tt", "kwh_cp_tt"), starts_with("PerHHD_tt_monthly"), starts_with("er_hc_tt_monthly"),  starts_with("er_sch_tt_monthly"), (starts_with("er_kwh") & !contains("surface")), starts_with("kwh_cp"), contains("IRREQ"), (starts_with("yield") & ends_with("_tot") & !contains("irr")), (starts_with("A_")), (starts_with("Y_")))

# add A_tot (all crops)

aa <- sf
aa <- aa %>% dplyr::select(starts_with("A_") & !contains("irr")) %>% st_set_geometry(NULL) %>% mutate(A_tot = rowSums(., na.rm=T))
sf$A_tot <- aa$A_tot

# add A_tot_irrig (all crops)

aa <- sf
aa <- aa %>% dplyr::select(starts_with("A_") & contains("irr")) %>% st_set_geometry(NULL) %>% mutate(A_tot_irr = rowSums(., na.rm=T))
sf$A_tot_irr <- aa$A_tot_irr

library(fasterize)
library(raster)

r <- raster(); res(r)<-.25;
r <- rgis::mask_raster_to_polygon(r, gadm3)

gadm3$field <- 1:nrow(gadm3)

gadm3_r <- fasterize::fasterize(gadm3, r, "field", fun="first")
gadm3_r <- rgis::mask_raster_to_polygon(gadm3_r, gadm3)

sf$field <- exactextractr::exact_extract(gadm3_r, sf, "majority")

gadm3_nogeo <- gadm3
gadm3_nogeo$geometry <- NULL

sf <- merge(sf, gadm3_nogeo, by="field")

sf <- filter(sf, !is.na(sf$NAME_2))

sf_split <- split(sf, sf$NAME_2)

up <- vector()

for (i in 1:length(sf_split)){
  
  write_sf(sf_split[[i]], paste0("D:/OneDrive - IIASA/RE4AFAGRI_platform/mled/results/zambia/", sf_split[[i]]$NAME_2[1], "_clusters_RE4AFAGRI.gpkg"))
  
  ups <- drive_upload(paste0("D:/OneDrive - IIASA/RE4AFAGRI_platform/mled/results/zambia/", sf_split[[i]]$NAME_2[1], "_clusters_RE4AFAGRI.gpkg"), path = as_id("10M6XISwiiU9Sy7vI37t7W_jcl1mvjQlF"))
  
  up[i] <- ups$drive_resource[[1]]$webContentLink 
            
  }

sf$geometry <- NULL
sf$field <- NULL

# summarise all numeric

sf <- sf %>% group_by(NAME_2) %>% summarise_if(is.numeric, sum, na.rm=T)

sf <- merge(sf, gadm3, "NAME_2")
sf$field <- NULL

sf <- bind_cols(sf, up)

colnames(sf)[(ncol(sf)-1):ncol(sf)] <- c("geometry", "link")

sf <- st_as_sf(sf)

write_sf(sf, "D:/OneDrive - IIASA/RE4AFAGRI_platform/online_platform/data/gadm_for_viz_zambia.geojson")
