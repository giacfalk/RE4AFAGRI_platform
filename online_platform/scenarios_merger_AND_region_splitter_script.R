library(sf)
library(tidyverse)
library(googledrive)

#

f <- list.files(path="D:/OneDrive - IIASA/RE4AFAGRI_platform/mled/results/zambia", pattern="gadm2_with", full.names = T)

f_out <- lapply(f, read_sf)

for (i in 1:length(f_out)){
  
  stri1 <- qdapRegex::ex_between(f[i], "ssp", "_")[[1]]
  stri2 <- qdapRegex::ex_between(f[i], "rcp", "_")[[1]]
  
  names(f_out[[i]])[15:534] <- paste0(names(f_out[[i]])[15:534], "_ssp", stri1, "_rcp", stri2)
  
}

#

for (i in c(2:length(f_out))){
  
  f_out[[i]] <- dplyr::select(f_out[[i]], 15:534)
  st_geometry(f_out[[i]]) <- NULL
  
}

f_out_b <- bind_cols(f_out)
sf <- st_as_sf(f_out_b)

#

sf_split <- split(sf, sf$NAME_2)

up <- vector()

for (i in 1:length(sf_split)){
  
  write_sf(sf_split[[i]], paste0("D:/OneDrive - IIASA/RE4AFAGRI_platform/mled/results/zambia/", sf_split[[i]]$NAME_2[1], "_clusters_RE4AFAGRI.gpkg"))
  
  ups <- drive_upload(paste0("D:/OneDrive - IIASA/RE4AFAGRI_platform/mled/results/zambia/", sf_split[[i]]$NAME_2[1], "_clusters_RE4AFAGRI.gpkg"), path = as_id("10M6XISwiiU9Sy7vI37t7W_jcl1mvjQlF"))
  
  up[i] <- ups$drive_resource[[1]]$webContentLink 
            
  }

#

sf <- bind_cols(sf, up)

colnames(sf)[ncol(sf)] <- c("link")

sf <- st_as_sf(sf)

write_sf(f_out_b, paste0("results/", countrystudy, "_gadm2_with_mled_loads_ALL_SCENARIOS.gpkg"))
