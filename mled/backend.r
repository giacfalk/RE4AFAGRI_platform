if (!require("pacman")) install.packages("pacman"); library(pacman)

pacman::p_load(sf, raster, exactextractr, dplyr, readxl, cowplot, ggplot2, scales, tidyr, tidyverse, rgeos, gdalUtils, chron, nngeo, strex, rgee, data.table, gdata, FactoMineR, factoextra, maps  , mapdata, maptools, grid, randomForestSRC, countrycode, remotes, stars, gdistance, rgl, rasterVis, qlcMatrix, stars, tvm, gtools, wbstats, stars, patchwork, ggrepel, terra, pbapply, googledrive, nnet, caret, randomForest, beepr, ncdf4, s2, zip, sfsmisc, dissever, lsa, doBy, geojsonio)

tmpDir(create=TRUE)

if (!require("rgis")) remotes::install_github("JGCRI/rgis"); library(rgis)
if (!require("fasterize")) remotes::install_github("ecohealthalliance/fasterize"); library(fasterize)

mask_raster_to_polygon <- function (raster_object, polygon) 
{
  if (class(polygon)[[1]] != "sf") 
    polygon <- st_as_sf(polygon)
  r_crs <- st_crs(projection(raster_object))
  polys <- polygon %>% st_transform(crs = r_crs)
  n_lcs <- crop(raster_object, polys) %>% mask(polys)
  return(n_lcs)
}

if (!isTRUE(ee_check())) {ee_install()}
ee_Initialize(email, drive = TRUE)

###########

repo_folder <- home_repo_folder <- getwd()

if (download_data==T){
  
  wd_bk <- getwd()
  
  out <- read_rds("download_data_index.rds")
  all_input_files_stub <- read_rds("download_data_index_stubs.rds")
  
  setwd(db_folder)
  
  r <- sapply(file.path(db_folder, dirname(all_input_files_stub)), 
         dir.create, recursive = TRUE, showWarnings = FALSE)
  
  for (i in 1:length(all_input_files_stub)){
    print(paste0("Downloading database. Progress: ", as.character(round((i/length(all_input_files_stub))*100, 3)), "%"))
    drive_download(file=out[[i]]$id,
                   path = paste0(getwd(), "/", all_input_files_stub[i]), overwrite = T
    )}
  
  setwd(wd_bk)
  
}


#

input_folder = paste0(db_folder , '/input_folder/')
dir.create(file.path(input_folder), showWarnings = FALSE)
processed_folder = paste0(input_folder , '/processed_folder/')
dir.create(file.path(processed_folder), showWarnings = FALSE)
output_figures_folder = paste0(repo_folder , '/output_figures/')
dir.create(file.path(output_figures_folder), showWarnings = FALSE)
input_country_specific <- paste0(input_folder, "/country_studies/", countrystudy, "/mled_inputs/")
dir.create(file.path(input_country_specific), showWarnings = FALSE)

all_input_files <- list.files(path=input_folder, recursive = T, full.names = T)

all_input_files <- all_input_files[-grep(exclude_countries, all_input_files,ignore.case=TRUE)]

all_input_files <- all_input_files[-grep("\\.ini$|\\.docx$|\\.png$|\\.r$|\\.mat$|r_tmp_|results|\\.pyc$|\\.pdf$|\\.rds$|\\.rdata$|\\.dbf$|\\.xml$", all_input_files,ignore.case=TRUE)] 

all_input_files <- gsub("//", "/", all_input_files)

all_input_files_basename <- basename(all_input_files)

user.input <- function(prompt) {
    x= readline(prompt)
    return(x)
  }


find_it <- function(X){
  
    out_file <- all_input_files[str_detect(all_input_files_basename, paste0('\\b', X, '\\b'))]
  
  if(length(out_file)>1){
    
    beep()
    print(out_file)
    pick_one <- user.input("Which one: ")
    return(out_file[as.numeric(pick_one)])
    
    } 
  
  if(length(out_file)==0){
    
    beep()
    stop("Cannot find file")

  } else {
  
  return(out_file)

}}
