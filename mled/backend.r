if (!require("pacman")) install.packages("pacman"); library(pacman)

pacman::p_load(sf, raster, exactextractr, dplyr, readxl, cowplot, ggplot2, scales, tidyr, tidyverse, rgeos, gdalUtils, chron, nngeo, strex, rgee, data.table, gdata, FactoMineR, factoextra, maps  , mapdata, maptools, grid, randomForestSRC, countrycode, remotes, stars, gdistance, rgl, rasterVis, qlcMatrix, stars, tvm, gtools, wbstats, stars, patchwork, ggrepel, terra, pbapply, googledrive)

tmpDir(create=TRUE)

if (!require("qgisprocess")) remotes::install_github("paleolimbot/qgisprocess"); library(qgisprocess)
qgis_configure()

if (!require("rgis")) remotes::install_github("JGCRI/rgis"); library(rgis)

ee_Initialize(email = email, drive = TRUE)

###########

repo_folder <- home_repo_folder <- getwd()
input_folder = paste0(db_folder , '/input_folder/')
processed_folder = paste0(db_folder , '/processed_folder/')
health_edu_folder = paste0(db_folder , '/health_edu_folder/')
output_figures_folder = paste0(repo_folder , '/output_figures/')
input_country_specific <- paste0(repo_folder, "/country_studies/", countrystudy, "/mled_inputs/")

all_input_files <- list.files(path=c(input_folder, processed_folder, repo_folder, processed_folder, health_edu_folder, input_country_specific), recursive = T, full.names = T)
all_input_files_basename <- basename(all_input_files)

find_it <- function(X){

  out_file <- all_input_files[match(X, all_input_files_basename)]
  
  if(length(out_file)>1){stop("Multiple files selected, fix duplicated filenames.")} else{
  
  return(out_file)

}}
