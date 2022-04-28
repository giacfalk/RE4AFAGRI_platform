if (!require("pacman")) install.packages("pacman"); library(pacman)

pacman::p_load(sf, raster, exactextractr, dplyr, readxl, cowplot, ggplot2, scales, tidyr, tidyverse, rgeos, gdalUtils, chron, nngeo, strex, rgee, data.table, gdata, FactoMineR, factoextra, maps  , mapdata, maptools, grid, randomForestSRC, countrycode, remotes, stars, gdistance, rgl, rasterVis, qlcMatrix, stars, tvm, gtools, wbstats, stars, patchwork, ggrepel, terra, pbapply, googledrive, nnet, caret, randomForest, fasterize, beepr, ncdf4, s2, zip, sfsmisc, dissever, lsa, doBy)

tmpDir(create=TRUE)

if (!require("qgisprocess")) remotes::install_github("paleolimbot/qgisprocess"); library(qgisprocess)
qgis_configure()

if (!require("rgis")) remotes::install_github("JGCRI/rgis"); library(rgis)

ee_Initialize(email = email, drive = TRUE)

###########

repo_folder <- home_repo_folder <- getwd()

if (download_data==T){
  
  wd_bk <- getwd()
  
  setwd(db_folder)
  
  #folder link to id
  jp_folder = "https://drive.google.com/drive/folders/1TYvZuIqkHFBSXDZ2O0jqbv3Wb0i901Qq"
  folder_id = drive_get(as_id(jp_folder))
  
  #find files in folder
  files = drive_ls(folder_id)
  
  map(files$id, ~ purrr::safely(drive_download)(as_id(.x)))
  
  setwd(wd_bk)
  
  input_folder <- processed_folder <- health_edu_folder <- output_figures_folder <- input_country_specific <- db_folder
  
}


#

if (download_data==F){

input_folder = paste0(db_folder , '/input_folder/')
dir.create(file.path(input_folder), showWarnings = FALSE)
processed_folder = paste0(db_folder , '/processed_folder/')
dir.create(file.path(processed_folder), showWarnings = FALSE)
output_figures_folder = paste0(repo_folder , '/output_figures/')
dir.create(file.path(output_figures_folder), showWarnings = FALSE)
input_country_specific <- paste0(repo_folder, "/country_studies/", countrystudy, "/mled_inputs/")
dir.create(file.path(input_country_specific), showWarnings = FALSE)

}

all_input_files <- unique(list.files(path=c(input_folder, processed_folder, repo_folder), recursive = T, full.names = T))

all_input_files <- all_input_files[-grep("\\.docx$", all_input_files,ignore.case=TRUE)]
all_input_files <- all_input_files[-grep("\\.png$", all_input_files,ignore.case=TRUE)]
all_input_files <- all_input_files[-grep("\\.r$", all_input_files,ignore.case=TRUE)]
all_input_files <- all_input_files[-grep("mat", all_input_files,ignore.case=TRUE)]
all_input_files <- all_input_files[-grep("r_tmp_", all_input_files,ignore.case=TRUE)]
all_input_files <- all_input_files[-grep("results", all_input_files,ignore.case=TRUE)]
all_input_files <- all_input_files[-grep(".pyc", all_input_files,ignore.case=TRUE)]
all_input_files <- all_input_files[-grep(".pdf", all_input_files,ignore.case=TRUE)]
all_input_files <- all_input_files[-grep(".rds", all_input_files,ignore.case=TRUE)]
all_input_files <- all_input_files[-grep(".rdata", all_input_files,ignore.case=TRUE)]
all_input_files <- all_input_files[-grep(".dbf", all_input_files)]
all_input_files <- all_input_files[-grep(".xml", all_input_files)]
all_input_files <- all_input_files[-grep(exclude_countries, all_input_files)]

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
