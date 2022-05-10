eastern <- c("Tanzania", "Kenya", "Uganda", "Ethiopia", "Rwanda", "Burundi")
western <- c( "Nigeria", "Benin", "Togo", "Ghana", "Cote d'Ivoire", "Liberia", "Sierra Leone", "Guinea",  "Guinea-Bissau")
northern <- c("Morocco", "Algeria", "Tunisia", "Libya", "Egypt")
sahel <- c( "Mauritania", "Senegal", "Gambia", "Mali", "Burkina Faso", "Niger", "Chad", "Sudan", "South Sudan", "Somalia", "Djibouti", "Eritrea")
southern <- c( "South Africa", "Namibia", "Botswana", "Lesotho",  "Eswanti")
southeast <- c( "Zimbabwe", "Zambia", "Mozambique", "Malawi")
central <- c( "Angola"," Democratic Republic of the Congo", "Congo", "Gabon", "Cameroon", "Equatorial Guinea", "Central African Republic")

eastern <- countrycode::countrycode(eastern, 'country.name', 'iso3c')
western <- countrycode::countrycode(western, 'country.name', 'iso3c')
northern <- countrycode::countrycode(northern, 'country.name', 'iso3c')
sahel <- countrycode::countrycode(sahel, 'country.name', 'iso3c')
southern <- countrycode::countrycode(southern, 'country.name', 'iso3c')
southeast <- countrycode::countrycode(southeast, 'country.name', 'iso3c')
central <- countrycode::countrycode(central, 'country.name', 'iso3c')

js <- read_sf(find_it("crop_mask-regions-deafrica-data.geojson"))
js <- st_intersection(js, gadm0)

y_bounds <- substr(js$label, 6, 8)
x_bounds <- substr(js$label, 2, 4)

types <- ifelse(countryiso3 %in% northern, "northern", ifelse(countryiso3 %in% central, "central", ifelse(countryiso3 %in% southern, "southern", ifelse(countryiso3 %in% western, "western", ifelse(countryiso3 %in% eastern, "eastern", ifelse(countryiso3 %in% southeast, "southeast", ifelse(countryiso3 %in% northern, "northern", NA)))))))

grid <- data.frame(types, x_bounds, y_bounds)
colnames(grid) <- c("types", "x", "y")

stub <- vector()

for (i in 1:nrow(grid)){

stub[i] <- paste0("https://deafrica-services.s3.af-south-1.amazonaws.com/crop_mask_", grid$types[i], "/1-0-0/x", grid$x[i], "/y", grid$y[i], "/2019--P1Y/crop_mask_", grid$types[i], "_x", grid$x[i], "y", grid$y[i], "_2019--P1Y_mask.tif")

}

###

grid <- grid[!(basename(stub) %in% all_input_files_basename),]

dir.create(file.path(input_folder, "hrs_cropland"), showWarnings = FALSE)
oldw <- getOption("warn")
options(warn = -1)

for (i in as.numeric(rownames(grid))){
print(i)
  tryCatch(download.file(stub[i], destfile = paste0(input_folder, "hrs_cropland/", basename(stub)[i]), mode="wb"), 
           error = function(e) print('Skipped'))    
}

options(warn = oldw)

#

r <- basename(stub)

cropland <- list()

for (i in 1:length(r)){
  print(i)
  cropland[[i]] <- exact_extract(raster(paste0(input_folder, "hrs_cropland/", r[i]))==1, clusters_voronoi, "sum")
  
}

clusters_voronoi$high_res_cropland_area_ha <- Reduce(`+`, cropland) * 0.01

###

id_raster <- files[[1]]
values(id_raster) <- 1:ncell(id_raster)
clusters_voronoi$downscaling_id <- exact_extract(id_raster, clusters_voronoi, "majority")

aa <- clusters_voronoi
aa$geom=NULL
aa$geometry=NULL

aa <- aa %>% group_by(downscaling_id) %>% mutate(total_crarea_underneath=sum(high_res_cropland_area_ha))
aa$crshare_sp <- aa$high_res_cropland_area_ha / aa$total_crarea_underneath
aa$crshare_sp <- ifelse(is.na(aa$crshare_sp), 0, aa$crshare_sp)

clusters_voronoi$crshare_sp <- aa$crshare_sp


