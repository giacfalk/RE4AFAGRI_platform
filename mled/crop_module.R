rainfed <- gsub("_2", "_1", rainfed) # to cope with current february bug!
rainfed2 <- mixedsort(rainfed)
rainfed2 <- pblapply(rainfed2, raster)

for (i in 1:length(rainfed2)){
  crs(rainfed2[[i]]) <- as.character(CRS("+init=epsg:4236"))
}

if(field_size_contraint==T){field_size <- projectRaster(field_size, mask_raster_to_polygon(rainfed2[[1]], st_as_sfc(st_bbox(clusters_voronoi))), method = "bilinear") ; m <- field_size; m[m > 29] <- NA; field_size <- mask(field_size, m); rainfed2 <- pblapply(rainfed2, function(X){mask_raster_to_polygon(X, st_as_sfc(st_bbox(clusters_voronoi)))}); for (i in 1:length(rainfed2)){crs(rainfed2[[i]]) <- crs(field_size)}; field_size <- projectRaster(field_size,rainfed2[[1]]); rainfed2 <- pblapply(rainfed2, function(X){mask(X, field_size)})}

rainfed2 <- split(rainfed2,  unlist(qdapRegex::ex_between(rainfed, "irrigation/", "/I_")))

rainfed <- pblapply(rainfed2, stack)

#

clusters_voronoi$area <- as.numeric(st_area(clusters_voronoi)) * 0.0001 # in hectares

# extract total bluewater demand in each cluster

files = list.files(path = paste0(input_folder, "spam_folder/spam2010v1r0_global_harv_area.geotiff") , pattern = 'r.tif', full.names = T)
files_crops <- unlist(qdapRegex::ex_between(files, "-area_", "_r.tif"))
files_crops <- files_crops %in% names(rainfed)
files <- files[files_crops]

# mm to m3 -> 1 mm supplies 0.001 m3 per m^2 of soil

files <- pblapply(files, raster)
gc()
rainfed <- pblapply(1:length(files), function(X) {stack(rainfed[[X]] * files[[X]] * 10)})

# sum by month

rainfed_sum <- rainfed

for (m in 1:12){
  
  rainfed_sum[[m]] <- do.call("sum", c(pblapply(1:sum(files_crops), function(X){rainfed[[X]][[m]]}), na.rm = TRUE))
  
}

rainfed_sum <- stack(rainfed_sum)
rainfed <- rainfed_sum

for (i in 1:12){
  
  clusters_voronoi[paste0('monthly_IRREQ' , "_" , as.character(i))] <- exact_extract(rainfed[[i]], clusters_voronoi, "sum")
}

# Apply sustainability constraint for groundwater depletion

s <- s[[c((nlayers(s)-11):nlayers(s))]] # for speed, consider only the latest year
index <- rep(1:12, nlayers(s)/12)
s <- stackApply(s, index, fun = mean)

s <- s * 60*60*24*30  #convert to mm per month

for (i in 1:12){
  
  clusters_voronoi[paste0('monthly_GQ' , "_" , as.character(i))] <- exact_extract(s[[i]], clusters_voronoi, "mean") * clusters_voronoi$area * 10
}


if(groundwater_sustainability_contraint==T){
  
  for (i in 1:12){
    
    aa <- clusters_voronoi
    aa$geom=NULL
    
    clusters_voronoi[paste0('monthly_unmet_IRRIG_share' , "_" , as.character(i))] <- as.numeric(ifelse((unlist(aa[paste0('monthly_GQ' , "_" , as.character(i))]) < unlist(aa[paste0('monthly_IRREQ' , "_" , as.character(i))]))==TRUE, (unlist(aa[paste0('monthly_IRREQ' , "_" , as.character(i))]) - unlist(aa[paste0('monthly_GQ' , "_" , as.character(i))]))/ unlist(aa[paste0('monthly_IRREQ' , "_" , as.character(i))]), 0))
    
  }}

clusters_voronoi$maxflow <- exact_extract(maxflow, clusters_voronoi, "mean")

clusters_voronoi_data <- clusters_voronoi
clusters_voronoi_data$geom <- NULL
clusters_voronoi_data <- dplyr::select(clusters_voronoi_data, starts_with("monthly"), area, maxflow)

clusters <- bind_cols(clusters, clusters_voronoi_data)

saveRDS(clusters, "clusters_crop_module.Rds")
