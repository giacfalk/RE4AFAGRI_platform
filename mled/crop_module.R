rainfed <- gsub("_2", "_1", rainfed) # to cope with current february bug!
rainfed2 <- mixedsort(rainfed)
rainfed2 <- pblapply(rainfed2, raster)

for (i in 1:length(rainfed2)){
  crs(rainfed2[[i]]) <- as.character(CRS("+init=epsg:4236"))
}

if(field_size_contraint==T){field_size <- projectRaster(field_size, mask_raster_to_polygon(rainfed2[[1]], st_as_sfc(st_bbox(clusters))), method = "bilinear") ; m <- field_size; m[m > 29] <- NA; field_size <- mask(field_size, m); rainfed2 <- pblapply(rainfed2, function(X){mask_raster_to_polygon(X, st_as_sfc(st_bbox(clusters)))}); for (i in 1:length(rainfed2)){crs(rainfed2[[i]]) <- crs(field_size)}; field_size <- projectRaster(field_size,rainfed2[[1]]); rainfed2 <- pblapply(rainfed2, function(X){mask(X, field_size)})}

rainfed2 <- split(rainfed2,  unlist(qdapRegex::ex_between(rainfed, "irrigation/", "/I_")))

rainfed <- pblapply(rainfed2, stack)

#

clusters$area <- as.numeric(st_area(clusters)) * 0.0001 # in hectares

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
  
  clusters[paste0('monthly_IRREQ' , "_" , as.character(i))] <- exact_extract(rainfed[[i]], clusters, "sum")
}

# Apply sustainability constraint for groundwater depletion

s <- s[[c((nlayers(s)-11):nlayers(s))]]
index <- rep(1:12, nlayers(s)/12)
s <- stackApply(s, index, fun = mean)

s <- s * 60*60*24*30  #convert to mm per month

for (i in 1:12){
  
  clusters[paste0('monthly_GQ' , "_" , as.character(i))] <- exact_extract(s[[i]], clusters, "mean") * clusters$area * 10
}

if(groundwater_sustainability_contraint){
  
  for (i in 1:12){
    
    aa <- clusters
    aa$x=NULL
    
    clusters[paste0('monthly_unmet' , "_" , as.character(i))] <- ifelse(unlist(aa[paste0('monthly_GQ' , "_" , as.character(i))]) < unlist(aa[paste0('monthly_IRREQ' , "_" , as.character(i))]), TRUE, FALSE)
    
    aa <- clusters
    aa$x=NULL
    
    clusters[paste0('monthly_IRREQ' , "_" , as.character(i))] <- ifelse(unlist(aa[paste0('monthly_unmet' , "_" , as.character(i))])==TRUE, unlist(aa[paste0('monthly_GQ' , "_" , as.character(i))]), unlist(aa[paste0('monthly_IRREQ' , "_" , as.character(i))]))
    
  }}

clusters$maxflow <- exact_extract(maxflow, clusters, "mean")