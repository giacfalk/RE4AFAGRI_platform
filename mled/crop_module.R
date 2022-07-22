
## This R-script:
##      1) calculates irrigation water needs at each cluster given the constaints imposed in the main M-LED file (e.g. smallholder farming only, or maximum distance to cluster threshold)
##      2) calculates environmental flow constraints at each cluster to avoid aquifer groundwater unsustainable extraction

clusters_buffers_cropland_distance <- fasterize(clusters_buffers_cropland_distance, field_size)

rainfed2 <- mixedsort(rainfed)
rainfed2 <- pblapply(rainfed2, raster)
for (i in 1:length(rainfed2)){
  crs(rainfed2[[i]]) <- as.character(CRS("+init=epsg:4236"))
}

if(field_size_contraint==T){field_size <- projectRaster(field_size, mask_raster_to_polygon(rainfed2[[1]], st_as_sfc(st_bbox(clusters_voronoi))), method = "bilinear") ; m <- field_size; m[m > 29] <- NA; field_size <- mask(field_size, m); rainfed2 <- pblapply(rainfed2, function(X){return(mask_raster_to_polygon(X, st_as_sfc(st_bbox(clusters_voronoi))))}); for (i in 1:length(rainfed2)){crs(rainfed2[[i]]) <- crs(field_size)}; field_size <- projectRaster(field_size,rainfed2[[1]]); rainfed2 <- pblapply(rainfed2, function(X){mask(X, field_size)})}

if(buffers_cropland_distance==T){clusters_buffers_cropland_distance <- projectRaster(clusters_buffers_cropland_distance,rainfed2[[1]], method = "ngb"); rainfed2 <- pblapply(rainfed2, function(X){mask(X, clusters_buffers_cropland_distance)})}

rainfed2 <- split(rainfed2,  tolower(unlist(qdapRegex::ex_between(rainfed, "watercrop/", "/cl"))))

rainfed <- pblapply(rainfed2, stack)

names(rainfed) <- c("barl", "cass", "coco", "cott", "grou", "maiz", "pmil", "smil", "oilp", "pota", "rape", "rice", "sorg", "soyb", "sugb", "sugc", "sunf", "whea", "yams")

#

clusters_voronoi$area <- as.numeric(st_area(clusters_voronoi)) * 0.0001 # in hectares

# extract total bluewater demand in each cluster

files = list.files(path = paste0(input_folder, "spam_folder/spam2017v2r1_ssa_harv_area.geotiff") , pattern = 'R.tif', full.names = T)
nomi <- tolower(unlist(qdapRegex::ex_between(files, "SSA_H_", "_R.tif")))
files <- pblapply(files, raster)
files <- stack(files)
names(files) <- nomi
files <- raster::subset(files, names(rainfed))

# convert crop water need to actualy water need by applying irrigation efficiency factors specific to each crop

crops_efficiency_irr <- crops[crops$crop %in% names(files),]
crops_efficiency_irr <- crops_efficiency_irr[order(crops_efficiency_irr$crop),]

gc()

# mm to m3 -> 1 mm supplies 0.001 m3 per m^2 of soil

rainfed <- pblapply(1:nlayers(files), function(X) {stack(rainfed[[X]] * (files[[X]] / crops_efficiency_irr$eta_irr[X]) * 10)})

# sum by month

rainfed_sum <- rainfed

for (m in 1:12){
  
  rainfed_sum[[m]] <- do.call("sum", c(pblapply(1:nlayers(files), function(X){rainfed[[X]][[m]]}), na.rm = TRUE))
  
}

rainfed_sum <- stack(rainfed_sum)
rainfed <- rainfed_sum


#########

for (timestep in planning_year){

  markup <- stack(find_it(paste0("markup_", ifelse(scenarios$ssp[scenario]=="ssp2", 245, 585), ".nc")))[[ifelse(timestep==2020, 1, ifelse(timestep==2030, 10, ifelse(timestep==2040, 20, 20)))]]
  
  clusters_voronoi$markup <- exact_extract(markup, clusters_voronoi, "median")
  clusters_voronoi$markup <- ifelse(clusters_voronoi$markup>1, 1, clusters_voronoi$markup)
  

for (i in 1:12){
  
  clusters_voronoi[paste0('monthly_IRREQ' , "_" , as.character(i), "_", timestep)] <- exact_extract(rainfed[[i]], clusters_voronoi, "sum") * (1 + clusters$markup) * irrigated_cropland_share_target * (match(timestep, planning_year) / length(planning_year))
  
}


# downscale irrigation / cropland demand

if (downscale_cropland==T){
  
  source("high_res_cropland.R")
  
  for (i in 1:12){
    
    aa <- clusters_voronoi
    aa$geom=NULL
    aa$geometry=NULL
    
    clusters_voronoi[paste0('monthly_IRREQ' , "_" , as.character(i), "_", timestep)] <- pull(aa[paste0('monthly_IRREQ' , "_" , as.character(i), "_", timestep)]) * clusters_voronoi$crshare_sp
    
  }}

  # Apply sustainability constraint for groundwater depletion
  
  index_qr <- ifelse(timestep==2020, 169, 169 + (timestep-2020-1)*12)
  
  qr_fut <- qr_baseline[[index_qr:(index_qr+11)]] # for speed, consider only the latest year
  qr_fut <- qr_fut * 60*60*24*30  #convert to mm per month
  
  for (i in 1:12){
    
    clusters_voronoi[paste0('monthly_GQ' , "_" , as.character(i), "_", timestep)] <- exact_extract(qr_fut[[i]], clusters_voronoi, "mean") * clusters_voronoi$area * 10
  }
  
if(groundwater_sustainability_contraint==T){
  
  for (i in 1:12){
    
    aa <- clusters_voronoi
    aa$geom=NULL
    aa$geometry=NULL
    
    clusters_voronoi[paste0('monthly_unmet_IRRIG_share' , "_" , as.character(i), "_", timestep)] <- as.numeric(ifelse((unlist(aa[paste0('monthly_GQ' , "_" , as.character(i), "_", timestep)]) < unlist(aa[paste0('monthly_IRREQ' , "_" , as.character(i), "_", timestep)]))==TRUE, (unlist(aa[paste0('monthly_IRREQ' , "_" , as.character(i), "_", timestep)]) - unlist(aa[paste0('monthly_GQ' , "_" , as.character(i), "_", timestep)]))/ unlist(aa[paste0('monthly_IRREQ' , "_" , as.character(i), "_", timestep)]), 0))
    
  }}

aa <- clusters_voronoi
aa$geometry=NULL
aa$geom=NULL

clusters_voronoi[paste0('yearly_IRREQ' , "_", timestep)] <- rowSums(dplyr::select(aa, starts_with("monthly_IRREQ") & contains(as.character(timestep))))

}

#

clusters_voronoi$maxflow <- exact_extract(maxflow, clusters_voronoi, "mean")

clusters_voronoi_data <- clusters_voronoi
clusters_voronoi_data$geom <- NULL
clusters_voronoi_data <- dplyr::select(clusters_voronoi_data, starts_with("monthly"), starts_with("yearly"), area, maxflow)
clusters_voronoi_data <- dplyr::rename(clusters_voronoi_data, area_voronoi_ha = area)
clusters_voronoi_data$id <- clusters_voronoi$id

clusters <- merge(clusters, clusters_voronoi_data, by="id")


save.image(paste0(processed_folder, "clusters_crop_module.Rdata"))

        