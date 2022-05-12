# 1- per processare x% del raccolto (fai diversi scenari) in 10 ore di attiività delle macchine al giorno per x mesi, hai bisogno di un tot di kw totali (kw_kg_h) * kg * h
# 2- assumi kw large-scale se è nei pressi della città, small scale se è remota, e calcola numero di macchine necessarie
# 3- energia = kw * ore * macchine

#Crop processing machinery: energy demand

# Extract yield 
# Import all Yield (kg/ha) cropland layers (Default datasets used: MapSPAM)
# NB: when using MapSPAM use harvested area, which accounts for multiple growing seasons per year)
files <- list.files(path=paste0(input_folder, "spam_folder/spam2017v2r1_ssa_yield.geotiff"), pattern="R.tif", full.names=T)
files <- files[grepl(paste(energy_crops[,1], collapse="|") , files, ignore.case =T)]

files2 = list.files(path = paste0(input_folder, "spam_folder/spam2017v2r1_ssa_harv_area.geotiff") , pattern = 'R.tif', full.names = T)
files2 <- files2[grepl(paste(energy_crops[,1], collapse="|") , files2, ignore.case =T)]

## implement these constraints

files <- stack(lapply(files, function(X)(raster(X))))
names(files) <- tolower(unlist(qdapRegex::ex_between(files, "SSA_Y_", "_R.tif")))
  
files2 <- stack(lapply(files2, function(X)(raster(X))))
names(files2) <- tolower(unlist(qdapRegex::ex_between(files2, "SSA_H_", "_R.tif")))
  
field_size <- raster(find_it("field_size_10_40_cropland.img"))
field_size <- mask_raster_to_polygon(field_size, st_as_sfc(st_bbox(clusters)))

if(field_size_contraint==T){field_size <- projectRaster(field_size, mask_raster_to_polygon(files[[1]], st_as_sfc(st_bbox(clusters_voronoi))), method = "bilinear") ; m <- field_size; m[m > 29] <- NA; field_size <- mask(field_size, m); files <- pblapply(files, function(X){return(mask_raster_to_polygon(X, st_as_sfc(st_bbox(clusters_voronoi))))}); for (i in 1:length(files)){crs(files[[i]]) <- crs(field_size)}; field_size <- projectRaster(field_size,files[[1]]); files <- pblapply(files, function(X){mask(X, field_size)})}


field_size <- raster(find_it("field_size_10_40_cropland.img"))
field_size <- mask_raster_to_polygon(field_size, st_as_sfc(st_bbox(clusters)))

if(field_size_contraint==T){field_size <- projectRaster(field_size, mask_raster_to_polygon(files2[[1]], st_as_sfc(st_bbox(clusters_voronoi))), method = "bilinear") ; m <- field_size; m[m > 29] <- NA; field_size <- mask(field_size, m); files2 <- pblapply(files2, function(X){return(mask_raster_to_polygon(X, st_as_sfc(st_bbox(clusters_voronoi))))}); for (i in 1:length(files2)){crs(files2[[i]]) <- crs(field_size)}; field_size <- projectRaster(field_size,files2[[1]]); files2 <- pblapply(files2, function(X){mask(X, field_size)})}

if(buffers_cropland_distance==T){clusters_buffers_cropland_distance <- projectRaster(clusters_buffers_cropland_distance,files[[1]], method = "ngb"); files <- pblapply(files, function(X){mask(X, clusters_buffers_cropland_distance)})}

if(buffers_cropland_distance==T){clusters_buffers_cropland_distance <- projectRaster(clusters_buffers_cropland_distance,files2[[1]], method = "ngb"); files2 <- pblapply(files2, function(X){mask(X, clusters_buffers_cropland_distance)})}

####

for (X in 1:length(files)){
  a = paste0("A_" , names(files)[X], "_r")
  clusters[a] <- exactextractr::exact_extract(files[[X]], clusters_voronoi, fun="sum") 
  
  aa <- clusters
  aa$geom=NULL
  aa$geometry=NULL
  
  clusters[a] <- ifelse(is.na( pull(aa[a])), 0,  pull(aa[a]))
  
  a = paste0("Y_" ,  names(files)[X], "_r")
  clusters[a] <- exactextractr::exact_extract(files[[X]], clusters_voronoi, fun="mean")
  
  aa <- clusters
  aa$geom=NULL
  aa$geometry=NULL
  
  clusters[a] <- ifelse(is.na( pull(aa[a])), 0,  pull(aa[a]))
  
  aa <- clusters
  aa$geom=NULL
  aa$geometry=NULL
  
  clusters <- clusters %>%  mutate(!!paste0("yield_",  names(files)[X]), "_r_tot") := (!!as.name(a)) * pull(!!aa[paste0("A_",  names(files)[X], "_r")]) * crop_processed_share_target * (match(scenarios$planning_year[scenario], planning_year) / length(planning_year)))
}

################
# Same but for already irrigated cropland

if (process_already_irrigated_crops==T){
  
  files <- list.files(path=paste0(input_folder, "spam_folder/spam2017v2r1_ssa_yield.geotiff"), pattern="I.tif", full.names=T)
  files <- files[grepl(paste(energy_crops[,1], collapse="|") , files, ignore.case =T)]
  
  files2 = list.files(path = paste0(input_folder, "spam_folder/spam2017v2r1_ssa_harv_area.geotiff") , pattern = 'I.tif', full.names = T)
  files2 <- files2[grepl(paste(energy_crops[,1], collapse="|") , files2, ignore.case =T)]
  
  ## implement these constraints
  
  files <- stack(lapply(files, function(X)(raster(X))))
  names(files) <- tolower(unlist(qdapRegex::ex_between(files, "SSA_Y_", "_I.tif")))
  
  files2 <- stack(lapply(files2, function(X)(raster(X))))
  names(files2) <- tolower(unlist(qdapRegex::ex_between(files2, "SSA_H_", "_I.tif")))
  
  field_size <- raster(find_it("field_size_10_40_cropland.img"))
  field_size <- mask_raster_to_polygon(field_size, st_as_sfc(st_bbox(clusters)))
  
  if(field_size_contraint==T){field_size <- projectRaster(field_size, mask_raster_to_polygon(files[[1]], st_as_sfc(st_bbox(clusters_voronoi))), method = "bilinear") ; m <- field_size; m[m > 29] <- NA; field_size <- mask(field_size, m); files <- pblapply(files, function(X){return(mask_raster_to_polygon(X, st_as_sfc(st_bbox(clusters_voronoi))))}); for (i in 1:length(files)){crs(files[[i]]) <- crs(field_size)}; field_size <- projectRaster(field_size,files[[1]]); files <- pblapply(files, function(X){mask(X, field_size)})}
  
  
  field_size <- raster(find_it("field_size_10_40_cropland.img"))
  field_size <- mask_raster_to_polygon(field_size, st_as_sfc(st_bbox(clusters)))
  
  if(field_size_contraint==T){field_size <- projectRaster(field_size, mask_raster_to_polygon(files2[[1]], st_as_sfc(st_bbox(clusters_voronoi))), method = "bilinear") ; m <- field_size; m[m > 29] <- NA; field_size <- mask(field_size, m); files2 <- pblapply(files2, function(X){return(mask_raster_to_polygon(X, st_as_sfc(st_bbox(clusters_voronoi))))}); for (i in 1:length(files2)){crs(files2[[i]]) <- crs(field_size)}; field_size <- projectRaster(field_size,files2[[1]]); files2 <- pblapply(files2, function(X){mask(X, field_size)})}
  
  if(buffers_cropland_distance==T){clusters_buffers_cropland_distance <- projectRaster(clusters_buffers_cropland_distance,files[[1]], method = "ngb"); files <- pblapply(files, function(X){mask(X, clusters_buffers_cropland_distance)})}
  
  if(buffers_cropland_distance==T){clusters_buffers_cropland_distance <- projectRaster(clusters_buffers_cropland_distance,files2[[1]], method = "ngb"); files2 <- pblapply(files2, function(X){mask(X, clusters_buffers_cropland_distance)})}
  
  ####
  
  for (X in 1:length(files)){
    a = paste0("A_" , names(files)[X]), "_irr")
    clusters[a] <- exactextractr::exact_extract(files[[X]], clusters_voronoi, fun="sum")
    
    aa <- clusters
    aa$geom=NULL
    aa$geometry=NULL
    
    clusters[a] <- ifelse(is.na( pull(aa[a])), 0,  pull(aa[a]))
    
    a = paste0("Y_" , names(files)[X]), "_irr")
    clusters[a] <- exactextractr::exact_extract(files[[X]], clusters_voronoi, fun="mean")
    
    aa <- clusters
    aa$geom=NULL
    aa$geometry=NULL
    
    clusters[a] <- ifelse(is.na( pull(aa[a])), 0,  pull(aa[a]))
    
    aa <- clusters
    aa$geom=NULL
    aa$geometry=NULL
    
    clusters <- clusters %>%  mutate(!!paste0("yield_", names(files)[X]),  "_irr_tot") := (!!as.name(a)) * pull(!!aa[paste0("A_", names(files)[X], "_irr")]) * crop_processed_share_target * (match(scenarios$planning_year[scenario], planning_year) / length(planning_year))) 
  }
}

#

# Multiply yearly yield of each crop by unit processing energy requirement to estimate yearly demand in each cluster as the sum of each crop processing energy demand
for (X in as.vector(energy_crops[,1])){
  aa <- clusters
  aa$geom=NULL
  aa$geometry=NULL
  
  clusters[paste0("kwh_" , X , "_r_tot")] = pull(aa[paste0("yield_", X, "_r_tot")]) * energy_crops$kw_kg._h[as.vector(energy_crops[,1]) == X] 
  
  aa <- clusters
  aa$geom=NULL
  aa$geometry=NULL
  
  clusters[paste0("kwh_" , X , "_r_tot")] = ifelse(clusters$suitable_for_local_processing==1, pull(aa[paste0("kwh_" , X , "_r_tot")]), 0)
  
  aa <- clusters
  aa$geom=NULL
  aa$geometry=NULL
  
  if (process_already_irrigated_crops==T){
    
    clusters[paste0("kwh_" , X , "_tot")] = pull(aa[paste0("kwh_" , X , "_r_tot")]) + pull(aa[paste0("yield_", X, "_irr_tot")]) * energy_crops$kw_kg._h[as.vector(energy_crops[,1]) == X]
    
    aa <- clusters
    aa$geom=NULL
    aa$geometry=NULL
    
    clusters[paste0("kwh_" , X , "_tot")] = ifelse(clusters$suitable_for_local_processing==1, pull(aa[paste0("kwh_" , X , "_tot")]), 0)
    
  }
  
}

aa <- clusters
aa$geom=NULL
aa$geometry=NULL

clusters$kwh_cp_tt = as.vector(aa %>%  dplyr::select(starts_with('kwh')) %>% rowSums(na.rm = T) %>% as.numeric())

save.image(paste0("results/", countrystudy, "/clusters_crop_processing.Rdata"))

# processing to take place in post-harvesting months: for each crop 1) take harvesting date 2) take plantation months. for those months between 1 and 2 equally allocate crop processing

gc()

crops <- crops[complete.cases(crops), ]
crops <-  crops[crops$crop %in% as.vector(energy_crops[,1]), ]

for (i in 1:nrow(crops)){
  for (m in 1:12){
    daily=data.frame("daily" = c(1:729))
    daily$date = seq(as.Date("2019-01-01"), length.out = 729, by = "days")
    daily$month = lubridate::month(daily$date)
    daily$day = lubridate::day(daily$date)
    
    pm1= as.Date(paste0(crops[i, 'pm_1'], "2019"), format= "%d%m%Y")
    pm2= as.Date(paste0(crops[i, 'pm_2'], "2019"), format= "%d%m%Y")
    
    a =  filter(daily, date>= pm1 + as.numeric(crops[i, 'nd_1']) + as.numeric(crops[i, 'nd_2']) + as.numeric(crops[i, 'nd_3']) + as.numeric(crops[i, 'nd_4']))
    a =  filter(a, date < as.Date("2020-03-15", format="%Y-%m-%d"))
    a =  filter(a, lubridate::month(month) == m)
    a = unique(a$month)
    
    aa <- clusters
    aa$geom=NULL
    aa$geometry=NULL
    
    
    clusters[paste0("kwh_cp" , as.character(crops$crop[i]) , "_" , as.character(m))] = pull(aa[paste0("kwh_" , as.character(crops$crop[i]) , "_tot")]) / ifelse(length(a)==0, 0, a)
    
    aa <- clusters
    aa$geom=NULL
    aa$geometry=NULL
    
    clusters[paste0("kwh_cp" , as.character(crops$crop[i]) , "_" , as.character(m))]  = ifelse(is.infinite(pull(aa[paste0("kwh_cp" , as.character(crops$crop[i]) , "_" , as.character(m))])), 0, pull(aa[paste0("kwh_cp" , as.character(crops$crop[i]) , "_" , as.character(m))]))
    
  }}

# sum all crops by months
for (z in 1:12){
  aa <- clusters
  aa$geom=NULL
  aa$geometry=NULL
  
  
  aa <- aa %>% dplyr::select(starts_with('kwh_cp')) %>% dplyr::select(ends_with(paste0('_' , as.character(z)))) %>% mutate(a=rowSums(., na.rm = T))
  
  clusters = clusters %>% mutate(!!as.name(paste0('monthly_kwh_cropproc', "_" , as.character(z))) := as.vector(aa$a))
  
}

# monthly_kwh_cropproc nel mese m / (potenza della macchina: assunta / numero di ore operazionali) = numero di macchine necessarie

gc()

for (X in as.vector(energy_crops[,1])){
  for (m in 1:12){
    
    aa <- clusters
    aa$geom=NULL
    aa$geometry=NULL
    
    clusters[paste0("n_machines_" , X , "_" , as.character(m))] <- ceiling(pull(aa[paste0("kwh_cp" , X , "_" , as.character(m))]) / ((energy_crops$kg_per_hour_kwmin[as.vector(energy_crops[,1]) == X]) * (sum(load_curve_cp>0))))
    
    aa <- clusters
    aa$geom=NULL
    aa$geometry=NULL
    
    clusters[paste0("kw_tot_machines_" , X , "_" , as.character(m))] <- pull(aa[paste0("n_machines_" , X , "_" , as.character(m))]) * energy_crops$kw_min[as.vector(energy_crops[,1]) == X]
    
    aa <- clusters
    aa$geom=NULL
    aa$geometry=NULL
    
    clusters[paste0("n_machines_" , X , "_" , as.character(m))] <- ifelse(clusters$suitable_for_local_processing==1, pull(aa[paste0("n_machines_" , X , "_" , as.character(m))]), 0)
    
    aa <- clusters
    aa$geom=NULL
    aa$geometry=NULL
    
    clusters[paste0("kw_tot_machines" , X , "_" , as.character(m))] <- ifelse(clusters$suitable_for_local_processing==1, pull(aa[paste0("kw_tot_machines_" , X , "_" , as.character(m))]), 0)
    
  }}

for (X in as.vector(energy_crops[,1])){
  
  aa <- clusters
  aa$geom=NULL
  aa$geometry=NULL
  
  clusters[paste0("n_machines_" , X)] <- as.vector(as.matrix(aa %>%  dplyr::select(starts_with(paste0("n_machines_" , X)))) %>% rowMax(.) %>% as.numeric())
  
  aa <- clusters
  aa$geom=NULL
  aa$geometry=NULL
  
  clusters[paste0("kw_tot_machines_" , X)] <- as.vector(as.matrix(aa %>%  dplyr::select(starts_with(paste0("kw_tot_machines_" , X)))) %>% rowMax(.) %>% as.numeric())
  
}



if (output_hourly_resolution==T){
  
  # simulate daily profile
  
  for (k in 1:12){
    
    aa <- clusters
    aa$geom=NULL
    aa$geometry=NULL
    
    
    clusters[paste0('kwh_cropproc_tt_', as.character(k))] = pull(aa[paste0('monthly_kwh_cropproc' , "_" , as.character(k))])/30
    
  }
  
  for (k in 1:12){
    for (i in 1:24){
      
      aa <- clusters
      aa$geom=NULL
      aa$geometry=NULL
      
      
      clusters[paste0('kwh_cropproc' , as.character(k) , "_" ,  as.character(i))] = pull(aa[paste0('kwh_cropproc_tt_' , as.character(k))])*load_curve_cp[i]
      
    }}
  
}

rm(files, files2)

save.image(paste0("results/", countrystudy, "/clusters_crop_processing.Rdata"))
