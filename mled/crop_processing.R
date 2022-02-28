#Crop processing machinery: energy demand

# Extract yield 
# Import all Yield (kg/ha) cropland layers (Default datasets used: MapSPAM)
# NB: when using MapSPAM use harvested area, which accounts for multiple growing seasons per year)
files = list.files(path = paste0(input_folder, "spam_folder/spam2010v1r0_global_yield.geotiff") , pattern = 'r.tif')

files2 = list.files(path = paste0(input_folder, "spam_folder/spam2010v1r0_global_harv_area.geotiff") , pattern = 'r.tif')

for (X in 1:length(files)){
  a = paste0("A_" , gsub("_r.tif", "", gsub("spam2010v1r0_global_harvested-area_", "", files2[X])))
  clusters[a] <- exactextractr::exact_extract(raster(paste0(input_folder, "spam_folder/spam2010v1r0_global_harv_area.geotiff/", files2[X])), clusters, fun="sum")
  
  a = paste0("Y_" , gsub("_r.tif", "", gsub("spam2010v1r0_global_yield_", "", files[X])))
  clusters[a] <- exactextractr::exact_extract(raster(paste0(input_folder, "spam_folder/spam2010v1r0_global_yield.geotiff/", files[X])), clusters, fun="mean")
  
  aa <- clusters
  aa$geometry=NULL
  
  
  clusters <- clusters %>%  mutate(!!paste0("yield_", gsub("_r.tif", "", gsub("spam2010v1r0_global_yield_", "", files[X])), "_tot") := (!!as.name(a)) * pull(!!aa[paste0("A_", gsub("_r.tif", "", gsub("spam2010v1r0_global_yield_", "", files[X])))])) 
}

# Same but for already irrigated cropland

files = list.files(path = paste0(input_folder, "spam_folder/spam2010v1r0_global_yield.geotiff") , pattern = 'i.tif')

files2 = list.files(path = paste0(input_folder, "spam_folder/spam2010v1r0_global_harv_area.geotiff") , pattern = 'i.tif')

for (X in 1:length(files)){
  a = paste0("A_" , gsub("_i.tif", "", gsub("spam2010v1r0_global_harvested-area_", "", files2[X])), "_irr")
  clusters[a] <- exactextractr::exact_extract(raster(paste0(input_folder, "spam_folder/spam2010v1r0_global_harv_area.geotiff/", files2[X])), clusters, fun="sum")
  
  a = paste0("Y_" , gsub("_i.tif", "", gsub("spam2010v1r0_global_yield_", "", files[X])), "_irr")
  clusters[a] <- exactextractr::exact_extract(raster(paste0(input_folder, "spam_folder/spam2010v1r0_global_yield.geotiff/", files[X])), clusters, fun="mean")
  
  aa <- clusters
  aa$geometry=NULL
  
  clusters <- clusters %>%  mutate(!!paste0("yield_", gsub("_i.tif", "", gsub("spam2010v1r0_global_yield_", "", files[X])), "_irr_", "_tot") := (!!as.name(a)) * pull(!!aa[paste0("A_", gsub("_i.tif", "", gsub("spam2010v1r0_global_yield_", "", files[X])), "_irr")])) 
}

# Multiply yearly yield of each crop by unit processing energy requirement to estimate yearly demand in each cluster as the sum of each crop processing energy demand
for (X in as.vector(energy_crops[,1])){
  aa <- clusters
  aa$geometry=NULL
  
  clusters[paste0("kwh_" , X , "_tot")] = pull(aa[paste0("yield_", X, "_tot")]) * energy_crops$kwh_kg[as.vector(energy_crops[,1]) == X] 
  
  aa <- clusters
  aa$geometry=NULL
  
  if (process_already_irrigated_crops==T){
    
    clusters[paste0("kwh_" , X , "_tot")] = pull(aa[paste0("kwh_" , X , "_tot")]) + pull(aa[paste0("yield_", X, "_tot")]) * energy_crops$kwh_kg[as.vector(energy_crops[,1]) == X] 
    
  }
  
}

aa <- clusters
aa$geometry=NULL


clusters$kwh_cp_tt = as.vector(aa %>%  dplyr::select(starts_with('kwh')) %>% rowSums(na.rm = T) %>% as.numeric())

# processing to take place in post-harvesting months: for each crop 1) take harvesting date 2) take plantation months. for those months between 1 and 2 equally allocate crop processing

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
    aa$geometry=NULL
    
    
    clusters[paste0("kwh_cp" , as.character(crops$crop[i]) , "_" , as.character(m))] = pull(aa[paste0("kwh_" , as.character(crops$crop[i]) , "_tot")]) / ifelse(length(a)==0, 0, a)
    
    aa <- clusters
    aa$geometry=NULL
    
    
    clusters[paste0("kwh_cp" , as.character(crops$crop[i]) , "_" , as.character(m))]  = ifelse(is.infinite(pull(aa[paste0("kwh_cp" , as.character(crops$crop[i]) , "_" , as.character(m))])), 0, pull(aa[paste0("kwh_cp" , as.character(crops$crop[i]) , "_" , as.character(m))]))
    
  }}

# sum all crops by months
for (z in 1:12){
  aa <- clusters
  aa$geometry=NULL
  
  
  aa <- aa %>% dplyr::select(starts_with('kwh_cp')) %>% dplyr::select(ends_with(paste0('_' , as.character(z)))) %>% mutate(a=rowSums(., na.rm = T))
  
  clusters = clusters %>% mutate(!!as.name(paste0('monthly_kwh_cropproc', "_" , as.character(z))) := as.vector(aa$a))
  
}

# simulate daily profile

for (k in 1:12){
  
  aa <- clusters
  aa$geometry=NULL
  
  
  clusters[paste0('kwh_cropproc_tt_', as.character(k))] = pull(aa[paste0('monthly_kwh_cropproc' , "_" , as.character(k))])/30
  
  aa <- clusters
  aa$geometry=NULL
  
  
}

for (k in 1:12){
  for (i in 1:24){
    
    aa <- clusters
    aa$geometry=NULL
    
    
    clusters[paste0('kwh_cropproc' , as.character(k) , "_" ,  as.character(i))] = pull(aa[paste0('kwh_cropproc_tt_' , as.character(k))])*load_curve_cp[i]
    
  }}

aa <- clusters
aa$geometry=NULL


out = aa %>% dplyr::select(starts_with("kwh_cropproc_tt_")) %>% rowSums(.)
clusters$kwh_cropproc_tt = out

saveRDS(clusters, "clusters_crop_processing.R")
