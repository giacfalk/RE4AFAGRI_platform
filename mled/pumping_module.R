geom <- ee$Geometry$Rectangle(c(as.vector(extent(clusters))[1], as.vector(extent(clusters))[3], as.vector(extent(clusters))[2], as.vector(extent(clusters))[4]))

i = ee$Image("JRC/GSW1_2/GlobalSurfaceWater")$select('max_extent')$clip(geom)
i = i$mask(i$eq(1))
maxDist = ee$Kernel$circle(25000, 'meters', TRUE, 1)
kernel = ee$Kernel$euclidean(25000,"meters");
i = i$distance(kernel,FALSE)$clip(geom)

img_02 <- ee_as_raster(
  image = i,
  via = "drive",
  region = geom,
  scale = 500
)

img_02 <- raster(img_02)

# # Calculate the mean distance from each cluster to the nearest source of surface water
clusters$surfw_dist <-  exact_extract(img_02, clusters, fun="mean")

# Groundwater depth
# Reclassify to numeric
DepthToGroundwater$depthwater = ifelse(DepthToGroundwater$DTWAFRICA_ == 'VS', 3.5, ifelse(DepthToGroundwater$DTWAFRICA_ == "S", 16, ifelse(DepthToGroundwater$DTWAFRICA_ == "SM", 37.5, ifelse(DepthToGroundwater$DTWAFRICA_ == "M", 75, ifelse(DepthToGroundwater$DTWAFRICA_ == "D", 175, ifelse(DepthToGroundwater$DTWAFRICA_ == "D", 250, 0))))))

DepthToGroundwater$depthwater <- as.numeric(DepthToGroundwater$depthwater)

groundwater_depth <- rasterFromXYZ(DepthToGroundwater[c("X", "Y", "depthwater")], crs = 4326)

# Extract mean value within each cluster
clusters$gr_wat_depth <- exactextractr::exact_extract(groundwater_depth, clusters, fun="mean")

# Groundwater storage
GroundwaterStorage$storagewater = ifelse(GroundwaterStorage$GWSTOR_V2 == 'VL', 0, ifelse(GroundwaterStorage$GWSTOR_V2 == "L", 500, ifelse(GroundwaterStorage$GWSTOR_V2 == "LM", 5500, ifelse(GroundwaterStorage$GWSTOR_V2 == "M", 17500, ifelse(GroundwaterStorage$GWSTOR_V2 == "H", 37500, 50000)))))

GroundwaterStorage$storagewater <- as.numeric(GroundwaterStorage$storagewater)

groundwater_storage <- rasterFromXYZ(GroundwaterStorage[c("X", "Y", "storagewater")], crs = 4326)

# Extract mean value within each cluster
clusters$gr_wat_storage <- exactextractr::exact_extract(groundwater_storage, clusters, fun="mean")

# Groundwate productivity
GroundwaterProductivity$Productivitywater = ifelse(GroundwaterProductivity$GWPROD_V2 == 'VH', 25, ifelse(GroundwaterProductivity$GWPROD_V2 == "H", 12.5, ifelse(GroundwaterProductivity$GWPROD_V2 == "M", 3, ifelse(GroundwaterProductivity$GWPROD_V2 == "LM", 0.75, ifelse(GroundwaterProductivity$GWPROD_V2 == "L", 0.3, 0.05)))))

GroundwaterProductivity$Productivitywater <- as.numeric(GroundwaterProductivity$Productivitywater)

groundwater_Productivity <- rasterFromXYZ(GroundwaterProductivity[c("X", "Y", "Productivitywater")], crs = 4326)

# Extract mean value within each cluster
clusters$gr_wat_productivity <- exactextractr::exact_extract(groundwater_Productivity, clusters, fun="mean")

# To fix potential bugs in the data, delete negative values
clusters$gr_wat_depth = ifelse(clusters$gr_wat_depth<0, 0, clusters$gr_wat_depth)
clusters$surfw_dist = ifelse(is.na(clusters$surfw_dist), Inf, clusters$surfw_dist)

# Calculate total groundwater pumps flow rate required in m3/s in each month
for (i in c(1:12)){
  aa <- clusters
  aa$geometry=NULL

  clusters[paste0("q" , as.character(i))] = aa[paste0('monthly_IRREQ' , "_" , as.character(i))] /30/nhours_irr/3600
}

# npumps required
aa <- clusters
aa$geometry=NULL
aa$maxq <- as.vector(rowMax(as.matrix(aa[grepl("q", colnames(aa))])))

clusters$npumps <- ceiling(aa$maxq / clusters$maxflow)

for (i in 1:12){
  print(i)
  aa <- clusters
  aa$geometry=NULL

  # RGH to estimate power for pump (in W), missing the head losses
  clusters[paste0('powerforpump', as.character(i))] = (rho* g * clusters$gr_wat_depth* pull(aa[paste0("q", as.character(i))]))/eta_pump / clusters$npumps
  
  aa <- clusters
  aa$geometry=NULL

  # clusters[paste0('powerforpump', as.character(i))] = ifelse(aa["gr_wat_depth"]>threshold_groundwater_pumping, Inf, pull(aa[paste0('powerforpump', as.character(i))]))
  
  aa <- clusters
  aa$geometry=NULL

  #Calculate monthly electric requirement
  clusters[paste0('wh_monthly', as.character(i))] = pull(aa[paste0('powerforpump', as.character(i))])*nhours_irr*30
  
  aa <- clusters
  aa$geometry=NULL

  clusters[paste0('er_kwh' , as.character(i))] = aa[paste0('wh_monthly', as.character(i))]/1000
  
  aa <- clusters
  aa$geometry=NULL

  clusters[paste0("surfw_w", as.character(i))] = aa[paste0("q", as.character(i))]*((32 * water_speed * aa["surfw_dist"] *water_viscosity)/pipe_diameter**2)/eta_pump/ clusters$npumps
  
  aa <- clusters
  aa$geometry=NULL

  clusters[paste0('surfw_w', as.character(i))] = ifelse(aa["surfw_dist"]>threshold_surfacewater_distance, Inf, pull(aa[paste0('surfw_w', as.character(i))]))
  
  aa <- clusters
  aa$geometry=NULL

  clusters[paste0('surface_er_kwh', as.character(i))] = aa[paste0('surfw_w', as.character(i))]*nhours_irr*30/1000
  
}


######################
# select less energy intensive option between surface and groundwater pumping

aa <- clusters
aa$geometry=NULL

clusters[paste0('er_kwh_tt')] <- as.numeric(rowSums(aa[,grep("^er_kwh", colnames(aa))], na.rm = T))
clusters[paste0('surface_er_kwh_tt')] <- as.numeric(rowSums(aa[,grep("^surface_er_kwh", colnames(aa))], na.rm = T))

clusters$which_pumping <- ifelse(clusters$er_kwh_tt<clusters$surface_er_kwh_tt, "groundwater", ifelse(clusters$er_kwh_tt>clusters$surface_er_kwh_tt,"surfacewater", "neither"))

for (i in 1:12){
  
  aa <- clusters
  aa$geometry=NULL

  clusters[paste0("er_kwh", as.character(i))] = ifelse(aa["which_pumping"]=="groundwater", pull(aa[paste0("er_kwh", as.character(i))]), ifelse(aa["which_pumping"]=="surfacewater", pull(aa[paste0("surface_er_kwh", as.character(i))]), NA))
  
  clusters[paste0("powerforpump", as.character(i))] = ifelse(aa["which_pumping"]=="groundwater", pull(aa[paste0("powerforpump", as.character(i))]), ifelse(aa["which_pumping"]=="surfacewater", pull(aa[paste0("surfw_w", as.character(i))]), NA))
  
}

clusters[is.na(clusters)] <- 0

for (i in 1:12){
  aa <- clusters
  aa$geometry=NULL
  print(summary(aa[paste0('er_kwh' , as.character(i))]  * clusters$npumps))
}

# # simulate daily profile
# 
# for (k in 1:12){
# 
#   print(k)
# 
#   for (i in 1:24){
# 
#     aa <- clusters
#     aa$geometry=NULL
# 
#     clusters[paste0('er_kwh_' , as.character(k) , "_" , as.character(i))] <- (aa[paste0('er_kwh', as.character(k))]/30)*load_curve_irr[i]
#   }}

saveRDS(clusters, "clusters_pumping_module.rds")
