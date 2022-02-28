# 4.1 import the PV cost layers

rr <- list.files(paste0(input_folder, "pv_cost"), full.names = T, pattern = "tif")

df <- data.frame(rr)
df$name <- gsub(".tif", "", df$rr)
df$name <- sub('.*/', '', df$name)

df$crop <- sub("\\&.*", "", df$name)
df$irr <- sub('.*&', '', df$name)

# 4.2 cluster the MapSPAM crops to match the crop-technology couplets from the Earth's future paper to all the crops
crops = crops %>% dplyr::select(crop, eta_irr)

crops$most_sim <- c("Maize", "Commonbeans", "Wheat", "Onions", "Sugarcane", "Banana", "Commonbeans", "Chickpeas", "Tomatoes", "Sugarcane", "Banana", "Onions", " ", "Onion", "Wheat", "Wheat", "Maize", "Wheat", "Sugarcane", "Sugarcane", "Maize", "Chickpeas", "Sugarcane", "Sugarcane")

crops$irr_type <- ifelse(crops$eta_irr==0.6, "flood", "drip")
crops <- merge(df, crops, by.x=c("crop"), by.y=c("most_sim"))
crops <- filter(crops, irr==irr_type)

# prevalent crop in each grid cell
files = list.files(path = paste0(spam_folder, "spam2010v1r0_global_harv_area.geotiff") , pattern = 'r.tif', full.names = T)

files <- files[as.character(substr(files, 103, 106)) %in% crops$crop.y]

for (X in files){
  a = paste0("A_" , as.character(substr(X, 103, 106)))
  clusters[a] <- exactextractr::exact_extract(raster(X), clusters, fun="sum")
}

aa <- clusters
aa$geometry=NULL
aa$geom=NULL
clusters$maxq <-  colnames(aa)[grepl("A_", colnames(aa))][apply(aa[grepl("A_", colnames(aa))],1,which.max)]
clusters$maxq <- gsub("A_", "", clusters$maxq)

aa <- clusters
aa$geometry=NULL
aa$geom=NULL
clusters$A_total <- as.vector(rowMax(as.matrix(aa[grepl("A_", colnames(aa))])))

clusters <- merge(clusters, data.frame(crops[,c(2, 5)]), by.y="crop.y", by.x="maxq", all.x=T)

##

list <- as.character(unique(clusters$rr))
list2 <- lapply(list, raster)

clusters$pvcost <- NA

for (i in 1:length(list2)){
  clusters[names(list2[[i]])] <- exact_extract(list2[[i]], clusters, fun="mean")
}

for (i in 1:nrow(clusters)){
  clusters$pvcost[i] <-   pull(clusters[i,(which(grepl("pvcost", colnames(clusters))))+match(as.character(clusters$rr[i]), list)] %>% st_set_geometry(NULL))
}

# extrapolate where NA
# extract solar radiation

files <- list.files(paste0(input_folder, "pv_cost"), pattern="asc", full.names = T)

for (i in 1:length(files)){
sr <- raster(files[i])
clusters[paste0("sr_", i)] <- exact_extract(sr, clusters, "mean")
}

# regress on solar radiation and prevalent crop and X and Y and predict

clusters <- bind_cols(clusters, data.frame(st_coordinates(st_centroid(clusters))))

model <- lm(pvcost ~ X + Y + sr_1 + sr_2 + sr_3 + sr_4 + sr_5 + sr_6 + sr_7 + sr_8 + sr_9 + sr_10 + sr_11 + sr_12, data=clusters)
summary(model)
pred <- predict(model, clusters)

clusters$pvcost <- ifelse(is.nan(clusters$pvcost), pred, clusters$pvcost)

###

dem_t <- clusters %>% dplyr::select(starts_with("er_kwh")) %>% st_set_geometry(NULL)
powerpump_t <- clusters %>% dplyr::select(starts_with("power")) %>% st_set_geometry(NULL)
pvout_t <- stack(lapply(list.files("F:/monthly", full.names = T, pattern = "asc"), raster))
pvout_t <-  exact_extract(pvout_t, clusters, "mean")
pvout_t <- (pvout_t/30) * 0.65

##

clusters$totalcost = NA
clusters$landuse_m2 = NA
clusters$pvsize_kw = NA
clusters$batterysize_kwh = NA

library(insol)

for (ncls in 1:nrow(clusters)){

print(ncls)
  
lat = clusters$Y[ncls]
long =  clusters$X[ncls]
powerpump <- max(powerpump_t[ncls,]) # power of the pump, in W

dem <- as.numeric(dem_t[ncls,]) / 30 # daily demand, in kwh for each day of each month
pvout <- as.numeric(pvout_t[ncls,]) # daily generation potential (kWh/kWp) for each day of each month
sunlighthours <- (daylength(lat=lat,long=long,JD(seq(ISOdate(2019,1,1),ISOdate(2019,12,31),by='month')),tmz=1)[,3]) * 0.65 # number of sunlight hours at 100% potential equivalent ("peak sun") per month

usehours_which <- c(6:9) # number of hours when the pump is operating
usehours <- length(usehours_which)

mat <- daylength(lat=lat,long=long,JD(seq(ISOdate(2019,1,1),ISOdate(2019,12,31),by='month')),tmz=1)

out <- matrix(ncol=length(6:9), nrow=12)

k = 1
for (i in usehours_which){
  out[,k] <-  cbind( mat[,1] <= i & mat[,2] >= i )
  k = k+1
}

demand_no_supply_hours <- usehours - rowSums(out) # number of hours when the pump use is not in sunlight hours

######################

toprovided_withbatteries_daily <- (demand_no_supply_hours/usehours) * dem # demand to be supplied when no generation potential is available

toprovided_directly_from_pv_daily <- (1-(demand_no_supply_hours/usehours)) * dem

# pv size
kwh_per_hour_direct_use <- toprovided_directly_from_pv_daily / (usehours - demand_no_supply_hours)

wp_direct_use <- (kwh_per_hour_direct_use / (pvout/sunlighthours)) * 1000 * 1.1 # energy lost in the system

kwh_per_hour_battery <- toprovided_withbatteries_daily / (sunlighthours - (usehours - demand_no_supply_hours))

wp_to_charge_battery <- (kwh_per_hour_battery / (pvout/(sunlighthours - (usehours - demand_no_supply_hours)))) * 1000 * 1.2 # energy lost in the system

wp <- max(wp_direct_use, wp_to_charge_battery) # required pv size
kwp <- wp/1000

# battery size

kwh_battery <- max((toprovided_withbatteries_daily / 0.85 / 0.8)) # required battery size, net of battery efficiency, depth of discharge, extra size for unexpected low generation buffer

kwh_battery <- kwh_battery + (max(dem)*1) # extra days of autonomy

# inverter size

inverter_size <- powerpump * 1.3 #W

## plots

# plot(dem, type="l", col="red")
# plot(pvout, type="l", col="blue")
# plot(sunlighthours, type="l", col="red")
# plot(toprovided_directly_from_pv_daily, type="l", col="red")
# plot(toprovided_withbatteries_daily, type="l", col="red")
# plot(kwp, kwh_battery)

################
# 2. estimate costs of installing the system and O&M

pvcost <- wp * clusters$pvcost[ncls] 

om_cost <- (pvcost*0.1*20)/(1+discount_rate)^20

usdperkwhbattery <- 500 

storcost <- kwh_battery * usdperkwhbattery

inverter_cost <- inverter_size * 0.2 #usd per watt

ch_contr_cost <- (pvcost + om_cost + storcost  + inverter_cost) * 0.1

totalcost <- (pvcost + om_cost + storcost + ch_contr_cost  + inverter_cost) * clusters$npumps[ncls]

#

landuse_m2 <- (kwp * 5 + ifelse(kwp>0, 9, 0)) * clusters$npumps[ncls] # m2 + space for charge controller, inverter, pump

clusters$totalcost[ncls] = totalcost
clusters$landuse_m2[ncls] = landuse_m2
clusters$pvsize_kw[ncls] = kwp 
clusters$batterysize_kwh[ncls] = kwh_battery
}

# mean(clusters$totalcost[clusters$totalcost!=0], na.rm=T)
# summary(clusters$pvsize_kw[clusters$pvsize_kw!=0])
# summary(clusters$batterysize_kwh[clusters$batterysize_kwh!=0])

saveRDS(clusters, "clusters_energy_costs.R")