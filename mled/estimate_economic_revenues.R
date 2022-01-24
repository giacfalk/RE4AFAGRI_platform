# import farmgate prices

prices <- read.csv(paste0(input_folder, "FAOSTAT_data_8-11-2021.csv"))

prices <- prices %>% group_by(Area, Item) %>% dplyr::summarise(Value=last(Value)) %>% ungroup()

parser <- read.csv(paste0(input_folder, "parser.csv"))

prices <- merge(prices, parser, all.x=T, by="Item")
prices <- na.exclude(prices)
prices <- prices %>% group_by(spam, Area) %>% summarise(Value=mean(Value, na.rm=T)) %>% ungroup()

for(i in 3:ncol(prices)){
  prices[,i][is.na(prices[,i])] <- mean(pull(prices[,i]), na.rm = TRUE)
}

# import potential extra yield thanks to irrigation (Marta's input) OR estimate it empirically (Shonali)

files <- list.files(path=paste0(input_folder, "yield_gain_potential"), pattern="txt", full.names = T)

files <- mixedsort(files)
files <- lapply(files, raster)
files <- stack(files) / 100
crs(files) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

names(files) -> nomi

#

files2 <- list.files(path=paste0(spam_folder, "spam2010v1r0_global_yield.geotiff"), pattern="r.tif", full.names=T)
files2 <- mixedsort(files2)
files2 <- mixedsort(files2)
files2 <- lapply(files2, raster)

files3 <- list.files(path=paste0(spam_folder, "spam2010v1r0_global_harv_area.geotiff"), pattern="r.tif", full.names=T)
files3 <- mixedsort(files3)
files3 <- mixedsort(files3)
files3 <- lapply(files3, raster)

files <- (files[[1]]*files2[[13]]) * files3[[13]]

names(files) <- nomi

#

for (i in names(files)){
  sr <- files[[i]]
  name <- i
  e <- exact_extract(sr, clusters, "sum")
  clusters[paste0("yg_", name)] <- ifelse(e<0, 0, e)
}


aa <- clusters
aa <- aa %>% dplyr::select(starts_with("yg_")) %>% st_set_geometry(NULL) %>% mutate(yg_total = rowSums(., na.rm=T))
clusters$yg_total <- aa$yg_total

# estimate waste and biomass potential (EXTRA)

# obtain economic benefit (additional yield * price) 

gadm <- read_sf(paste0(input_folder, "gadm_africa.shp")) %>% dplyr::select(ISO3)
clusters <- st_join(clusters, gadm, largest=TRUE)
prices$ISO3 <- countrycode(prices$Area, 'country.name', 'iso3c')
prices <- pivot_wider(prices, names_from = spam, values_from = Value, names_prefix = "pri_")

clusters <- merge(clusters, prices, by="ISO3", all.x=T)

for (i in unique(crops$crop.y)[unique(crops$crop.y) %in% nomi]){

aa <- clusters
aa$geometry=NULL
aa$geom=NULL

clusters[paste0('add_val_', as.character(i))] = aa[paste0('pri_', as.character(i))] * aa[paste0('yg_', as.character(i))] / 1000

}

# calculate total added value

aa <- clusters
aa <- aa %>% dplyr::select(starts_with("add_val_")) %>% st_set_geometry(NULL) %>% mutate(tt_ddvl = rowSums(., na.rm=T))
clusters$tt_ddvl <- aa$tt_ddvl

# replicate little model of transport costs to market
# Calculate transportation cost for crops
# Formula: TC = 2 * (TTM x fuelcost x lpermin) * n

clusters$diesel_price = exact_extract(diesel_price, clusters, 'mean')

# geom <- ee$Geometry$Rectangle(c(as.vector(extent(clusters))[1], as.vector(extent(clusters))[3], as.vector(extent(clusters))[2], as.vector(extent(clusters))[4]))
# 
# img_02 <- ee_as_raster(
#    image = traveltime_market,
#    via = "drive",
#    region = geom,
#    scale = 1000
#  )

img_02 <- raster(paste0(input_folder, "accessibility.tif"))

clusters$traveltime_market = exact_extract(img_02, clusters, "mean")

# impose limit travel time to market 
clusters$remote_from_market = ifelse(clusters$traveltime_market>360, 1, 0)

clusters$transp_costs = 2 * (clusters$traveltime_market/60*fuel_consumption*clusters$diesel_price) * ceiling( (clusters$yg_total/1000/truck_bearing_t))

# obtain economic cost (pump, energy and transport)

clusters$total_system_cost <- clusters$totalcost + clusters$totalpumpcost + clusters$transp_costs

#clusters$tt_ddvl = ifelse(clusters$total_system_cost>clusters$tt_ddvl, 0, clusters$tt_ddvl)

# NPV of profits ten years into the future
discount_rate = 0.15

clusters$total_system_cost_discounted_yearly <- NA

for (i in 1:nrow(clusters)){
  clusters$total_system_cost_discounted_yearly[i] = npv(discount_rate, rep(clusters$total_system_cost[i], planning_horizon))/planning_horizon
}
 
clusters$profit_yearly <- clusters$tt_ddvl - clusters$total_system_cost_discounted_yearly

clusters$profit_yearly_capita <- clusters$profit_yearly / clusters$pop

# 9) Paybacktime of investment in each cluster (in years)
clusters$PBT = clusters$total_system_cost / clusters$tt_ddvl
