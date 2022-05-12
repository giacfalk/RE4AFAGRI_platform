# import farmgate prices

prices <- filter(prices, (Area!="Sudan (former)" & Area!="Ethiopia PDR"))

# min_max_norm <- function(x) {
#   (x - min(x, na.rm=T)) / (max(x, na.rm=T) - min(x, na.rm=T))
# }
# 
# prices_var <- prices %>% group_by(Area, Item) %>% mutate(Value=min_max_norm(Value)) %>%  dplyr::summarise(PSI=sd(Value, na.rm=T)) %>% ungroup() %>% group_by(Area, Item) %>%  dplyr::summarise(PSI=mean(PSI, na.rm=T)) %>% ungroup()

prices <- prices %>% filter(Year>2012) %>% group_by(Area, Item) %>% dplyr::summarise(Value=median(Value, na.rm=T)) %>% ungroup()

#prices <- merge(prices, prices_var, by=c("Area", "Item"), all.x=T)

# prices$Value_upper <- prices$Value * (1+prices$PSI)
# prices$Value_lower <- prices$Value - (1+prices$PSI)
# 
# prices$Value_upper <- ifelse(is.nan(prices$PSI), prices$Value, prices$Value_upper)
# prices$Value_lower <- ifelse(is.nan(prices$PSI), prices$Value, prices$Value_lower)

###########

prices <- merge(prices, parser, all.x=T, by="Item")
prices <- na.exclude(prices)
prices <- prices %>% group_by(spam, Area) %>% summarise(Value=mean(Value, na.rm=T)) %>% ungroup()

for(i in 3:ncol(prices)){
  prices[,i][is.na(prices[,i])] <- mean(pull(prices[,i]), na.rm = TRUE)
}

files <- list.files(paste0(input_folder, "watercrop"), full.names = T, pattern = "variation.txt", recursive=T)
nomi <- unlist(qdapRegex::ex_between(files, "watercrop/", "/yield_percentage"))
files <- pblapply(files, raster)
files <- stack(files)
names(files) <-  c("barl", "cass", "coco", "cott", "grou", "maiz", "pmil", "smil", "oilp", "pota", "rape", "rice", "sorg", "soyb", "sugb", "sugc", "sunf", "whea", "yams")

crs(files) <- as.character(CRS("+init=epsg:4236"))

#

files2 <- list.files(path=paste0(input_folder, "spam_folder/spam2017v2r1_ssa_yield.geotiff"), pattern="R.tif", full.names=T)
nomi <- tolower(as.character(substr(basename(files2), 20, 23)))
files2 <- pblapply(files2, raster)
files2 <- stack(files2)
names(files2) <- nomi
files2 <- subset(files2, names(files))

files3 = list.files(path = paste0(input_folder, "spam_folder/spam2017v2r1_ssa_harv_area.geotiff") , pattern = 'R.tif', full.names = T)
nomi <- tolower(as.character(substr(basename(files3), 20, 23)))
files3 <- pblapply(files3, raster)
files3 <- stack(files3)
names(files3) <- nomi
files3 <- subset(files3, names(files))

for (i in 1:nlayers(files)){
print(i)
files[[i]] <- (files[[i]] / 100) * files2[[i]] * files3[[i]]
}

names(files) <- nomi

 
# calculate total added value

aa <- clusters
aa <- aa %>% dplyr::select(starts_with("add_val_")) %>% st_set_geometry(NULL) %>% mutate(tt_ddvl = rowSums(., na.rm=T))
clusters$tt_ddvl <- aa$tt_ddvl

clusters$tt_ddvl <- ifelse(clusters$totalcost>0, clusters$tt_ddvl, 0)
clusters$yg_total<- ifelse(clusters$totalcost>0, clusters$yg_total, 0)
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

clusters$traveltime_market = exact_extract(traveltime, clusters, "mean")

# impose limit travel time to market
#clusters$remote_from_market = ifelse(clusters$traveltime_market>360, 1, 0)

clusters$transp_costs = 2 * (clusters$traveltime_market/60*fuel_consumption*clusters$diesel_price) * ceiling( (clusters$yg_total/1000/truck_bearing_t))

for (i in 1:nrow(clusters)){
  clusters$transp_costs[i] = npv(discount_rate, rep( clusters$transp_costs[i], lifetimepump), lifetimepump)
}

# obtain economic cost (pump, energy and transport)

clusters$total_system_cost_discounted <- clusters$totalcost + clusters$totalpumpcost + clusters$transp_costs

#clusters$tt_ddvl = ifelse(clusters$total_system_cost>clusters$tt_ddvl, 0, clusters$tt_ddvl)

# NPV of profits ten years into the future

for (i in 1:nrow(clusters)){
  clusters$total_revenues_discounted[i] = npv(discount_rate, rep(clusters$tt_ddvl[i], lifetimepump))
}

clusters$total_system_cost_discounted_yeary <- clusters$total_system_cost_discounted / lifetimepump

clusters$total_revenues_discounted_discounted_yearly <- clusters$total_revenues_discounted / lifetimepump

clusters$profit_yearly <- clusters$total_revenues_discounted_discounted_yearly - clusters$total_system_cost_discounted_yeary

clusters$profit_yearly <- ifelse(clusters$profit_yearly>0, clusters$profit_yearly, NA)

clusters$pop <- clusters$pop_start_un

clusters$profit_yearly_capita <- clusters$profit_yearly / clusters$pop

# 9) Paybacktime of investment in each cluster (in years)
clusters$PBT = clusters$total_system_cost_discounted / clusters$total_revenues_discounted_discounted_yearly

clusters$PBT <- ifelse(clusters$profit_yearly>0, clusters$PBT, NA)

save.image(paste0("results/", countrystudy, "/estimate_economic_revenues.Rdata"))
