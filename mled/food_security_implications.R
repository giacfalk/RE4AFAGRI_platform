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

files3 <- list.files(path=path=paste0(spam_folder, "spam_folder/spam2010v1r0_global_harv_area.geotiff"), pattern="r.tif", full.names=T)
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

#

# csv containing calories and nutritional detail about each crop in the mapspam db

crop_parser <- read.csv(paste0(input_folder, "4-Methodology-Crops-of-SPAM-2005-2015-02-26.csv"), stringsAsFactors = F)

calories <- read.csv(paste0(input_folder, "calories.csv"), stringsAsFactors = F)

crop_parser$parsed <- NA

for (i in 1:length(crop_parser$FAONAMES)){
  a <- agrep(crop_parser$FAONAMES[i], calories$ï..ITEM, value=T, max.distance = 1, ignore.case = T)[which.min(nchar(agrep(crop_parser$FAONAMES[i], calories$ï..ITEM, value=T, max.distance = 1, ignore.case = T)))]
  
  crop_parser$parsed[i] <-ifelse(identical(a, character(0)), NA, a)
  
  }

crop_parser$FAONAMES[is.na(crop_parser$parsed)]
crop_parser$parsed[is.na(crop_parser$parsed)] <- c("CEREALS NES", "YAUTIA (COCOYAM)", "BROAD BEANS,GREEN", "GROUNDNUTS IN SHELL", "OLIVES", "COTTONSEED", NA, NA, "ORANGES", "APPLES", "CABBAGES", "NA")
crop_parser$parsed[2] <- "RICE PADDY"

calories <- merge(calories, crop_parser, by.x="ï..ITEM", by.y="parsed") %>% dplyr::select(1:5, 11)

calories[,2:4] <- calories[,2:4]*10 # convert to kg

###

# apply factor to convert kg of yield to kg of food


# calculate nutrition generation potential in each cell

aa <- clusters
aa$geometry=NULL
aa$geom=NULL

c <- head(colnames(clusters)[grep("yg_", colnames(clusters))], -1)

for (i in c){
  
  clusters[paste0("calories_gain_" , as.character(i))] = calories$kcal[calories$SPAM.short.name==substr(c, 4, 7)] * aa[,c]
  clusters[paste0("proteins_gain_" , as.character(i))] = calories$PROTEIN_g[calories$SPAM.short.name==substr(c, 4, 7)] * aa[,c]
  clusters[paste0("fats_gain_" , as.character(i))] = calories$FAT_g[calories$SPAM.short.name==substr(c, 4, 7)] * aa[,c]
  
}

aa <- clusters
aa <- aa %>% dplyr::select(starts_with("calories_gain_")) %>% st_set_geometry(NULL) %>% mutate(calories_gain_total = rowSums(., na.rm=T))
clusters$calories_gain_total <- aa$calories_gain_total

aa <- clusters
aa <- aa %>% dplyr::select(starts_with("proteins_gain_")) %>% st_set_geometry(NULL) %>% mutate(proteins_gain_total = rowSums(., na.rm=T))
clusters$proteins_gain_total <- aa$proteins_gain_total

aa <- clusters
aa <- aa %>% dplyr::select(starts_with("fats_gain_")) %>% st_set_geometry(NULL) %>% mutate(fats_gain_total = rowSums(., na.rm=T))
clusters$fats_gain_total <- aa$fats_gain_total

# calculate potential people fed thanks to nutrition generation potential

calories_yearly_need <- 2000 * 365
proteinsg_yearly_need <- 50 * 365
fatsg_yearly_need <- 60 * 365

clusters$calories_newly_fed_people <- clusters$calories_gain_total /  calories_yearly_need 
clusters$proteins_newly_fed_people <- clusters$proteins_gain_total /  proteinsg_yearly_need 
clusters$fats_newly_fed_people <- clusters$fats_gain_total /  fatsg_yearly_need 

clusters$calories_gain_capita_day <- clusters$calories_gain_total / ifelse(clusters$pop==0, NA, clusters$pop) / 365
clusters$proteins_gain_capita_day <- clusters$proteins_gain_total / ifelse(clusters$pop==0, NA, clusters$pop) / 365
clusters$fats_gain_capita_day <- clusters$fats_gain_total / ifelse(clusters$pop==0, NA, clusters$pop) / 365

saveRDS(clusters, "clusters_food_security.R")