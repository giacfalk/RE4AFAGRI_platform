# csv containing calories and nutritional detail about each crop in the mapspam db

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

#source("http://michael.hahsler.net/SMU/EMIS7332/R/copytable.R")
#copytable(calories)

###

# apply factor to convert kg of yield to kg of food


# calculate nutrition generation potential in each cell

clusters <- clusters %>%
  mutate_at(vars(contains('yg_')), funs(ifelse(clusters$profit_yearly>0, ., 0)))

aa <- clusters
aa$geometry=NULL
aa$geom=NULL

#

#

c <- head(colnames(clusters)[grep("yg_", colnames(clusters))], -1)

for (i in c){

  clusters[paste0("calories_gain_" , as.character(i))] = calories$kcal[calories$SPAM.short.name==substr(i, 4, 7)] * aa[,i]
  clusters[paste0("proteins_gain_" , as.character(i))] = calories$PROTEIN_g[calories$SPAM.short.name==substr(i, 4, 7)] * aa[,i]
  clusters[paste0("fats_gain_" , as.character(i))] = calories$FAT_g[calories$SPAM.short.name==substr(i, 4, 7)] * aa[,i]

}

aa <- clusters
aa <- aa %>% dplyr::select(starts_with("calories_gain_")) %>% st_set_geometry(NULL) %>% mutate(calories_gain_total = rowSums(., na.rm=T))
clusters$calories_gain_total <- as.numeric(aa$calories_gain_total)

aa <- clusters
aa <- aa %>% dplyr::select(starts_with("proteins_gain_")) %>% st_set_geometry(NULL) %>% mutate(proteins_gain_total = rowSums(., na.rm=T))
clusters$proteins_gain_total <- as.numeric(aa$proteins_gain_total)

aa <- clusters
aa <- aa %>% dplyr::select(starts_with("fats_gain_")) %>% st_set_geometry(NULL) %>% mutate(fats_gain_total = rowSums(., na.rm=T))
clusters$fats_gain_total <- as.numeric(aa$fats_gain_total)

# calculate potential people fed thanks to nutrition generation potential

clusters$calories_newly_fed_people <- clusters$calories_gain_total /  calories_yearly_need
clusters$proteins_newly_fed_people <- clusters$proteins_gain_total /  proteinsg_yearly_need
clusters$fats_newly_fed_people <- clusters$fats_gain_total /  fatsg_yearly_need

clusters$calories_gain_capita_day <- clusters$calories_gain_total / ifelse(clusters$pop==0, NA, clusters$pop) / 365
clusters$proteins_gain_capita_day <- clusters$proteins_gain_total / ifelse(clusters$pop==0, NA, clusters$pop) / 365
clusters$fats_gain_capita_day <- clusters$fats_gain_total / ifelse(clusters$pop==0, NA, clusters$pop) / 365

# take national food insecurity estimates

food_insecurity$iso3c <- countrycode(food_insecurity$Country, 'country.name', 'iso3c')
pop <- wbstats::wb(indicator ="SP.POP.TOTL", mrv=1)
merger <- merge(food_insecurity, pop, by="iso3c")

merger <- dplyr::select(merger, value, Calories_gap, insec, iso3c)

#

clusters <- merge(clusters, merger, all.x=T, by.x="sov_a3", by.y="iso3c")

#

save.image(paste0("results/", countrystudy, "/food_security_implications.Rdata"))

mean(clusters$calories_gain_capita_day[!is.na(clusters$profit_yearly)], na.rm=T) 
mean(clusters$proteins_gain_capita_day[!is.na(clusters$profit_yearly)], na.rm=T) 
mean(clusters$fats_gain_capita_day[!is.na(clusters$profit_yearly)], na.rm=T) 
