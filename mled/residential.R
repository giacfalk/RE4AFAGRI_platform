# Calculate the number of people in each tier in each cluster
clusters$popdens <- clusters$population / clusters$area

#plot_raster_tiers <- rasterVis::levelplot(ratify(raster_tiers))

#values(raster_tiers) <- ifelse(values(raster_tiers)==0, NA, values(raster_tiers))
clusters$tier <- exact_extract(raster_tiers, clusters, "majority")

#

set.seed(123)

clusters_rf <- dplyr::select(clusters, tier, gdp_capita_2020, popdens, isurban)
clusters_rf <- na.omit(clusters_rf)

clusters_rf$tier <- as.factor(clusters_rf$tier)
clusters_rf$isurban <- as.factor(clusters_rf$isurban)

clusters_rf$geometry <- NULL
clusters_rf$geom <- NULL

train_data <- clusters_rf %>%
  group_by(tier, isurban) %>%
  slice_sample(prop = 0.7) 

test_data <- clusters_rf %>%  anti_join(train_data)

metric <- "Accuracy"
mtry <- sqrt(ncol(clusters_rf))
tunegrid <- expand.grid(.mtry=mtry)

w <- 1/table(train_data$tier)
w <- w/sum(w)
weights <- rep(0, nrow(train_data))
weights[train_data$tier == 0] <- w['0']
weights[train_data$tier == 1] <- w['1']
weights[train_data$tier == 2] <- w['2']
weights[train_data$tier == 3] <- w['3']
weights[train_data$tier == 4] <- w['4']

model <- multinom(tier ~ gdp_capita_2020*popdens*isurban, train_data, weights = weights)

# testing accuracy
confusionMatrix(predict(model, newdata=test_data), test_data$tier)

# make predictions bases on future data

clusters$population_future <- clusters$population

for (i in 2021:planning_year){
  
  aa <- clusters
  aa$geometry <- NULL
  aa$geom <- NULL
  
  clusters$population_future <-  clusters$population_future*(1+pull(aa[paste0("pop_gr_", i)]))
  
}

clusters$popdens_future <- clusters$population_future / clusters$area

#

#############

newdata <- clusters %>% dplyr::select(gdp_capita_2050, popdens_future, isurban_future) %>% mutate(isurban_future=as.factor(isurban_future)) %>%  as.data.frame() 
newdata$geometry <- NULL
newdata$geom <- NULL
colnames(newdata) <- c("gdp_capita_2020", "popdens", "isurban")

clusters[complete.cases(newdata), "predicted_tier"] <- predict(model, newdata=newdata[complete.cases(newdata),]) 

####################################à

# Calculate number of households in each cluster
clusters$HHs = ifelse(clusters$isurban_future>0, clusters$population_future/urban_hh_size, clusters$population_future/rural_hh_size)

clusters$acc_pop_t1_new =  clusters$HHs * as.numeric(clusters$predicted_tier==0) + clusters$HHs * as.numeric(clusters$predicted_tier==1)
clusters$acc_pop_t2_new =  clusters$HHs * as.numeric(clusters$predicted_tier==2)
clusters$acc_pop_t3_new =  clusters$HHs * as.numeric(clusters$predicted_tier==3)
clusters$acc_pop_t4_new =  clusters$HHs * as.numeric(clusters$predicted_tier==4)

for (m in 1:12){
  for (i in 1:24){

    aa <- clusters
    aa$geometry=NULL
    aa$geom=NULL
    
    clusters = mutate(clusters, !!paste0('PerHHD_' , as.character(m) , "_" , as.character(i)) := ifelse(aa$isurban_future > 0,  pull(!!as.name(paste0('urb1', "_" , as.character(m))))[i] * aa$acc_pop_t1_new +  pull(!!as.name(paste0('urb2', "_" , as.character(m))))[i] * aa$acc_pop_t2_new +  pull(!!as.name(paste0('urb3', "_" , as.character(m))))[i] * aa$acc_pop_t3_new +  pull(!!as.name(paste0('urb4', "_" , as.character(m))))[i] * aa$acc_pop_t4_new * 0.75 +  pull(!!as.name(paste0('urb5', "_" , as.character(m))))[i] * aa$acc_pop_t4_new * 0.25, ifelse(aa$isurban_future == 0,  pull(!!as.name(paste0('rur1', "_" , as.character(m))))[i] * aa$acc_pop_t1_new +  pull(!!as.name(paste0('rur2', "_" , as.character(m))))[i] * aa$acc_pop_t2_new +  pull(!!as.name(paste0('rur3', "_" , as.character(m))))[i] * aa$acc_pop_t3_new +  pull(!!as.name(paste0('rur4', "_" , as.character(m))))[i] * aa$acc_pop_t4_new * 0.75 +  pull(!!as.name(paste0('rur5', "_" , as.character(m))))[i] * aa$acc_pop_t4_new * 0.25 , 0)))
  }}

aa <- clusters
aa$geometry=NULL
aa$geom=NULL

for (m in 1:12){
  
  aa <- clusters
  aa$geometry=NULL
  aa$geom=NULL
  
  out = aa %>% dplyr::select(starts_with(paste0("PerHHD_", as.character(m), "_"))) %>% rowSums(.)
  clusters[paste0('PerHHD_tt' ,"_monthly_" , as.character(m))] = out
}

aa <- clusters
aa$geometry=NULL
aa$geom=NULL

out = aa %>% dplyr::select(starts_with("PerHHD_tt_monthly_")) %>% rowSums(.)
clusters$PerHHD_tt = out
clusters$PerHHD_tt_avg <- clusters$PerHHD_tt / clusters$HHs

clusters <- st_as_sf(clusters)

save.image(paste0("results/", countrystudy, "/clusters_residential.Rdata"))
