# Calculate the number of people in each tier in each cluster
clusters$pop <- exact_extract(population_baseline, clusters, "sum")
clusters$popdens <- clusters$Population / clusters$Area
clusters$gdp_capita <- exact_extract(gdp_capita_baseline, clusters, "mean")

plot_raster_tiers <- rasterVis::levelplot(ratify(raster_tiers))

values(raster_tiers) <- ifelse(values(raster_tiers)==0, NA, values(raster_tiers))
clusters$tier <- exact_extract(raster_tiers, clusters, "majority")

#

set.seed(123)

clusters_rf <- na.omit(clusters)

clusters_rf <- dplyr::select(clusters_rf, tier, gdp_capita, popdens, IsUrban)

clusters_rf$tier <- as.factor(clusters_rf$tier)
clusters_rf$IsUrban <- as.factor(clusters_rf$IsUrban)

clusters_rf$geometry <- NULL

train_data <- clusters_rf %>%
  group_by(tier, IsUrban) %>%
  slice_sample(prop = 0.7) 

test_data <- clusters_rf %>%  anti_join(train_data)

metric <- "Accuracy"
mtry <- sqrt(ncol(clusters_rf))
tunegrid <- expand.grid(.mtry=mtry)

w <- 1/table(train_data$tier)
w <- w/sum(w)
weights <- rep(0, nrow(train_data))
weights[train_data$tier == 1] <- w['1']
weights[train_data$tier == 2] <- w['2']
weights[train_data$tier == 3] <- w['3']
weights[train_data$tier == 4] <- w['4']

rf_default <- train(tier ~  .,
                    data=train_data,
                    method='rf',
                    metric='Accuracy',
                    tuneLength  = 5,
                    weights=weights,
                    ntree = 500)#, num.threads = 3)

# training accuracy
rf_default

# testing accuracy
confusionMatrix(predict(rf_default, newdata=test_data), test_data$tier)

# make predictions bases on future data

clusters$pop_future <- exact_extract(population, clusters, "sum")
clusters$popdens_future <- clusters$pop_future / clusters$Area
clusters$gdp_capita_future <- exact_extract(gdp_capita, clusters, "mean")

newdata <- clusters %>% dplyr::select(gdp_capita_future, popdens_future, IsUrban) %>% mutate(IsUrban=as.factor(IsUrban)) %>%  as.data.frame() 
newdata$geometry <- NULL
colnames(newdata) <- c("gdp_capita", "popdens", "IsUrban")

clusters[complete.cases(newdata), "predicted_tier"] <- predict(rf_default, newdata=newdata[complete.cases(newdata),]) 

# transition matrix
table(clusters$predicted_tier, clusters$tier)

# Calculate number of households in each cluster
clusters$HHs = ifelse(clusters$IsUrban>0, clusters$pop/urban_hh_size, clusters$pop/rural_hh_size)

clusters$acc_pop_t1_new =  clusters$HHs * as.numeric(clusters$predicted_tier==1)
clusters$acc_pop_t2_new =  clusters$HHs * as.numeric(clusters$predicted_tier==2)
clusters$acc_pop_t3_new =  clusters$HHs * as.numeric(clusters$predicted_tier==3)
clusters$acc_pop_t4_new =  clusters$HHs * as.numeric(clusters$predicted_tier==4)

for (m in 1:12){
  for (i in 1:24){

    aa <- clusters
    aa$geometry=NULL
    aa$geom=NULL
    
    clusters = mutate(clusters, !!paste0('PerHHD_' , as.character(m) , "_" , as.character(i)) := ifelse(aa$IsUrban > 0,  pull(!!as.name(paste0('urb1', "_" , as.character(m))))[i] * aa$acc_pop_t1_new +  pull(!!as.name(paste0('urb2', "_" , as.character(m))))[i] * aa$acc_pop_t2_new +  pull(!!as.name(paste0('urb3', "_" , as.character(m))))[i] * aa$acc_pop_t3_new +  pull(!!as.name(paste0('urb4', "_" , as.character(m))))[i] * aa$acc_pop_t4_new * 0.75 +  pull(!!as.name(paste0('urb5', "_" , as.character(m))))[i] * aa$acc_pop_t4_new * 0.25, ifelse(aa$IsUrban == 0,  pull(!!as.name(paste0('rur1', "_" , as.character(m))))[i] * aa$acc_pop_t1_new +  pull(!!as.name(paste0('rur2', "_" , as.character(m))))[i] * aa$acc_pop_t2_new +  pull(!!as.name(paste0('rur3', "_" , as.character(m))))[i] * aa$acc_pop_t3_new +  pull(!!as.name(paste0('rur4', "_" , as.character(m))))[i] * aa$acc_pop_t4_new * 0.75 +  pull(!!as.name(paste0('rur5', "_" , as.character(m))))[i] * aa$acc_pop_t4_new * 0.25 , 0)))
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

saveRDS(clusters, "clusters_residential.R")
