# calculate paved road density in each cluster 
clusters$roadslenght = exact_extract(roads, clusters, "mean")

# calculate travel time to 50 k in each cluster
clusters$traveltime = exact_extract(traveltime, clusters, 'mean')

# calculate employment rate in each cluster
empl_wealth <- dplyr::select(empl_wealth, (starts_with("EM") | starts_with("id")))

empl_wealth_1 <- fasterize::fasterize(empl_wealth, traveltime, "EMEMPLWTOT", "first")
empl_wealth_2 <- fasterize::fasterize(empl_wealth, traveltime, "EMEMPLMTOT", "first")

clusters$EMEMPLWEMC = exact_extract(traveltime, clusters, 'mean') / 100
clusters$EMEMPLMEMC = exact_extract(traveltime, clusters, 'mean') / 100

clusters$EMEMPLWEMC <- ifelse(clusters$EMEMPLWEMC>1, 1, clusters$EMEMPLWEMC)
clusters$EMEMPLMEMC <- ifelse(clusters$EMEMPLMEMC>1, 1, clusters$EMEMPLMEMC)

# run PCA
clusters$employment = (clusters$EMEMPLMEMC + clusters$EMEMPLWEMC)/2

data_pca = dplyr::select(clusters, employment, popdens_future, traveltime)
data_pca$geometry=NULL

data_pca[] <- lapply(data_pca, function(x) { 
x[is.na(x)] <- mean(x, na.rm = TRUE)
x
})

data_pca <- lapply(data_pca, function(x) round((x-min(x, na.rm=T))/(max(x, na.rm=T)-min(x, na.rm=T)), 2)) %>% bind_cols()

data_pca <- data_pca[ , colSums(is.na(data_pca)) == 0]

data_pca_bk <- data_pca

data_pca <- prcomp(data_pca)

PCs <- as.data.frame(data_pca$x)
PCs$PCav <- PCs$PC1

# scales::rescale PCA to 0.3  - 0.6 range

PCs$PCav <- scales::rescale(PCs$PCav, to = c(0.6, 0.3))


#########

clusters_productive = dplyr::select(clusters, id, starts_with("PerHHD_")) %>% as.data.frame()
clusters_productive$geometry= NULL
clusters_productive$PerHHD_tt = NULL
clusters_productive$PerHHD_tt_avg = NULL

clusters_productive = dplyr::select(clusters_productive, 290:301)

clusters_productive = PCs$PCav * clusters_productive

colnames(clusters_productive) <- gsub("PerHHD_tt", "residual_productive_tt", colnames(clusters_productive))

clusters <- bind_cols(clusters, clusters_productive)

aa <- clusters
aa$geometry=NULL

out = aa %>% dplyr::select(starts_with("residual_productive_tt_")) %>% rowSums(.)

clusters$residual_productive_tt = out

saveRDS(clusters, "clusters_other_productive.R")

