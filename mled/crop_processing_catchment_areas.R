pop <- population_baseline # gridded population raster of reference

all_facilities <- cities
all_facilities$id <- 1:nrow(all_facilities)

# Prepare the friction layer

function_sens <- function(x){
  a <-(1/mean(x))
  ifelse(a<0, 0, a)
}

#friction <- aggregate(friction, fact=10, fun=function_sens, na.rm=TRUE)## to reduce resolution of orignal friction layer and hasten the process (to the cost of accuracy, of course)

Tr <- transition(friction, function_sens, 8) # RAM intensive, can be very slow for large areas

saveRDS(Tr, "T_sens.rds")

T.GC <- geoCorrection(Tr)

saveRDS(T.GC, "T.GC_sens.rds")

T.filename <- 'T.rds'

T.GC.filename <- 'T.GC.rds'

#############

# create catchment areas

functpop <-lapply(1:nrow(all_facilities),function(i){
  id_exp = all_facilities[i, ]$id
  xy.matrix <-st_coordinates(all_facilities[i, ])
  servedpop <- accCost(T.GC, xy.matrix)
  servedpop[servedpop>minutes_cluster] <- NA
  servedpop <- trim(servedpop)
  servedpop <- stars::st_as_stars(servedpop)
  servedpop <- st_as_sf(servedpop)
  p = servedpop %>% summarise()
  p = st_sf(p)
  p$id = id_exp
  p <- st_cast(p, "POLYGON")
  p
})

pol <-sf::st_as_sf(data.table::rbindlist(functpop))

clusters_traveltime_processing <- pol %>% group_by(id) %>% summarise()

clusters_traveltime_processing <- fasterize(st_collection_extract(clusters_traveltime_processing, "POLYGON"), population_baseline, "id")

clusters$clusters_traveltime_processing_i <- exact_extract(clusters_traveltime_processing, clusters, "sum")

clusters_traveltime_processing_i <- filter(clusters, clusters_traveltime_processing_i>0)

#write_sf(clusters_traveltime_processing_i, paste0(processed_folder, "clusters_traveltime_processing_i.gpkg"))

clusters$suitable_for_local_processing <- ifelse(clusters$id %in% clusters_traveltime_processing_i$id, 1, 0)

