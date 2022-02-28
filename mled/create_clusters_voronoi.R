# generate voronoi clusters for cropland loads

if (length(find_it("clusters_voronoi.gpkg"))>0){
  
  clusters_voronoi <- read_sf(find_it("clusters_voronoi.gpkg"))

} else {

clusters_centroids <- st_centroid(clusters)
p <- clusters_centroids

st_voronoi_point <- function(points){
  ## points must be POINT geometry
  if(!all(st_geometry_type(points) == "POINT")){
    stop("Input not  POINT geometries")
  }
  g = st_combine(st_geometry(points)) # make multipoint
  v = st_voronoi(g)
  v = st_collection_extract(v)
  return(v[unlist(st_intersects(p, v))])
}

clusters_voronoi = st_voronoi_point(p)
clusters_voronoi = st_set_geometry(p, clusters_voronoi)
clusters_voronoi <- st_intersection(clusters_voronoi, gadm0)

write_sf(clusters_voronoi, paste0(input_country_specific, "clusters_voronoi.gpkg"))

}