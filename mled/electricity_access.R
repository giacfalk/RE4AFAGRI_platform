geom <- ee$Geometry$Rectangle(c(as.vector(extent(gadm0))[1], as.vector(extent(gadm0))[3], as.vector(extent(gadm0))[2], as.vector(extent(gadm0))[4]))

GHSSMOD2015 = ee$ImageCollection("JRC/GHSL/P2016/BUILT_LDSMT_GLOBE_V1")$select('built')$filterDate('2014-01-01', '2016-01-01')$median()

GHSSMOD2015 = GHSSMOD2015$gte(3)

nl19 =  ee$ImageCollection("NOAA/VIIRS/DNB/MONTHLY_V1/VCMCFG")$filterDate('2019-01-01', '2020-01-01')$select('avg_rad')$median()$subtract(0.125)
nl19 = nl19$where(nl19$lt(0.25), ee$Image(0))

GHSSMOD2015_lit <- GHSSMOD2015$mask(nl19$gt(0))

if (paste0("builtup_", countryiso3, ".tif") %in% all_input_files_basename){
  
  GHSSMOD2015 <- raster(find_it(paste0("builtup_", countryiso3, ".tif")))
  
} else {

GHSSMOD2015 <- ee_as_raster(
  image = slope,
  via = "drive",
  region = geom,
  scale = 500,
  dsn = paste0(processed_folder, "builtup_", countryiso3, ".tif")
)

}


if (paste0("builtup_lit_", countryiso3, ".tif") %in% all_input_files_basename){
  
  GHSSMOD2015 <- raster(find_it(paste0("builtup_lit_", countryiso3, ".tif")))
  
} else {
  

GHSSMOD2015_lit <- ee_as_raster(
  image = slope,
  via = "drive",
  region = geom,
  scale = 500,
  dsn = paste0(processed_folder, "builtup_lit_", countryiso3, ".tif")
)

}

clusters$elrate <-  exact_extract(GHSSMOD2015_lit, clusters, fun="sum") / exact_extract(GHSSMOD2015, clusters, fun="sum")


#


# Insert dissever for consumption here #
