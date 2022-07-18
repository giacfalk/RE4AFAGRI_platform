ssps <- c("ssp245", "ssp370")
Vars <- c("tmin", "tmax", "prec")
models <- c("ACCESS-ESM1-5", "BCC-CSM2-MR", "CMCC-ESM2", "GFDL-ESM4", "GISS-E2-1-G", "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0")

df <- expand.grid(ssps, models, Vars)

stub <- paste0("https://geodata.ucdavis.edu/cmip6/5m/", df$Var2, "/", df$Var1, "/wc2.1_5m_", df$Var3, "_", df$Var2, "_", df$Var1, "_2041-2060.tif")

setwd("R:/falchetta/watercrop")

for (i in 1:length(stub)){
  tryCatch({
print(i)
download.file(stub[i], sub(".*\\/", "", stub)[i], mode="wb")
}, error=function(e){})}

# mean of models by year

list.cdds <- list.files(getwd(), full.names = T, recursive = T, pattern = "tif")

# select vars to calculate median ensembles

list.tmax <- list.cdds[grepl("tmax", list.cdds)]
list.tmin <- list.cdds[grepl("tmin", list.cdds)]
list.prec <- list.cdds[grepl("prec", list.cdds)]

list.tmax <- list.tmax[grepl("ssp245", list.tmax)]
list.tmin <- list.tmin[grepl("ssp245", list.tmin)]
list.prec <- list.prec[grepl("ssp245", list.prec)]


# mean of years to get 2050 tmin, tmax, prec

library(raster)

indices=rep(1:12, times=length(list.tmax))
list.tmax <- lapply(list.tmax, stack)
list.tmax <- stack(list.tmax)
list.tmax <- - stackApply(list.tmax, indices = indices, fun=mean)
list.tmax <- stack(list.tmax)

writeRaster(list.tmax, "tmax_2050_median_ssp245.tif", overwrite=T)

indices=rep(1:12, times=length(list.tmin))
list.tmin <- lapply(list.tmin, stack)
list.tmin <- stack(list.tmin)
list.tmin <- - stackApply(list.tmin, indices = indices, fun=mean)
list.tmin <- stack(list.tmin)

writeRaster(list.tmin, "tmin_2050_median_ssp245.tif", overwrite=T)

indices=rep(1:12, times=length(list.prec))
list.prec <- lapply(list.prec, stack)
list.prec <- stack(list.prec)
list.prec <- - stackApply(list.prec, indices = indices, fun=mean)
list.prec <- stack(list.prec)

writeRaster(list.prec, "prec_2050_median_ssp245.tif", overwrite=T)

#

# select vars to calculate median ensembles

list.tmax <- list.cdds[grepl("tmax", list.cdds)]
list.tmin <- list.cdds[grepl("tmin", list.cdds)]
list.prec <- list.cdds[grepl("prec", list.cdds)]

list.tmax <- list.tmax[grepl("ssp370", list.tmax)]
list.tmin <- list.tmin[grepl("ssp370", list.tmin)]
list.prec <- list.prec[grepl("ssp370", list.prec)]


# mean of years to get 2050 tmin, tmax, prec

library(raster)

indices=rep(1:12, times=length(list.tmax))
list.tmax <- lapply(list.tmax, stack)
list.tmax <- stack(list.tmax)
list.tmax <- - stackApply(list.tmax, indices = indices, fun=mean)
list.tmax <- stack(list.tmax)

writeRaster(list.tmax, "tmax_2050_median_ssp370.tif", overwrite=T)

indices=rep(1:12, times=length(list.tmin))
list.tmin <- lapply(list.tmin, stack)
list.tmin <- stack(list.tmin)
list.tmin <- - stackApply(list.tmin, indices = indices, fun=mean)
list.tmin <- stack(list.tmin)

writeRaster(list.tmin, "tmin_2050_median_ssp370.tif", overwrite=T)

indices=rep(1:12, times=length(list.prec))
list.prec <- lapply(list.prec, stack)
list.prec <- stack(list.prec)
list.prec <- - stackApply(list.prec, indices = indices, fun=mean)
list.prec <- stack(list.prec)

writeRaster(list.prec, "prec_2050_median_ssp370.tif", overwrite=T)

####

