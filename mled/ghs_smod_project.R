r <- list.files(db_folder, pattern="GHS_SMOD_", full.names = T, recursive = T)
r <- r[grepl("tif", r)]
r <- r[!grepl("ovr", r)]

smod <- lapply(r, raster)
smod <- stack(smod)

gadm0 <- st_as_sf(raster::getData('GADM', country="ZMB", level=0))

smod <- mask_raster_to_polygon(smod, gadm0)

#plot(smod)

sdata = as.data.frame(smod, xy=T)
sdata$cellid <- 1:nrow(sdata)
sdata <- reshape2::melt(sdata, c(1:2, 8))
sdata$variable <- substr(gsub("GHS_SMOD_", "", sdata$variable), 2, 5)
sdata$variable <- as.numeric(sdata$variable )

#


sdata$value = ifelse(sdata$value > 20, 1, 0)


#


sdata <- dplyr::group_by(sdata, cellid) %>% dplyr::mutate(value_demeaned = value - mean(value, na.rm=T))

sdata$value_demeaned = ifelse(sdata$value_demeaned < 0, 0, sdata$value_demeaned)

means <-  dplyr::group_by(sdata, cellid) %>% dplyr::summarise(mean_hist = mean(value, na.rm=T))

#

library(splines)

model <- glm(value_demeaned ~  I(variable^2)*bs(x, degree = 5)*bs(y, degree = 5), sdata, family = binomial(logit), na.action=na.omit)

summary(model)

#

data_2040 <- sdata[sdata$variable==2020,]
data_2040$variable <- 2040

data_2050 <- sdata[sdata$variable==2020,]
data_2050$variable <- 2050

data_2060 <- sdata[sdata$variable==2020,]
data_2060$variable <- 2060

data_2040$pred <- predict(model, newdata=data_2040, type="response")
data_2050$pred <- predict(model, newdata=data_2050, type="response")
data_2060$pred <- predict(model, newdata=data_2060, type="response")

data_2040$mean_hist <- means$mean_hist
data_2050$mean_hist <- means$mean_hist
data_2060$mean_hist <- means$mean_hist

data_2040 <- dplyr::group_by(data_2040, cellid) %>% dplyr::mutate(pred = pred + mean_hist)
data_2050 <- dplyr::group_by(data_2050, cellid) %>% dplyr::mutate(pred = pred + mean_hist)
data_2060 <- dplyr::group_by(data_2060, cellid) %>% dplyr::mutate(pred = pred + mean_hist)

smod[[6]] <- smod[[5]]
smod[[7]] <- smod[[5]]
smod[[8]] <- smod[[5]]

data_2040 <- data_2040[order(data_2040$cellid),]
data_2050 <- data_2050[order(data_2050$cellid),]
data_2060 <- data_2060[order(data_2060$cellid),]

values(smod[[6]]) <- data_2040$pred
values(smod[[7]]) <- data_2050$pred
values(smod[[8]]) <- data_2060$pred

values(smod) <- ifelse(values(smod)>=0.5, 1, 0)

smod <- mask_raster_to_polygon(smod, gadm0)

values(smod[[6]])[values(smod[[5]])==1] <- 1
values(smod[[7]])[values(smod[[5]])==1] <- 1
values(smod[[8]])[values(smod[[5]])==1] <- 1

names(smod) <- c(2010, 2015, 2020, 2025, 2030, 2040, 2050, 2060)

#

for (i in 1:nlayers(smod)){
  
  print(table(values(smod[[i]])))

}

#

smod <- projectRaster(smod, crs="+proj=longlat +ellps=WGS84 +datum=WGS84", method="ngb")

rasterVis::levelplot(smod)

writeRaster(smod, filename=paste0(db_folder, "/input_folder/smod_projected_2010_2060.tif"), overwrite=T)
