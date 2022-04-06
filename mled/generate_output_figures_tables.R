clusters <- read_rds("clusters_with_data_7_1.Rds")
clusters_bk <- read_rds("clusters_bk.Rds")

clusters_bk <- filter(clusters_bk, !(clusters_bk$cl_id %in% clusters$cl_id))

clusters <- bind_rows(clusters, clusters_bk)
clusters <- st_as_sf(clusters)

library(tmap)
data(World)

world_all <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

fun_plot <- function(var_to_plot, title_plot="", style="quantile"){
  
  tm_shape ( clusters ) + tm_fill ( col=as.character(var_to_plot), title=title_plot, style=style, legend.reverse = TRUE,  colorNA = "white", showNA = F, n=5,
                                    legend.hist = TRUE) +  tm_layout( frame = FALSE , scale = 1) + tm_shape(World) + tm_borders("black", lwd = .5)
  
}

#

#output = st_intersection(clusters, st_union(world_all$geometry))

######

# Figure 2
library(lwgeom)
library(ggsci)

regions <- filter(world_all, region_un=="Africa") %>% st_transform(3395) %>%
  st_snap_to_grid(size = 1000) %>%
  st_make_valid() %>%
  group_by(subregion) %>% 
  summarize() %>% 
  st_transform(4326)


r <- raster(); res(r)<-.25;
irreq <- stack(lapply(grep("monthly_IRREQ_", colnames(clusters), value = TRUE), function(X){fasterize::fasterize(clusters, r, X)})) / 1e6

regions_data <- exact_extract(irreq, regions, "sum")
regions_data$region <- regions$subregion
regions_data <- melt(regions_data, 13)

regions_data$variable <- gsub("sum.layer.", "", regions_data$variable)

fig2a <- ggplot(regions_data)+
  theme_classic()+
  geom_point(aes(x=as.numeric(variable), y=value/1e3, group=region, colour=region), size=2)+
  geom_line(aes(x=as.numeric(variable), y=value/1e3, group=region, colour=region), linetype="dashed", size=1)+
  scale_x_continuous(labels=month.abb, breaks=c(1:12))+
  ylab("Water requirements to close \nthe irrigation gap (cubic km / month)")+
  xlab("Month")+
  scale_colour_npg(name="Region")

enreq <- stack(lapply(grep("^er_kwh", colnames(clusters), value = TRUE)[1:12], function(X){fasterize::fasterize(clusters, r, X)})) 

npumps <- fasterize::fasterize(clusters, r, "npumps")

regions_data <- exact_extract(enreq*npumps, regions, "sum") / 1e6
regions_data$region <- regions$subregion
regions_data <- melt(regions_data, 13)

regions_data$variable <- gsub("sum.layer.", "", regions_data$variable)

fig2b <- ggplot(regions_data)+
  theme_classic()+
  geom_point(aes(x=as.numeric(variable), y=value, group=region, colour=region), size=2)+
  geom_line(aes(x=as.numeric(variable), y=value, group=region, colour=region), linetype="dashed", size=1)+
  scale_x_continuous(labels=month.abb, breaks=c(1:12))+
  ylab("Electricity requirements to close \nthe irrigation gap (GWh)")+
  xlab("Month")+
  scale_colour_npg(name="Region")

library(patchwork)

(fig2a + fig2b + plot_layout(guides = "collect"))

clusters$pumps_ha <- clusters$npumps / clusters$A_total * 100

clusters$pumps_ha_c <- cut(clusters$pumps_ha,
                         breaks=c(0, 1, 2, 5, 10, 20, Inf),
                         labels=c('<1', '1-2', '2-5', '5-10', '10-20', '>20'))

fig2c <- fig2d <- fig2e <- ggplot() + geom_sf(data = clusters,
                 aes(fill = pumps_ha_c), colour=NA)+
      scale_fill_viridis_d(name="", na.value = NA, na.translate = F)+
      geom_sf(data=world_all, fill=NA, size =.3)+
      theme_classic()+
      coord_sf(ylim=c(-34, 36), xlim=c(-20, 51))+
      theme(legend.position = c(0.2, 0.28), legend.direction = "vertical", legend.text=element_text(size=8), axis.line=element_blank(),axis.text.x=element_blank(), legend.title=element_text(size=8, face = "bold"), panel.border = element_rect(colour = "black", fill=NA, size=1),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))+
      ggtitle("Required pumps per sq. km of cropland")+
     xlab("")+
     ylab("")


#

clusters$pvsize_mwh_total <- (clusters$pvsize_kw * clusters$npumps) / 1000

clusters$pvsize_mwh_total <- cut(clusters$pvsize_mwh_total,
                                 breaks=c(0, 0.5, 1, 2, 5, Inf),
                                 labels=c('<0.5', '0.5', '1-2', '2-5', '>10'))

fig2d <- ggplot() + geom_sf(data = clusters,
                                              aes(fill = pvsize_mwh_total), colour=NA)+
  scale_fill_viridis_d(name="", na.value = NA, na.translate = F)+
  geom_sf(data=world_all, fill=NA, size =.3)+
  theme_classic()+
  coord_sf(ylim=c(-34, 36), xlim=c(-20, 51))+
  theme(legend.position = c(0.2, 0.28), legend.direction = "vertical", legend.text=element_text(size=8), axis.line=element_blank(),axis.text.x=element_blank(), legend.title=element_text(size=8, face = "bold"), panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))+
  ggtitle("PV installation requirements (MW)")+
  xlab("")+
  ylab("")

#

clusters$battery_mwh <- (clusters$batterysize_kwh / clusters$pvsize_kw)

clusters$battery_mwh <- cut(clusters$battery_mwh,
                                 breaks=c(0, 0.5, 1, 1.5, 2, Inf),
                                 labels=c('<0.5', '0.5-1', '1-1.5', '1.5-2', '>2'))

fig2e <- ggplot() + geom_sf(data = clusters,
                            aes(fill = battery_mwh), colour=NA)+
  scale_fill_viridis_d(name="", na.value = NA, na.translate = F)+
  geom_sf(data=world_all, fill=NA, size =.3)+
  theme_classic()+
  coord_sf(ylim=c(-34, 36), xlim=c(-20, 51))+
  theme(legend.position = c(0.2, 0.28), legend.direction = "vertical", legend.text=element_text(size=8), axis.line=element_blank(),axis.text.x=element_blank(), legend.title=element_text(size=8, face = "bold"), panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))+
  ggtitle("Battery requirements  (kWh / KW of PV)")+
  xlab("")+
  ylab("")

fig2_upper <- (fig2a + fig2b + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A')) 
fig2_lower <- cowplot::plot_grid(fig2c, fig2d, fig2e, nrow=1, labels=c("C", "D", "E"), label_fontface = "plain")

fig2 <- plot_grid(fig2_upper, fig2_lower, nrow = 2, rel_heights = c(0.9 ,1))
  
ggsave("fig2.png", fig2, scale=1.85, height = 4, width = 6)

#########################################

# Figure 3 

clusters$totalpumpcost_cl <- ifelse(is.na(clusters$profit_yearly) , 0, clusters$totalpumpcost)
clusters$totalcost_cl <- ifelse(is.na(clusters$profit_yearly) , 0, clusters$totalcost)
clusters$transp_costs_cl <- ifelse(is.na(clusters$profit_yearly) , 0, clusters$transp_costs)

costs1 <- stack(lapply(grep("totalpumpcost_cl", colnames(clusters), value = TRUE), function(X){fasterize::fasterize(clusters, r, X)})) / 1e9

costs2 <- stack(lapply(grep("totalcost_cl", colnames(clusters), value = TRUE), function(X){fasterize::fasterize(clusters, r, X)})) / 1e9

costs3 <- stack(lapply(grep("transp_costs_cl", colnames(clusters), value = TRUE), function(X){fasterize::fasterize(clusters, r, X)})) / 1e9

costs <- stack(costs1, costs2, costs3)

costs_data <- data.frame(exact_extract(costs, regions, "sum"))
costs_data$region <- regions$subregion
colnames(costs_data)[1:3] <- c("totalpumpcost", "totalpvcost", "transp_costs")

revenues <- stack(lapply(grep("total_revenues_discounted_discounted_yearly", colnames(clusters), value = TRUE), function(X){fasterize::fasterize(clusters, r, X)})) / 1e9

revenues_data <- data.frame(exact_extract(revenues, regions, "sum"))
revenues_data$region <- regions$subregion
colnames(revenues_data)[1] <- "value"

profits <- stack(lapply(grep("profit_yearly", colnames(clusters), value = TRUE), function(X){fasterize::fasterize(clusters, r, X)})) / 1e9

profits_data <- data.frame(exact_extract(profits, regions, "sum"))
profits_data$region <- regions$subregion
colnames(profits_data)[1] <- "value"

data <- data.frame(costs_data$region, costs_data$totalpumpcost/lifetimepump, costs_data$totalpvcost/lifetimepump, costs_data$transp_costs/lifetimepump, revenues_data$value, profits_data$value)
colnames(data) <- c("region", "Pump costs", "PV costs", "Transport costs", "Revenues", "Profits")
data <- pivot_longer(data, cols=2:6)

data$type <- ifelse(grepl("cost", data$name), "Costs", ifelse(grepl("Revenues", data$name), "Revenues", "Profits"))

data <- filter(data, region != "Northern Africa")

library(stringr)

make_labels <- function(labels) {
  result <- str_split(labels, "\\.")
  unlist(lapply(result, function(x) x[2]))
}


fig3 <- ggplot(data)+
  theme_classic()+
  geom_col(aes(x=interaction(type, region), y=value, fill=name, group=type), position = "stack")+
  scale_fill_npg(name="")+
  ylab("Yearly average discounted value (bn. USD)")+
  xlab("Region")+
  scale_x_discrete(labels = c("", "Eastern Africa", "", "", "Central Africa", "", "", "Southern Africa", "", "", "Western Africa", ""))+
  ggtitle("Economics of solar irrigation in sites where technology is profitable")

ggsave("fig3.png", fig3, scale=1.8)

####

# Figure 4

clusters$total_system_cost_discounted_yeary_d <- cut(clusters$total_system_cost_discounted_yeary / clusters$npumps,
                           breaks=c(0, 100, 200, 500, 1000, Inf),
                           labels=c('<100', '100-200', '200-500', '500-1000', '>1000'))

fig4a <-  ggplot() + geom_sf(data = clusters,
                                              aes(fill = total_system_cost_discounted_yeary_d), colour=NA)+
  scale_fill_viridis_d(name="", na.value = NA, na.translate = F)+
  geom_sf(data=world_all, fill=NA, size =.3)+
  theme_classic()+
  coord_sf(ylim=c(-34, 36), xlim=c(-20, 51))+
  theme(legend.position = c(0.2, 0.28), legend.direction = "vertical", legend.text=element_text(size=8), axis.line=element_blank(),axis.text.x=element_blank(), legend.title=element_text(size=8, face = "bold"), panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))+
  ggtitle("Yearly average discounted costs \n(USD / solar pumping system)")+
  xlab("")+
  ylab("")

#

clusters$total_revenues_discounted_discounted_yearly_d <- cut(clusters$total_revenues_discounted_discounted_yearly / clusters$npumps,
                                                     breaks=c(0, 100, 200, 500, 1000, Inf),
                                                     labels=c('<100', '100-200', '200-500', '500-1000', '>1000'))

fig4b <-  ggplot() + geom_sf(data = clusters,
                             aes(fill = total_revenues_discounted_discounted_yearly_d), colour=NA)+
  scale_fill_viridis_d(name="", na.value = NA, na.translate = F)+
  geom_sf(data=world_all, fill=NA, size =.3)+
  theme_classic()+
  coord_sf(ylim=c(-34, 36), xlim=c(-20, 51))+
  theme(legend.position = c(0.2, 0.28), legend.direction = "vertical", legend.text=element_text(size=8), axis.line=element_blank(),axis.text.x=element_blank(), legend.title=element_text(size=8, face = "bold"), panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))+
  ggtitle("Yearly average discounted revenues \n(USD / solar pumping system)")+
  xlab("")+
  ylab("")

clusters$profit_yearly_d <- cut(clusters$profit_yearly / clusters$npumps,
                                                              breaks=c(0, 100, 200, 500, 1000, Inf),
                                                              labels=c('<100', '100-200', '200-500', '500-1000', '>1000'))

fig4c <-  ggplot() + geom_sf(data = clusters,
                             aes(fill = profit_yearly_d), colour=NA)+
  scale_fill_viridis_d(name="", na.value = NA, na.translate = F)+
  geom_sf(data=world_all, fill=NA, size =.3)+
  theme_classic()+
  coord_sf(ylim=c(-34, 36), xlim=c(-20, 51))+
  theme(legend.position = c(0.2, 0.28), legend.direction = "vertical", legend.text=element_text(size=8), axis.line=element_blank(),axis.text.x=element_blank(), legend.title=element_text(size=8, face = "bold"), panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))+
  ggtitle("Yearly average discounted profits \n(USD / solar pumping system)")+
  xlab("")+
  ylab("")

# clusters$economic_feas <- !is.na(clusters$profit_yearly)
# clusters$economic_feas <- ifelse(clusters$economic_feas==1, "Profitable", "Not profitable")


clusters$which_pumping <- ifelse((is.na(clusters$profit_yearly) & clusters$yearly_IRREQ>0) |  clusters$which_pumping=="Neither possible", "Unprofitable", clusters$which_pumping)
clusters$which_pumping <- ifelse(clusters$which_pumping=="Ground water pumping" , "Groundwater", clusters$which_pumping)
clusters$which_pumping <- ifelse(clusters$which_pumping=="Surface water pumping" , "Surfacewater", clusters$which_pumping)

fig4d <-  ggplot() + geom_sf(data = clusters,
                             aes(fill = as.factor(which_pumping)), colour=NA)+
  scale_fill_manual(name="", values = c("#f54242", "#4290f5", "#bbbdbf"), na.value = NA, na.translate = F)+
  geom_sf(data=world_all, fill=NA, size =.3)+
  theme_classic()+
  coord_sf(ylim=c(-34, 36), xlim=c(-20, 51))+
  theme(legend.position = c(0.22, 0.34), legend.direction = "vertical", legend.text=element_text(size=8), axis.line=element_blank(),axis.text.x=element_blank(), legend.title=element_text(size=7, face = "bold"), panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))+
  ggtitle("Solar pumping: economic feasibility areas")+
  xlab("")+
  ylab("")


clusters$PBT_f <- cut(clusters$PBT,
                                breaks=c(0, 1, 2, 5, 10, Inf),
                                labels=c('<1', '1-2', '2-5', '5-10', '>10'))

fig4e <-  ggplot() + geom_sf(data = clusters,
                             aes(fill = PBT_f), colour=NA)+
  scale_fill_viridis_d(name="", na.value = NA, na.translate = F)+
  geom_sf(data=world_all, fill=NA, size =.3)+
  theme_classic()+
  coord_sf(ylim=c(-34, 36), xlim=c(-20, 51))+
  theme(legend.position = c(0.2, 0.28), legend.direction = "vertical", legend.text=element_text(size=8), axis.line=element_blank(),axis.text.x=element_blank(), legend.title=element_text(size=7, face = "bold"), panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))+
  ggtitle("Payback time, years")+
  xlab("")+
  ylab("")

fig4_upper <- cowplot::plot_grid(fig4a, fig4b, fig4c, nrow=1, labels=c("A", "B", "C"), label_fontface = "plain")
fig4_lower <- cowplot::plot_grid(fig4d, fig4e, nrow=1, labels=c("D", "E"), label_fontface = "plain")
fig4 <- plot_grid(fig4_upper, fig4_lower, nrow = 2, rel_heights = c(1 ,1))

#fig4 <- cowplot::plot_grid(fig4a, fig4b, fig4c, fig4d, nrow=2, labels="AUTO", label_fontface = "plain")

ggsave("fig4.png", fig4, scale=1.85, height = 4, width = 5)

#### Figure 5

#

summ <- clusters
summ$geometry<-NULL
summ <- dplyr::group_by(summ, sov_a3) %>% dplyr::summarise(Calories_gap=mean(Calories_gap, na.rm=T), calories_gain_total=sum(calories_gain_total, na.rm=T), value=mean(value, na.rm=T), insec=mean(insec, na.rm=T))

fig5a <- ggplot(summ)+
  theme_classic()+
  geom_point(aes(x=calories_gain_total/value/365, y=Calories_gap, size=insec, fill=insec), shape = 21, alpha = 0.7)+
  geom_label_repel(aes(x=calories_gain_total/value/365, y=Calories_gap, label=sov_a3))+
  xlab("Food generation potential due to solar pumps adoption (Kcal/person/day)")+
  ylab("Food gap (Kcal/person/day)")+
  scale_fill_viridis_c(name="% of population who is food insecure", guide = "legend") +
  scale_size_continuous(name="% of population who is food insecure", range = c(1, 15))

clusters$calories_gain_capita_day <- ifelse(clusters$calories_gain_capita_day==0, NA, clusters$calories_gain_capita_day)

clusters$calories_gain_capita_day_d <- cut(clusters$calories_gain_capita_day,
                                                              breaks=c(0, 100, 200, 500, 1000, Inf),
                                                              labels=c('<100', '100-200', '200-500', '500-1000', '>1000'))


fig5b <- ggplot() + geom_sf(data = clusters,
                            aes(fill = calories_gain_capita_day_d), colour=NA)+
  scale_fill_viridis_d(name="", na.value = NA, na.translate = F)+
  geom_sf(data=world_all, fill=NA, size =.3)+
  theme_classic()+
  coord_sf(ylim=c(-34, 36), xlim=c(-20, 51))+
  theme(legend.position = c(0.2, 0.28), legend.direction = "vertical", axis.line=element_blank(),axis.text.x=element_blank(), legend.title=element_text(size=8, face = "bold"), panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))+
  ggtitle("Calories potential (Kcal/person/day)")+
  xlab("")+
  ylab("")


clusters$proteins_gain_capita_day <- ifelse(clusters$proteins_gain_capita_day==0, NA, clusters$proteins_gain_capita_day)

clusters$proteins_gain_capita_day_d <- cut(clusters$proteins_gain_capita_day,
                                           breaks=c(0, 1, 2, 5, 10, Inf),
                                           labels=c('<1', '1-2', '2-5', '5-10', '>10'))


fig5c <- ggplot() + geom_sf(data = clusters,
                            aes(fill = proteins_gain_capita_day_d), colour=NA)+
  scale_fill_viridis_d(name="", na.value = NA, na.translate = F)+
  geom_sf(data=world_all, fill=NA, size =.3)+
  theme_classic()+
  coord_sf(ylim=c(-34, 36), xlim=c(-20, 51))+
  theme(legend.position = c(0.2, 0.28), legend.direction = "vertical", axis.line=element_blank(),axis.text.x=element_blank(), legend.title=element_text(size=8, face = "bold"), panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))+
  ggtitle("Proteins potential (g/person/day)")+
  xlab("")+
  ylab("")


clusters$fats_gain_capita_day <- ifelse(clusters$fats_gain_capita_day==0, NA, clusters$fats_gain_capita_day)


clusters$fats_gain_capita_day_d <- cut(clusters$fats_gain_capita_day,
                                           breaks=c(0, 1, 2, 5, 10, Inf),
                                           labels=c('<1', '1-2', '2-5', '5-10', '>10'))

fig5d <- ggplot() + geom_sf(data = clusters,
                            aes(fill = fats_gain_capita_day_d), colour=NA)+
  scale_fill_viridis_d(name="", na.value = NA, na.translate = F)+
  geom_sf(data=world_all, fill=NA, size =.3)+
  theme_classic()+
  coord_sf(ylim=c(-34, 36), xlim=c(-20, 51))+
  theme(legend.position = c(0.2, 0.28), legend.direction = "vertical", axis.line=element_blank(),axis.text.x=element_blank(), legend.title=element_text(size=8, face = "bold"), panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))+
  ggtitle("Fats potential (g/person/day)")+
  xlab("")+
  ylab("")

fig5_upper <- (fig5a + plot_annotation(tag_levels = 'A')) 
fig5_lower <- cowplot::plot_grid(fig5b, fig5c, fig5d, nrow=1, labels=c("B", "C", "D"), label_fontface = "plain")

fig5 <- plot_grid(fig5_upper, fig5_lower, nrow = 2, rel_heights = c(1 ,1.2))

ggsave("fig5.png", fig5, scale=1.75, height = 4.7, width = 6)


#### Figure 6

clusters$share_pvout_used_for_pumping <- ifelse(!is.na(clusters$profit_yearly), clusters$share_pvout_used_for_pumping , NA)

clusters$share_pvout_used_for_pumping_d <- cut(clusters$share_pvout_used_for_pumping,
                                           breaks=c(0, 0.05, 0.1, 0.2 ,0.3, Inf),
                                           labels=c('<5%', '5%-10%', '10%-20%', '20-30%', '>30%'))


fig6a <- ggplot() + geom_sf(data = clusters,
                            aes(fill = share_pvout_used_for_pumping_d), colour=NA)+
  scale_fill_viridis_d(name="", na.value = NA, na.translate = F)+
  geom_sf(data=world_all, fill=NA, size =.3)+
  theme_classic()+
  coord_sf(ylim=c(-34, 36), xlim=c(-20, 51))+
  theme(legend.position = c(0.2, 0.28), legend.direction = "vertical", axis.line=element_blank(),axis.text.x=element_blank(), legend.title=element_text(size=8, face = "bold"), panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))+
  ggtitle("Average % PV output used for pumping")+
  xlab("")+
  ylab("")

clusters$residual_pvout_notused_for_pumping <- ifelse(!is.na(clusters$profit_yearly), clusters$residual_pvout_notused_for_pumping , NA)

clusters$residual_pvout_notused_for_pumping_d <- cut(clusters$residual_pvout_notused_for_pumping/365,
                                               breaks=c(0, 1, 2, 5 , 10, Inf),
                                               labels=c('<1 kWh/d', '1-2 kWh/d', '2-5 kWh/d', '5-10 kWh/d', '>10 kWh/d'))


fig6b <- ggplot() + geom_sf(data = clusters,
                            aes(fill = residual_pvout_notused_for_pumping_d), colour=NA)+
  scale_fill_viridis_d(name="", na.value = NA, na.translate = F)+
  geom_sf(data=world_all, fill=NA, size =.3)+
  theme_classic()+
  coord_sf(ylim=c(-34, 36), xlim=c(-20, 51))+
  theme(legend.position = c(0.2, 0.28), legend.direction = "vertical", axis.line=element_blank(),axis.text.x=element_blank(), legend.title=element_text(size=8, face = "bold"), panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))+
  ggtitle("Average residual PV output (kWh/pump/day)")+
  xlab("")+
  ylab("")

fig6 <- cowplot::plot_grid(fig6a, fig6b, nrow=1, labels="AUTO", label_fontface = "plain")

ggsave("fig6.png", fig6, scale=1.85, height = 2, width = 4)

####

# print some interesting details to report aggregate results in the paper, or even consider making a table!

sum(clusters$total_system_cost_discounted[!is.na(clusters$profit_yearly)], na.rm=T) / 1e9

sum(clusters$total_system_cost_discounted_yeary[!is.na(clusters$profit_yearly)], na.rm=T) / 1e9 

sum(clusters$total_revenues_discounted_discounted_yearly[!is.na(clusters$profit_yearly)], na.rm=T) / 1e9

sum(clusters$profit_yearly[!is.na(clusters$profit_yearly)], na.rm=T) / 1e9

sum(clusters$profit_yearly[!is.na(clusters$profit_yearly)], na.rm=T) * lifetimepump / 1e9 

mean(clusters$residual_pvout_notused_for_pumping, na.rm=T) / 365

sum(clusters$residual_pvout_notused_for_pumping * clusters$npumps, na.rm=T) / 1e6 / 365
sum(clusters$residual_pvout_notused_for_pumping * clusters$npumps, na.rm=T) / 1e9 

sum(clusters$npumps[!is.na(clusters$profit_yearly)], na.rm=T)/1e6

sum(clusters$yearly_IRREQ)/1e9 # water demand to close the irrigation gap in km3

