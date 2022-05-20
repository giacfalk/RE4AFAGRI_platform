if (no_productive_demand_in_small_clusters == T){
  
  clusters <- clusters %>% mutate_at(vars(contains("residual_productive" )), funs(ifelse(population<pop_threshold_productive_loads, 0, .)))
  
  clusters <- clusters %>% mutate_at(vars(contains("kwh_cp" )), funs(ifelse(population<pop_threshold_productive_loads, 0, .)))
  
  clusters <- clusters %>% mutate_at(vars(contains("mining_kwh" )), funs(ifelse(population<pop_threshold_productive_loads, 0, .)))
  
}

aa <- clusters
aa$geometry=NULL
aa$geom=NULL

PerHHD_tt <- melt(as.vector(aa %>% dplyr::select(starts_with("PerHHD_tt") & !contains("monthly")) %>% summarise_all(.funs = "sum", na.rm=T)))
residual_productive_tt <- melt(as.vector(aa %>% dplyr::select(starts_with("residual_productive_tt") & !contains("monthly")) %>% summarise_all(.funs = "sum", na.rm=T)))
er_hc_tt <- melt(as.vector(aa %>% dplyr::select(starts_with("er_hc_tt") & !contains("monthly")) %>% summarise_all(.funs = "sum", na.rm=T)))
er_sch_tt <- melt(as.vector(aa %>% dplyr::select(starts_with("er_sch_tt") & !contains("monthly")) %>% summarise_all(.funs = "sum", na.rm=T)))
er_kwh_tt <- melt(as.vector(aa %>% dplyr::select(starts_with("er_kwh_tt") & !contains("monthly")) %>% summarise_all(.funs = "sum", na.rm=T)))
kwh_cp_tt <- melt(as.vector(aa %>% dplyr::select(starts_with("kwh_cp_tt") & !contains("monthly")) %>% summarise_all(.funs = "sum", na.rm=T)))
mining_kwh_tt <- melt(as.vector(aa %>% dplyr::select(starts_with("mining_kwh_tt") & !contains("monthly")) %>% summarise_all(.funs = "sum", na.rm=T)))

#

all_sectors <- bind_rows(PerHHD_tt, residual_productive_tt, er_hc_tt, er_sch_tt, er_kwh_tt, kwh_cp_tt, mining_kwh_tt)

all_sectors$variable <- as.character(all_sectors$variable)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

all_sectors$year <- as.numeric(substrRight(all_sectors$variable, 4))
all_sectors$variable <- sub('_[^_]*$', "", all_sectors$variable )

ggplot(all_sectors)+
  geom_line(aes(x=year, y=value/1e9, colour=variable, group=variable))+
  xlab("Year")+
  ylab("National electricity demand (TWh)")+
  scale_color_discrete(name="Sector")
