if (no_productive_demand_in_small_clusters == T){
  
  clusters <- clusters %>% mutate_at(vars(contains("residual_productive" )), funs(ifelse(population<pop_threshold_productive_loads, 0, .)))
  
  clusters <- clusters %>% mutate_at(vars(contains("kwh_cp_tt" )), funs(ifelse(population<pop_threshold_productive_loads, 0, .)))
  
  clusters <- clusters %>% mutate_at(vars(contains("mining_kwh_tt" )), funs(ifelse(population<pop_threshold_productive_loads, 0, .)))
  
}

# filter_columns <- read.csv("filter_columns.csv")
# 
# clusters <- dplyr::select(clusters, filter_columns$columns)

