# ------------------------------------------------------------------------------
## Define interpolating function over the aproximated surface
# ------------------------------------------------------------------------------
# Find nearest neighbor interpolating function.
nninterp <- function(q, h, type, yearly_IRREQ) {
  
  df = data.frame(x, y, z)
  
  # As numeric.
  x = as.vector(x$`spline[["xyz.est"]][["x"]]`)
  y = as.vector(y$`spline[["xyz.est"]][["y"]]`)
  z = as.matrix(z)
  
  # Apply NN.
  near_q = which.min(abs(x-q))
  near_h = which.min(abs(y-h))
  
  # Find pump cost.
  # Convert from GBP to USD.
  
  if (instalments_business_model==F){
    
    cost_inv_pump = as.numeric(z[near_h,near_q]) * 1.38
    
  } else{
    
    cost_inv_pump =  npv(discount_rate, rep(((as.numeric(z[near_h,near_q]) * 1.38) / lifetimepump)), 0:(lifetimepump-1)) 
    
  }
  
  if (water_tank_storage==T){
    
    st_tank_cost <-  yearly_IRREQ * tank_usd_lit #- proportional to water requirements
    
  } else{st_tank_cost <- 0}
  
  # Fixed costs
  # Siting costs: mean between the only values we have
  cost_inv_sit = mean(c(563, 1000))
  
  # Mobilization & Demobilization costs: raw range, use mean
  cost_inv_mob = mean(c(500, 720, 1000, 1600))
  
  ## Variable costs: drilling and casing costs
  # from regression in "groundwater_pumps_costs_FS.r".
  cost_var_dril_cas = 122.1362 * h - 1661.8328
  
  # Variable costs: failure costs.
  # Mean value from Xenarios (see "failure_cost_FS.r"), can refine selecting costs
  # by location (country), but needs country or coordinates as input.
  
  cost_fail_tot = npv(discount_rate, rep(((31.55 * h) / lifetimepump)), 0:(lifetimepump-1))
  
  # Total cost.
  
  if (type == "Ground water pumping") {
    
    cost_tot = cost_inv_sit + cost_inv_pump + cost_inv_mob + cost_var_dril_cas + cost_fail_tot + st_tank_cost
    
  } 
  
  
  if (type == "Surface water pumping") {
    
    if (instalments_business_model){
      
      cost_fail_tot = npv(discount_rate, rep(((31.55 * 15) / lifetimepump)), 0:(lifetimepump-1))
      
      cost_inv_pump = as.numeric(z[which.min(y),near_q]) * 1.38
      cost_tot = cost_inv_pump + cost_inv_mob + cost_fail_tot + st_tank_cost
      
    } else{
      
      cost_fail_tot = npv(discount_rate, rep(((31.55 * 15) / lifetimepump)), 0:(lifetimepump-1))
      
      cost_inv_pump =  npv(discount_rate, rep(((as.numeric(z[which.min(y),near_q]) * 1.38) / lifetimepump)), 0:(lifetimepump-1)) 
      cost_tot = cost_inv_pump + cost_inv_mob + cost_fail_tot + st_tank_cost
      
    }}
  
  return(cost_tot)
  
}

cl <- clusters %>% dplyr::select(starts_with("q")) %>% st_set_geometry(NULL)
cl <- apply(cl, 1, max)

clusters$totalpumpcost <- NA
where_index <- (is.finite(cl/clusters$npumps) & clusters$er_kwh_tt>0)

clusters$totalpumpcost[where_index] <- pbapply::pbmapply(nninterp, cl[where_index]/clusters$npumps[where_index], clusters$gr_wat_depth[where_index], clusters$which_pumping[where_index], scales::rescale(clusters$yearly_IRREQ, to=range_tank)[where_index]) * clusters$npumps[where_index]

clusters$totalpumpcost <- ifelse(is.infinite(clusters$totalpumpcost), NA, clusters$totalpumpcost)

save.image(paste0("results/", countrystudy, "/pumps_installation_costs.Rdata"))
