# ------------------------------------------------------------------------------
## Define interpolating function over the aproximated surface
# ------------------------------------------------------------------------------
# Find nearest neighbor interpolating function.
nninterp <- function(q, h) {
  
  # Read xlsx of spline surface.
  x = read_xlsx("interp_surface_cost/spline_q.xlsx", col_names = T)
  y = read_xlsx("interp_surface_cost/spline_h.xlsx", col_names = T)
  z = read_xlsx("interp_surface_cost/spline_c.xlsx", col_names = T)
  
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
  cost_inv_pump = as.numeric(z[near_h,near_q]) * 1.38
  
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
  cost_fail_tot = 31.55 * h
  
  # Check conditions and compute total cost.
  if (q > max(x) | q < min(x)) {
    #print("WARNING: EXTRAPOLATION ON q")
  } else {}
  
  if (h > max(y) | h < min(x)) {
    #print("WARNING: EXTRAPOLATION ON h")
  } else {}
  
  if (q < 0 | h < 0) {
    
    # This line doesn't work currently: there is a built-in error message for negative values.
    print("ERROR: negative values for q and h are not accepted")
    break
    
  } else {
    
    # Total cost.
    cost_tot = cost_inv_sit + cost_inv_pump + cost_inv_mob + cost_var_dril_cas + cost_fail_tot
    
    # Return total cost.
    #print("Pump cost [USD]:")
    return(cost_tot)
    
  }
}

##

cl <- clusters %>% dplyr::select(starts_with("q_sust")) %>% st_set_geometry(NULL)
cl <- apply(cl, 1, max)
cl <- as.numeric(cl) * 3600

clusters$totalpumpcost <- mapply(nninterp, cl/clusters$npumps, clusters$gr_wat_depth) * clusters$npumps
clusters$totalpumpcost <- ifelse(is.na(clusters$totalpumpcost), 0, clusters$totalpumpcost)
clusters$totalpumpcost <- ifelse(clusters$totalpumpcost<0, 0, clusters$totalpumpcost)

write_rds(clusters, "clusters_with_data_3.Rds")
