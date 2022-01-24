################################################################################
################################### FUNCTION ###################################
################################################################################

## GIVEN: 
##                q = inflow
##                h = hydraulic depth
##
## THE FUNCTION RETURNS THE COST OF THE PUMP, BASED ON AN APROXIMATED COST SURFACE,
## AND USING NEAREST NEIGHBOR INTERPOLATION.

# Libraries
library(readxl)

# Read xlsx of spline surface
x = read_xlsx("interp_surface_cost/spline_q.xlsx", col_names = T)
y = read_xlsx("interp_surface_cost/spline_h.xlsx", col_names = T)
z = read_xlsx("interp_surface_cost/spline_c.xlsx", col_names = T)

df = data.frame(x, y, z)

# As numeric
x = as.vector(x$`spline[["xyz.est"]][["x"]]`)
y = as.vector(y$`spline[["xyz.est"]][["y"]]`)
z = as.matrix(z)

# Find nearest neighbor
nninterp <- function(q, h) {
  
  if (is.na(q) | is.na(h)){
    
    return(NA)
    
  }
  
  else{
    
    # Apply NN
    near_q = which.min(abs(x-q))
    near_h = which.min(abs(y-h))
    
    # Find cost
    cost = as.numeric(z[near_h, near_q])
    
    if (q > max(x) | q < min(x)) {
      #print("WARNING: EXTRAPOLATION ON q")
    } else {}
    
    if (h > max(y) | h < min(x)) {
      #print("WARNING: EXTRAPOLATION ON h")
    } else {}
    
    if (q < 0 | h < 0) {
      
      # Currently not working: there is a built-in error message for negative values.
      #print("ERROR: negative values for q and h are not accepted")
      break
      
    } else {
      
      #print("Pump cost [GBP]:")
      return(cost)
      
    }
  }}

##

cl <- clusters %>% dplyr::select(starts_with("q_sust")) %>% st_set_geometry(NULL)
cl <- apply(cl, 1, max)
cl <- as.numeric(cl) * 3600

clusters$totalpumpcost <- mapply(nninterp, cl/clusters$npumps, clusters$gr_wat_depth) * clusters$npumps
clusters$totalpumpcost <- clusters$totalpumpcost * 1.38 #convert GBP to USD
clusters$totalpumpcost <- ifelse(is.na(clusters$totalpumpcost), 0, clusters$totalpumpcost)
clusters$totalpumpcost <- ifelse(clusters$totalpumpcost<0, 0, clusters$totalpumpcost)
