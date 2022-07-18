rho <- 1000
g <- 9.81
h <- 50
eta_pump <- 0.75
eta_motor <- 0.75
q <- 10

rho* g * h * q /(3.6*10^6)/eta_pump/eta_motor

##########################

pipe_diameter <- 0.8
d <- 581.318  

v = (q / (3600 * pi * (pipe_diameter/2)^2)) # in m/s

delta_p_psi <- ((0.1 * v^2 * d * 1000 *  1) / (2 * pipe_diameter)) * 1e6 # pascal

(delta_p_psi * q) /(3.6*10^6)/eta_pump/eta_motor

#######


