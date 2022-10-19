#--------------------------------------------------------------------
# MSFR parameters
#--------------------------------------------------------------------
# general parameters
s=93 # daily distance the predator traveled  - km/day
phi=0.5 #average proportion of time the predator spent active

# Lemmings (prey 1)
d1=0.0075 #maximum reaction distance - km
f21_f31=0.15 # Average detection and attack probabilities of a lemming
f41=0.51 # Average success probability of an attack
Tp1 = 0.024/24 #Pursue time - day/nest
Te1= 0.00038 #consumption time - day/nest
e1= 0.48 # consumption probability
To1= 0.000486 # hoarding time - day/nest
o1= 0.32 # hoarding probability
Tde1= 0.00388# delivery time- day/nest
de1= 0.20 #delivery probability

# Geese (prey 2)
inc_lay_goose_days <- c(1:28) #Average duration between the laying date and hatching date for geese (28 days)
w= 1-0.021 #0.021 # nest attendance prob.
d2_a=0.033 #Maximum reaction distance - km
d2_ua=0.11 #Maximum reaction distance - km
f22_ua=0.366 # detection probability
f42_a=0.098 # success probability
f42_ua=0.934 # success probability
f32_a= 1#0.05#0.11 # attack probability (nest attended)
p_c_ua=0.69 #complete predation probability
p_c_a=0.47 #complete predation probability
Tp2=0.02/24 #Pursue time - day/nest
Tm2 = 0.14/24 #Manipulation time - day/nest

# Sandpiper (prey 3)
inc_lay_sandpiper_days <- c(1:24) #Average duration between the laying date and hatching date for sandpipers (24 days)
d3=0.085 #Maximum reaction distance - km
f23=0.029 #detection probability
Tm3=0.069/24 #consumption time of a nest -day/nest

# Density-dependent functions of s and phi to lemming density
s_L_concav <- function(x) {
64 + (93-64)* (exp(-x/150))
}
phi_L_concav <- function(x) {
0.40 + (0.53-0.40)* (exp(-x/180))
}
