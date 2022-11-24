# prey 1 - Lemming
# prey 2 - Goose nest
# prey 3 - Sandpiper nest
# -------------------------------------------------------
# Compute acquisition rate of sandpiper
# -------------------------------------------------------
MSFR_sandpiper <- function(N1,N2,N3) {
  phi = phi_L_concav(N1) #Phi value is a function of lemming density
  s = s_L_concav(N1)#s value is a function of lemming density

  alpha_1=s*(2*d1)*f21_f31*f41 #Capture efficiency of a lemming (km2/day)
  alpha_3=s*(2*d3)*f23 #Capture efficiency of a sandpiper nest (km2/day)
  alpha_2a_complete = s*f32_a*(2*d2_a)*f42_a*p_c_a #Capture rate of a nest by a predator - COMPLETE
  alpha_2a_partial = (s*f32_a*(2*d2_a)*f42_a*(1-p_c_a))/3.7 #Capture rate of a nest by a predator - PARTIAL
  alpha_2a = alpha_2a_complete + alpha_2a_partial # Predation totale
  alpha_2ua_complete = s*f22_ua*(2*d2_ua)*p_c_ua*f42_ua #Capture rate of a nest by a predator - COMPLETE
  alpha_2ua_partial = (s*f22_ua*(2*d2_ua)*f42_ua*(1-p_c_ua))/3.7 #Capture rate of a nest by a predator - PARTIAL
  alpha_2ua = alpha_2ua_complete + alpha_2ua_partial # Predation totale

  h_1=  (Tp1/f41) + ((Te1 * e1) + (To1 * o1) + (Tde1*de1)) #Handling time of a lemming
  h_2ua= Tp2/(f42_ua * p_c_ua) + Tm2 #Handling time of a nest unattended
  h_2a= Tp2/(f42_a * p_c_a) + Tm2 #Handling time of a nest attended
  h_3= Tm3 #consumption time of a sandpiper nest

  AR3 <- (alpha_3 * N3 * phi)/(1+(alpha_1 * h_1 * N1) + (alpha_3 * h_3* N3)
      + (alpha_2ua * h_2ua* ((1-w)*N2)) + (alpha_2a * h_2a* (w*N2)))
  return(AR3)
}

# -------------------------------------------------------
# Compute acquisition rate of goose nests
# -------------------------------------------------------

MSFR_goose <- function(N1,N2,N3) {
  phi = phi_L_concav(N1) #Phi value is a function of lemming density
  s = s_L_concav(N1) #s value is a function of lemming density

  alpha_1=s*(2*d1)*f21_f31*f41 #Capture efficiency of a lemming (km2/day)
  alpha_3=s*(2*d3)*f23 #Capture efficiency of a passerine nest (km2/day)
  alpha_2a_complete = s*f32_a*(2*d2_a)*f42_a*p_c_a #Capture rate of a nest by a predator - COMPLETE
  alpha_2a_partial = (s*f32_a*(2*d2_a)*f42_a*(1-p_c_a))/3.7 #Capture rate of a nest by a predator - PARTIAL
  alpha_2a = alpha_2a_complete + alpha_2a_partial # Predation totale
  alpha_2ua_complete = s*f22_ua*(2*d2_ua)*p_c_ua*f42_ua #Capture rate of a nest by a predator - COMPLETE
  alpha_2ua_partial = (s*f22_ua*(2*d2_ua)*f42_ua*(1-p_c_ua))/3.7 #Capture rate of a nest by a predator - PARTIAL
  alpha_2ua = alpha_2ua_complete + alpha_2ua_partial # Predation totale

  h_1=  (Tp1/f41) + ((Te1 * e1) + (To1 * o1) + (Tde1*de1)) #Handling time of a lemming
  h_2ua= Tp2/(f42_ua * p_c_ua) + Tm2 #Handling time of a nest unattended
  h_2a= Tp2/(f42_a * p_c_a) + Tm2 #Handling time of a nest attended
  h_3= Tm3 #consumption time of a sandpiper nest

  AR2ga <- (alpha_2a * (w*N2) * phi)/(1+(alpha_1 * h_1 * N1) + (alpha_3 * h_3* N3)
        + (alpha_2ua * h_2ua* ((1-w)*N2)) + (alpha_2a * h_2a* (w*N2)))
  AR2gua <- (alpha_2ua * ((1-w)*N2) * phi)/(1+(alpha_1 * h_1 * N1) + (alpha_3 * h_3* N3)
        + (alpha_2ua * h_2ua* ((1-w)*N2)) + (alpha_2a * h_2a* (w*N2)))
  AR2=AR2ga+AR2gua
  return(AR2)
  #list("ARl"=ARl,"ARp"=ARp,"ARs"=ARs,"ARg"=ARg,"ARga"=ARga,"ARgua"=ARgua,"L"=x,"P"=y,"S"=z,"G"=a)
}

# -------------------------------------------------------
# numerical response equations - return the number of foxes within 50 km2
# -------------------------------------------------------
NR_sim<- function (hr) {
  area = 50 #spatial scale - outside or inside the goose colony
  overlap = 0.18 # 2019: 7 HR in 52 km2 donc 7.4 km2 sans chevauchement - HR average 9.1 km2 - overlap=1-0.82
  y = (area/(hr*(1-overlap))) * 2
  return(y)
}
NR_sim(10.8)
# ---------------------------------------------------------------------
# fct to compute the decreasing of goose nest density in 28 days
# ---------------------------------------------------------------------
dec_goosenest_density <- function(t,state,parameters)
 {with(as.list(c(t,state,parameters)),{  # unpack the parameters
          dAR2 <- MSFR_goose(N1,N2,N3) * NR_sim(hr)
          dN2 <- - N2 + ((n_max_2 - AR2)/plot_size)
    list(c(dN2,dAR2),
        c(N1=N1,N3=N3,n_max_2=n_max_2))
    })}
