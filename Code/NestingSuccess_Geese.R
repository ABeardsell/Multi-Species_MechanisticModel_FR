library(deSolve)

# Load parameters and dd fcts
source("Code/Parameters_values.R")

# Load MSFR equations
source("Code/MSFR_NR_equations.R")

#------------------------------------------------------------------------------------------------------
# ---- Compute Nesting success for different values of prey density  -----------
#------------------------------------------------------------------------------------------------------
N2_range = c(255) #average density of goose nests in the colony is 255
#N1_range = c(42,281,14,384,504,9,2,648,365,253,19,1.6,137) #empirical time series from 2007 à 2019
N1_range = c(2) #empirical time series from 2007 à 2019
#hr_range = seq(3.75,48.5,by=4.5) #empirical range observed
hr_range=10.8 # average in the goose colony

d_grid = expand.grid(N1_vec = N1_range, N3_vec = 7.5, N2_vec= N2_range,hr_vec=hr_range)
Out <- list()
  for (k in seq_along(d_grid$N2_vec)){
              state_2d <- c(N2=d_grid$N2_vec[k],AR2=MSFR_goose(d_grid$N1_vec[k],d_grid$N2_vec[k],d_grid$N3_vec[k]) * NR_sim(d_grid$hr_vec[k]))
              output_2d <- data.frame(ode(y=state_2d,times=inc_lay_goose_days,func=dec_goosenest_density,parms=c(N1=d_grid$N1_vec[k],
              N3=d_grid$N3_vec[k],hr=d_grid$hr_vec[k], plot_size=50,n_max_2= d_grid$N2_vec[k] * 50)))
              N2_density <- output_2d$N2
              pred_out <- output_2d[28,]$AR2/output_2d[28,]$n_max_2 # predation rate/fox after 24 days
              Out[[k]] <- c(pred_out,d_grid$N1_vec[k],d_grid$N2_vec[k],d_grid$N3_vec[k],d_grid$hr_vec[k])
          }
df_ing <- do.call(rbind,Out)
df_ing <- as.data.frame(df_ing)
colnames(df_ing) <- c("PR","N1","N2","N3","HR")
dff = df_ing
dff
################################################################################
# Mean predation rate and nesting success - Need to aggregate lemming density
group_mean <- aggregate(dff$PR, list(dff$N2), mean)
ave_PR = as.data.frame(group_mean)
colnames(ave_PR) = c("N2","PR")
# Nesting success - 1-PR
ave_PR$NS= 1- ave_PR$PR
ave_PR

dff_lowk = ave_PR
dff_highk = ave_PR
dff_veryhighk = ave_PR
dff_veryhighk
dff_highk
dff_lowk
##################################################################################
# Plot Goose nesting success in fct to average goose nest density
plot(dff_lowk$N2,dff_lowk$NS,ylim=c(0.5,0.9),bty="n",lwd=4,type="l",col="black",ylab="Nesting success",xlab="Goose nest density")
lines(dff_highk$N2,dff_highk$NS,lwd=4,type="l",col="dodgerblue4")
lines(dff_veryhighk$N2,dff_veryhighk$NS,lwd=4,type="l",col="firebrick")
abline(v=255)
dev.copy2pdf(file="NS_N2_k0.05k0.20k0.5.pdf")
dev.off()
