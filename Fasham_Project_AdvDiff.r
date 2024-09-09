################ Project 2 - Fasham
library(deSolve)
library(fields)

# Create an empty list to add the parameters
param <- list()

# Define parameters
param$Dv <- 10.8*2 # Diffusivity (m^2/d)
param$PAR0 <- 0.41 # unit less, PAR/ total irradiance
param$I0 <- 200 # W/m2, annual average surface irradiance 
param$C <- 4 # Oktas, cloudinessl
param$kw <- 0.04 # 1/m, light attenuation due to water
param$gP <- 2 # 1/d, phytoplankton maximum growth rate
param$alpha <- 0.025 # 1/(W/m2) / d, initial slope of the P-I curve
param$HP <- 0.5 # mMol/m3, half saturation for phytoplankton nutrient uptake
param$mP <- 0.1 # 1/d,phytoplankton specific mortality rate
param$kc <- 0.03 # m2/(mMol N), light attenuation by phytoplankton
param$gamma <- 0.03 # phytoplankton, exudation fraction
param$Sigma <- 1.5 # 1/(mMol N), NH4 inhibition parameter for phytoplankton
param$gZ <- 0.5 # 1/d, zooplankton maximum growth rate
param$beta <- 0.75 # zooplankton assimilation efficiency
param$muZ <- 0.075 # 1/d, zooplankton specific excretion rate
param$mZ <- 0.04 # 1/d, zooplankton specific mortality rate
param$HZ <- 1 # mMol/m3, zooplankton half saturation for ingestion
param$Omega <- 0.33 # detrital fraction of zooplankton mortality
param$eps <- 0.75 # ammonium fraction of zooplankton excretion
param$gB <- 2 # 1/d, bacterial maximum growth rate
param$muB <- 0.0375 # 1/d, bacterial specific excretion rate
param$HB <- 0.1 # (mMol N) / m3, bacterial half saturation for uptake
param$eta <- 0.6 # NH4/DON uptake ratio (bacteria?)
param$mD <- 0.05 # 1/d, detrital breakdown rate
param$uD <- 100/2 # m/d, detrital sinking rate 
param$uP <- 2/2 # m/d, phytoplankton sinking rate 
param$dz <- c(rep(10,20),rep(50,16)) # grid spacing (m)
param$z <- c(seq(5,200, by=10),seq(225,1000,by=50)) # depth (m)
param$n <- length(param$z) # number of grid cells
param$theta <- 66.3 # degrees north, latitude
param$pP <- 0.7 # preference for phytoplankton
param$pB <- 0.1 # preference for bacteria
param$pD <- 0.2 # preference for detritus
param$Nb <- 15 # nutrient content at the bottom (mmol/m^3)
param$dzb <- 50

# Define initial conditions - concentrations of N_P_DON_A_B_Z_D
InitN <- c(0.1631374,  0.1832916 , 0.2238936 , 0.2854092,  0.3682608 , 0.4725690 , 0.5979124,  0.7431647,  0.9064525,  1.0852453,
           1.2765633,  1.4772550,  1.6842886,  1.8949980,  2.1072446,  2.3194771,2.5306998,  2.7403759,  2.9482999,  3.1544724,
           3.7680565,  4.7608986,  5.7198282,  6.6394219,  7.5159375,  8.3470951 , 9.1319886 , 9.8710197, 10.5657890, 11.2189556,
           11.8340791, 12.4154571, 12.9679701, 13.4969400, 14.0080075, 14.5070306)
InitA <- c(0.1308610,  0.1434906,  0.1689071,  0.2073515,  0.2590228,  0.3239333,  0.4017857,  0.4919028,
           0.5932246,  0.7043719,  0.8237562,  0.9497095,  1.0806085,  1.2149730,  1.3515289,  1.4892381,
           1.6272992,  1.7651302,  1.9023409,  2.0387003,  2.4449173,  3.0977752,  3.7380258,  4.3728636,
           5.0077142,  5.6477295,  6.2979107,  6.9630548,  7.6477419 , 8.3563199 , 9.0927813,  9.8605113,
           10.6628731, 11.5058617, 12.3826930, 13.1369661)

INIT <- c(InitN, rep(0.05,param$n), rep(0.001,param$n), InitA, rep(0.5,param$n), rep(0.0001,param$n), rep(0.01,param$n))

# Light function
CalLight <- function(t, P, param) {
  #  season <-(1-0.8 * sin(pi*param$theta/180) * cos(2*pi*t/365))
  season <- 1+cos(pi+2*pi/365*t+4*pi/73)
  damp <- param$kc * param$dz * (cumsum(P) - P/2)
  
  I <- param$I0 * param$PAR0 * exp(-param$kw * param$z - damp) * season
  
  return(I)
}


# Create function that creates the differential equation at each step
FASHAM <- function(t, INIT, param) {
  N <- INIT[1:param$n]
  P <- INIT[(1+param$n):(2*param$n)]
  DON <- INIT[(2*param$n+1):(3*param$n)]
  A <- INIT[(3*param$n+1):(4*param$n)]
  B <- INIT[(4*param$n+1):(5*param$n)]
  Z <- INIT[(5*param$n+1):(6*param$n)]
  D <- INIT[(6*param$n+1):(7*param$n)]
  
  # Advection and diffusion
  #####
  # Nitrate flux
  JdN <- rep(0,param$n+1)
  
  for (i in 2:param$n){
    JdN[i] <- -param$Dv * (N[i] - N[i-1]) / ((param$dz[i]+param$dz[i-1])/2)
  }
  
  
  # Diffusive flux boundary
  JdN[1] = 0
  JdN[param$n+1] = -param$Dv*(param$Nb-N[param$n])/param$dzb
  
  JN = JdN
  
  # Phytoplankton flux
  JaP <- rep(0,param$n+1)
  JdP <- rep(0,param$n+1)
  
  for (i in 2:param$n){
    JaP[i] <- param$uP * P[i-1]
    JdP[i] <- -param$Dv * (P[i] - P[i-1]) / (param$dz[i]+param$dz[i-1])/2
  }
  
  # Advective flux boundary
  JaP[1] = 0
  JaP[param$n+1] = 0
  
  # Diffusive flux boundary
  JdP[1] = 0
  JdP[param$n+1] = 0
  
  JP = JaP + JdP
  
  
  # DON flux
  JdDON <- rep(0,param$n+1)
  
  for (i in 2:param$n){
    JdDON[i] <- -param$Dv * (DON[i] - DON[i-1]) / ((param$dz[i]+param$dz[i-1])/2)
  }
  
  
  # Diffusive flux boundary
  JdDON[1] = 0
  JdDON[param$n+1] = 0
  
  JDON = JdDON
  
  
  # Ammonium flux
  JdA <- rep(0,param$n+1)
  
  for (i in 2:param$n){
    JdA[i] <- -param$Dv * (A[i] - A[i-1]) / ((param$dz[i]+param$dz[i-1])/2)
  }
  
  
  # Diffusive flux boundary
  JdA[1] = 0
  JdA[param$n+1] = 0
  
  JA = JdA
  
  # Bacteria flux
  JdB <- rep(0,param$n+1)
  
  for (i in 2:param$n){
    JdB[i] <- -param$Dv * (B[i] - B[i-1]) / ((param$dz[i]+param$dz[i-1])/2)
  }
  
  
  # Diffusive flux boundary
  JdB[1] = 0
  JdB[param$n+1] = 0
  
  JB = JdB
  
  # Zooplankton flux
  JdZ <- rep(0,param$n+1)
  
  for (i in 2:param$n){
    JdZ[i] <- -param$Dv * (Z[i] - Z[i-1]) / ((param$dz[i]+param$dz[i-1])/2)
  }
  
  
  # Diffusive flux boundary
  JdZ[1] = 0
  JdZ[param$n+1] = 0
  
  JZ = JdZ
  
  
  # Detritus flux
  JaD <- rep(0,param$n+1)
  JdD <- rep(0,param$n+1)
  
  for (i in 2:param$n){
    JaD[i] <- param$uD * D[i-1]
    JdD[i] <- -param$Dv * (D[i] - D[i-1]) / ((param$dz[i]+param$dz[i-1])/2)
  }
  
  # Advective flux boundary
  JaD[1] = 0
  JaD[param$n+1] = 0
  
  # Diffusive flux boundary
  JdD[1] = 0
  JdD[param$n+1] = 0
  
  JD = JaD + JdD
  
  
  #####
  
  
  # Call the light function
  I <- CalLight(t, P, param)
  
  # Function describing P-I curve (Light limited growth rate)
  J <- param$gP * param$alpha * I / (param$gP^2 + param$alpha^2 * I^2)^(0.5)
  
  # Nutrient limiting growth rate
  Q <- N * exp(-param$Sigma*A) / (param$HP + N) + A / (param$HP + A)
  
  # Nutrient limiting growth rate (nitrate)
  Q1 <- N * exp(-param$Sigma*A) / (param$HP + N)
  
  # Nutrient limiting growth rate (Ammonium)
  Q2 <- A / (param$HP + A)
  
  # Light and nutrient limitation
  sig <- J * Q
  
  # Total food available 
  food <- param$pP*P + param$pB*B + param$pD*D 
  
  # Zooplankton grazing function on phytoplankton
  GrazP <- param$gZ * Z * (param$pP * P) / (param$HZ + food)
  
  # Zooplankton grazing function on bacteria
  GrazB <- param$gZ * Z * (param$pB * B) / (param$HZ + food)
  
  # Zooplankton grazing function on detritus
  GrazD <- param$gZ * Z * (param$pD * D) / (param$HZ + food)
  
  # Differential equation for phytoplankton 
  dPdt <- (1-param$gamma) * sig * P - GrazP - param$mP*P -(JP[2:(param$n+1)] - JP[1:param$n]) / param$dz 
  
  # Differential equation for zooplankton 
  dZdt <- param$beta*GrazP + param$beta*GrazB + param$beta*GrazD - param$muZ*Z - param$mZ*Z-(JZ[2:(param$n+1)] - JZ[1:param$n]) / param$dz 
  
  # Total bacterial nitrogenous substrate S
  S <- min(A,param$eta*DON)
  
  # Bacterial DON uptake
  U1 <- param$gB*B*DON / (param$HB + S + DON)
  
  # Bacterial ammonia uptake
  U2 <- param$gB*B*S / (param$HB + S + DON)
  
  # Differential equation for bacteria
  dBdt <- U1 + U2 - GrazB - param$muB * B -(JB[2:(param$n+1)] - JB[1:param$n]) / param$dz 
  
  # Differential equation for detritus
  dDdt <- (1-param$beta)*GrazP + (1-param$beta)*GrazB - param$beta*GrazD - param$mD*D + param$mP*P -(JD[2:(param$n+1)] - JD[1:param$n]) / param$dz 
  
  # Differential equation for nitrate
  dNdt <- -(J*Q1) * P -(JN[2:(param$n+1)] - JN[1:param$n]) / param$dz 
  
  # Differential equation for ammonium
  dAdt <- -(J*Q2) * P - U2 + param$muB*B + (param$eps*param$muZ+(1-param$Omega)*param$mZ) * Z -(JA[2:(param$n+1)] - JA[1:param$n]) / param$dz 
  
  # Differential equation for dissolved organic nitrogen (DON)
  dDONdt <- param$gamma*sig*P + param$mD*D + (1-param$eps)*param$muZ*Z - U1 -(JDON[2:(param$n+1)] - JDON[1:param$n]) / param$dz 
  
  return(list(c(dNdt, dPdt, dDONdt, dAdt, dBdt, dZdt, dDdt)))
}

# Define time step
time <- seq(1,365*30, by=1)

# Solve our differential equations
# How much time does the run take?
start.time <- Sys.time()
res <- ode(INIT, time, FASHAM, param)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(paste("Runtime=",as.character(round(time.taken,1)),"s"))
#,method = "ode45", atol = 1e-14, rtol = 1e-14

N <- res[,2:(param$n+1)]
P <- res[,(2+param$n):(2*param$n+1)]
DON <- res[,(2*param$n+2):(3*param$n+1)]
A <- res[,(3*param$n+2):(4*param$n+1)]
B <- res[,(4*param$n+2):(5*param$n+1)]
Z <- res[,(5*param$n+2):(6*param$n+1)]
D <- res[,(6*param$n+2):(7*param$n+1)]

# Surface plot of phytoplankton
par(mfrow=c(1,1))

image.plot(time/365, param$z, log10(Z), ylim = rev(range(param$z)),
           xlab = substitute(paste(bold("Year"))), ylab = substitute(paste(bold("Depth [m]"))),
           main = "XXX [mmol N/m^3]")
box()




xlim=c(0,2)
plot(D[365*9+180,], param$z, ylim = rev(range(param$z)),xlim=c(0,0.0001), pch = 19, type='l', lwd=2, col = "orange", main = "Detritus (year 10)", ylab = "Depth", xlab = "Concentration (mMol N / m3]")
points(D[365*9+270,], param$z, ylim = rev(range(param$z)), pch = 19, type='l', lwd=2, col = "brown")
points(D[365*9,], param$z, ylim = rev(range(param$z)), pch = 19, type='l', lwd=2, col = "blue")
points(D[365*9+90,], param$z, ylim = rev(range(param$z)), pch = 19, type='l', lwd=2, col = "green")
legend(0.06,600, legend = c("Summer","Fall","Winter","Spring"), col = c("orange","brown","blue","green"), lty = 1, cex = 0.9, lwd=3, box.col = "white")

