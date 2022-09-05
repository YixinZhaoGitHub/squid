
## Script name: Base patameters
##
## Authors: Daniel Ottmann 
## Email: daniel.ottmann.riera@gmail.com
##
## Date created: March 2022
## Last update:  June 2022
##
## ---------------------------
##
## Readme:
##
## This function sets basic parameters to run FEISTY model
##
## ---------------------------


#####################################################################
# rm(param) # clear parameters

baseparameters <- function(nstage) {
  
  # Define functions:
  logspace <- function(d1, d2, n) {
    exp(log(10) * seq(d1, d2, length.out = n))}
  
  erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
  
  # Create parameter list
  param <- list() 
  
  # Number of iterations: 
  param$tEnd <-  200 # years
  
  #
  # Create functional groups and stages: ------------------------------------
  #
  
  param$RId <- c('SZoo','BZoo','Bent')
  param$SpId <- c('Spel','Meso','Lpel', 'Dem', 'Squid', 'Bpel')
  param$RSpId <- c(param$RId, param$SpId)
  param$RSpName <- c("Small zooplankton", "Big zooplankton", "Benthos", "Small pelagics",
                     "Mesopelagics", "Large pelagics", "Demersals", "Squid", "Bathypelagics")  
  
  param$my_names <- c("Res" = "Resources",
                "SZoo" = "Small zooplankton",
                "BZoo" = "Big zooplankton",
                "Bent" = "Benthos",
                "Spel" = "Small pelagics",
                "Meso" = "Mesopelagics",
                "Lpel" = "Large pelagics",
                "Dem" = "Demersals", 
                "Squid" = "Squid", 
                "Bpel" =  "Bathypelagics")
  
  # Set color palette: 
  param$my_palette <- c("Res" = "#F57C0D",
                  "SZoo" = "#FFEE58",
                  "BZoo" = "#F9A825",
                  "Bent" = "#795548",
                  "Spel" = "#BBDEFB",
                  "Meso" = "#9E9E9E",
                  "Lpel" = "#2196F3",
                  "Dem" = "#000000", 
                  "Squid" = "#AD1457", 
                  "Bpel" =  "#0D47A1")

  # Resources
  param$ixR <- c(1, 2, 3, 4) # 4 resources: Small & large zoo,  small & large benthos 
  param$w[param$ixR] <- c(2e-06, 0.001, 0.5e-03, 0.25) # weight lower limit
  param$wc[param$ixR] <- c(2e-06 * sqrt(500), 0.001 * sqrt(500), 0.5e-03 * sqrt(250000), 0.25 * sqrt(500)) # weight central size
  param$wu[param$ixR] <- c(0.001, 0.5,  125, 125) # weight upper limit (Small benthos cover large benthos) 

  # Fish:
  param$nstage <- nstage 
  param$nsize  <- param$nstage + 1
  param$sfish <- 0.001 # smallest size fish (all fish)
  param$sfish_asymp <- 250 # Asymptotic size of small pelagics
  param$lfish_asymp <- 125000 # largest size fish (only predator)
  param$mat_const <- 0.28 # Andersen 2019 pp45
  param$smat <- param$sfish_asymp * param$mat_const #  weight at maturity forage/meso; other option: 250 * 0.5 
  param$lmat <- param$lfish_asymp * param$mat_const # weight at maturity predators; other option: 250
  
  # cephalopods: 
  param$sceph <- 0.01 # smallest size all cephalopod
  param$lceph <- 3.5 * 10^3 # 3.5e+03; # largest size allcephalopod 5.5902e+03
  param$SmallCeph <- 100 # small cephalopod speceis
  param$sizes <- logspace(log10(param$sfish), log10(param$lfish_asymp), param$nsize) # All the size on log scale 
  param$sizes_Ceph <- logspace(log10(param$sceph), log10(param$lceph), param$nsize)
  param$maxsmall <- which.min(abs(param$sizes - 250))
  
  #
  # Species: ----------------------------------------------------------------
  # 
  
  # Indices and weight classes 
  # Small pelagics
  param$w <- c(param$w, param$sizes[1:param$maxsmall - 1])
  param$ix1 <- length(param$ixR) + 1 
  param$ix2 <- length(param$ixR) + (param$maxsmall - 1)
  # Mesopelagics:
  param$w <- c(param$w, param$sizes[1:param$maxsmall - 1])
  param$ix1 <- c(param$ix1, param$ix2[1] + 1) 
  param$ix2 <- c(param$ix2, param$ix2[1] + (param$maxsmall - 1))
  # Large pelagic:
  param$w <- c(param$w, param$sizes[1:length(param$sizes) - 1])
  param$ix1 <- c(param$ix1, param$ix2[2] + 1)
  param$ix2 <- c(param$ix2, param$ix2[2] + (param$nsize - 1))
  # Large demersal:
  param$w <- c(param$w, param$sizes[1:length(param$sizes) - 1])
  param$ix1 <- c(param$ix1, param$ix2[3] + 1)
  param$ix2 <- c(param$ix2, param$ix2[3] + (param$nsize - 1))
  # Cephalopods:
  param$w <- c(param$w, param$sizes_Ceph[1:length(param$sizes) - 1]) # intermediate size
  param$ix1 <- c(param$ix1, param$ix2[4] + 1)
  param$ix2 <- c(param$ix2, param$ix2[4] + (param$nsize - 1))
  # Bathypelagics:
  param$w <- c(param$w, param$sizes[1:length(param$sizes) - 1])
  param$ix1 <- c(param$ix1, param$ix2[5] + 1)
  param$ix2 <- c(param$ix2, param$ix2[5] + (param$nsize - 1))
  
  # Index for fish:  
  param$nSpecies <- length(param$ix1) # add one group
  param$ixFish <- param$ix1[1]:param$ix2[length(param$ix2)] # 2 groups with 4 stages and 4 groups (incl. cephalopods) with 6 stages
  param$NbSizeGrp <-  param$ixFish[length(param$ixFish)]
  
  # Creating central and upper size limits from param$w (lower limit): with
  # potential difference in size structure. 
  param$stage <- param$w[param$ix2] / param$w[param$ix2 - 1] # log distance between two size bin (for each group).
  for (i in 1:param$nSpecies) {
    idx <- param$ix1[i]:param$ix2[i] 
    param$wc[idx] <- param$w[idx] * sqrt(param$stage[i]) # central sizes
    param$wu[idx] <- param$w[idx] * param$stage[i] # Upper sizes
  }
  
  #
  # predator prey preference ------------------------------------------------
  #
  
  param$beta[1:param$ixFish[length(param$ixFish)]] <- 400 # optimal pred/prey mass ratio fish
  param$beta[param$ix1[5]:param$ix2[5]] <- 50 # optimal pred/prey mass ratio Cephalopod 
  param$sigma <- 1.3
  
  # Size-preference matrix
  thetaP <- matrix(0, length(param$wc), length(param$wc))
  thetaP[-(1:length(param$ixR)), ] <- sqrt(pi/2) * param$sigma * 
    (erf(t(outer(log(param$wu), log(param$wc[param$ixFish] / param$beta[param$ixFish]), '-')) / (sqrt(2) * param$sigma)) - 
    erf(t(outer(log(param$w), log(param$wc[param$ixFish] / param$beta[param$ixFish]), '-')) / (sqrt(2) * param$sigma))) %*%
    diag(1 / log(param$wu/param$w))
  param$prefer <- thetaP
  
  #
  # Physiology: -------------------------------------------------------------
  #
  
  param$epsAssim <- 0.7 # assimilation efficiency Fish
  param$epst <- 0.1 # efficiency of benthos community 
  param$h <- rep(20, param$NbSizeGrp) # Value fish
  param$h[param$ix1[5]:param$ix2[5]] <- 28 / (param$epsAssim * (0.6 - 0.4)) # Value cephalopods
  param$met <- 0.2 * param$h # maintenance costs, 20% of h
  param$q <- 0.8
  param$n <- 0.75
  param$m <- 0.825
  param$gamma <- 70 # factor for the max clearance rate (area per time)  
  param$eRepro <- rep(0.01, param$nSpecies)
  param$A <- param$epsAssim * param$h * (0.6 - 0.4) # here I assume a constant feeding level
  param$mort0 <- (0 * param$ixFish + .1)
  
  #
  # Temperature scaling on physiology: --------------------------------------
  #
  
  Q10 <-  1.88 # Assumption of similar Q10 for all the groups. 
  param$Q10 <- rep(Q10, param$NbSizeGrp)
  param$Q10m <- param$Q10
  
  # Physio: 
  param$z <- (param$w / param$wu)
  param$Cmax <- (param$h * param$wc^param$n) / param$wc
  param$V <- (param$gamma * param$wc^param$q) / param$wc
  param$Mc <- (param$met * param$wc^param$m) / param$wc
  
  #
  # Fishing mortality: ------------------------------------------------------
  # Traul Fishing: (small and large species)
  
  param$mortFi <- c(0.3, 0, 0.3, 0.3, 0.3, 0) # default fishing intensity.
  mortF <- c()
  for (i in 1:param$nSpecies) {
    mortF <- c(mortF, param$mortFi[i] * (1 + (param$wc[param$ix1[i]:param$ix2[i]] / (param$wu[param$ix2[i]] * 0.05))^(-3))^(-1))}
  param$mortF <- mortF 
  
  #
  # Maturation investment: --------------------------------------------------
  #
  
  # param$matstageS <- which.min(abs(param$sizes - param$smat))
  # param$matstageL <- which.min(abs(param$sizes - param$lmat))

  # Alometric function investment in growth: 
  param$kappaS <- 1 - ((1 + (param$wc[param$ix1[1]:param$ix2[1]] / param$smat)^(-5))^(-1) *
                         (param$wc[param$ix1[1]:param$ix2[1]] / param$sfish_asymp)^(1-param$n))
  param$kappaL <- 1 - ((1 + (param$wc[param$ix1[3]:param$ix2[3]] / param$lmat)^(-5))^(-1) *
                         (param$wc[param$ix1[3]:param$ix2[3]] / param$lfish_asymp)^(1-param$n))
  param$kappa <- c(param$kappaS, param$kappaS, param$kappaL, param$kappaL, rep(1, param$nstage), param$kappaL)
  

  #
  # Habitat: ----------------------------------------------------------------
  #
  
  param$photic = 150
  param$mesop = 250
  param$visual = 1.5 # scalar; >1 visual predation primarily during the day, = 1 equal day and night 
  param$S2P = 0.5 # predation from Squid to pelagics
  
  #
  # Vertical distribution: --------------------------------------------------
  #
  
  sigmad <- 10 # width of initial distribution
  tau <- 10  # increase in width
  param$sigmap <- sigmad + tau * log10(param$wc / param$wc[1]) # width for each size class
  
  # first stages as juvenile/adult for predators
  param$ixjuv <- which.min(abs(param$sizes - param$smat/15)) # Dani: I have divided param$smat by 15 so at ~0.5 g they migrate to benthos
  param$ixadult <- which.min(abs(param$sizes - param$lmat))
  
  # For initial condition: 
  param$B0 <- 0 * param$ixFish + .01

  return(param)
}


#                                          END OF SCRIPT
#############################################################################################################