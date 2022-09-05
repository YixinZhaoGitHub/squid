
## Script name: Baserun
##
##
## Authors: Daniel Ottmann 
## Email: daniel.ottmann.riera@gmail.com
##
## Date created: March 2022
## Last update:  April 2022
##
## ---------------------------
##
## Readme:
##
## This script runs the FEISTY derivative
##
## ---------------------------


#####################################################################
feisty_deriv <- function(t, y, param) {
  
  # Load functions:
  source("../R/calcEncounter.R") 
  
  y[y < 0] <- 0
  R <- y[param$ixR]
  B <- y[param$ixFish]
  #
  # Feeding
  #
  encounter <- calcEncounter(y, param)
  mortpred <- encounter$mortpred
  Eavail <- encounter$Eavail
  
  #
  # Fish:
  #
  
  ixFish <- param$ixFish
  
  # Total mortality:
  mort <- mortpred[ixFish] + param$mort0 + param$mortF                   
  
  # Flux out of the size group:
  v <- Eavail[param$ixFish]  
  vplus <- pmax(0, v)
  gamma <- (param$kappa * vplus - mort) / (1 - param$z[param$ixFish]^(1 - mort / (param$kappa * vplus)))
  Fout <- gamma * B
  Repro <- ((1 - param$kappa) * vplus * B)                                        # A fraction 1-k of the available energy is
                                                                                  # invested in reproduction. 
# Flux into the size group (where Fout in last stage stays is included in repro):
  Fin <- c()
  for(i in 1:param$nSpecies) {
    ix <- (param$ix1[i]:param$ix2[i]) - length(R)                          # select Size class in Species
    ixPrev <- c(ix[length(ix)], ix[1]:(ix[length(ix)] - 1))         
    Fin[ix] <- Fout[ixPrev]
    # Reproduction:
    Fin[ix[1]] <- param$eRepro[i] * (Fin[ix[1]] + sum(Repro[ix]))
  }

dBdt <- Fin - Fout + ((v - mort) * B) - Repro                 

#
# Resource
#

dRdt <- param$r[1:2] * (param$K[1:2] - R[1:2]) - mortpred[1:2] * R[1:2]

# Detritus flux out: 
F_D_out <- 121 + 2.58 * (mortpred[1:2] %*% R[1:2])   #  <<------ Where does this equaiton come from?
dRdt[3] <- F_D_out * param$martin * param$epst - param$r[3] * R[3] - mortpred[3] * R[3]
dRdt[4] <- 0

# Ad fish + benthic resources:
dydt <- c(dRdt, dBdt)
  
  return(list(dydt))
}


#                                          END OF SCRIPT
#############################################################################################################