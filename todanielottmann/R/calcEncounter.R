
## Script name: calculate encounter
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
## This function calculates consumption and predator mortality
##
## ---------------------------


#####################################################################
calcEncounter <- function(y, param) {
  #
  # Consumption
  #
  Encspecies <- c(param$V * param$theta %*% y)
  f <- Encspecies / (param$Cmax + Encspecies) # correction for total prey consumption 
  f[is.na(f)] <- 0
  Eavail <- param$Cmax * param$epsAssim * f - param$Mc
  
  #
  # Mortality: from prey on i
  #
  mortpred <- c(t((param$Cmax * param$V) * param$theta / (param$Cmax + Encspecies)) %*% y)

  return(list(f = f, mortpred = mortpred, Eavail = Eavail))
} 


#                                          END OF SCRIPT
#############################################################################################################