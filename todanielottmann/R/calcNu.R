
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
## This function describes the flow of mass (energy) from one size group to the next.
##
## ---------------------------


#####################################################################
calcNu <- function(Eavail, mort, param){
  v <- (Eavail[param$ixFish])
  vplus <- pmax(0, v)
  nu <- (vplus - mort) / (1 - param$z[param$ixFish]^(1 - mort/vplus))
  return(list(v = v, nu = nu))
}


#                                          END OF SCRIPT
#############################################################################################################