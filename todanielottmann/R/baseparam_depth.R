
## Script name: base parameters depth
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
## This function complements the base parameters by setting the depth distribution of organisms depending on the depth of the ecosystem
##
## ---------------------------


#####################################################################
# Parameters related to depth 

baseparam_depth <- function(param, depth) {
  
  # Load functions:
  source("../R/calcpreference.R") 
  
  #
  # Habitat and interactions: -----------------------------------------------
  #
  
  param$bottom <- depth # depth in meters
  param$xrange <- seq(0, param$bottom, length.out = (param$bottom + 1))  # depths range
  param$martin <- min((param$bottom / param$photic)^(-0.86), 1) # martin curve depth
  param <- calcpreference(param)
  
  #
  # Default initial conditions: ---------------------------------------------
  # 
  
  param$r <-  c(1,  1,  1,  0) # g ww/m2/yr (here should be per year). 
  
  return(param)
}


#                                          END OF SCRIPT
#############################################################################################################