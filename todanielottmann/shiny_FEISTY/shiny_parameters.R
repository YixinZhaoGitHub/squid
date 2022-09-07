
## Script name: shiny FEISTY parameter setup
##
##
## Authors: Daniel Ottmann 
## Email: daniel.ottmann.riera@gmail.com
##
## Date created: April 2022
## Last update:  July 2022
##
## ---------------------------
##
## Readme:
##
## This script sets up basic parameters to run FEISTY in a shiny app
##
## ---------------------------


#####################################################################
shiny_params <- function(depth, prod, nstage, small_pel, meso_pel, large_pel, demersals, squid, bathypelagics) {

  #
  ## Run FEISTY: 
  #
  
  # Load functions:
  source("../R/baseparameters.R") 
  source("../R/baseparam_depth.R") 
  source("../R/feisty_species_setup.R") 
  

  # parameter for FEISTY 
  param <- baseparameters(nstage)
  param <- baseparam_depth(param, depth) # param and depth (m)
  param$K <-  c(prod, prod, 0, 0)  # (g ww/m2 )

  ##### temprary stuff
  small_pel <- small_pel
  meso_pel <- meso_pel
  large_pel <- large_pel
  demersals <- demersals
  squid <- squid
  bathypelagics <- bathypelagics
  #######
  param$y0 <- species_setup(param, small_pel, meso_pel, large_pel, demersals, squid, bathypelagics)

  return(param)
}


#                                          END OF SCRIPT
#############################################################################################################