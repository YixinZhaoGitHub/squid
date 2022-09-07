
## Script name: FEISTY species setup
##
##
## Authors: Daniel Ottmann 
## Email: daniel.ottmann.riera@gmail.com
##
## Date created: July 2022
## Last update:  July 2022
##
## ---------------------------
##
## Readme:
##
## This function creates a function to include/exclude functional groups in the model
## It also determines the presence/absence of mesopelagic and bathypelagic fish depending on depth
##
## ---------------------------


#####################################################################


# Create function:
species_setup <- function(param, small_pel, meso_pel, large_pel, demersals, squid, bathypelagics) {
  
  presence <- c(small_pel, meso_pel, large_pel, demersals, squid, bathypelagics)
  presence <- ifelse(presence == T, 0.01, 0)

  param$B0  <- c()  

  for(i in 1:length(param$SpId)) {
    param$B0 <- c(param$B0, param$ix1[i]:param$ix2[i] * 0 + presence[i])
  }

  param$y0 <- c(0.1 * param$K, 0.01 * param$B0)
  
  
  # Presence of mesopelagic and bathypelagic species depends on depth:
  if(param$bottom <= param$mesop) {
    param$y0[c(param$ix1[2]:param$ix2[2], param$ix1[6]:param$ix2[6])] <- 0}
  
  
  return(param$y0)
  
}


#                                          END OF SCRIPT
#############################################################################################################