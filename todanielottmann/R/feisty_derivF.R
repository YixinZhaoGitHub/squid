
## Script name:
##
##
## Authors: Yixin Zhao
## Email: 
##
## Date created: September 2022
## Last update:  September 2022
##
## ---------------------------
##
## Readme:
##
## This script runs the FEISTY derivative by dynamic link library .dll
##
## ---------------------------


#####################################################################
feisty_derivF <- function(t, y, param) {
  
  derivF = .Fortran("f_calcderivativesSquid", 
                    u= as.numeric(y),
                    dudt=as.numeric(y))
  
  return(derivF$dudt)
}



#                                          END OF SCRIPT
#############################################################################################################