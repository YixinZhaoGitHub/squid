
## Script name: FEISTY
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
## This script and produces the FEISTY output
##
## ---------------------------

# loadlibrary func
loadFEISTYmodel = function() {
  sys=Sys.info()['sysname']
  
  if (sys=='Darwin') 
    sLibname = '../lib/libFEISTY.dylib'
  if (sys=='Linux') 
    sLibname = '../lib/libFEISTY.so'
  if (sys=='Windows')
    sLibname = '../lib/libFEISTY.dll'
  
  dyn.load(sLibname)
}
#####################################################################
feisty <- function(param, USEdll=TRUE, result) {
  
  # Load functions:
  source("../R/feisty_deriv.R") 
  source("../R/feisty_derivF.R") 
  source("../R/calcEncounter.R") 
  source("../R/calcNu.R") 
  
  #
  # Init
  #
  y0 <- param$y0
  t <- seq(0, param$tEnd, by = 1)
  
  
  
  #
  # Run:
  #
  if (USEdll){
    loadFEISTYmodel()
    dummy = .Fortran("f_setupsquid", pprod=as.numeric(param$K[1]),
                     bottom=as.numeric(param$bottom),
                     nStages=as.integer(param$nstage))
    
    out = ode(y=y0,
            times = t,
            func = function(t,y,parms) list(feisty_derivF(t,y,parms)),# Run by dll
            parms = param)
  }else{
    
        out <- ode(y = y0, times = t, func = feisty_deriv, parms = param, method = "ode23")
        
  }
  #
  # Construct output:
  #
  result <- list()
  result$y <- out[, -1]
  result$R <- out[, param$ixR + 1] # resource
  result$B <- out[, param$ixFish + 1] # Other groups
  result$t <- t
  result$Yield <- t(t(result$B) * param$mortF)

  encounter <- calcEncounter(out[nrow(out), -1], param)
  result$f <- encounter$f
  result$mortpred <- encounter$mortpred
  result$Eavail <- encounter$Eavail
  result$mort <- result$mortpred[param$ixFish] + param$mort0 + param$mortF
  
  Nu <- calcNu(result$Eavail, result$mort, param)
  result$v <- Nu$v
  result$nu <- Nu$nu

  return(result)
}


#                                          END OF SCRIPT
#############################################################################################################