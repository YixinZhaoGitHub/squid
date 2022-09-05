
## Script name: Baserun
##
##
## Authors: Daniel Ottmann 
## Email: daniel.ottmann.riera@gmail.com
##
## Date created: March 2022
## Last update:  July 2022
##
## ---------------------------
##
## Readme:
##
## This function calculates overlap from depth distribution
##
## ---------------------------


#####################################################################
calcpreference <- function(param) {
  
  # This part can be write in a more efficient way: matrix of Xloc for all
  # the size class for day and night: Then just using it once in the main
  # equation. 
  
  # Depth strata
  xrange <- 0:param$bottom 
  
  # Vertical migration depth: 
  dvm <- param$photic + 500 
  dvm[param$bottom < (param$photic + 500)] <- param$bottom # migration to bottom in intermediate habitats
  dvm[param$bottom <= param$mesop] <- 0 # no migration in shallow habitats

  #
  # Zooplankton: ------------------------------------------------------------
  #
  
  # Night
  xloc <- 0 
  zp_n <- exp(-(outer((xrange - xloc)^2, (2 * param$sigmap[param$ixR[1:2]]^2), "/"))) %*% diag(1 / (sqrt(2 * 3.14 * param$sigmap[param$ixR[1:2]]^2)))
  zp_n <- zp_n %*% diag(1 / colSums(zp_n))
  
  # Day: half at surface, half at dvm depth  
  xloc <- dvm
  ix <- param$ixR[1:2]
  zp_d <- exp(-(outer((xrange - xloc)^2, 2 * param$sigmap[ix]^2, "/"))) %*% diag(1 / (sqrt(2 * 3.14 * param$sigmap[ix]^2)))
  zp_d <- zp_d %*% diag(1 / colSums(zp_d))
  zp_d <- (zp_n + zp_d) / 2
  
  #
  # Benthos small and large: ------------------------------------------------
  # (at bottom with width sigma)
  
  xloc <- param$bottom
  bent <- (1 / (sqrt(2 * 3.14 * 10^2))) * exp(-((xrange - xloc)^2 / (2 * 10^2)))
  bent <- bent /sum(bent)
  bent_dn <- cbind(bent, bent) 
  
  
  #
  # Small pelagic fish: -----------------------------------------------------
  # (day + night)  
  
  xloc <- 0
  ix <- param$ix1[1]:param$ix2[1]
  spe <- exp(-(outer((xrange - xloc)^2, (2 * param$sigmap[ix]^2), "/"))) %*% diag(1 / (sqrt(2 * 3.14 * param$sigmap[ix]^2))) 
  spel_dn <- spe %*% diag(1 / colSums(spe))
  
  
  #
  # Mesopelagic: -----------------------------------------------------------
  #
  # night
  mpel_n <- spel_dn
  
  # day: at dvm      <<------- It assumes that larvae follow dvm? In reality Larvae remain closer to surface 24h (see Alvarez 2021 mdpi). Can be fixed as in Large pelagics.
  xloc <- dvm
  ix <- param$ix1[2]:param$ix2[2]
  mpe <- exp(-(outer((xrange - xloc)^2, (2 * param$sigmap[ix]^2), "/"))) %*% diag(1 / (sqrt(2 * 3.14 * param$sigmap[ix]^2)))
  mpel_d <- mpe %*% diag(1 / colSums(mpe))
  
  
  #
  # large pelagic fish: -----------------------------------------------------
  #
  # night: surface
  
  xloc <- 0
  ix <- param$ix1[3]:param$ix2[3]
  lpe <- exp(-(outer((xrange - xloc)^2, (2 * param$sigmap[ix]^2), "/"))) %*% diag(1 / (sqrt(2 * 3.14 * param$sigmap[ix]^2))) 
  lpel_n <- lpe %*% diag(1 / colSums(lpe))
  
  
  # Day: surface + dvm   
  xloc <- rep(0, param$nstage)
  xloc[param$ixadult:length(xloc)] <- dvm
  ix <- param$ix1[3]:param$ix2[3]
  lpe <- exp(-(outer(xrange, xloc, "-")^2 %*% diag(1 / (2 * param$sigmap[ix]^2)))) %*% diag((1 / (sqrt(2 * 3.14 * param$sigmap[ix]^2))))
  lpel_d <- lpe %*% diag(1 / colSums(lpe))
  lpel_d <- (lpel_d + lpel_n)/2
  
  
  #
  # Demersal fish: ----------------------------------------------------------
  #
  
  # night
  xloc <- rep(0, param$nstage)
  xloc[param$ixjuv:length(xloc)] <- param$bottom
  ix <- param$ix1[4]:param$ix2[4]
  dem <- exp(-(outer(xrange, xloc, "-")^2 %*% diag(1 / (2 * param$sigmap[ix]^2)))) %*% diag((1 / (sqrt(2 * 3.14 *param$sigmap[ix]^2))))
  dem_n <- dem %*% diag(1 / colSums(dem))

  # day   <<---------------- day and night yield the same depth distribution
  demmig <- dvm
  demmig[(param$bottom - dvm) >= 1200] <- dvm + (param$bottom - dvm - 1200)
  demmig[(param$bottom - dvm) >= 1500] <- param$bottom
  xloc[param$ixadult:length(xloc)] <- demmig
  ix <- param$ix1[4]:param$ix2[4]
  dem <- exp(-(outer(xrange, xloc, "-")^2 %*% diag(1 / (2 * param$sigmap[ix]^2)))) %*% diag((1 / (sqrt(2 * 3.14 * param$sigmap[ix]^2))))
  dem_d <- dem %*% diag(1 / colSums(dem))
      
  # if shallower than euphotic depth, adult demersals feed across-habitats
  if (param$bottom <= param$photic) {
    dem_d <- (dem_d + dem_n)/2
    dem_n <- dem_d
  }

  #
  # Squids: -----------------------------------------------------------------
  #
  
  # Reset xloc = 0 
  xloc <- rep(0, param$nstage)     
  ix <- param$ix1[5]:param$ix2[5]
  if (param$bottom > param$mesop) {
    # As Mesopelagic  
    xloc[1:length(xloc)] <- dvm
    cph_d <- exp(-(outer(xrange, xloc, "-")^2 %*% diag(1 / (2*param$sigmap[ix]^2)))) %*% diag((1 / (sqrt(2 * 3.14 * param$sigmap[ix]^2))))
  } else { 
    # In shallow water - as demersals 
    # large cephalopods 
    xloc <- param$bottom
    cph_d <- exp(-(outer(xrange, xloc, "-")^2 %*% (1 / (2*param$sigmap[ix]^2)))) %*% diag((1 / (sqrt(2 * 3.14 * param$sigmap[ix]^2))))
  }
  cph_d <- cph_d %*% diag(1 / colSums(cph_d))
  cph_n <- lpel_n
                                                             
  
  #
  # Bathypelagic: -----------------------------------------------------------------
  #
  
  # night: adults in midwater, others at surface
  xloc <- rep(0, param$nstage)
  xloc[param$ixadult:length(xloc)] <- dvm
  ix <- param$ix1[6]:param$ix2[6]
  bpe <- exp(-(outer(xrange, xloc, "-")^2 %*% diag(1 / (2 * param$sigmap[ix]^2)))) %*% diag(1 / (sqrt(2 * 3.14 * param$sigmap[ix]^2)))
  bpel_n <- bpe %*% diag(1 / colSums(bpe))
  
  # day (dvm)
  xloc <- rep(dvm, param$nstage)
  ix <- param$ix1[6]:param$ix2[6]
  bpe <-  exp(-(outer(xrange, xloc, "-")^2 %*% diag(1 / (2 * param$sigmap[ix]^2)))) %*% diag(1 / (sqrt(2 * 3.14 * param$sigmap[ix]^2)))
  bpel_d <- bpe %*% diag(1 / colSums(bpe))
  
  
  #
  # calculate overlap during day and night: ---------------------------------
  #
  
  depthDay <- cbind(zp_d, bent_dn, spel_dn, mpel_d, lpel_d, dem_d, cph_d, bpel_d)
  depthNight <- cbind(zp_n, bent_dn, spel_dn, mpel_n, lpel_n, dem_n, cph_n, bpel_n)
  dayout <- matrix(0, param$ixFish[length(param$ixFish)], param$ixFish[length(param$ixFish)])
  nightout <- matrix(0, param$ixFish[length(param$ixFish)], param$ixFish[length(param$ixFish)])
  
  for(i in 1:param$ixFish[length(param$ixFish)]) {
    test <- matrix(pmin(depthDay[ , i], depthDay), nrow(depthDay), param$ixFish[length(param$ixFish)])
    dayout[ , i] <- colSums(test)
    test <-  matrix(pmin(depthNight[ , i], depthNight), nrow(depthNight), param$ixFish[length(param$ixFish)])
    nightout[ , i] <- colSums(test)
  }
  
  
  # visual predation (pelagics) is good at light, bad in the dark
  visualpred <- c((param$ix1[1]:param$ix2[1]), (param$ix1[3]:param$ix2[3])) # Spel Lpel 
  dayout[visualpred, 1:(param$ix1[5])] <- dayout[visualpred, 1:(param$ix1[5])] * param$visual
  nightout[visualpred, param$ix1[1]:param$ix2[5]] <- nightout[visualpred, param$ix1[1]:param$ix2[5]] * (2 - param$visual) # Here we implicitly assume that all prey have less pred from pel at night
  
  # Large pelagic predators limited vision in twilight zone during day
  # Pelagics: 
  pelpred <- param$ix1[3]:param$ix2[3] # Selects Lpel
  pelpred <- pelpred[param$ixadult:length(pelpred)] # Only adults
  preytwi <- c(param$ix1[2]:param$ix2[2], param$ix1[6]:param$ix2[6]) # prey are Mesopelagics and bathypelagics
  dayout[pelpred, preytwi] <-  dayout[pelpred, preytwi]/param$visual * (2 - param$visual)
  
  
  # Squids: 
  ceph <- param$ix1[5]:param$ix2[5]
  if (param$bottom > param$mesop) { # In deep regions, cephalopods are hiding in the mesopelgaic regions during the day (less predation)
                                    # and are going up at night (less predation from visual predators) -> we multiply x 0.75
    dayout[visualpred, ceph] <- dayout[visualpred, ceph] * 0.75   
    nightout[visualpred, ceph] <- nightout[visualpred, ceph] * 0.75
  } 
  
  # Average overlap during the whole day
  location <- (dayout + nightout) * 0.5
  
  # calculate combined preference matrix   
  theta <- param$prefer * location
  
  #
  # change specific interactions -----------------------------------------  
  #
  
  # Benthivory is a strategy (larvae + pelagics do not eat benthos)   
  idx_be <- c(5:(param$ix1[4] + (param$ixjuv - 2)), param$ix1[5]:param$ix2[6]) # all larvae + pelagics (incl cephalopods)
  theta[idx_be, 3:4] <- 0 
  
  # small demersals are less preyed on
  idx_smd <- (param$ix1[4] + (param$ixjuv - 1)):(param$ix1[4] + (param$ixadult - 2))
  theta[idx_be, idx_smd] <- theta[idx_be, idx_smd] * .25
  
  # demersals do not eat much zooplankton
  idx_dems <- (param$ix1[4] + (param$ixjuv - 1)):param$ix2[4]
  theta[idx_dems, 1:2] <-  theta[idx_dems, 1:2] * 0
  
  # provide benefit to forage and mesopelagic fish (predator avoidance)
  pred1 <- (param$ix1[3] + (param$ixadult - 1)):param$ix2[3] # large pelagic
  pred2 <- (param$ix1[4] + (param$ixadult - 1)):param$ix2[4] # demersal
  pred3 <- (param$ix1[6] + (param$ixadult - 1)):param$ix2[6] # bathypelagics
  
  prey1 <- (param$ix1[1] + (param$ixjuv-1)):param$ix2[1] # small pelagics
  prey2 <- (param$ix1[2] + (param$ixjuv-1)):param$ix2[2] # mesopelagic
  idx_predat <- c(pred1, pred2, pred3)
  idx_prey <- c(prey1, prey2)
  theta[idx_predat, idx_prey] <- theta[idx_predat, idx_prey] * 0.5 
  
  
  # Predation from Squid:
  theta[ceph, (param$ix1[3]):(param$ix2[3])] <- param$S2P * theta[ceph, (param$ix1[3]):(param$ix2[3])]
  theta[ceph, (param$ix1[4]):(param$ix2[4] - 4)] <- param$S2P * theta[ceph, (param$ix1[4]):(param$ix2[4] - 4)]
  theta[ceph, prey1] <- theta[ceph, prey1] * param$S2P
  
  # calculate center of vertical distribution during the day
  idi <- apply(depthDay, 2, which.max)
  avlocDay <- xrange[idi]
  
  # calculate center of vertical distribution during the night
  idi <- apply(depthNight, 2, which.max)
  avlocNight <- xrange[idi]
  
  param$theta <- theta
  param$depthDay <- depthDay
  param$depthNight <- depthNight
  param$avlocDay <- avlocDay
  param$avlocNight <- avlocNight
  
  return(param)
}


#                                          END OF SCRIPT
#############################################################################################################