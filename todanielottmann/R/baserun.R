
## Script name: Baserun
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
## This script sources other R-script to set basic parameters of FEISTY and the model
## It also produces plots of size- and taxon-specific biomass, feeding and depredation mortality, as well as diet, and trophic network
## Earlier version of the model can be found for MATLAB at https://github.com/Dvandenderen/Fish_foodwebs 
##
## ---------------------------


#####################################################################
# Clear environment:
rm(list = ls())

# Load libraries:
require(deSolve)
require(tidyverse)
require(ggthemes)
require(scales)
require(patchwork)

# Load functions:
source("../R/baseparameters.R") 
source("../R/baseparam_depth.R") 
source("../R/feisty.R") 
source("../R/feisty_species_setup.R") 
source("../R/plot_weight.R") 
source("../R/plotdiet.R")
source("../R/PlotEcosystem.R") 
source("../R/plot_network.R") 


# Simple Run:
nstage <- 6 # number of stages predator use 3, 6, 9, etc (prey = 2/3)
depth <- 100
param <- baseparameters(nstage)
param <- baseparam_depth(param, depth) # param and depth (m)
param$K <-  c(50, 50, 0, 0)  # Set up the secondary production (g ww/m2 )

# Include/exclude functional groups:
small_pel <- T
meso_pel <- T
large_pel <- T
demersals <- T  
squid <- T
bathypelagics <- T
param$y0 <- species_setup(param, small_pel, meso_pel, large_pel, demersals, squid, bathypelagics)


# Run FEISTY:
result <- feisty(param,USEdll=FALSE)

# Get some plots:
# Feisty weight:
plotFeistyf(param, result)

# Diet:
plotdiet(param, result)

# Network:
plot_network(param, result)

# Make a quick ecosystem plot with specific depth, productivity and ngroups:
# Ecosystem plot: 
PlotEcosystem(10, 20, 6)


#                                          END OF SCRIPT
#############################################################################################################