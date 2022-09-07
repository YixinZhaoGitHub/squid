
## Script name: plot ecosystem
##
## Authors: Daniel Ottmann 
## Email: daniel.ottmann.riera@gmail.com
##
## Date created: March 2022
## Last update:  June 2022
##
## ---------------------------
##
## Readme:
##
## This an envelope function to run FEISTY and produce a community network plot
## It enables to determine specific depth, productivity and number of functional groups
##
## ---------------------------


#####################################################################
PlotEcosystem <- function(depth, prod, nstage) {
  
  # Load functions:
  source("../R/baseparameters.R") 
  source("../R/baseparam_depth.R") 
  source("../R/feisty.R") 
 
  # Set parameters: 
  param <- baseparameters(nstage)
  param <- baseparam_depth(param, depth) # param and depth (m)
  param$K <-  c(prod, prod, 0, 0)  # (g ww/m2 )
  param$y0 <- c(0.1 * param$K, 0.01 * param$B0)

  # Run FEISTY: 
  result <- feisty(param)
  
  # Extract number of groups and biomass:
  ngroup <- param$ix2[length(param$ix2)]
  y <- result$y
  
  # Average of the biomass:                                                     
  Bin <- nrow(y) - ngroup
  Bi <- colMeans(y[Bin:nrow(y),]) # take values for the 37 last time steps

  #
  # Parameters for plot
  #
  # Calculate average depth day/night
  Av_depth_Day <- 1:ngroup
  Av_depth_night <- 1:ngroup
  for(i in 1:ngroup) {
    Av_depth_Day[i] <- which.max(param$depthDay[ ,i])
    Av_depth_night[i] <- which.max(param$depthNight[ ,i])
      }

  Av_depth <- -(Av_depth_Day + Av_depth_night) / 2
  
  # Change a bit for visualisation: 
  Av_depth[param$ix1[1]:param$ix2[1]] <- Av_depth[param$ix1[1]:param$ix2[1]] + 0.1 * param$bottom
  Av_depth[param$ix1[3]:param$ix2[3]] <- Av_depth[param$ix1[3]:param$ix2[3]] - 0.1 * param$bottom
  
  # Marker size depends on biomass: 
  Msize <- Bi / max(Bi)
  Msize[Msize == 0] <- NA
  idxM <- quantile(Msize, prob = c(0.2, 0.4, 0.6, 0.8), na.rm = T) # get quantiles for the distribution.
  Msize[Msize >= idxM[4] & !is.na(Msize)] <- 20
  Msize[Msize >= idxM[3] & Msize < idxM[4] & !is.na(Msize)] <- 15
  Msize[Msize >= idxM[2] & Msize < idxM[3] & !is.na(Msize)] <- 8
  Msize[Msize >= idxM[1] & Msize < idxM[2] & !is.na(Msize)] <- 3
  Msize[Msize < idxM[1] & !is.na(Msize)] <- .8
  
  # Create flux from interaction: 
  # Create coordinate for line between points: 
  SpId <- c("SZoo", "BZoo", "Bent", "Bent", 
             rep(param$SpId[1], param$maxsmall - 1),
             rep(param$SpId[2], param$maxsmall - 1),
             rep(param$SpId[3], param$nsize - 1),
             rep(param$SpId[4], param$nsize - 1),
             rep(param$SpId[5], param$nsize - 1),
             rep(param$SpId[6], param$nsize - 1))

  # Create Line width: 
  Mat <- rep(0, ngroup) 
  Mat[Bi != 0] <- 1
  Theta <- t(t(param$theta) * Bi) * Mat # flux equal the rate * the prey biomass (* 0 if pred <- 0)
  Theta <- c(Theta) 
  threshold <- 0.05 # min(tail(sort(Theta), 100)) # Alternatively, use 100 strongest relations regardless of absolute value of the threshold
  indx <- which(Theta >= threshold) # takes the x highest values of theta
  LineWdth <- Theta # give the appropriate size

  coord_1 <- data.frame(index = 1:ngroup^2,
                        wc = rep(param$wc[1:ngroup], ngroup), 
                        depth = rep(Av_depth[1:ngroup], ngroup), 
                        SpId = rep(SpId, ngroup),
                        Msize = rep(Msize, ngroup),
                        LineWdth = Theta/max(Theta),
                        Alpha = Theta/max(Theta))
  
  coord_2 <- data.frame(index = 1:ngroup^2,
                        wc = rep(param$wc[1:ngroup], each = ngroup), 
                        depth = rep(Av_depth[1:ngroup], each = ngroup), 
                        SpId = rep(SpId, each = ngroup),
                        Msize = rep(Msize, each = ngroup),
                        LineWdth = Theta/max(Theta),
                        Alpha = Theta/max(Theta))
  df <- rbind(coord_1, coord_2)
  

  # Select major interactions and scale sizes:  
  df <- df %>% filter(index %in% indx) %>%
    arrange(desc(Msize))
  
  p <- ggplot(data = df) +
    geom_line(aes(x = wc, y = depth, group = index, size = LineWdth, color = SpId, alpha = Alpha), show.legend = F) +
    geom_point(aes(x = wc, y = depth, color = SpId, size = Msize)) +
    scale_color_manual(values = my_palette[attr(my_palette, "names") %in% df$SpId], labels = param$RSpName) +
    scale_size_continuous(range = c(1, 15)) +
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
#    annotation_logticks(sides = "b") +
    labs(x ="Weight (grams)", y = "Depth (m)", color = "Group") +
    theme_base() + 
    guides(size = "none") +
    theme(plot.background = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.background = element_blank()) #+
    # guides(color = guide_legend(ncol = 2, byrow = F))
  
    ggsave("plot.png", p, height = 45 , width = 80, units = "mm", scale = 3)
    
   return(p)
}


#                                          END OF SCRIPT
#############################################################################################################