
## Script name: Plot diet
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
## This script produces barplots with the diet composition of different size-class functional groups
##
## ---------------------------


#####################################################################
plotdiet <- function(param, result) {
  y <- result$y
  Bin <- floor(0.8 * nrow(y))
  yend <- colMeans(y[Bin:nrow(y),])
  ystage <- param$ixFish[length(param$ixFish)]
  ysmall <- param$nstage - param$nstage * 2/3
  f <- calcEncounter(yend, param)$f
  
  bom <- t(t(param$theta[5:ystage, ]) * colMeans(y[Bin:nrow(y),])) 
  fbom <- f[5:ystage] / rowSums(bom)
  output <- bom * fbom
  
  SpId <- c("SZoo", "BZoo", "Bent", "Bent", 
             rep(param$SpId[1], param$maxsmall - 1),
             rep(param$SpId[2], param$maxsmall - 1),
             rep(param$SpId[3], param$nsize - 1),
             rep(param$SpId[4], param$nsize - 1),
             rep(param$SpId[5], param$nsize - 1),
             rep(param$SpId[6], param$nsize - 1))
  
  # small pelagics: ---------------------------------------------------------
  small_pel <- output[(param$ix1[1] - length(param$ixR)):(param$ix2[1] - length(param$ixR)), ] 
  small_pel <- t(rbind(small_pel, matrix(0, ysmall, ystage)))
  small_pel <- data.frame(val = c(small_pel), 
                          stage = rep(1:param$nstage, each = nrow(small_pel)), 
                          SpId = as.factor(rep(SpId, param$nstage)))

  p1 <- ggplot(data = small_pel) +
    geom_bar(aes(x = stage, y = val, fill = SpId), stat = "identity") +
    scale_fill_manual(values = param$my_palette) +
    theme_base() +
    labs(x = "", y = "Fraction of stomach", fill = "Prey group", title = param$RSpName[4]) +
    theme(legend.position = "none")
  
  # Large pelagics: --------------------------------------------------------- 
  large_pel <- t(output[(param$ix1[3] - length(param$ixR)):(param$ix2[3] - length(param$ixR)), ])
  large_pel <- data.frame(val = c(large_pel), 
                          stage = rep(1:param$nstage, each = nrow(large_pel)), 
                          SpId = as.factor(rep(SpId, param$nstage)))

  p2a <- ggplot(data = large_pel) +
    geom_bar(aes(x = stage, y = val, fill = SpId), stat = "identity") +
    scale_fill_manual(values = param$my_palette) +
    theme_base() +
    labs(x ="size-class", y = "", fill = "Prey group", title = param$RSpName[6]) +
    theme(legend.position = "none") 
  
  p2b <- ggplot(data = large_pel) +
    geom_bar(aes(x = stage, y = val, fill = SpId), stat = "identity") +
    scale_fill_manual(values = param$my_palette) +
    theme_base() +
    labs(x ="", y = "", fill = "Prey group", title = param$RSpName[6]) +
    theme(legend.position = "none") 
  
  # Demersal: ---------------------------------------------------------------
  demers <- t(output[(param$ix1[4] - length(param$ixR)):(param$ix2[4] - length(param$ixR)), ])
  demers <- data.frame(val = c(demers), 
                       stage = rep(1:param$nstage, each = nrow(demers)), 
                       SpId = as.factor(rep(SpId, param$nstage)))

  p3a <- ggplot(data = demers) +
    geom_bar(aes(x = stage, y = val, fill = SpId), stat = "identity") +
    scale_fill_manual(values = param$my_palette) +
    theme_base() +
    labs(x ="size-class", y = "", fill = "Prey group", title = param$RSpName[7]) +
    theme(legend.position = "none")
  
  p3b <- ggplot(data = demers) +
    geom_bar(aes(x = stage, y = val, fill = SpId), stat = "identity") +
    scale_fill_manual(values = param$my_palette) +
    theme_base() +
    labs(x ="", y = "", fill = "Prey group", title = param$RSpName[7]) +
    theme(legend.position = "none")
  
  # Squid: ------------------------------------------------------------------ 
  small_Ceph <- t(output[(param$ix1[5] - length(param$ixR)):(param$ix2[5] - length(param$ixR)), ])
  small_Ceph <- data.frame(val = c(small_Ceph), 
                           stage = rep(1:param$nstage, each = nrow(small_Ceph)), 
                           SpId = as.factor(rep(SpId, param$nstage)))

  p4 <- ggplot(data = small_Ceph) +
    geom_bar(aes(x = stage, y = val, fill = SpId), stat = "identity") +
    scale_fill_manual(values = param$my_palette) +
    theme_base() +
    labs(x ="size-class", y = "Fraction of stomach", fill = "Prey group", title = param$RSpName[8]) +
    theme(legend.position = "none") 
  
  # Mesopelagics: ----------------------------------------------------------- 
  meso_pel <- output[(param$ix1[2] - length(param$ixR)):(param$ix2[2] - length(param$ixR)), ]
  meso_pel <- t(rbind(meso_pel, matrix(0, ysmall, ystage)))
  meso_pel <- data.frame(val = c(meso_pel), 
                         stage = rep(1:param$nstage, each = nrow(meso_pel)), 
                         SpId = as.factor(rep(SpId, param$nstage)))

  p5 <- ggplot(data = meso_pel) +
    geom_bar(aes(x = stage, y = val, fill = SpId), stat = "identity") +
    scale_fill_manual(values = param$my_palette) +
    theme_base() +
    labs(x ="size-class", y = "", fill = "Prey group", title = param$RSpName[5]) +
    theme(legend.position = "none") 
  
  # Bathypelagics: ---------------------------------------------------------- 
  bathy_pel <- t(output[(param$ix1[6] - length(param$ixR)):(param$ix2[6] - length(param$ixR)), ])
  bathy_pel <- data.frame(val = c(bathy_pel), 
                         stage = rep(1:param$nstage, each = nrow(bathy_pel)), 
                         SpId = as.factor(rep(SpId, param$nstage)))

  p6 <- ggplot(data = bathy_pel) +
    geom_bar(aes(x = stage, y = val, fill = SpId), stat = "identity") +
    scale_fill_manual(values = param$my_palette) +
    theme_base() +
    labs(x ="size-class", y = "", fill = "Prey group", title = param$RSpName[9]) +
    theme(legend.position = "none") 
  
  # Legend: ----------------------------------------------------------------- 
  legend_colors <- data.frame(val = 0, stage = 0, SpId = unique(large_pel$SpId))
  
  p7 <- ggplot(data = legend_colors) +
    geom_bar(aes(x = stage, y = val, fill = SpId), stat = "identity") +
    scale_fill_manual(values = param$my_palette[attr(param$my_palette, "names") %in% legend_colors$SpId]) +
    labs(fill = "Prey group") +
    theme_void() +
    theme(legend.position = c(1, .5),
          legend.direction = "horizontal",
          legend.text = element_text(size = 12))
  
  
  # Put all panels together:
  if(sum(yend[(param$ix1[5] - 4):(param$ix2[5] - 4)]) != 0) {
    if(param$bottom > param$mesop) {
      p <- p1 + p2b + p3b + p4 + p5 + p6 + p7 & 
        theme(plot.background = element_blank()) 
    } else {
      p <- p1 + p2a + p3a + p4 + p7 & 
        theme(plot.background = element_blank()) 
    }
  } else {
    p <- p1 + p2b + p3a + p5 + p6 +p7 & 
      theme(plot.background = element_blank()) 
  }
  
  return(p)
}


#                                          END OF SCRIPT
#############################################################################################################