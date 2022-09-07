
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
## This script plots somatic weight vs biomass, feeding activity and predation mortality
##
## ---------------------------


#####################################################################
plotFeistyf <- function(param, result) {
  
  # Load functions:
  source("../R/calcEncounter.R") 
  
  # Define objects:
  y <- result$y
  Bin <- floor(0.8 * nrow(y))
  yend <- colMeans(y[Bin:nrow(y),])
  encounter <- calcEncounter(yend, param)

  # Create data frame:
  df <- data.frame(SpId = c("Res"), wc = param$wc, yend, f = encounter$f, mortpred = encounter$mortpred)
  
  for (i in 1:length(param$SpId)) {
    df[param$ix1[i]:param$ix2[i], "SpId"] <- param$SpId[i]
  }
  
  # Remove resource group:
  df <- df[df$SpId != "Res", ]
  
  # Limit minimum biomass to 10^-5 g
  df$yend[df$yend < 10^-10] <- 0  
  
  # Plot: 
  # Biomass:-----------------------------------------------------------------
  p1 <- ggplot(data = df) +
    geom_line(aes(x = wc, y = yend, color = SpId), size = 1) +
    scale_color_manual(values = param$my_palette[attr(param$my_palette, "names") %in% df$SpId]) +
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    annotation_logticks(sides = "bl") +
    theme_base() +
    labs(x ="Weight (grams)", y = expression("Biomass (g m"^-2*")"), color = "Funcitional group") +
    theme(legend.title = element_blank(),
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())

  # Feeding level: ----------------------------------------------------------
  p2 <- ggplot(data = df) +
    geom_line(aes(x = wc, y = f, color = SpId), size = 1) +
    scale_color_manual(values = param$my_palette[attr(param$my_palette, "names") %in% df$SpId]) +
    geom_hline(yintercept = 0.2, linetype = "dashed") +
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    annotation_logticks(sides = "b") +
    ylim(c(0, 1)) +
    theme_base() +
    labs(x ="Weight (grams)", y = "Feeding level", color = "Funcitional group") +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  # Predation Mortality: ----------------------------------------------------
  p3 <- ggplot(data = df) +
    geom_line(aes(x = wc, y = mortpred, color = SpId), size = 1) +
    scale_color_manual(values = param$my_palette[attr(param$my_palette, "names") %in% df$SpId]) +
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    annotation_logticks(sides = "b") +
    theme_base() +
    labs(x ="Weight (grams)", y = expression("Predation mort. (yr"^-1*")"), color = "Funcitional group") +
    theme(legend.position = "none")
  
  # Legend: -----------------------------------------------------------------  
  p4 <- ggplot(data = df) +
    geom_line(aes(x = 0, y = 0, color = SpId), size = 1) +
    scale_color_manual(values = param$my_palette[attr(param$my_palette, "names") %in% df$SpId]) +
    labs(color = "Group") +
    theme_void() +
    theme(legend.position = c(0.5, .5),
          legend.direction = "horizontal",
          legend.text = element_text(size = 12),
          legend.title = element_blank())

  p <- p1 / p2 / p3 / p4 & 
    theme(plot.background = element_blank()) 
  
  return(p)
}


#                                          END OF SCRIPT
#############################################################################################################