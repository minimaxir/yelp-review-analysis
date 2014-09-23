library(dplyr)
library(ggplot2)
library(extrafont)
library(scales)
library(grid)
library(RColorBrewer)

fontFamily <- "Source Sans Pro"
fontTitle <- "Source Sans Pro Semibold"

colors = c("#16a085","#27ae60","#2980b9","#8e44ad","#f39c12","#c0392b","#1abc9c", "#2ecc71", "#3498db", "#9b59b6", "#f1c40f","#e74c3c")

neutral_colors = function(number) {
	return (brewer.pal(11, "RdYlBu")[-c(5:7)][number %% 8])
}

theme_custom <- function() {theme_bw(base_size = 8) + 
                             theme(panel.background = element_rect(fill="#eaeaea"),
                                   plot.background = element_rect(fill="white"),
                                   panel.grid.minor = element_blank(),
                                   panel.grid.major = element_line(color="#dddddd"),
                                   axis.ticks.x = element_blank(),
                                   axis.ticks.y = element_blank(),
                                   axis.title.x = element_text(family=fontTitle, size=8, vjust=-.3),
                                   axis.title.y = element_text(family=fontTitle, size=8, vjust=1.5),
                                   panel.border = element_rect(color="#cccccc"),
                                   text = element_text(color = "#1a1a1a", family=fontFamily),
                                   plot.margin = unit(c(0.25,0.1,0.30,0.35), "cm"),
                                   plot.title = element_text(family=fontTitle, size=9, vjust=1))                          
}
