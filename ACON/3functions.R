require(ggplot2)
require(dplyr)
require(grid)
require(ggthemes)


yearqtrplot <- function(chartdata, col, title, ylab){
  # chartdata <- as.data.frame(chartdata) %>%
  #   mutate(maxmin = (max(chartdata[[col]]) == chartdata[[col]] | min(chartdata[[col]]) == chartdata[[col]]))
  chartdata <- as.data.frame(chartdata) %>%
    mutate(maxmin = case_when(max(chartdata[[col]]) == chartdata[[col]] ~ "max",
                              min(chartdata[[col]]) == chartdata[[col]] ~ "min",
                              chartdata[[col]] == chartdata[[col]]  ~ "none"))
  
  minn = chartdata[which.min(chartdata[[col]]),col]
  maxn = chartdata[which.max(chartdata[[col]]),col]
  y0 = floor(minn*0.9)
  quarn = length(chartdata[["Quarter"]])+1
  
  yeardiv = length(chartdata[["Quarter"]])%%4
  yearwhole = length(chartdata[["Quarter"]])%/%4
  
  g1 <-  ggplot(data = chartdata, 
                aes(x = interaction(chartdata[["Year"]],
                                    chartdata[["Quarter"]],
                                    lex.order = TRUE),
                    y = chartdata[[col]],
                    group = 1)) +
    
  geom_line(colour = "blue", size = 2) +

  coord_cartesian(ylim = c(y0, ceiling(maxn*1.05)),  expand = FALSE) +
  
  scale_y_continuous(breaks = c(minn,maxn)) +
  theme_few(base_size = 18) +
  theme(legend.position="none",
        plot.margin = unit(c(1, 1, 5, 1), "lines"), # Adjust margins of window
        axis.title.x = element_blank(), # Turn off x axis title
        axis.text.x = element_blank(), # Turn off x axis labels
        axis.ticks.x = element_line(), # add in x axis ticks
        axis.ticks.length = unit(4,"mm"), # make those ticks longer
        panel.grid.major.x = element_blank(), # turn off major grid lines
        panel.grid.minor.x = element_blank()) + # turn off minor grid lines.
  ylab(ylab) +
  
   # # Y AXIS: grid lines at max and min values.
  annotate(geom = "segment",
            x = 0,
            xend = quarn,
            y = maxn,
            yend = maxn,
            colour = "grey",
            linetype = 5) +
  annotate(geom = "segment",
            x = 0,
            xend = length(chartdata[["Quarter"]])+1,
            y = minn,
            yend = minn,
            colour = "grey",
            linetype = 5) +

  # Colour in the max and min values
  geom_point(aes(group = 2, colour = maxmin), size = 4) + 
  
  scale_color_manual(values = c("green", "red", NA)) +

  ##LABELS ############################################################

  # X AXIS: quarter labels
  annotate(geom = "text",
           x = seq_len(nrow(chartdata)),
           y = y0-4,
           label = chartdata[["Quarter"]],
           size = 4.5,
           angle = 0) +
  
  # X AXIS: horizontal separator between year and quarter
  annotate(geom = "segment",
           x = 0,
           xend = nrow(chartdata)+1,
           y = y0-7,
           yend = y0-7,
           colour = "grey") +
    
  # X AXIS: vertical lines dividing years
  annotate(geom = "segment",
           x = 4.5 + 4 * (0:(length(unique(chartdata[["Year"]]))-2)),
           xend = 4.5 + 4 * (0:(length(unique(chartdata[["Year"]]))-2)),
           y = y0,
           yend = y0-13,
           colour = "grey",
           linetype = 1) +
    
  # X AXIS: vertical lines at start and end of separator.
  annotate(geom = "segment",
           x = 1 * c(0,length(chartdata[,"Quarter"])+1),
           xend = 1 * c(0,length(chartdata[,"Quarter"])+1),
           y = y0,
           yend = y0-13,
           colour = "grey") +
  
  if(yeardiv == 0) {

  # X AXIS: year labels
  annotate(geom = "text",
           x = 2.5 + 4 * (0:(length(unique(chartdata[["Year"]]))-1)), # error if not whole years
           y = y0-10,
           label = unique(chartdata[,"Year"]),
           size = 5)

  } else {

    # X AXIS: year labels
    annotate(geom = "text",
             x = c(2.5 + 4*(0:eval(yearwhole-1)),
                   2.25 + 4*(eval(yearwhole))-(eval(yeardiv))), # error if not whole years
             y = y0-10,
             label = unique(chartdata[,"Year"]),
             size = 5)
}
# remove clipping of x axis labels
g2 = ggplot_gtable(ggplot_build(g1))
g2$layout$clip[g2$layout$name == "panel"] <- "off"

# Create Title element
titlegrob <- textGrob(title, gp = gpar(fontsize = 16))
# padding = distance from title to chart
padding <- unit(5,"mm")
# add space for the title on top of the chart.
g3 <- gtable::gtable_add_rows(g2,heights = grobHeight(titlegrob) + padding, pos = 0)
# add title into that space.
g3 <- gtable::gtable_add_grob(g3,titlegrob,1,1,1,ncol(g3))
# draw the chart.
#grid::grid.draw(g3, recording = TRUE)
return(g3)
}
