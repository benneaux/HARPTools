y0 <- floor(mindata$OOS*0.8)
g1 <- ggplot(data = reportdata, 
             aes(x = interaction(Year,
                                 Quarter, 
                                 lex.order = TRUE), 
                 y = OOS, 
                 group = 1)) +
  geom_line(colour = "blue", size = 2) +
  
  coord_cartesian(ylim = c(y0, 65),
                  expand = FALSE) +
  scale_y_continuous(breaks = c(maxdata$OOS,mindata$OOS)) +
  
##LABELS ############################################################ 

# X AXIS: quarter labels
annotate(geom = "text",
         x = seq_len(nrow(reportdata)), 
         y = y0-4, 
         label = reportdata$Quarter, 
         size = 4.5,
         angle = 0) +
  
  # X AXIS: year labels
  annotate(geom = "text",
           x = 2.5 + 4 * (0:(length(unique(reportdata$Year))-1)),
           y = y0-10,
           label = unique(reportdata$Year), 
           size = 5) +
  
  ##LINES ###############################################################

# X AXIS: horizontal separator between year and quarter
annotate(geom = "segment",
         x = 0,
         xend = nrow(reportdata) + 1,
         y = y0-7, 
         yend = y0-7,
         colour = "grey") +
  
  # X AXIS: vertical lines dividing years
  annotate(geom = "segment",
           x = 4.5 + 4 * (0:(length(unique(reportdata$Year))-2)),
           xend = 4.5 + 4 * (0:(length(unique(reportdata$Year))-2)),
           y = y0,
           yend = y0-13,
           colour = "grey",
           linetype = 1) +
  
  # X AXIS: vertical lines at start and end of separator.
  annotate(geom = "segment",
           x = 1 * c(0,length(reportdata$Quarter)+1),
           xend = 1 * c(0,length(reportdata$Quarter)+1),
           y = y0,
           yend = y0-13,
           colour = "grey") +
  
  # Y AXIS: grid lines at max and min values.
  annotate(geom = "segment",
           x = 0,
           xend = length(reportdata$Quarter)+1,
           y = maxdata$OOS,
           yend = maxdata$OOS,
           colour = "grey",
           linetype = 5) +
  annotate(geom = "segment",
           x = 0,
           xend = length(reportdata$Quarter)+1,
           y = mindata$OOS,
           yend = mindata$OOS,
           colour = "grey",
           linetype = 5) +
  
  # Colour in the max and min values
  geom_point(data = maxdata,
             aes(y = OOS,
                 x = interaction(Year,
                                 Quarter,
                                 lex.order = TRUE)),
             color = "green",
             size = 4) +
  geom_point(data = mindata,
             aes(y = OOS,
                 x = interaction(Year,
                                 Quarter,
                                 lex.order = TRUE)),
             color = "red",
             size = 4) +
  
  
  # using a custom theme with font size = 18
  ggthemes::theme_few(base_size = 18) +
  
  # Adjusting that theme
  theme(plot.margin = unit(c(1, 1, 5, 1), "lines"), # Adjust margins of window
        axis.title.x = element_blank(), # Turn off x axis title
        axis.text.x = element_blank(), # Turn off x axis labels
        axis.ticks.x = element_line(), # add in x axis ticks
        axis.ticks.length = unit(4,"mm"), # make those ticks longer
        panel.grid.major.x = element_blank(), # turn off major grid lines
        panel.grid.minor.x = element_blank()) # turn off minor grid lines.

# remove clipping of x axis labels
g2 <- ggplot_gtable(ggplot_build(g1))
g2$layout$clip[g2$layout$name == "panel"] <- "off"

# Create Title element
title <- textGrob("ASHW Occasions of Service by Quarter", gp = gpar(fontsize = 16))
# padding = distance from title to chart
padding <- unit(5,"mm")
# add space for the title on top of the chart.
g3 <- gtable::gtable_add_rows(g2,heights = grobHeight(title) + padding, pos = 0)
# add title into that space.
g3 <- gtable::gtable_add_grob(g3,title,1,1,1,ncol(g3))
# draw the chart.
grid::grid.draw(g3)
ggsave("ASHWChart.tiff",plot = g3)