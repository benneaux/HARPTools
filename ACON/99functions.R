require(ggplot2)
require(dplyr)
require(grid)
library(gtable)
require(ggthemes)

fig_size <- c(9, 5) # inches
year.period.plot <- function(chartdata, period, col, title, ylab){

  chartdata <- as.data.frame(chartdata) %>%
    mutate(maxmin = case_when(max(chartdata[[col]]) == chartdata[[col]] ~ "max",
                              min(chartdata[[col]]) == chartdata[[col]] ~ "min",
                              chartdata[[col]] == chartdata[[col]]  ~ "none"))
  
    minn = chartdata[which.min(chartdata[[col]]),col]
    maxn = chartdata[which.max(chartdata[[col]]),col]
    y0 = floor(minn*0.9)
  
    if(length(unique(chartdata[[period]])) == 12){  # i.e. you're plotting years
      
      perioddiv = 12
      yeardiv = length(chartdata[[period]])%%12                      # remainder
      yearwhole = length(chartdata[[period]])%/%12        # number of full years 
      
    } else {                       # if the periods we are plotting are quarters
      
      perioddiv = 4
      yeardiv = length(chartdata[[period]])%%4                       # remainder
      yearwhole = length(chartdata[[period]])%/%4         # number of full years
      
    }
    
    if(yeardiv == 0){
      if(yearwhole == 2) {
        linelocs = 0
      } else{
        linelocs = yearwhole-2
      }
    } else {
      linelocs = yearwhole-1
    }
    
    periodn = length(chartdata[[period]])+1          # number of periods to plot
    
  
    
    g1 <-  ggplot(data = chartdata, 
                  aes(x = interaction(chartdata[["Year"]], # Keeps the order of
                                      chartdata[[period]], # qtr and year intact
                                      lex.order = TRUE),   
                      y = chartdata[[col]],
                      group = 1)) +
      
    geom_line(colour = "grey", size = 1.5) +             # creates the main line
  
    coord_cartesian(ylim = c(y0, ceiling(maxn*1.05)),
                    expand = FALSE) +                     # adjusts the y - axis
    
    scale_y_continuous(breaks = c(minn,maxn)) +     # marks max and min val on y
    
    theme_few(base_size = 16) +
    
    theme(
      legend.position="none",                                  # Turn off legend
      plot.margin = unit(c(1, 1, 5, 1),
                         "lines"),                    # Adjust margins of window
      axis.title.x = element_blank(),                    # Turn off x axis title
      axis.text.x = element_blank(),                    # Turn off x axis labels
      axis.ticks.x = element_line(),                       # Add in x axis ticks
      axis.ticks.length = unit(2,"mm"),                # Make those ticks longer
      panel.grid.major.x = element_blank(),          # Turn off major grid lines
      panel.grid.minor.x = element_blank()) +        # Turn off minor grid lines
    
    ylab(ylab) +
    
    ############################################################################
    #
    # Y AXIS: grid lines at max and min values. 
    #
    ############################################################################   
    
    annotate(geom = "segment",                                 # maximum y value
              x = 0,
              xend = periodn,
              y = maxn,
              yend = maxn,
              colour = "green",
              linetype = 5) +
    annotate(geom = "segment",                                 # minimum y value
              x = 0,
              xend = periodn,
              y = minn,
              yend = minn,
              colour = "red",
              linetype = 5) +
  
    
    geom_point(aes(group = 2,                 # Colour in the max and min values 
                   colour = maxmin), 
               size = 4) + 
    
    scale_color_manual(values = c("green", "red", NA)) +
  
  
    ############################################################################
    #
    # Lines 
    #
    ############################################################################
    # X AXIS: horizontal separator between year and quarter
    annotate(geom = "segment",
             x = 0,
             xend = nrow(chartdata)+1,
             y = y0-10,
             yend = y0-10,
             colour = "grey",
             size = 0.5) +
      
    # X AXIS: vertical lines dividing years
    annotate(
      geom = "segment",
      x = (perioddiv + 0.5) + perioddiv * c(0:linelocs),
      xend = (perioddiv + 0.5) + perioddiv * c(0:linelocs),
      y = y0,
      yend = y0-13.5,
      colour = "grey",
      linetype = 1) +
      
    # X AXIS: vertical lines at start and end of separator.
    annotate(geom = "segment",
             x = 1 * c(0,(periodn)),
             xend = 1 * c(0,(periodn)),
             y = y0,
             yend = y0-14,
             colour = "grey") +
    
    ############################################################################
    #
    # LABELS 
    #
    ############################################################################
    
    # X AXIS: quarter labels
    annotate(geom = "text",
             x = seq_len(nrow(chartdata)),
             y = y0-5,
             label = chartdata[[period]],
             size = 4.5,
             angle = 90) +  
      
    if(yeardiv == 0) {
  
    # X AXIS: year labels
      annotate(
        geom = "text",
        x = (perioddiv/2 + 0.5) + perioddiv * (0:(yearwhole - 1)), 
        y = y0-13,
        label = unique(chartdata[,"Year"]),
        size = 5)
  
    } else {
  
      # X AXIS: year labels
      annotate(
        geom = "text",
        x = c((perioddiv/2 + 0.5) + perioddiv*(0:(yearwhole-1)),
              (2/perioddiv + yeardiv-0.25) + perioddiv*yearwhole),
        y = y0-13,
        label = unique(chartdata[,"Year"]),
        size = 5)
    }
    ############################################################################
    #
    # Grid Draw
    #
    ############################################################################    
    
    g2 =  ggplot_gtable(ggplot_build(g1))     # remove clipping of x axis labels
    
    g2$layout$clip[g2$layout$name == "panel"] <- "off"          # Turn panel off
    
    g2$vp <- viewport(width = unit(fig_size[1],"in"),     # set viewport to size
                      height = unit(fig_size[2], "in"))
    
    
    titlegrob <- textGrob(                                # Create Title element
      title, 
      gp = gpar(fontsize = 16)
      )
    
    padding <- unit(3,"mm")             # padding = distance from title to chart
    
    g3 <- gtable_add_rows(         # add space for the title on top of the chart
      g2,
      heights = grobHeight(titlegrob) + padding, 
      pos = 0)
    
    
    g3 <- gtable_add_grob(g3,titlegrob,1,1,1,ncol(g3))    # add title into space
    
    grid.draw(g3)                                               # draw the chart
}

