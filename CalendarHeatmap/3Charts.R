# Plot
ggplot(df, aes(monthf, monthweek, fill = adjmonthweektotal)) + 
  geom_tile(colour = "grey",linetype = 1) + 
  facet_grid(year~.) + 
  scale_fill_gradient2(limits=c(20,max(df$adjmonthweektotal)), 
                       low="#c0392b", 
                       mid = "white",
                       high="#1e8449",
                       na.value = "#c0392b",
                       midpoint = median(df$adjmonthweektotal)) +
  labs(x="Month",
       y="",
       title = "Occasions of Service Heatmap", 
       subtitle = "Weighted OOS* per Week by Month & Year \n-- (*Total/Number of days in week)", 
       fill="") +
  # geom_text(aes(label = round(df$monthweekdays, 1)), alpha= 0.1) +
  ggthemes::theme_tufte(base_size = 16)
