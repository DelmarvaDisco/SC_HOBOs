#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: prelim_plot
# Coder: James Maze
# Date: 13 Jan 2021
# Purpose: Quick dygraph
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

prelim_plot <- function(temp){
  
  #convert to an xts
  xts_form <- temp %>% 
    select(c(Timestamp, SpC_low_range))
  
  xts_form <- xts(xts_form, order.by = xts_form$Timestamp)
  
  #Plot data
  
  dygraph(xts_form) %>% 
    dyRangeSelector() %>% 
    dyLegend() 
}
