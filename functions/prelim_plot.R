#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: prelim_plot
# Coder: James Maze
# Date: 13 Jan 2021
# Purpose: Quick dygraph for SpC data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

prelim_plot <- function(temp){
  
  #Select cols of interest
  xts_form <- temp %>% 
    select(c(Timestamp, SpC_low_range))
  
  #Convert data to xts
  xts_form <- xts(xts_form, order.by = xts_form$Timestamp)
  
  #Plot data with dygraphs
  dygraph(xts_form) %>% 
    dyRangeSelector() %>% 
    dyLegend() 
}
