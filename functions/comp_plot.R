#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Comparative Plot
# Coder: James Maze
# Date: 2 Feb 2023
# Purpose: Comparing raw and processed SpC data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

comp_plot <- function(raw, temp) {
  
  #Bind temp and raw data with "v" to designate the version of data
  plot_data <- bind_rows(raw %>% mutate(v = "raw"), 
                         temp %>% mutate(v = "temp")) %>% 
    #Multiply Temp by 7x for dual axis plot
    mutate(Temp_C_7x = Temp_C * 7)
  
  #Plot data
  plot <- ggplot(data = plot_data, 
                 mapping = aes(x = Timestamp,
                               color = v)) +
    geom_line(mapping = aes(x = Timestamp,
                            y = SpC_low_range)) #+
    # #Adding temp helps identify when sensor is out of the water
    # geom_point(mapping = aes(x = Timestamp, 
    #                          y = Temp_C_7x))
  
  return(plot)
  print(plot)
  
}
