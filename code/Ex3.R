ex3 = function(sea.deep, sea.pred){
  
  ################## A #################
  plot.a = sea.deep %>%
    #Remove annual averages to focus only on monthly data 
    filter(mes != 'Annual Average') %>%
    #Group by year and depth to calculate changes within specific time frames
    group_by(any, fondaria) %>%
    summarise(
      diff = diff(temperatura), #Difference between consecutive temperature values
      x = 1:11,
      .groups = 'drop_last'
      
    ) %>% #Visualize the rate of change per month
    ggplot(aes(x=x, y = diff, color = factor(fondaria))) +
    geom_line() +
    geom_point() +
    facet_wrap(~ any, ncol = 4)+
    labs(
      title = "Month-to-Month Temperature Change",
      x = "Month",
      y = "Temperature Change (°C) (Month N - Month N-1)",
      color = "Depth (m)"
    ) +
    theme_minimal()
  
  
  ################## B #################
  sea.deep$tempHist = sea.pred$temperatura
  
  plot.b1 = sea.deep %>% #Compare current temperatures against historical
    filter(mes != 'Annual Average') %>%
    group_by(any, fondaria) %>%
    summarise(
      #Calculate the deviation from the historical average for that specific month/depth
      diff = temperatura-tempHist,
      x = 1:12,
      .groups = 'drop_last'
      
    ) %>%
    ggplot(aes(x=x, y = diff, color = factor(fondaria))) +
    geom_line() +
    geom_point() +
    facet_wrap(~ any, ncol = 4)+
    labs(
      title = "Current year to previous 30 years",
      x = "Month",
      y = "Temperature Change (°C) (Month N - Hist Month N)",
      color = "Depth (m)"
    ) +
    theme_minimal()
  
  library(dplyr)
  library(ggplot2)
  
  
  mes_levels = c("January", "February", "March", "April", "May", "June", 
            "July", "August", "September", "October", "November", "December")
  
  plot.b2 <- sea.deep %>% #Average seasonal cycle across all years in the dataset
    filter(mes != 'Annual Average') %>%
    mutate(mes = factor(mes, levels = mes_levels)) %>% #'Mes' to factor so that x-axis follows calendar order 
        group_by(mes, fondaria) %>%
    summarise(
      mean = mean(temperatura), #Calculate mean temperature for each month
      .groups = 'drop_last'
    ) %>%
    
    ggplot(aes(x = mes, y = mean, color = factor(fondaria), group = factor(fondaria))) +
    geom_line() +
    geom_point() +
    
    labs(
      title = "Average monthly changes",
      x = "Month",
      y = "Mean Temperature (°C)", # Changed Y-axis label to reflect the mean temperature being plotted
      color = "Depth (m)"
    ) +
    theme_minimal()
  
  # print(plot.b2) #Display
  
  list(a = plot.a, b1 = plot.b1, b2 = plot.b2) #Return plots
    
    
}