ex3 = function(sea.deep, sea.pred){
  
  ################## A #################
  plot.a = sea.deep %>%
    filter(mes != 'Mitjana anual') %>%
    group_by(any, fondària) %>%
    summarise(
      diff = diff(temperatura),
      x = 1:11,
      
    ) %>%
    ggplot(aes(x=x, y = diff, color = factor(fondària))) +
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
  
  plot.b1 = sea.deep %>%
    filter(mes != 'Mitjana anual') %>%
    group_by(any, fondària) %>%
    summarise(
      diff = temperatura-tempHist,
      x = 1:12,
      
    ) %>%
    ggplot(aes(x=x, y = diff, color = factor(fondària))) +
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
  
  
  mes_levels <- c("Gener", "Febrer", "Març", "Abril", "Maig", "Juny", 
                  "Juliol", "Agost", "Setembre", "Octubre", "Novembre", "Desembre")
  
  plot.b2 <- sea.deep %>%
    filter(mes != 'Mitjana anual') %>%
    mutate(mes = factor(mes, levels = mes_levels)) %>%
        group_by(mes, fondària) %>%
    summarise(
      mean = mean(temperatura),
      .groups = 'drop'
    ) %>%
    
    ggplot(aes(x = mes, y = mean, color = factor(fondària), group = factor(fondària))) +
    geom_line() +
    geom_point() +
    
    labs(
      title = "Average monthly changes",
      x = "Month",
      y = "Mean Temperature (°C)", # Changed Y-axis label to reflect the mean temperature being plotted
      color = "Depth (m)"
    ) +
    theme_minimal()
  
  print(plot.b2)
  
  list(a = plot.a, b1 = plot.b1, b2 = plot.b2)
    
    
}