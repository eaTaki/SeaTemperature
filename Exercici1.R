
##Exercises 1.a and 1.b

library(readr)

sea.deep = data.frame()
sea.pred = data.frame()

years = seq(2000, 2017)
csv_cols = c('row', 'col', 'ref', 'value')
col_names = c('mes', 'fondària', 'any', 'temperatura')



for (y in years){
  
  # Individual year
  df <- read.csv(paste("https://api.idescat.cat/rc/v1/aec/15196/1/ssv?t=", y, '00', sep = ''), sep=';')
  df = df[csv_cols]
  names(df) = col_names
  df['any'] = y
  sea.deep <- rbind(sea.deep,df)
  
  # Averaged preceding
  df <- read.csv(paste("https://api.idescat.cat/rc/v1/aec/15196/2/ssv?t=", y, '00', sep = ''), sep=';')
  df = df[csv_cols]
  names(df) = col_names
  df['any'] = y
  sea.pred <- rbind(sea.pred,df)
}

# Fixing format and making numerical
sea.deep$fondària <- parse_number(sea.deep$fondària)
sea.deep$temperatura <- as.numeric(gsub(",", ".", sea.deep$temperatura, fixed = TRUE))
sea.pred$fondària <- parse_number(sea.pred$fondària)
sea.pred$temperatura <- as.numeric(gsub(",", ".", sea.pred$temperatura, fixed = TRUE))

##Exercise 1.c
cat("Dimension df_yearly\n")
dim_sea.deep <- dim(sea.deep)
cat("Rows (obs.):", dim_sea.deep[1], "\n")
cat("Columns (var.):", dim_sea.deep[2], "\n")
print(str(sea.deep)) #Structure of the df, dimensions, variables and data types
print(summary(sea.deep)) #Descriptive statistical summary: mean, quantiles, min, max, NAs...
print(sum(is.na(sea.deep))) #Just to make sure 

##Exercise 1.d - Opció 1 - Global (2000-2017)
library(dplyr)
library(ggplot2) #Execute libraries

temp_dy <- sea.deep %>% 
  group_by(fondària, any) %>% #Variables of interest
  summarise(
    temp_mitja = mean(temperatura),
    .groups = 'drop'
  ) #New variable average temperature per depth per year
print(temp_dy)

ggplot(temp_dy, aes( #Means of boxplots generation
  x = as.numeric(fondària), 
  y = temp_mitja, 
  group = fondària ####No faig que fondaria sigui factor, pero si no poso group, no xuta...
)) +              ####Decidir si ok factor; llavors fill no group; fill més maco
  geom_boxplot() +
  labs(
    title = "2000-2017 Mitjana de temperatura per fondària",
    subtitle = "Variació de la temperatura anual per cada fondària",
    x = "fondària (m)",
    y = "Temperatura mitjana anual (°C)"
 ) +
    theme_minimal() +
    theme(legend.position = "none")
  
print(temp_dy)

##Exercise 1.d - Opció 2 - Anys individuals

ggplot(sea.deep, aes(x = factor(fondària), y = temperatura)) +
  geom_boxplot() +
  facet_wrap(~any) +
  labs(
    title = "Boxplot de la temperatura per fondària (2000-2017)",
    x = "Fondària (m)", 
    y = "Temperatura (°C)"
  )

##Exercise 1.e 

stats1 <- sea.deep %>%
  group_by(fondària, any) %>%
  summarise(
    Mitjana = mean(temperatura),
    Mediana = median(temperatura),
    SD = sd(temperatura),
    IQR = IQR(temperatura),
    Min = min(temperatura), #Other statistics of interest
    Max = max(temperatura),
    Range = Max - Min, #Difference between max and min values
    CV = SD / Mitjana, #Coefficient of Variation (CV)
    .groups = 'drop'
   ) 

print(stats1) 
View(stats1) #Average temperature per depth per year

stats2 <- temp_dy %>%
  group_by(fondària) %>%
  summarise(
    Mitjana = mean(temp_mitja),
    Mediana = median(temp_mitja),
    SD = sd(temp_mitja),
    IQR = IQR(temp_mitja),
    Min = min(temp_mitja), #Other statistics of interest
    Max = max(temp_mitja),
    Range = Max - Min, #Difference between max and min values
    CV = SD / Mitjana, #Coefficient of Variation (CV)
    .groups = 'drop'
  )

print(stats2) 
View(stats2) #Average temperature per depth across 2000-2017   

##Exercise 1.f 

stats1 <- stats1 %>%
  mutate(fondària = factor(fondària, levels = c(0, -20, -50, -80)))

ggplot(stats1, aes(
  x = any, 
  y = Mitjana, 
  color = fondària)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Variació anual de la temperatura mitjana per fondària",
    subtitle = " (2000-2017) Mar Mediterrani, punt d'observació: 42º 03' N, 3º 15' E", #####Acabar de decidir títols i subtítols
    x = "Any",
    y = "Temperatura mitjana (°C)",
    color = "Fondària (m)"
  ) +
  theme_minimal()

##Exercise 1.g

####Dues interpretacions: afegir a sea.deep noves variables i canviar nom; crear excel de 0 amb les noves variables:
####Crear Excel de 0:
library(openxlsx)

new_var <- createWorkbook()
#Average temperature per depth per year
addWorksheet(new_var, "Stats_per_any")
writeData(new_var, "Stats_per_any", stats1)
#Average temperature per depth across 2000-2017   
addWorksheet(new_var, "Stats_globals")
writeData(new_var, "Stats_globals", stats2)
#Save on a new excel file
saveWorkbook(new_var, "NUEVO.xlsx", overwrite = TRUE)




