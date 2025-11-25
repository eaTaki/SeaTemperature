
df_yearly = data.frame()
df_cum = data.frame()
years = seq(2000, 2017)

csv_cols = c('row', 'col', 'ref', 'value')
col_names = c('mes', 'fondaria', 'any', 'temperatura')



for (y in years){
  
  # Individual year
  df <- read.csv(paste("https://api.idescat.cat/rc/v1/aec/15196/1/ssv?t=", y, '00', sep = ''), sep=';')
  df = df[csv_cols]
  names(df) = col_names
  df['any'] = y
  df_yearly <- rbind(df_yearly,df)
  
  # Averaged preceding
  df <- read.csv(paste("https://api.idescat.cat/rc/v1/aec/15196/2/ssv?t=", y, '00', sep = ''), sep=';')
  df = df[csv_cols]
  names(df) = col_names
  df['any'] = y
  df_cum <- rbind(df_cum,df)
}