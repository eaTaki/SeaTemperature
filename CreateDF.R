library(readr)

download_data = function(filename){

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

save(sea.deep, sea.pred, file=filename)



}