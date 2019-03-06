library(Quandl)
library(tidyverse)
library(readr)

#Set Quandl API
Quandl.api_key(Sys.getenv("API_QUANDL"))


#fetch data
fundamentals_data <- Quandl.datatable("SHARADAR/SF1", paginate = T)


write_csv(fundamentals_data,'companies_fundamentals_data.csv')

