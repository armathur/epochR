library(Quandl)
library(tidyverse)
library(readr)

Quandl.api_key(Sys.getenv("API_QUANDL"))
fundamentals_data <- Quandl.datatable("SHARADAR/SF1")
write_csv(fundamentals_data,'companies_fundamentals_data.csv')

