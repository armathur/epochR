library(Quandl)
library(tidyverse)
library(readr)

set.seed(20)

Quandl.api_key(Sys.getenv("API_QUANDL"))
# Constatnt value assigned to revenue
WEIGHT = 0.4 #range from 0 to 1
DELAY_MODELS = 8




# RUN ONLY WHEN: data does not come in with the github repo
# fundamentals_data <- Quandl.datatable("SHARADAR/SF1")
# write_csv(fundamentals_data,'companies_fundamentals_data.csv')

fundamentals<- read_csv("companies_fundamentals_data.csv")

#select tickrs that have 8 years worth of data
qualifying_tickers <- names(which(table(fundamentals$ticker) == DELAY_MODELS))

#subset the data
fundamentals_qualified <- subset(fundamentals, fundamentals$ticker %in% qualifying_tickers)


#aggregate data by avg over income, revenue and assets over 10 years by ticker name
aggregate_by_ticker <- aggregate(fundamentals_qualified[c( "assets", "netinccmnusd", "revenueusd")], 
                                 by = list(fundamentals_qualified$ticker), 
                                 mean)


aggregate_by_ticker$avg_income_ratio <- round(aggregate_by_ticker$netinccmnusd/aggregate_by_ticker$assets,2)
aggregate_by_ticker$avg_revenue_ratio <- round(aggregate_by_ticker$revenueusd/aggregate_by_ticker$assets,2)
aggregate_by_ticker$weighted_avg <- (aggregate_by_ticker$avg_revenue_ratio * WEIGHT) + (aggregate_by_ticker$avg_income_ratio * (1-WEIGHT))


head(aggregate_by_ticker)
#check num of NAs
colSums(is.na((aggregate_by_ticker)))

# Drop All NAs
aggregate_by_ticker<- drop_na(aggregate_by_ticker)

#Double Check
colSums(is.na((aggregate_by_ticker)))
head(aggregate_by_ticker)


clusters <- kmeans(aggregate_by_ticker[,7],5)
aggregate_by_ticker$cluster <- clusters$cluster



# #create a new dataframe with ticker information and AVG revenue, income and assests value
# 
# analytical_dataset <- as.data.frame(qualifying_tickers)
# 
# #append avg assests data across 8 years
# 
