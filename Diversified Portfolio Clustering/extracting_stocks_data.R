library(Quandl)
library(tidyverse)
library(readr)
library(factoextra)
library(cluster)
library(gridExtra)

#Set Quandl API
# Quandl.api_key(Sys.getenv("API_QUANDL"))


#fetch data
# fundamentals_data <- Quandl.datatable("SHARADAR/SF1", paginate = T)
# 
# 
# write_csv(fundamentals_data,'companies_fundamentals_data.csv')
# 
# set.seed(20)

# Quandl.api_key(Sys.getenv("API_QUANDL"))
# Constatnt value assigned to revenue
WEIGHT = 0.4 #range from 0 to 1
DELAY_MODELS = 8




# RUN ONLY WHEN: data does not come in with the github repo
# fundamentals_data <- Quandl.datatable("SHARADAR/SF1")
# write_csv(fundamentals_data,'companies_fundamentals_data.csv')

fundamentals <- read_csv("companies_fundamentals_data.csv")



#select tickrs that have 8 years worth of data
qualifying_tickers <-
  names(which(table(fundamentals$ticker) == DELAY_MODELS))

#subset the data
fundamentals_qualified <-
  subset(fundamentals, fundamentals$ticker %in% qualifying_tickers)


#aggregate data by avg over income, revenue and assets over 10 years by ticker name
aggregate_by_ticker <-
  aggregate(fundamentals_qualified[c("assets", "netinccmnusd", "revenueusd")],
            by = list(fundamentals_qualified$ticker),
            mean)


aggregate_by_ticker$avg_income_ratio <-
  round(aggregate_by_ticker$netinccmnusd / aggregate_by_ticker$assets,
        2)
aggregate_by_ticker$avg_revenue_ratio <-
  round(aggregate_by_ticker$revenueusd / aggregate_by_ticker$assets,
        2)
aggregate_by_ticker$weighted_avg <-
  (aggregate_by_ticker$avg_revenue_ratio * WEIGHT) + (aggregate_by_ticker$avg_income_ratio * (1 -
                                                                                                WEIGHT))


head(aggregate_by_ticker)
#check num of NAs
colSums(is.na((aggregate_by_ticker)))

# Drop All NAs
aggregate_by_ticker <- drop_na(aggregate_by_ticker)

#Double Check
colSums(is.na((aggregate_by_ticker)))
head(aggregate_by_ticker)


# creating clusters with centers ranging from 2 to 7

clusters_2 <- kmeans(aggregate_by_ticker[, 7], 2, nstart = 25)
clusters_3 <- kmeans(aggregate_by_ticker[, 7], 3, nstart = 25)
clusters_4 <- kmeans(aggregate_by_ticker[, 7], 4, nstart = 25)
clusters_5 <- kmeans(aggregate_by_ticker[, 7], 5, nstart = 25)
clusters_6 <- kmeans(aggregate_by_ticker[, 7], 6, nstart = 25)
clusters_7 <- kmeans(aggregate_by_ticker[, 7], 7, nstart = 25)

# creating visualizations

p2 <- fviz_cluster(clusters_2, aggregate_by_ticker[, 2:7])
p3 <- fviz_cluster(clusters_3, aggregate_by_ticker[, 2:7])
p4 <- fviz_cluster(clusters_4, aggregate_by_ticker[, 2:7])
p5 <- fviz_cluster(clusters_5, aggregate_by_ticker[, 2:7])
p6 <- fviz_cluster(clusters_6, aggregate_by_ticker[, 2:7])
p7 <- fviz_cluster(clusters_7, aggregate_by_ticker[, 2:7])




# visualize different clusters
grid.arrange(p5, p2, p3, p4, p6, p7, nrow = 3)




# determine the right number of K

# Using WSS
fviz_nbclust(aggregate_by_ticker[, 2:7], kmeans, method = "wss")

# Using silhouette
fviz_nbclust(aggregate_by_ticker[, 2:7], kmeans, method = "silhouette")

# using Gap Stat
gap_stat <-
  clusGap(
    aggregate_by_ticker[, 2:7],
    FUN = kmeans,
    nstart = 25,
    K.max = 10,
    B = 50
  )
fviz_gap_stat(gap_stat)



#### FOR STASH ::::::

# Subset only tickers that are in Stash
fundamentals_stash <-
  subset(fundamentals, fundamentals$ticker %in% stash_tickers)

# find qualifying tickers (with 8 years worth of data)

qualifying_tickers_stash <-
  names(which(table(fundamentals_stash$ticker) == DELAY_MODELS))


fundamentals_stash <-
  subset(fundamentals_stash, fundamentals_stash$ticker %in% qualifying_tickers_stash)



aggregate_by_ticker_stash <-
  aggregate(fundamentals_stash[c("assets", "netinccmnusd", "revenueusd")],
            by = list(fundamentals_stash$ticker),
            mean)


aggregate_by_ticker_stash$avg_income_ratio <-
  round(aggregate_by_ticker_stash$netinccmnusd / aggregate_by_ticker_stash$assets,
        2)
aggregate_by_ticker_stash$avg_revenue_ratio <-
  round(aggregate_by_ticker_stash$revenueusd / aggregate_by_ticker_stash$assets,
        2)
aggregate_by_ticker_stash$weighted_avg <-
  (aggregate_by_ticker_stash$avg_revenue_ratio * WEIGHT) + (aggregate_by_ticker_stash$avg_income_ratio * (1 -
                                                                                                WEIGHT))


head(aggregate_by_ticker_stash)
#check num of NAs
colSums(is.na((aggregate_by_ticker_stash)))

# Drop All NAs
aggregate_by_ticker_stash <- drop_na(aggregate_by_ticker_stash)

#Double Check
colSums(is.na((aggregate_by_ticker_stash)))
head(aggregate_by_ticker_stash)


# Finally we have about 118 tickers to work with


# determine the right number of K

# Using WSS
fviz_nbclust(aggregate_by_ticker_stash[, 2:7], kmeans, method = "wss")

# Using silhouette
fviz_nbclust(aggregate_by_ticker_stash[, 2:7], kmeans, method = "silhouette")

# using Gap Stat
gap_stat <-
  clusGap(
    aggregate_by_ticker_stash[, 2:7],
    FUN = kmeans,
    nstart = 25,
    K.max = 10,
    B = 50
  )
fviz_gap_stat(gap_stat)


clusters_2 <- kmeans(aggregate_by_ticker_stash[, 7], 2, nstart = 25)
clusters_3 <- kmeans(aggregate_by_ticker_stash[, 7], 3, nstart = 25)
clusters_4 <- kmeans(aggregate_by_ticker_stash[, 7], 4, nstart = 25)
clusters_5 <- kmeans(aggregate_by_ticker_stash[, 7], 5, nstart = 25)
clusters_6 <- kmeans(aggregate_by_ticker_stash[, 7], 6, nstart = 25)
clusters_7 <- kmeans(aggregate_by_ticker_stash[, 7], 7, nstart = 25)

# creating visualizations

p2 <- fviz_cluster(clusters_2, aggregate_by_ticker_stash[, 2:7])
p3 <- fviz_cluster(clusters_3, aggregate_by_ticker_stash[, 2:7])
p4 <- fviz_cluster(clusters_4, aggregate_by_ticker_stash[, 2:7])
p5 <- fviz_cluster(clusters_5, aggregate_by_ticker_stash[, 2:7])
p6 <- fviz_cluster(clusters_6, aggregate_by_ticker_stash[, 2:7])
p7 <- fviz_cluster(clusters_7, aggregate_by_ticker_stash[, 2:7])




# visualize different clusters
grid.arrange(p2, p3, p4, p5, p6, p7, nrow = 3)






######## Unused code
# table(clusters$cluster)
# aggregate_by_ticker$cluster <- clusters$cluster
#
#
# ggplot(
#   aggregate_by_ticker,
#   aes(
#     aggregate_by_ticker$avg_income_ratio,
#     aggregate_by_ticker$avg_revenue_ratio,
#     color = as.factor(aggregate_by_ticker$cluster)
#   )
# ) + geom_point()
