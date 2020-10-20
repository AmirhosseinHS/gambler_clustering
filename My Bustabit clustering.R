# **Clustering Bustabit gamblers behaviour**

#The purpose of this project is to cluster online gamblers based on their behaviour.

# importing library & data
library(tidyverse)
library(GGally)

bustabit <- read_csv("~/Desktop/My Courses/Projects/bustabit.csv")

# 'Bet': The number of Bits bet by a gambler
# 'CashedOut': The multiplier at which this particular gambler cashed out

head(bustabit)

# Preprocessing
# set 'NA' in 'CashedOut' to be one unit greater than 'BustedAt', the user failed to out before busting
# set 'NA' in 'Profit' to 0
# define 'Loss' to show the value a player lost
# define 'win' & 'lose' to indicate the game outcome
bustabit_processed <- bustabit %>% 
  mutate(CashedOut = ifelse(is.na(CashedOut), BustedAt + 0.01, CashedOut),
         Profit = ifelse(is.na(Profit), 0, Profit),
         Loss = ifelse(Profit == 0, -1*Bet, 0),
         Win = ifelse(Profit == 0, 0, 1),
         Lose = ifelse(Profit == 0, 1, 0))

head(bustabit_processed)

# Summerizing gamblers profiles
gamblers_profile <- bustabit_processed %>%
  group_by(Username) %>%
  summarize(AverageCashedOut = mean(CashedOut), 
            AverageBet = mean(Bet),
            TotalProfit = sum(Profit),
            TotalLoss = sum(Loss), 
            GamesWin = sum(Win),
            GamesLose = sum(Lose))

head(gamblers_profile)

# Standardization
# Removing usernames because they are not numeric
gamblers_unnamed <- gamblers_profile[-1]

standardized <- scale(gamblers_unnamed)

summary(standardized)

# Finding the number of clusters using the Elbow method
set.seed(20190101)
wcss = vector()
for (i in 1:15) wcss[i] = sum(kmeans(standardized, i,nstart = 6)$withinss)
plot(1:15,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# At 5, adding one more cluster, does not improve WCSS considerably
# So, number of clusters should be 5.

# Clustering Gamblers
clusterer <- kmeans(standardized, centers = 5)

# Assign the cluster number of each gambler in new column
gamblers_profile$cluster <- as.factor(clusterer$cluster)

table(gamblers_profile$cluster)

# clusters statistics
gamblers_profile_avg <- gamblers_profile %>%
  group_by(cluster) %>%
  summarize_if(is.numeric,mean)


print(gamblers_profile_avg)

# visualising the clusters
normalizer <- function(x) {(x - min(x))/(max(x)-min(x))}

gamblers_profile_avg_norm <- gamblers_profile_avg %>%
  mutate_if(is.numeric, normalizer) 

ggparcoord(gamblers_profile_avg_norm, columns = 2:ncol(gamblers_profile_avg_norm), groupColumn = "cluster", scale = "globalminmax", order = "skewness")

# Conclusion: This data can be interpreted by number of gamblers and their profiles in each cluster.
table(gamblers_profile$cluster)

# There are few risk takers who bet on high multiplier, or bet large amount of money. While, the former group seems to be passersby because they play the least number of games, the latter play more often.

# the largest cluster tend to keep the multiplier low, but bet on the relatively more considerable amount of money. Although, they do not play very often, this is the only cluster with a net negative income.

# The second largest cluster, are also at the second place in terms of number of games to play. However, they bet on average multipliers and average amount of money.

# The most addicted group, bet on the least amount of money and relatively low multiplier.