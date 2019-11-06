count <- read.csv("Dataframe_Dates.csv", header =T,stringsAsFactors = F)
?read.csv
library(dplyr); library(tidyr); library(ggplot2)
count$A.Date <- as.Date(count$A.Date, format='%d/%m/%Y')
count$H.Date <- as.Date(count$H.Date, format='%d/%m/%Y')

test1<-count$A.Date[20]
test2<- count$A.Date[1]
str(count$A.Date)
difftime(test1, test2, units="days")

date1 <- list()
date2 <- list()
D.O.R.A <- matrix(ncol = 2, nrow = nrow(count))

#column of home team days of rest

for(i in 6:20){ 
  for (j in (i-1):1){
    if (count$H.Team.ID[i] == count$Away.Team.ID[j] | count$H.Team.ID[i] == count$H.Team.ID[j]){
      mutate(date.lag[i] = date - lag(date, 1))
    }
    D.O.R.A[i,2] <- as.integer(date2 - date1 - 1)
  }
}

D.O.R.A[1:20,]
# 
# for(i in 1:20){ 
#   for (j in (i-1):1){
#     if (count$Away.Team.ID[i] == count$Away.Team.ID[j] | count$Away.Team.ID[i] == count$H.Team.ID[j]){
#       date2 <- as.Date(count$H.Date[j])
#       date1 <- count$A.Date[i]
#     }
#     D.O.R.A[i,1] <- as.integer(date2 - date1 - 1)
#   }
# }

count1<- as.list(countfile)

count$A.Date <- as.Date(count$A.Date, format='%d/%m/%Y')
#count1$H.Date <- as.Date(count$H.Date, format='%d/%m/%Y')

df <-  count %>% 
  mutate(A.Date = as.Date(A.Date)) %>% 
  select( Game.ID, A.Date, Away.Team.ID, H.Team.ID) %>% 
  rename( date = A.Date)

View(df)

df$date.lag <- NaN
for(i in 1:22){ 
  for (j in (i-1):1){
      if (df$Away.Team.ID[i] == df$Away.Team.ID[j] | df$Away.Team.ID[i] == df$H.Team.ID[j]){
      mutate(count, date.lag[i] <- as.integer(date[i] - date[j]))
    }
  }
}
