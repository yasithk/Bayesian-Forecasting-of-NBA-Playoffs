count <- read.csv("XXX1.csv", header =T,stringsAsFactors = F)
library(dplyr); library(tidyr); library(ggplot2)
count$A.Date <- as.Date(count$A.Date, format='%d/%m/%Y')
count$H.Date <- as.Date(count$H.Date, format='%d/%m/%Y')

#day of rest = number of full days in between games

df <-  count %>% 
  mutate(A.Date = as.Date(A.Date)) %>% 
  select( Game.ID, A.Date, Away.Team.ID, H.Team.ID) %>% 
  rename( date = A.Date)

#Away Team days of rest
df$Away.Team.DaysofRest <- NaN
for(i in 1230:2){ 
  for (j in (i-1):1){
    if (df$Away.Team.ID[i] == df$Away.Team.ID[j] || df$Away.Team.ID[i] == df$H.Team.ID[j]){
      df$Away.Team.DaysofRest[i] = as.integer(df$date[i] - df$date[j])
      break
    }
  }
}


#Home Team days of rest
df$Home.Team.DaysofRest <- NaN
for(i in 1230:2){ 
  for (j in (i-1):1){
    if (df$H.Team.ID[i] == df$H.Team.ID[j] || df$H.Team.ID[i] == df$Away.Team.ID[j]){
      df$Home.Team.DaysofRest[i] = as.integer(df$date[i] - df$date[j])
      break
    }
  }
}

#all-star break is beteween the 2017-02-19 until 
df$Home.Team.DaysofRest[802:816] <- NaN
df$Away.Team.DaysofRest[802:818] <- NaN

# #score dif home - away
# scoredif <- count$H.Score - count$A.Score
# cor(scoredif, df$Home.Team.DaysofRest)

count$DaysofRest.Away.Team <- df$Away.Team.DaysofRest
count$DaysofRest.Home.Team  <- df$Home.Team.DaysofRest

count <- count[, c(1,5, 2:4, 60, 6:10, 61, 12:59)]

count <- count %>% rename(Date=A.Date)



#write.csv(count, "daysofrestdata1.csv", row.names = FALSE)

count$DaysofRest.Away.Team[is.na(count$DaysofRest.Away.Team)] <- 0
count$DaysofRest.Home.Team[is.na(count$DaysofRest.Home.Team)] <- 0

write.csv(count, "XXX2.csv", row.names = FALSE)



#attach home_week and away_week
dataframe1.0 <- read.csv("daysofrestdata1.csv", header = T)
#create lists
dataframe1.0$home_week <- c(); dataframe1.0$away_week <- c();


for (g in 1:1230) {
  dataframe1.0$home_week[g] <- sum(dataframe1.0$H.Team.ID[1:g] == dataframe1.0$H.Team.ID[g]) +
    sum(dataframe1.0$Away.Team.ID[1:g] == dataframe1.0$H.Team.ID[g])
  dataframe1.0$away_week[g] <- sum(dataframe1.0$Away.Team.ID[1:g] == dataframe1.0$Away.Team.ID[g]) +
    sum(dataframe1.0$H.Team.ID[1:g] == dataframe1.0$Away.Team.ID[g])
}


dataframe1.2 <- dataframe1.0[, c(1:5, 62, 6:11, 61, 12:60)]

write.csv(dataframe1.2, "daysofrestdata1.1.csv", row.names = FALSE)

####################
dataframe1.0 <- read.csv("daysofrestdata2.csv", header = T)
dataframe1.0$home_week <- c(); dataframe1.0$away_week <- c();

for (g in 1:1230) {
  dataframe1.0$home_week[g] <- sum(dataframe1.0$H.Team.ID[1:g] == dataframe1.0$H.Team.ID[g]) +
    sum(dataframe1.0$Away.Team.ID[1:g] == dataframe1.0$H.Team.ID[g])
  dataframe1.0$away_week[g] <- sum(dataframe1.0$Away.Team.ID[1:g] == dataframe1.0$Away.Team.ID[g]) +
    sum(dataframe1.0$H.Team.ID[1:g] == dataframe1.0$Away.Team.ID[g])
}


dataframe1.2 <- dataframe1.0[, c(1:5, 62, 6:11, 61, 12:60)]

write.csv(dataframe1.2, "daysofrestdata2.1.csv", row.names = FALSE)
