count <- read.csv("Dataframe_Dates.csv", header =T,stringsAsFactors = F)
count$A.Date <- as.Date(count$A.Date, format='%d/%m/%Y')
library(dplyr); library(tidyr); library(ggplot2)

#day of rest = number of full days in between games

df <-  count %>% 
  mutate(A.Date = as.Date(A.Date)) %>% 
  select( Game.ID, A.Date, Away.Team.ID, H.Team.ID) %>% 
  rename( date = A.Date)

#df <- df %>% arrange(desc(Game.ID), desc(date))

# df$date.lag <- NaN
# for(i in 1:10){ 
#      for (j in (i+1):20){
#           while (df$Away.Team.ID[i] == df$Away.Team.ID[j] | df$Away.Team.ID[i] == df$H.Team.ID[j]){
#              df$date.lag[i] = as.integer(df$date[i] - df$date[j] )
#           }
#        }
#   }

#if you check row 36 of df, it shows 3 day break between Away.Team.ID 10 previous game, 
#but it should be 1 since they played a game on the 30th (row 26) 

# df$date.lag <- rep(NaN, 1230)
# for(i in 9:9){
#   for (j in (i-1):1){
#     if (df$Away.Team.ID[i] == df$Away.Team.ID[j] | df$Away.Team.ID[i] == df$H.Team.ID[j]){
#       df$date.lag[i] <- df$date[i] - lag(df$date[j], 1)
#       mutate(df, date.lag)
#       }
#   }
# }

# mutate(date.lag[i] = date - lag(date, 1))