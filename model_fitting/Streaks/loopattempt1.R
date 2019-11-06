nba <- read.csv("data.csv", header =T)
nba <- nba[,1:14]

library(dplyr)
library(magrittr)
library(tidyr)

attach(nba)

attach(nba)

for( i in 1:nrow(nba)){
  if( A.Score[i] < H.Score[i]){
    nba$A.outcome[i] <- "L"  
    nba$H.outcome[i] <- "W"
  }
  if(A.Score[i] > H.Score[i]){
    nba$H.outcome[i] <- "L" 
    nba$A.outcome[i] <- "W" 
  }
}
View(nba)
nba$H_Streak <- NA
nba$A_Streak <- NA


for ( i in 1:30){ 
  a <- paste("Team", i, sep = "")
  a <- filter(nba, Away.Team.ID==i | H.Team.ID==i) %>% 
    select(Away.Team.ID, away_week,A.outcome, H.Team.ID, home_week,H.outcome)
  
  x <- paste("Team1", i, sep = "")
  y <- paste("Team2", i, sep = "")
  
  x <- select(filter(a, Away.Team.ID==i), Away.Team.ID, away_week, A.outcome)
  y <- select(filter(a, H.Team.ID==i), H.Team.ID, home_week, H.outcome)
                
  colnames(x)<- c("ID", "week", "outcome")
  colnames(y) <- c("ID", "week", "outcome")
  
  z <- paste("Team3", i, sep = "")
  z <- bind_rows(x,y) %>% arrange(week)
  
  W<- 0 
  L <- 0
  # attach(z)
  for (i in 1:82){
    if (z$outcome[i] == "W"){
      W <- W +1 
      L <- 0
      z$Streak[i] <- paste("W", W)
    }
    if( z$outcome[i]== "L"){
      W <- 0
      L <- L + 1
      z$Streak[i] <- paste("L", L)
    }
  }
  #fill in Home streak
  for(i in 2:82){
    for( g in 3:nrow(nba)){
      if( nba$H.Team.ID[g] == z$ID[i] & nba$home_week[g] == z$week[i] ){
        nba$H_Streak[g] <- z$Streak[i-1]
      }
    }
  }
  #fill in away streak
  for(i in 2:82){
    for( g in 3:nrow(nba)){
      if( nba$Away.Team.ID[g] == z$ID[i] & nba$away_week[g] == z$week[i] ){
        nba$A_Streak[g] <- z$Streak[i-1]
      }
    }
  }

}

nba_final <- nba[, c(1:7,18,8:14,17)]

nba_final$A_Streak[is.na(nba_final$A_Streak)] <- 0
nba_final$A_Streak[is.na(nba_final$A_Streak)] <- 0
write.csv(nba_final, "data_withstreaks.csv", row.names = FALSE)
