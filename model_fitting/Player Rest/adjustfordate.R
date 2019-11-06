library(tidyr)
library(dplyr)
untidy <- read.csv("lol.csv", header = TRUE, stringsAsFactors = FALSE)
untidy <- transform(untidy,id=as.numeric(factor(team)))
for(i in 1:nrow(untidy)){
  untidy$Starters[i] <- gsub( "$", untidy$team[i], subset[i,2])
}
unitidy1 <- transform(untidy, Player.ID=as.numeric(factor(Starters)))

#subset <- unitidy1[1:500,]
subset <- unitidy1[, -(4:22)]
subset <- subset[, -9]

subset1 <- filter(subset, type != "DNP")
subset1 <- filter(subset1, MP != "Player Suspended")


df3 <- unite(subset1, "Game.Team.HA", game.id, team,H_A, sep = ",")
df3 <- unite(df3, "Player.ID.MinPlayed", Player.ID, MP, sep = "/" )
df3 <- unite(df3, "TeamID.Score.Date", id, Score,Day, sep = "/")
#df3 <- unite(subset1, "Player.ID.MinPlayed", Starters, MP, sep = "/" )
res2 <- aggregate(Player.ID.MinPlayed~Game.Team.HA+TeamID.Score.Date,
                  data= df3,  paste, collapse = ",")


#res2 <- aggregate(Player.ID.MinPlayed~game.id, data= df3,  paste, collapse = ",")

#seperate Game ID and Home team name 
res8 <- separate(res2, col=Game.Team.HA, into = c("ID", "H.Team","HA"), sep = ",", remove = TRUE)

res8$HA <-as.factor(res8$HA)
# res8 <-  res8[order(res8$ID),]
res8$ID <- as.integer(res8$ID)

res8 <-res8[order(res8[,1],res8[,3]),]

res3 <- separate(res8, Player.ID.MinPlayed, c("H.Player1", "H.Player2", "H.Player3", "H.Player4", "H.Player5", "H.Player6", "H.Player7", "H.Player8", "H.Player9", "H.Player10", "H.Player11", "H.Player12"
), sep= ",")
res3 <- separate(res3, TeamID.Score.Date, c("TeamID", "Score","Date"), sep="/")
#res6 <- separate(res3, 
#                col= c(2:13,),
#                into =c("MP", "MP", "MP", "MP", "MP", "MP", "MP", "MP", "MP", "MP", "MP", "MP", "MP"), sep="/")

#pl1 <- separate(res3, col= Player1, into= "MP", sep = "/", remove = FALSE)

res7 <- separate(res3, col= H.Player1, into= c("Player1", "MP1"), sep = "/", remove = TRUE)
res7 <- separate(res7, col= H.Player2, into= c("Player2", "MP2"), sep = "/", remove = TRUE)
res7 <- separate(res7, col= H.Player3, into= c("Player3", "MP3"), sep = "/", remove = TRUE)
res7 <- separate(res7, col= H.Player4, into= c("Player4", "MP4"), sep = "/", remove = TRUE)
res7 <- separate(res7, col= H.Player5, into= c("Player5", "MP5"), sep = "/", remove = TRUE)
res7 <- separate(res7, col= H.Player6, into= c("Player6", "MP6"), sep = "/", remove = TRUE)
res7 <- separate(res7, col= H.Player7, into= c("Player7", "MP7"), sep = "/", remove = TRUE)
res7 <- separate(res7, col= H.Player8, into= c("Player8", "MP8"), sep = "/", remove = TRUE)
res7 <- separate(res7, col= H.Player9, into= c("Player9", "MP9"), sep = "/", remove = TRUE)
res7 <- separate(res7, col= H.Player10, into= c("Player10", "MP10"), sep = "/", remove = TRUE)
res7 <- separate(res7, col= H.Player11, into= c("Player11", "MP11"), sep = "/", remove = TRUE)
res7 <- separate(res7, col= H.Player12, into= c("Player12", "MP12"), sep = "/", remove = TRUE)
#seperate Game ID and Home team name 
# res8 <- separate(res7, col=Game.Team, into = c("ID", "H.Team"), sep = ",", remove = TRUE)

# regmatches(LA, regexpr(" ", LA), invert = TRUE)

#place home and away team next to each other
res8 <- arrange(res7, ID)
res9<- as.data.frame(append(res8[1,], res8[2,]))


#create matrix with equal number of cols and rows are res8
result <- matrix(ncol = ncol(res8)*2, nrow = nrow(res8)/2)
#convert res8(df) into matrix
res8m <- as.matrix(res8)
for (i in 1:nrow(res8m)){
  if ( i %% 2 == 1){
    temp <- as.vector(res8m[i,])
  }
  if (i %% 2 == 0){
    temp <- append(temp, as.vector(res8m[i,]))
    #storing temp onto the result
    result[i/2,] <- as.vector(temp)
  }
}



result <- result[, -3]
result <- result[,-30]
result <- result[,-31]

colnames(result) <- c("Game.ID","Away.Team","Away.Team.ID", "A.Score","A.Date",  "A.Player1", 
                      "A.MP1", "A.Player2", "A.MP2","A.Player3", "A.MP3","A.Player4", "A.MP4",
                      "A.Player5", "A.MP5","A.Player6","A.MP6","A.Player7", "A.MP7","A.Player8", 
                      "A.MP8","A.Player9", "A.MP9","A.Player10", "A.MP10","A.Player11", "A.MP11","A.Player12", "A.MP12",
                      "Home.Team","H.Team.ID","H.Score","H.Date", "H.Player1", "MP1", "H.Player2", 
                      "MP2","H.Player3", "MP3","H.Player4", "MP4","H.Player5", "MP5","H.Player6","MP6","H.Player7", "MP7","H.Player8", "MP8",
                      "H.Player9", "MP9","H.Player10", "MP10","H.Player11", "MP11","H.Player12", "MP12")


result.df <- as.data.frame(result)

# result.df$Game.ID <- as.numeric(result.df$Game.ID)
# result.df2 <-  result.df[order(result.df$Game.ID),]

# write.csv(result.df2, "Trial.csv", row.names = FALSE)

# write.csv(result.df2, "Trial2.csv", row.names = FALSE)
result.df3 <- result.df[, c(1:5,30:33,6:29,34:57)]

# write.csv(result.df2, "Trial3.csv", row.names = FALSE)

#create columns for no. of players played by h.team
result.df3$No.of.Players.H <- NaN
result.df3$No.of.Players.A <- NaN

# result.df3$A.Date <- as.Date(result.df3$A.Date)

for (i in seq_len(nrow(result.df3))) {
  result.df3$No.of.Players.A[i] <- 12 - sum(is.na(result.df3[i,10:33])) / 2
  result.df3$No.of.Players.H[i] <- 12 - sum(is.na(result.df3[i,34:57])) / 2
}

result.df3 <- result.df3[, c(1:7,58,59,8:57)]

#away team
old <- as.character(result.df3$A.Date)
n <- 5
old1 <-paste(substr(old, 1, n-1), "-", substr(old, n, nchar(old)), sep = "")
n1 <- 8
old2<- paste(substr(old1, 1, n1-1), "-", substr(old1, n1, nchar(old1)), sep = "")
result.df3$A.Date <- old2

#result.df3$A.Date<- as.Date(result.df3$A.Date)

#home team
old2 <- as.character(result.df3$H.Date)
n <- 5
old3 <-paste(substr(old2, 1, n-1), "-", substr(old2, n, nchar(old2)), sep = "")
n1 <- 8
old4 <- paste(substr(old3, 1, n1-1), "-", substr(old3, n1, nchar(old3)), sep = "")
result.df3$H.Date <- old4

#result.df3$H.Date<- as.Date(result.df3$H.Date)



write.csv(result.df3, "XXX1.csv", row.names = FALSE)
 
# dat2 <- read.csv("UpdatedFinalDataFrame1.csv", header = TRUE)
# dat21 <- read.csv("UpdatedFinalDataFrame11.csv", header = TRUE)
