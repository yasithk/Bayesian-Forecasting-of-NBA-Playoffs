nba <- read.csv("data.csv", header =T)
nba <- nba[,1:14]
  
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

W <- 0
L <- 0

attach(nba)

# check the game it played before to see if its a win or loss. If h.team wins current game and lost prev game 
# W <- 0 and L <- L + 1


for( i in 1:nrow(nba)){
  if( A.outcome[i] == "W" ){
    W <- W + 1
    nba$A.Streak[i] <- "W"
  }
  # if( A.outcome[i] == "L"){
  #   L <- L +1 
  #   nba$A.Streak[i] <- L 
  # }
}
