#2015-16 season

#start with this link
url.test <- "http://www.basketball-reference.com/leagues/NBA_2016_games.html"

# Duration October 27, 2015 - April 13, 2016
#date
#starting date is Oct 27, 2015
#first game


#we need to stop this loop after the 2016 04 13
## first step is to create the URLs for all the games in the regular season. 
library(rvest)
library(dplyr)

urlbase <- "http://www.basketball-reference.com/boxscores/index.cgi?month="

start <- as.Date("27-10-15", format="%d-%m-%y")
end <- as.Date("28-10-15", format="%d-%m-%y")

res <- list()
theDate <- start
while(theDate <= end){
  xx <- strsplit(as.character(theDate), "-")[[1]]
  url <- paste0(urlbase, xx[2], "&day=", xx[3], "&year=", xx[1])
  XXX <- read_html(url)
  YYY <- html_nodes(XXX, css=".links a:nth-child(1)")
  if(length(YYY) != 0){
    ZZZ <- sapply(strsplit(as.character(YYY), '"'), function(x) x[2])
    ZZZ <- paste0("http://www.basketball-reference.com", ZZZ)
    res <- c(res, ZZZ)
  }
  theDate <- theDate+1
}
res <- unlist(res)

save(res, file="BoxScoresURLs.rda")
      
#reformat table
reformat_table <- function(tbl) {
  # Fix the column names
  colnames(tbl) <- tbl[1, ]
  
  # Missing values for players who did not play
  dnp <- tbl[[2]] == "Did Not Play"
  tbl[dnp, -1] <- NA
  
  # Add a type column to signify starters and reserves
  reserves <- which(tbl[[1]] == "Reserves")[1]
  tbl$type <- "Reserve"
  tbl$type[seq_len(reserves - 1)] <- "Starter"
  tbl$type[dnp] <- "DNP"
  
  # Remove header and summary columns
  tbl[c(-1, -reserves, -NROW(tbl)),]
}

#extract data for each url using loop
#e.g https://www.basketball-reference.com/boxscores/201510270CHI.html
for( url1 in res){

  start <- regexpr("[[:digit:]]", url1)
  date <- substr(url1, start, start + 7)
  
  XXX <- read_html(url1)
  teams <- html_text(html_nodes(XXX, css="strong a"))
  
# extracting the box score table using html_table()
  YYY <- html_nodes(XXX, ".stats_table[id*='_basic']") 
  ZZZ <-  html_table(YYY)
  
  #location and date of game
  WWW <-  html_text(html_nodes(XXX, css=".scorebox_meta div"))
  Location_1 <- WWW[2]
  Location_2 <- strsplit(Location_1, ",")
  Location <- as.character(Location_2[[1]][2]) 
  trim.leading <- function(t) sub("^\\s+", "", t)
  Location <- trim.leading(Location)
  
  #final score of the game
  QQQ <-  html_text(html_nodes(XXX, css=".score"))
  
  HS <- as.numeric(QQQ[1])
  AS <- as.numeric(QQQ[2])

  ZZZ <- lapply(ZZZ, reformat_table)
#AWay team is first, followed by home
  AAA <- cbind(rbind(data.frame(ZZZ[[1]], Score= HS, team= teams[1], H_A = "Away"),
                     data.frame(ZZZ[[2]], Score= AS, team= teams[2], H_A = "Home")), Day=date, Venue = Location)
  
 
  write.csv(AAA, paste0(date, teams[2], "vs", teams[1], ".csv"), row.names = FALSE) 
}


