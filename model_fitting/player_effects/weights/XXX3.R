data2 <- read.csv("XXX2.csv", header = TRUE, stringsAsFactors = F )

####################
x<- unlist(strsplit("22:30:00 23:00:00 36:00:00 44:05:00 4:00:00 ", "[[:space:]]+"))
#takes out the milliseconds
x <- strtrim(x, 5)

#takes in 22:30 format and outputs mins
as.numeric(strptime(x, format="%M:%S") - as.POSIXct(format(Sys.Date())), units="mins")

#######################
#data2$MP2 <- gsub(":", "", data2$MP2, perl = TRUE)
for( i in 1:12){
data2[,paste("MP", i, sep = "")] <- strtrim(data2[,paste("MP", i,sep = "")], 5)
data2[,paste("MP", i,sep = "")]<- as.numeric(strptime(data2[,paste("MP", i,sep = "")], format = "%M:%S") - as.POSIXct(format(Sys.Date())), units="mins")
}

for( i in 1:12){
  data2[,paste("A.MP", i, sep = "")] <- strtrim(data2[,paste("A.MP", i,sep = "")], 5)
  data2[,paste("A.MP", i,sep = "")]<- as.numeric(strptime(data2[,paste("A.MP", i,sep = "")], format = "%M:%S") - as.POSIXct(format(Sys.Date())), units="mins")
}
data2$A.MP8[is.na(data2$A.MP8)] <- 0
data2$A.MP9[is.na(data2$A.MP9)] <- 0
data2$A.MP12[is.na(data2$A.MP12)] <- 0
data2$A.MP11[is.na(data2$A.MP11)] <- 0
data2$A.MP10[is.na(data2$A.MP10)] <- 0

data2$MP8[is.na(data2$MP8)] <- 0
data2$MP9[is.na(data2$MP9)] <- 0
data2$MP10[is.na(data2$MP10)] <- 0
data2$MP11[is.na(data2$MP11)] <- 0
data2$MP12[is.na(data2$MP12)] <- 0

#calculate and replace home weights
for(i in 1:12){
  for(g in 1:nrow(data2)){
    data2[g,paste("WH", i, sep = "")] <- data2[g,paste("MP", i,sep = "")]/sum(data2[g,"MP1"], data2[g,"MP2"],data2[g,"MP3"],
                                                                              data2[g,"MP4"],data2[g,"MP5"],data2[g,"MP6"],
                                                                              data2[g,"MP7"], data2[g,"MP8"], data2[g,"MP9"],
                                                                              data2[g,"MP10"],data2[g,"MP11"], data2[g,"MP12"])
  }
}

# for( i in 1:12){
#   data3 <-  data2[c(1:39,paste("WH", i, sep = ""), 40:41,paste("WH", i+1, sep = ""),42:43, paste("WH", i+3, sep = ""),
#                  44:45, paste("WH", i+4, sep = ""), 46:47, paste("WH", i+5, sep = ""), 48:49, paste("WH", i+6, sep = ""),
#                  50:51, paste("WH", i+7, sep = ""),52:53, paste("WH", i+8, sep = ""), 54:55 ,paste("WH", i+9, sep = ""),
#                  56:57, paste("WH", i+10, sep = ""), 58:59, paste("WH", i+11, sep = ""),60:61, paste("WH", i+12, sep = ""),62),]
# }

data2 <-  data2[c(1:38,61, 39:40,62,41:42, 63,
                  +                     43:44, 64, 45:46, 65, 47:48, 66,
                  +                     49:50, 67, 51:52, 68, 53:54 ,69,
                  +                     55:56, 70, 57:58, 71,59:60, 72)]
#calculate and replace away player weights
for(i in 1:12){
  for(g in 1:nrow(data2)){
    data2[g,paste("WA", i, sep = "")] <- data2[g,paste("A.MP", i,sep = "")]/sum(data2[g,"A.MP1"], data2[g,"A.MP2"],data2[g,"A.MP3"],
                                                                              data2[g,"A.MP4"],data2[g,"A.MP5"],data2[g,"A.MP6"],
                                                                              data2[g,"A.MP7"], data2[g,"A.MP8"], data2[g,"A.MP9"],
                                                                              data2[g,"A.MP10"],data2[g,"A.MP11"], data2[g,"A.MP12"])
  }
}

data2 <-  data2[c(1:14,73, 
                  15:16, 74,
                  17:18, 75, 
                  19:20, 76, 
                  21:22, 77,
                  23:24, 78, 
                  25:26, 79, 
                  27:28 ,80,
                  29:30, 81, 
                  31:32, 82,
                  33:34, 83,
                  35:36, 84,
                  37:72)]


?write.csv
write.csv(data2, file = "XXX3.csv", col.names = TRUE, row.names = FALSE)

##############################

df <- read.csv("playereffects_data.csv", header = TRUE,stringsAsFactors = F )

df.matrix <- df[c("Game.ID",)]

