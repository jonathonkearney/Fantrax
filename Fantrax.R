library(ggplot2)
library(tidyverse)

#set the directory as the R folder and save the two tables as variables
setwd("C:/Users/Joe/Documents/R")
FS <- read.csv("FS.csv", header = TRUE)
FT <- read.csv("FT.csv", header = TRUE)

#merge the two tables together
df <- merge(x = FT, y = FS)

minMins <- 180

#remove comma from data$Min and AP and convert to numeric 
df$Min <- as.numeric(gsub("\\,", "", df$Min))
df$Min <- as.numeric(as.character(df$Min))
df$AP <- as.numeric(gsub("\\,", "", df$AP))
df$AP <- as.numeric(as.character(df$AP))

#remove all players with less than a certain amount of mins
df <- subset(df, Min > minMins)

#change certain column names to .90 names
for(i in 1:ncol(df)){
  x <- colnames(df)[i]
  if(x != "ID" && x != "Player" && x != "Team" && x != "Position"
     && x != "Rk" && x != "Status" && x != "Opponent"
     && x != "GP" && x != "PC" && x != "Min"){
    colnames(df)[i] <- paste(colnames(df)[i], ".90", sep="" )
  }
}

#Make most scores now be based of 90 mins
for(i in 1:ncol(df)) { 
  if(endsWith(colnames(df)[i], '90')){
    df[,i] <- round((df[,i] / df$Min)*90, digits = 2)
  }
}

ggplot(df, aes(G.90, A.90, colour = Position)) +
  geom_point() + # Show dots
  geom_text(
    aes(label = Player),
    check_overlap = T,
    adj = -0.1
  )
