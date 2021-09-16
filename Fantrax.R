library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(corrplot)
library(rvest)

url_data <- "https://en.wikipedia.org/wiki/2021%E2%80%9322_Premier_League"
css_selector <- "#mw-content-text > div.mw-parser-output > table:nth-child(25)"
league_table <- url_data %>% 
  read_html() %>% 
  html_element(css = css_selector) %>% 
  html_table()

#abbreviate the Team name column in the league table
league_table$Team <- sub("Tottenham Hotspur", "TOT", league_table$Team)
league_table$Team <- sub("West Ham United", "WHU", league_table$Team)
league_table$Team <- sub("Manchester United", "MUN", league_table$Team)
league_table$Team <- sub("Chelsea", "CHE", league_table$Team)
league_table$Team <- sub("Liverpool", "LIV", league_table$Team)
league_table$Team <- sub("Everton", "EVE", league_table$Team)
league_table$Team <- sub("Manchester City", "MCI", league_table$Team)
league_table$Team <- sub("Brighton & Hove Albion", "BHA", league_table$Team)
league_table$Team <- sub("Leicester City", "LEI", league_table$Team)
league_table$Team <- sub("Brentford", "BRF", league_table$Team)
league_table$Team <- sub("Aston Villa", "AVL", league_table$Team)
league_table$Team <- sub("Watford", "WAT", league_table$Team)
league_table$Team <- sub("Southampton", "SOU", league_table$Team)
league_table$Team <- sub("Crystal Palace", "CRY", league_table$Team)
league_table$Team <- sub("Leeds United", "LEE", league_table$Team)
league_table$Team <- sub("Burnley", "BUR", league_table$Team)
league_table$Team <- sub("Newcastle United", "NEW", league_table$Team)
league_table$Team <- sub("Wolverhampton Wanderers", "WOL", league_table$Team)
league_table$Team <- sub("Norwich City", "NOR", league_table$Team)
league_table$Team <- sub("Arsenal", "ARS", league_table$Team)

#set the directory as the R folder and save the two tables as variables
setwd("C:/Users/OEM/OneDrive/Documents/R/Fantrax") 
FS <- read.csv("FS.csv", header = TRUE)
FT <- read.csv("FT.csv", header = TRUE)

#merge the two tables together
df <- merge(x = FT, y = FS)

minMins <- 45

#clean up the opponent column in df
df$Opponent <- ifelse(startsWith(df$Opponent, "@"), substring(df$Opponent, 2), df$Opponent)
df$Opponent <- substr(df$Opponent, 1, 3) 

#add in the OppGA and OPPGF and OppRank columns
df$OppGA <- league_table$GA[match(df$Opponent, league_table$Team)]
df$OppGA <- as.numeric(as.character(df$OppGA))
df$OppGF <- league_table$GF[match(df$Opponent, league_table$Team)]
df$OppGF <- as.numeric(as.character(df$OppGF))
df$OppGD <- df$OppGF - df$OppGA
df$OppPos <- league_table$Pos[match(df$Opponent, league_table$Team)]
df$TeamPos <- league_table$Pos[match(df$Team, league_table$Team)]
MaxOppGF <- max(df$OppGF)
MaxOppGA <- max(df$OppGA)
MaxOppGD <- max(df$OppGD)
MinOppGF <- min(df$OppGF)
MinOppGA <- min(df$OppGA)
MinOppGD <- min(df$OppGD)
AvgOppGF <- mean(df$OppGF)
AvgOppGA <- mean(df$OppGA)
AvgOppGD <- mean(df$OppGD)
RangeOppGA <- MaxOppGA - MinOppGA
RangeOppGF <- MaxOppGF - MinOppGF
RangeOppGD <-MaxOppGD - MinOppGD

#remove comma from data$Min and AP and convert to numeric 
df$Min <- as.numeric(gsub("\\,", "", df$Min))
df$Min <- as.numeric(as.character(df$Min))
df$AP <- as.numeric(gsub("\\,", "", df$AP))
df$AP <- as.numeric(as.character(df$AP))

#remove all players with less than a certain amount of mins
df <- subset(df, Min > minMins)

#Make new columns for the .90 scores
for(i in 1:ncol(df)){
  x <- colnames(df)[i]
  if(x != "ID" && x != "Player" && x != "Team" && x != "Position"
     && x != "Rk" && x != "Status" && x != "Opponent" && x != "FP.G" && x != "X.D"
     && x != "ADP"&& x != "GP" && x != "PC." && x != "Min" && x != "G.G"
     && x != "X..Owned" && x != "X..." && x != "xG" && x != "xA" && x != "influence"
     && x != "creativity" && x != "threat" && x != "X..." && x != "Ros." && x != "OppGA" 
     && x != "OppGF" && x != "OppGD" && x != "OppGDxFP.G" && x != "OppPos"
     && x != "PosDifxFP.G"){
    name <- paste(colnames(df)[i], ".90", sep="")
    df[,name] <- round((df[,i] / df$Min)*90, digits = 2)
  }
}

df$Min.GP <- round((df$Min / df$GP), digits = 2)

df$PotentialFP <- round(df$FPts.90 - df$FP.G, digits = 2)
df$AT.KP <- round(df$AT.90 / df$KP.90, digits = 2)

#create adjusted scores based off the league table position differences
AdjFactor <- 0.3 #I added the *.3 because I dont think they vary that much. and the score is an adjusted avg, not a upper or lower limit 

df$PosDif <- df$OppPos - df$TeamPos

df$PosAdjFP.G <- round(df$FP.G * (1 + (((1/10) * (df$OppPos - 10.5) )*AdjFactor)), 2)

df$PosAdjFP.90 <- round(df$FPts.90 * (1 + (((1/19) * df$PosDif)*AdjFactor)), 2)


#create adjusted scores by calculating position against OppGA, OppGF, OppGA
df$GDAdjFP.G <- ifelse(grepl("F", df$Position), round(df$FP.G * (1 + (((1/ (RangeOppGA/2) ) * (df$OppGA - (RangeOppGA/2)) )*AdjFactor)), 2), 
                       ifelse(grepl("D", df$Position), round(df$FP.G * (1 + (((1/ (RangeOppGF/2) ) * ((RangeOppGF/2) - df$OppGF) )*AdjFactor)), 2), 
                              round(df$FP.G * (1 + (((1/ (RangeOppGD/2) ) * (0 - df$OppGD) )*AdjFactor)), 2)
                       ))

#add in the percetage games played variable
# gameweeks <-max(df$GP)
# df$GPxPosAdjFP.G<- round(df$PosAdjFP.G *(df$GP / gameweeks), 2)


PosPred <- Top10(df, "PosAdjFP.G")
GDPred <- Top10(df, "GDAdjFP.G")

AggTemp1 <- aggregate(df$PosAdjFP.G, by=list(Team=df$Status), FUN=sum)
colnames(AggTemp1)[2] <- "Total_PosAdjFP.G"
AggTemp2 <- aggregate(df$PosDif, by=list(Team=df$Status), FUN=sum)
colnames(AggTemp2)[2] <- "Total_PosDif"
AggTemp3 <- aggregate(df$GDAdjFP.G, by=list(Team=df$Status), FUN=sum)
colnames(AggTemp3)[2] <- "Total_GDAdjFP.G"

AggTemp4 <- aggregate(PosPred$PosAdjFP.G, by=list(Team=PosPred$Status), FUN=sum)
colnames(AggTemp4)[2] <- "Top10_Total_PosAdjFP.G"
AggTemp5 <- aggregate(GDPred$GDAdjFP.G, by=list(Team=GDPred$Status), FUN=sum)
colnames(AggTemp5)[2] <- "Top10_Total_GDAdjFP.G"

AggTemp6 <- aggregate(df$OppPos, by=list(Team=df$Status), FUN=sum)
colnames(AggTemp6)[2] <- "Total_OppPos"

temp <- merge(AggTemp1, AggTemp2, by="Team")
temp <- merge(temp, AggTemp3, by="Team")
temp2 <- merge(AggTemp4, AggTemp5, by="Team")
temp2 <- merge(temp2, AggTemp6, by="Team")
FTeams <- merge(temp, temp2, by="Team")

FTeams <- FTeams[!startsWith(FTeams$Team, "W (") & FTeams$Team != "FA",]

Top10 <- function(Data, Metric) {
  Pred <- select(df, Status, Player, Position, Metric)
  Pred <- Pred[FALSE,]
  TheTeams <- unique(df[["Status"]])
  for (i in 1:length(TheTeams)) {
    MaxD <- 5
    MaxM <- 5
    MaxF <- 3
    DCount <- 0
    MCount <- 0
    FCount <- 0
    
    Players <- filter(Data, Data$Status == TheTeams[i]) 
    Players <- select(Players, Status, Player, Position, Metric)
    Players <- arrange(Players, desc(Players[,Metric]))
    
    The10 <- Players[FALSE,] #makes an empty DF with the columns we want
    
    for (j in 1:nrow(Players)) {
      if((DCount + MCount + FCount) < 10){
        if(grepl("F", Players[j, 3]) & FCount < MaxF){
          The10 <- rbind(The10, Players[j,])
          FCount <- FCount + 1
        }
        else if(grepl("D", Players[j, 3]) & DCount < MaxD){
          The10 <- rbind(The10, Players[j,])
          DCount <- DCount + 1
        }
        else if(grepl("M", Players[j, 3]) & MCount < MaxM){
          The10 <- rbind(The10, Players[j,])
          MCount <- MCount + 1
        }
      }
    }
    Pred <- rbind(Pred, The10)
  }
  return(Pred)
}

#Write to CSV for Markdown and Dashboard
write.csv(df,"C:/Users/OEM/OneDrive/Documents/R/Fantrax/Fantrax_Data.csv", row.names = FALSE)

write.csv(FTeams,"C:/Users/OEM/OneDrive/Documents/R/Fantrax/Fantrax_Agg_Data.csv", row.names = FALSE)
