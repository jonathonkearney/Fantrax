library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(rvest)
library(worldfootballR)
library(stringi)
library(shinyWidgets)

# **************************************************
rm(list = ls())

setwd("C:/Users/OEM/OneDrive/Documents/R/Fantrax")

FS <- read.csv("FS.csv", header = TRUE)
FT <- read.csv("FT.csv", header = TRUE)

#merging the dfs
df <- merge(x = FT, y = FS)

#remove the ID column now that we don't need it
df <- select(df, -ID)

#load the data from Understat
understat <- understat_team_players_stats(
  team_url = c(
    "https://understat.com/team/Arsenal/2022",
    "https://understat.com/team/Aston_Villa/2022",
    "https://understat.com/team/Bournemouth/2022",
    "https://understat.com/team/Brentford/2022",
    "https://understat.com/team/Brighton/2022",
    "https://understat.com/team/Chelsea/2022",
    "https://understat.com/team/Crystal_Palace/2022",
    "https://understat.com/team/Everton/2022",
    "https://understat.com/team/Fulham/2022",
    "https://understat.com/team/Leeds/2022",
    "https://understat.com/team/Leicester/2022",
    "https://understat.com/team/Liverpool/2022",
    "https://understat.com/team/Manchester_City/2022",
    "https://understat.com/team/Manchester_United/2022",
    "https://understat.com/team/Newcastle_United/2022",
    "https://understat.com/team/Nottingham_Forest/2022",
    "https://understat.com/team/Southampton/2022",
    "https://understat.com/team/Tottenham/2022",
    "https://understat.com/team/West_Ham/2022",
    "https://understat.com/team/Wolverhampton_Wanderers/2022"
              ))

#extract the Understate columns we want 
understat <- understat %>% select(player_name, xG, xA, npxG, xGChain, xGBuildup)

#change the Player column name so it matches df
names(understat)[names(understat) == 'player_name'] <- 'Player'

#Convert all special characters to raw characters
understat$Player <- stri_trans_general(str = understat$Player, id = "Latin-ASCII")

#specific player cleanups
understat$Player[understat$Player == "N&#039;Golo Kante"] <- "N'Golo Kante"
understat$Player[understat$Player == "Lewis O&#039;Brien"] <- "Lewis O'Brien"
understat$Player[understat$Player == "Rayan Ait Nouri"] <- "Rayan Ait-Nouri"
understat$Player[understat$Player == "Josh Dasilva"] <- "Pelenda Joshua Da Silva"
understat$Player[understat$Player == "Hee-Chan Hwang"] <- "Hwang Hee-Chan"
understat$Player[understat$Player == "Emerson"] <- "Emerson Royal"
understat$Player[understat$Player == "Bobby Reid"] <- "Bobby De Cordova-Reid"
understat$Player[understat$Player == "Ezri Konsa Ngoyo"] <- "Ezri Konsa"
understat$Player[understat$Player == "Gabriel"] <- "Gabriel Magalhaes"
understat$Player[understat$Player == "Matthew Cash"] <- "Matty Cash"
understat$Player[understat$Player == "Thiago Alcantara"] <- "Thiago"
understat$Player[understat$Player == "Emile Smith-Rowe"] <- "Emile Smith Rowe"
understat$Player[understat$Player == "Estupinan"] <- "Pervis Estupinan"


#merge the two tables
df <- merge(x = df, y = understat, by = "Player", all.x = TRUE)

#remove comma from data$Min and AP and convert to numeric 
df$Min <- as.numeric(gsub("\\,", "", df$Min))
df$Min <- as.numeric(as.character(df$Min))
df$AP <- as.numeric(gsub("\\,", "", df$AP))
df$AP <- as.numeric(as.character(df$AP))

#remove all players with less than a certain amount of mins
#this needs to be non-zero so that we don't get any div/0 errors
minMins <- 10
df <- subset(df, Min > minMins)

#Statuses
statuses <- c("W (Mon)", "W (Tue)", "W (Wed)", "W (Thu)", "W (Fri)", "W (Sat)", "W (Sun)", "FA")

#add in the .90 columns
df <- mutate(df, FPts.90 = round(((FPts / Min)*90),2))
df <- mutate(df, G.90 = round(((G / Min)*90),2))
df <- mutate(df, A.90 = round(((A / Min)*90),2))
df <- mutate(df, Pts.90 = round(((Pts / Min)*90),2))
df <- mutate(df, S.90 = round(((S / Min)*90),2))
df <- mutate(df, SOT.90 = round(((SOT / Min)*90),2))
df <- mutate(df, FC.90 = round(((FC / Min)*90),2))
df <- mutate(df, FS.90 = round(((FS / Min)*90),2))
df <- mutate(df, YC.90 = round(((YC / Min)*90),2))
df <- mutate(df, RC.90 = round(((RC / Min)*90),2))
df <- mutate(df, DPt.90 = round(((DPt / Min)*90),2))
df <- mutate(df, Off.90 = round(((Off / Min)*90),2))
df <- mutate(df, PKG.90 = round(((PKG / Min)*90),2))
df <- mutate(df, CS.90 = round(((CS / Min)*90),2))
df <- mutate(df, A2.90 = round(((A2 / Min)*90),2))
df <- mutate(df, KP.90 = round(((KP / Min)*90),2))
df <- mutate(df, AT.90 = round(((AT / Min)*90),2))
df <- mutate(df, TkW.90 = round(((TkW / Min)*90),2))
df <- mutate(df, DIS.90 = round(((DIS / Min)*90),2))
df <- mutate(df, ErG.90 = round(((ErG / Min)*90),2))
df <- mutate(df, AP.90 = round(((AP / Min)*90),2))
df <- mutate(df, SFTP.90 = round(((SFTP / Min)*90),2))
df <- mutate(df, ACNC.90 = round(((ACNC / Min)*90),2))
df <- mutate(df, Int.90 = round(((Int / Min)*90),2))
df <- mutate(df, CLR.90 = round(((CLR / Min)*90),2))
df <- mutate(df, CoS.90 = round(((CoS / Min)*90),2))
df <- mutate(df, AER.90 = round(((AER / Min)*90),2))
df <- mutate(df, PKM.90 = round(((PKM / Min)*90),2))
df <- mutate(df, OG.90 = round(((OG / Min)*90),2))
df <- mutate(df, GAD.90 = round(((GAD / Min)*90),2))
df <- mutate(df, CSD.90 = round(((CSD / Min)*90),2))
df <- mutate(df, CSM.90 = round(((CSM / Min)*90),2))

#new columns
df <- mutate(df, Min.GP = round((Min / GP) ,2))
df <- mutate(df, xGandxA = xG + xA)
df <- mutate(df, TkWAndIntAndCLR.90 = round((((TkW + Int + CLR) / Min)*90),2))
df <- mutate(df, SOTAndKP.90 = round((((SOT + KP) / Min)*90),2))
df <- mutate(df, CoSMinusDIS.90 = round((((CoS - DIS) / Min)*90),2))
df <- mutate(df, SOTMinusG.90 = round((((SOT - G) / Min)*90),2))
df <- mutate(df, KPMinusA.90 = round((((KP - A) / Min)*90),2))
df <- mutate(df, xGPerf = G / xG)
df <- mutate(df, xAPerf = A / xA)
df <- mutate(df, xGPerfandxAPerf = xGPerf + xAPerf)

#Team data
teams <- aggregate(FPts.90 ~ Team, df, mean )
teams$Opponent <- df[match(teams$Team, df[,"Team"]), "Opponent"]
teams$Opponent <- gsub("@","",as.character(teams$Opponent))
teams$Opponent <- substr(teams$Opponent, start = 1, stop = 3)
teams$OFPts.90 <- teams[match(teams$Opponent, teams$Team), "FPts.90"]
teams$MatchupScore <- teams$FPts.90 / teams$OFPts.90


#New
teams <- aggregate(FPts.90 ~ Team, df, median)
teams$Opponent <- df[match(teams$Team, df[,"Team"]), "Opponent"]
teams$Opponent <- gsub("@","",as.character(teams$Opponent))
teams$Opponent <- substr(teams$Opponent, start = 1, stop = 3)
teams$OFPts.90 <- teams[match(teams$Opponent, teams$Team), "FPts.90"]
teams <- arrange(teams, desc(OFPts.90))
teams <- rowid_to_column(teams, "ID")
teams$MatchupScore <- teams$ID
teams$MatchupScore <- ((teams$MatchupScore - 10) / 15) + 1
teams$MatchupScore <- ifelse(teams$MatchupScore > 1, ((teams$MatchupScore -1) / 2) + 1, teams$MatchupScore)

teamDiff <- subset(teams, select = c(Team, MatchupScore))
df <- merge(x = df, y = teamDiff)
df$MatchupFP.G <- df$FP.G * df$MatchupScore
df$MatchupFPts.90 <- df$FPts.90 * df$MatchupScore

top10 <- subset(df, !(df$Status %in% statuses))
top10 <- top10 %>%                                      
  arrange(desc(MatchupFP.G)) %>% 
  group_by(Status) %>%
  slice(1:10)

fTeams <- top10

top10$Top10 <- 1
top10 <- subset(top10, select = c(Player, Top10))
df <- full_join(df, top10, by = "Player")
df$Top10 <- ifelse(is.na(df$Top10), 0, df$Top10)


#Status aggregate scores
temp1 <- aggregate(MatchupFP.G ~ Status, fTeams, sum)
temp2 <- aggregate(FP.G ~ Status, fTeams, sum)
temp3 <- aggregate(MatchupScore ~ Status, fTeams, mean)
statusTable <- merge(x = temp1, y = temp2, by = "Status", all.x = TRUE)
statusTable <- merge(x = statusTable, y = temp3, by = "Status", all.x = TRUE)

# **************************************************

ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  navbarPage("Fantrax",
     tabPanel("Plot",
        sidebarLayout(
          
          sidebarPanel(
            
            width = "2",
            
            selectInput("pTeam","Choose a Team", choices = c("All",unique(sort(df$Team))), selected = "All"),
            selectInput("pStatus","Choose a Status", choices = c("All", "All Available", "All Taken", unique(sort(df$Status)), "Waiver"), selected = "All Available"),
            selectInput("pPosition","Choose a Position", choices = c("All", "D", "M", "F"), selected = "All"),
            selectInput("pYAxis","Choose the Y Axis", choices = sort(names(df)), selected = "FPts.90"),
            selectInput("pXAxis","Choose the X Axis", choices = sort(names(df)), selected = "SOTAndKP.90"),
            sliderInput("pMinMinsPerGP", "Minimum Minutes Per GP", min = min(df$Min.GP), max = max(df$Min.GP), value = min(df$Min.GP)),
            sliderInput("pGamesPlayed", "Minimum Games Played", min = min(df$GP), max = max(df$GP), value = min(df$GP)),
            sliderInput("pMinFPts.90", "Minimum FPts per 90", min = min(df$FPts.90), max = max(df$FPts.90), value = min(df$FPts.90)),
            checkboxInput("pAddLines", "Add Lines", value = FALSE, width = NULL)
            
          ),
          
          mainPanel(
            plotOutput(outputId = "plot",width = "1500px", height = "900px")
          )
        )
     ),
     tabPanel("Table",
        sidebarLayout(
          
          sidebarPanel(
            
            width = "2",
            
            selectInput("tTeam","Choose a team", choices = c("All",unique(df$Team)), selected = "All"),
            selectInput("tStatus","Choose a Status", choices = c("All", "All Available", "All Taken", unique(df$Status), "Waiver"), selected = "All"),
            selectInput("tPosition","Choose a Position", choices = c("All", "D", "M", "F"), selected = "All"),
            sliderInput("tMinMinsPerGP", "Minimum Minutes Per GP", min = min(df$Min.GP), max = max(df$Min.GP), value = min(df$Min.GP)),
            sliderInput("tGamesPlayed", "Minimum Games Played", min = min(df$GP), max = max(df$GP), value = min(df$GP)),
            pickerInput("tPicker", "Columns", choices = sort(names(df)), options = list(`actions-box` = TRUE), selected=NULL, multiple=TRUE)
          ),
          
          mainPanel(
            DT::dataTableOutput("table")
          )
        )
     ),
     tabPanel("Box",
        sidebarLayout(
          
          sidebarPanel(
            
            width = "2",
            
            selectInput("bYAxis","Choose the Y Axis", choices = sort(names(df)), selected = "MatchupScore"),
            selectInput("bXAxis","Choose the X Axis", choices = c("Status", "Team"), selected = "Status"),
            selectInput("bPlotType","Plot Type", choices = c("Box", "Violin"), selected = "Box"),
            checkboxInput("bTop10", "Top 10 Only", value = FALSE, width = NULL)
          ),
          
          mainPanel(
            plotOutput(outputId = "box",width = "1500px", height = "900px")
          )
        )
     ),
     tabPanel("Prediction",
        sidebarLayout(
          
          sidebarPanel(

            width = "2",
            
          ),
          
          mainPanel(
            DT::dataTableOutput("pTable")
          )
        )
     )
  )
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    df_temp <- df
    df_temp <- filter(df_temp, Min.GP >= input$pMinMinsPerGP)
    df_temp <- filter(df_temp, GP >= input$pGamesPlayed)
    df_temp <- filter(df_temp, FPts.90 >= input$pMinFPts.90)
    if (input$pTeam != "All") {
      df_temp <- filter(df_temp, Team == input$pTeam)
    }
    if (input$pStatus != "All") {
      if (input$pStatus == "Waiver") {
        df_temp <- filter(df_temp, str_detect(df_temp$Status, "^W \\("))
      }
      else if (input$pStatus == "All Available") {
        df_temp <- filter(df_temp, str_detect(df_temp$Status, "^W \\(") | str_detect(df_temp$Status, "^FA"))
      }
      else if (input$pStatus == "All Taken") {
        df_temp <- filter(df_temp, !Status %in% statuses)
      }
      else{
        df_temp <- filter(df_temp, Status == input$pStatus)  
      }
    }
    if (input$pPosition != "All") {
      if(input$pPosition == "D"){
        df_temp <- filter(df_temp, str_detect(df_temp$Position, "D"))
      }
      else if(input$pPosition == "M"){
        df_temp <- filter(df_temp, str_detect(df_temp$Position, "M"))
      }
      else if(input$pPosition == "F"){
        df_temp <- filter(df_temp, str_detect(df_temp$Position, "F"))
      }
    }
    
    p <- ggplot(df_temp, aes(colour = Position)) + aes_string(input$pYAxis, input$pXAxis) +
      geom_point() + 
      geom_text(
        aes(label = Player),
        check_overlap = F,
        adj = -0.1,
        vjust="inward"
      ) +
      coord_flip(clip = "off")
    
    if(input$pAddLines == TRUE) {
      p + theme_classic() + geom_smooth(method="lm", se=F)
    }
    else{
      p + theme_classic()
    }
    
  }, res = 90) #the resolution of the plot
  

  output$table = DT::renderDataTable({
    df_temp <- df
    df_temp <- filter(df_temp, Min.GP >= input$tMinMinsPerGP)
    df_temp <- filter(df_temp, GP >= input$tGamesPlayed)
    
    columns <- c("Player", "Team", "Status", "Position")
    
    columns <- append(columns, input$tPicker)
    
    df_temp <- df_temp[, which((names(df_temp) %in% columns)==TRUE)]
    
    if (input$tTeam != "All") {
      df_temp <- filter(df_temp, Team == input$tTeam)
    }
    if (input$tStatus != "All") {
      if (input$tStatus == "Waiver") {
        df_temp <- filter(df_temp, str_detect(df_temp$Status, "^W \\("))
      }
      else if (input$tStatus == "All Available") {
        df_temp <- filter(df_temp, str_detect(df_temp$Status, "^W \\(") | str_detect(df_temp$Status, "^FA"))
      }
      else if (input$tStatus == "All Taken") {
        df_temp <- filter(df_temp, !Status %in% statuses)
      }
      else{
        df_temp <- filter(df_temp, df_temp$Status == input$tStatus)  
      }
    }
    if (input$tPosition != "All") {
      if(input$tPosition == "D"){
        df_temp <- filter(df_temp, str_detect(df_temp$Position, "D"))
      }
      else if(input$tPosition == "M"){
        df_temp <- filter(df_temp, str_detect(df_temp$Position, "M"))
      }
      else if(input$tPosition == "F"){
        df_temp <- filter(df_temp, str_detect(df_temp$Position, "F"))
      }
    }
    df_temp
  })
  
  output$box <- renderPlot({
    
    df_temp <- df
    
    if(input$bTop10 == TRUE){
      df_temp <- subset(df_temp, Top10 == 1)
    }
    
    if(input$bXAxis == "Status"){
      df_temp <- df_temp %>%  subset(Status != "W (Fri)" & Status != "W (Sat)" & Status != "W (Sun)" & 
                                       Status != "W (Mon)" & Status != "W (Tue)" & Status != "W (Wed)" &
                                       Status != "W (Thu)" & Status != "FA")
      
      if(input$bPlotType == "Box"){
        #input$fTeamY is a character, so you have to use get() in aes 
        ggplot(df_temp, aes(x = reorder(Status, get(input$bYAxis), FUN=mean), fill=Status)) + aes_string(y = input$bYAxis) +
          geom_boxplot(coef = 5) + labs(x = "Teams")
        
      } else {
        #input$fTeamY is a character, so you have to use get() in aes 
        ggplot(df_temp, aes(x = reorder(Status, get(input$bYAxis), FUN=mean), fill=Status)) + aes_string(y = input$bYAxis) +
          geom_violin(aes(fill = factor(Status))) +
          geom_boxplot(coef = 5, width=0.1, color="grey", alpha=0.2) + labs(x = "Teams") +
          labs(x = "Teams")
      }
      
      
      
    }else if (input$bXAxis == "Team"){
      
      if(input$bPlotType == "Box"){
        #input$fTeamY is a character, so you have to use get() in aes 
        ggplot(df_temp, aes(x=reorder(Team, get(input$bYAxis), FUN=mean), get(input$bYAxis), fill=Team)) +
          geom_boxplot(coef = 5) + labs(x = "Teams")
        
      } else {
        #input$fTeamY is a character, so you have to use get() in aes 
        ggplot(df_temp, aes(x=reorder(Team, get(input$bYAxis), FUN=mean), get(input$bYAxis), fill=Team)) +
          geom_violin(aes(fill = factor(Team))) +
          geom_boxplot(coef = 5, width=0.1, color="grey", alpha=0.2) + labs(x = "Teams") +
          labs(x = "Teams")
      }
      
    }

  })
  
  output$pTable = DT::renderDataTable({
    DT::datatable(statusTable, options = list(pageLength = 15))
  })
}

shinyApp(ui, server)

