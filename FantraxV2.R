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



#change the Player column name so it matches df
names(understat)[names(understat) == 'player_name'] <- 'Player'

#specific player cleanups
understat$Player[understat$Player == "N&#039;Golo Kante"] <- "N'Golo Kante"
understat$Player[understat$Player == "Lewis O&#039;Brien"] <- "Lewis O'Brien"
understat$Player[understat$Player == "Rayan Ait Nouri"] <- "Rayan Ait-Nouri"
understat$Player[understat$Player == "Josh Dasilva"] <- "Pelenda Joshua Da Silva"
understat$Player[understat$Player == "Hee-Chan Hwang"] <- "Hwang Hee-Chan"
understat$Player[understat$Player == "Emerson" & understat$team_name == "Tottenham"] <- "Emerson Royal"
understat$Player[understat$Player == "Bobby Reid"] <- "Bobby De Cordova-Reid"
understat$Player[understat$Player == "Ezri Konsa Ngoyo"] <- "Ezri Konsa"
understat$Player[understat$Player == "Gabriel"] <- "Gabriel Magalhaes"
understat$Player[understat$Player == "Matthew Cash"] <- "Matty Cash"
understat$Player[understat$Player == "Thiago Alcantara"] <- "Thiago"
understat$Player[understat$Player == "Emile Smith-Rowe"] <- "Emile Smith Rowe"
understat$Player[understat$Player == "Estupinan"] <- "Pervis Estupinan"

#remove double ups (such as when a player moves team mid season)
understat <- subset(understat, !(Player == "Morgan Gibbs-White" & team_name == "Wolverhampton Wanderers"))
understat <- subset(understat, !(Player == "Wesley Fofana" & team_name == "Leicester"))

#extract the Understate columns we want 
understat <- understat %>% select(Player, xG, xA, npxG, xGChain, xGBuildup)

#Convert all special characters to raw characters
understat$Player <- stri_trans_general(str = understat$Player, id = "Latin-ASCII")

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

#Potential
df <- mutate(df, Potential = round((FPts.90 / FP.G),2))

#Game Start Perc
df <- mutate(df, GSPercentage = round((GS / max(df$GP)),2))
df <- mutate(df, GPPercentage = round((GP / max(df$GP)),2))

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
df <- mutate(df, xG.90 = round(((xG / Min)*90),2))
df <- mutate(df, xA.90 = round(((xA / Min)*90),2))
df <- mutate(df, xA.90AndxG.90 = xG + xA)
df <- mutate(df, xGChain.90 = round(((xGChain / Min)*90),2))
df <- mutate(df, xGBuildup.90 = round(((xGBuildup / Min)*90),2))
df <- mutate(df, xA.90Perf = A.90 - xA.90)
df <- mutate(df, xG.90Perf = G.90 - xG.90)

#Mark the top 10 players for each Status by their regular FP.G score
top10 <- subset(df, !(df$Status %in% statuses))
top10 <- top10 %>%                                      
  arrange(desc(FP.G)) %>% 
  group_by(Status) %>%
  slice(1:10)
top10$Top10 <- 1
top10 <- subset(top10, select = c(Player, Top10))
df <- full_join(df, top10, by = "Player")
df$Top10 <- ifelse(is.na(df$Top10), 0, df$Top10)


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
            selectInput("pYAxis","Choose the Y Axis", choices = sort(names(df)), selected = "SFTP.90"),
            selectInput("pXAxis","Choose the X Axis", choices = sort(names(df)), selected = "xA.90AndxG.90"),
            sliderInput("pMinMinsPerGP", "Minimum Minutes Per GP", min = min(df$Min.GP), max = max(df$Min.GP), value = min(df$Min.GP)),
            sliderInput("pMinMins", "Minimum Total Minutes", min = min(df$Min), max = max(df$Min), value = min(df$Min)),
            sliderInput("pMinFPts.90", "Minimum FPts per 90", min = min(df$FPts.90), max = max(df$FPts.90), value = min(df$FPts.90)),
            sliderInput("pPCofGamesStarted", "% of Games Started", min = min(df$GSPercentage), max = max(df$GSPercentage), value = min(df$GSPercentage)),
            checkboxInput("pAddLines", "Add Lines", value = FALSE, width = NULL),
            checkboxInput("pTop10", "Top 10 Only", value = FALSE, width = NULL)
            
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
            sliderInput("tMinMins", "Minimum Total Minutes", min = min(df$Min), max = max(df$Min), value = min(df$Min)),
            pickerInput("tPicker", "Columns", choices = sort(names(df)), options = list(`actions-box` = TRUE), selected=NULL, multiple=TRUE),
            sliderInput("tPCofGamesStarted", "% of Games Started", min = min(df$GSPercentage), max = max(df$GSPercentage), value = min(df$GSPercentage)),
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
            
            selectInput("bYAxis","Choose the Y Axis", choices = sort(names(df)), selected = "FP.G"),
            selectInput("bXAxis","Choose the X Axis", choices = c("Status", "Team"), selected = "Status"),
            selectInput("bPlotType","Plot Type", choices = c("Box", "Violin"), selected = "Box"),
            checkboxInput("bTop10", "Top 10 Only", value = FALSE, width = NULL)
          ),
          
          mainPanel(
            plotOutput(outputId = "box",width = "1500px", height = "900px")
          )
        )
     )
  )
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    df_temp <- df
    df_temp <- filter(df_temp, Min.GP >= input$pMinMinsPerGP)
    df_temp <- filter(df_temp, Min >= input$pMinMins)
    df_temp <- filter(df_temp, FPts.90 >= input$pMinFPts.90)
    df_temp <- filter(df_temp, GSPercentage >= input$pPCofGamesStarted)
    if (input$pTop10 == TRUE) {
      df_temp <- subset(df_temp, Top10 == 1)
    }
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
    df_temp <- filter(df_temp, Min >= input$tMinMins)
    df_temp <- filter(df_temp, df$GSPercentage >= input$tPCofGamesStarted)
    
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
          geom_boxplot(coef = 5) + labs(x = "Teams") +
          stat_summary(fun = mean, geom = "point", col = "white") +  # Add points to plot
          stat_summary(fun = mean, geom = "text", col = "White",     # Add text to plot
                       vjust = 1.5, aes(label = paste("Mean:", round(..y.., digits = 1))))
        
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
          geom_boxplot(coef = 5) + labs(x = "Teams")  +
          stat_summary(fun = mean, geom = "point", col = "white") +  # Add points to plot
          stat_summary(fun = mean, geom = "text", col = "White",     # Add text to plot
                       vjust = 1.5, aes(label = paste("Mean:", round(..y.., digits = 1))))
        
      } else {
        #input$fTeamY is a character, so you have to use get() in aes 
        ggplot(df_temp, aes(x=reorder(Team, get(input$bYAxis), FUN=mean), get(input$bYAxis), fill=Team)) +
          geom_violin(aes(fill = factor(Team))) +
          geom_boxplot(coef = 5, width=0.1, color="grey", alpha=0.2) + labs(x = "Teams") +
          labs(x = "Teams")
      }
      
    }

  })
}

shinyApp(ui, server)

