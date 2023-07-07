library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(shinyWidgets)
library(stats)
library(PerformanceAnalytics)
library(viridis)

#----------------------- NOTES -----------------------#
#
#All .90 metrics need to be calculated at the end, otherwise you get crazy big numbers like 2000k FPTs per 90
#
#Check FPts.Mean against FPts.GP on Fantrax regularly throughout the season
#
#Removing all Min == 0 rows is cleaner than making the values NA
#
#If there end up being 3 games in a gameweek then the code for fixing double gameweeks will need to change
#
#----------------------- SETUP -----------------------#

rm(list = ls())

#WILL NEED TO DELETE THE OLD DATA FROM THE V4 FOLDER WHEN THE SEASON BEGINS
setwd("C:/Users/OEM/OneDrive/Documents/R/Fantrax/FantraxV4")

#----------------------- LOADING DATA -----------------------#

files <- list.files(path = "Gameweeks")
FTList <- files[grep("^FT", files, ignore.case = TRUE)]
FSList <- files[grep("^FS", files, ignore.case = TRUE)]

gws <- list()
for(i in 1:length(FTList)){
  FT <- paste0("Gameweeks/", FTList[i])
  FS <- paste0("Gameweeks/", FSList[i])
  
  gameweek <- merge(x = read.csv(FT, header = TRUE), y = read.csv(FS, header = TRUE))
  gws[[i]] <- gameweek
}

gwdf <- bind_rows(gws, .id = "Gameweek")
gwdf$Gameweek <- as.numeric(gwdf$Gameweek)

#----------------------- TEMPLATE -----------------------#

#Template needs to be made first before you remove rows with 0 mins
template <- subset(gwdf, Gameweek == max(gwdf$Gameweek))
template <- select(template, c(Player, Team, Position, Status))

#----------------------- DATA CLEANING -----------------------#

#remove comma from data$Min and AP and convert to numeric 
gwdf <- mutate(gwdf, Min = as.numeric(gsub("\\,", "", Min)))
gwdf <- mutate(gwdf, Min = as.numeric(as.character(Min)))
gwdf <- mutate(gwdf, AP = as.numeric(gsub("\\,", "", AP)))
gwdf <- mutate(gwdf, AP = as.numeric(as.character(AP)))

#Split out Opponent and HomeAway
#NOTE - Opponent might not be correct for each game week. It depends when the data was extracted
gwdf <- mutate(gwdf, HomeOrAway = ifelse(startsWith(gwdf$Opponent, "@"), "Away", "Home"))
gwdf <- mutate(gwdf, Opponent = ifelse(startsWith(gwdf$Opponent, "@"), substring(gwdf$Opponent,2,4), substring(gwdf$Opponent,1,3)))

#Remove the row if they didnt play e.g. Min == 0
gwdf <- subset(gwdf, Min != 0)
# gwdf <- mutate(gwdf, across(1:length(gwdf),  ~replace(.x, .x == 0 & Min == 0, NA)))

#----------------------- NEW GW COLUMNS -----------------------#

#new columns
gwdf <- mutate(gwdf, TkWAndIntAndCLR = TkW + Int + CLR)
gwdf <- mutate(gwdf, SOTAndKP = SOT + KP)
gwdf <- mutate(gwdf, CoSMinusDIS = CoS - DIS)
gwdf <- mutate(gwdf, SOTMinusG = SOT - G)
gwdf <- mutate(gwdf, KPMinusA = KP - A)

#----------------------- GLOBAL VARIABLES -----------------------#

#Statuses
statuses <- c("W (Mon)", "W (Tue)", "W (Wed)", "W (Thu)", "W (Fri)", "W (Sat)", "W (Sun)", "FA")

#Gameweek numbers
gwNumbers <- list.files(path = "Gameweeks", pattern = "^FS.*", full.names = FALSE)
gwNumbers <- lapply(gwNumbers, function(s) substr(s, 6, nchar(s) - 4))
gwNumbers <- sort(as.numeric(gwNumbers))

characterColumns <- c("Gameweek", "ID", "Player", "Team", "Position", "RkOv",
                      "Status", "Opponent", "Ros..", "X...", "PC.", "HomeOrAway")

numericColumns <-  c("Min", "FPts", "GP", "GS", "G", "A", "Pts", "S", "SOT", "YC", "RC", "A2","KP",
                     "AT", "TkW", "DIS", "ErG", "AP", "SFTP", "ACNC", "Int",
                     "CLR", "CoS", "AER", "PKM", "OG", "GAD", "CSD", "CSM",         
                     "FC", "FS", "DPt", "Off", "CS",  "TkWAndIntAndCLR",
                     "SOTAndKP", "CoSMinusDIS", "SOTMinusG", "KPMinusA")

#Variable and calculation combos
varCombos <- numericColumns
for(i in numericColumns){
  for(j in c("SD", "Mean", "Med", "MAD", "DownDev", "90", "MeanMinusDD")){
    varCombos <- c(varCombos, paste(i, j, sep = ".")) 
  }
}

#Identify double gameweeks
doubleGwRows <- subset(gwdf, GP == 2)
doubleGws <- unique(doubleGwRows$Gameweek)

#----------------------- FIX DOUBLE GAMEWEEKS -----------------------#

newRows <- data.frame()
for (i in 1:nrow(gwdf)) {
  if(gwdf$GP[i] == 2){
    newRow <- gwdf[i,]
    newRow$Gameweek <- newRow$Gameweek + 0.5
    for(col in colnames(gwdf)){
      if(!(col %in% characterColumns)){
        newRow[,col] <- newRow[,col] / 2
        gwdf[i,col] <- gwdf[i,col] / 2
      }
    }
    newRows <- rbind(newRows, newRow)
  }
}
gwdf <- rbind(gwdf, newRows)

#----------------------- FUNCTIONS -----------------------#

Add_Columns <- function(df, cols, startGW, endGW){
  
  gwWindow <- subset(gwdf, Gameweek >= startGW & Gameweek <= endGW)
  
  for(i in cols){
    
    if(!(i %in% colnames(df))){
      
      #if it doesn't contain a . then it must be just the var without a calc (e.g. the sum of the var)
      if (!(grepl("\\.", i))) {
        df <- left_join(df, summarise(group_by(gwdf, Player), "{i}" := sum(get(i))), by = "Player")
      }
      else{
        var <- strsplit(i, ".", fixed = TRUE)[[1]][1]
        calc <- strsplit(i, ".", fixed = TRUE)[[1]][2]
        
        if(calc == "SD"){
          df <- left_join(df, summarise(group_by(gwdf, Player), "{var}.SD" := sd(get(var), na.rm = TRUE)), by = "Player")
        }
        else if(calc == "Mean"){
          df <- left_join(df, summarise(group_by(gwdf, Player), "{var}.Mean" := round(mean(get(var), na.rm = TRUE),2)), by = "Player")
        }
        else if(calc == "Med"){
          df <- left_join(df, summarise(group_by(gwdf, Player), "{var}.Med" := median(get(var), na.rm = TRUE)), by = "Player")
        }
        else if(calc == "MAD"){
          df <- left_join(df, summarise(group_by(gwdf, Player), "{var}.MAD" := mad(get(var), constant = 1, na.rm = TRUE)), by = "Player")
        }
        else if(calc == "DownDev"){
          df <- left_join(df,  summarise(group_by(gwdf, Player), "{var}.DownDev" := round(DownsideDeviation(get(var), MAR = mean(get(var)), na.rm = TRUE),2)), by = "Player")
          df[, ncol(df)] <- as.vector(df[, ncol(df)])
        }
        else if(calc == "MeanMinusDD"){
          removeMean <- FALSE
          removeDD <- FALSE
          if (!(paste(var, "Mean", sep = ".") %in% colnames(df))) {
            df <- left_join(df, summarise(group_by(gwdf, Player), "{var}.Mean" := round(mean(get(var), na.rm = TRUE),2)), by = "Player")
            removeMean <- TRUE
          }
          if (!(paste(var, "DownDev", sep = ".") %in% colnames(df))) {
            df <- left_join(df, summarise(group_by(gwdf, Player), "{var}.DownDev" := round(DownsideDeviation(get(var), MAR = mean(get(var)), na.rm = TRUE),2)), by = "Player")
            df[, ncol(df)] <- as.vector(df[, ncol(df)])
            removeDD <- TRUE
          }
          df <- mutate(df, !!paste0(var, ".MeanMinusDD") := round(get(paste0(var, ".Mean")) - get(paste0(var, ".DownDev")), 2))
          if(removeMean == TRUE){
            df <- subset(df, select = -get(paste0(var, ".Mean")))
          }
          if(removeDD == TRUE){
            df <- subset(df, select = -get(paste0(var, ".DownDev")))
          }
        }
        else if(calc == "90"){
          removeVar = FALSE
          removeMin = FALSE
          if (!(var %in% colnames(df))) {
            df <- left_join(df, summarise(group_by(gwdf, Player), "{var}" := sum(get(var))), by = "Player")
            removeVar = TRUE
          }
          if (!("Min" %in% colnames(df))) {
            df <- left_join(df, summarise(group_by(gwdf, Player), Min := sum(Min)), by = "Player")
            removeMin = TRUE
          }
          df <- mutate(df, "{var}.90" := round(((get(var) / Min)*90),2))
          if(removeVar == TRUE){
            df <- subset(df, select = -get(var))
          }
          if(removeMin == TRUE){
            df <- subset(df, select = -Min)
          }
        }
      }
    }
  }
  return(df)
}

test <- template
test <- Add_Columns(test, c("A.90", "SFTP"), 1, 38)

Create_Data <- function(team, status, position, vars, minMins, minFPts.mean, maxFPts.mean, minFPts.90, maxFPts.90, startGW, endGW) {
  
  df <- template
  
  #All tables need Min, FPts.Mean, FPts.90 for the sliders
  df <- Add_Columns(df, c("Min", "FPts.Mean", "FPts.90", vars), startGW, endGW)
  
  df <- filter(df, Min >= minMins)
  df <- filter(df, FPts.Mean >= minFPts.mean)
  df <- filter(df, FPts.Mean <= maxFPts.mean)
  df <- filter(df, FPts.90 >= minFPts.90)
  df <- filter(df, FPts.90 <= maxFPts.90)
  
  if (team != "All") {
    df <- filter(df, Team == team)
  }
  if (status != "All") {
    if (status == "Waiver") {
      df <- filter(df, str_detect(df$Status, "^W \\("))
    }
    else if (status == "All Available") {
      df <- filter(df, str_detect(df$Status, "^W \\(") | str_detect(df$Status, "^FA"))
    }
    else if (status == "All Taken") {
      df <- filter(df, !Status %in% statuses)
    }
    else{
      df <- filter(df, Status == status)  
    }
  }
  if (position != "All") {
    if(position == "D"){
      df <- filter(df, str_detect(df$Position, "D"))
    }
    else if(position == "M"){
      df <- filter(df, str_detect(df$Position, "M"))
    }
    else if(position == "F"){
      df <- filter(df, str_detect(df$Position, "F"))
    }
  }

  return(df)
}

Create_Player_Data <- function(player1, player2, metric, startGW, endGW){
  
  df <- data.frame(Value = numeric(),
                   Gameweek = numeric(),
                   Player = character())
  
  gwWindow <- subset(gwdf, Gameweek >= startGW & Gameweek <= endGW)
  
  players <- c(player1)
  
  if(player2 != "None"){
    players <- c(players, player2)
  }
  
  for(i in players){
    
    values <- gwWindow[[metric]][gwWindow$Player == i]
    values <- as.data.frame(values)
    colnames(values)[1] <- "Value"
    values <- values %>% mutate(Gameweek = row_number())
    values <- values %>% mutate(Player = i)
    
    df <- rbind(df, values)
  }
  
  return(df)
}

Create_Team_Table <- function(vars){
  
  df <- template
  
  df <- Add_Columns(df, vars, 1, length(gwNumbers))
  
  dfFinal <- as.data.frame(unique(df$Status))
  colnames(dfFinal)[] <- "Status"
  
  for(i in vars){
    if(!(i %in% colnames(dfFinal))){
      temp <- df %>% group_by(Status) %>% summarise("{i}" := sum(get(i)))
      dfFinal <- left_join(dfFinal, temp, by = "Status")
    }
  }
  
  dfFinal <- dfFinal[!grepl("^W \\(", dfFinal$Status), ]
  return(dfFinal)
}

#----------------------- CREATE FILTERS REFERENCES -----------------------#
#create basic overall dataframe, so that you have min/max for sliders
overall <- template
overall <- Add_Columns(overall, c("Min", "FPts.Mean", "FPts.90"), 1, max(gwNumbers)) 

#List of our teams for the status dropdowns
fantraxTeams <- unique(overall$Status)
fantraxTeams <- sort(fantraxTeams[!grepl("^W \\(|^FA", fantraxTeams)])

#----------------------- UI -----------------------#
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  navbarPage("Fantrax",
             tabPanel("Plot",
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          width = "2",
                          
                          selectInput("pTeam","Choose a Team", choices = c("All",unique(sort(overall$Team))), selected = "All"),
                          selectInput("pStatus","Choose a Status", choices = c("All", "All Available", "All Taken", "Waiver", fantraxTeams), selected = "All Available"),
                          selectInput("pPosition","Choose a Position", choices = c("All", "D", "M", "F"), selected = "All"),
                          selectInput("pXAxis","Choose the X Axis", choices = sort(varCombos), selected = "FPts.MeanMinusDD"),
                          selectInput("pYAxis","Choose the Y Axis", choices = sort(varCombos), selected = "Min.Mean"),
                          sliderInput("pMinMins", "Minimum Total Minutes", min = min(overall$Min, na.rm = TRUE), max = max(overall$Min, na.rm = TRUE), value = min(10, na.rm = TRUE)),
                          sliderInput("pFPts.Mean", "FPts.Mean", min = min(overall$FPts.Mean, na.rm = TRUE), max = max(overall$FPts.Mean, na.rm = TRUE), value = c(min(overall$FPts.Mean, na.rm = TRUE), max(overall$FPts.Mean, na.rm = TRUE))),
                          sliderInput("pFPts.90", "FPts per 90", min = min(overall$FPts.90, na.rm = TRUE), max = max(overall$FPts.90, na.rm = TRUE), value = c(min(overall$FPts.90, na.rm = TRUE), max(overall$FPts.90, na.rm = TRUE))),
                          sliderInput("pWindow", "Gameweek Window", min = min(gwNumbers), max = max(gwNumbers), value = c(min(gwNumbers), max(gwNumbers)))
                          
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
                          
                          selectInput("tTeam","Choose a team", choices = c("All",unique(sort(overall$Team))), selected = "All"),
                          selectInput("tStatus","Choose a Status", choices = c("All", "All Available", "All Taken", "Waiver", fantraxTeams), selected = "All"),
                          selectInput("tPosition","Choose a Position", choices = c("All", "D", "M", "F"), selected = "All"),
                          sliderInput("tMinMins", "Minimum Total Minutes", min = min(overall$Min, na.rm = TRUE), max = max(overall$Min, na.rm = TRUE), value = min(10, na.rm = TRUE)),
                          sliderInput("tFPts.Mean", "FPts.Mean", min = min(overall$FPts.Mean, na.rm = TRUE), max = max(overall$FPts.Mean, na.rm = TRUE), value = c(min(overall$FPts.Mean, na.rm = TRUE), max(overall$FPts.Mean, na.rm = TRUE))),
                          sliderInput("tFPts.90", "FPts per 90", min = min(overall$FPts.90, na.rm = TRUE), max = max(overall$FPts.90, na.rm = TRUE), value = c(min(overall$FPts.90, na.rm = TRUE), max(overall$FPts.90, na.rm = TRUE))),
                          pickerInput("tPicker", "Columns", choices = sort(varCombos), options = list(`actions-box` = TRUE), selected=NULL, multiple=TRUE),
                          sliderInput("tWindow", "Gameweek Window", min = min(gwNumbers), max = max(gwNumbers), value = c(min(gwNumbers), max(gwNumbers)))
                        ),
                        
                        mainPanel(
                          DT::dataTableOutput("table")
                        )
                      )
             ),
             tabPanel("PlayerPlot",
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          width = "2",
                          
                          selectInput("plPlayer1","Select a Player", choices = sort(template$Player), selected = 1),
                          selectInput("plPlayer2","Select a 2nd Player", choices = c("None", sort(template$Player)), selected = "None"),
                          selectInput("plMetric","Choose a Metric", choices = sort(numericColumns), selected = "FPts"),
                          sliderInput("plWindow", "Gameweek Window", min = min(gwNumbers), max = max(gwNumbers), value = c(min(gwNumbers), max(gwNumbers)))
                          
                        ),
                        
                        mainPanel(
                          plotOutput(outputId = "playerPlot",width = "1500px", height = "900px")
                        )
                      )
             ),
             tabPanel("Team Stats",
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          width = "2",
                          
                          pickerInput("sPicker", "Columns", choices = sort(varCombos), options = list(`actions-box` = TRUE), selected=NULL, multiple=TRUE),
                        ),
                        
                        mainPanel(
                          DT::dataTableOutput("teamTable")
                        )
                      )
             )
  )
)

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    df_temp <- Create_Data(input$pTeam, input$pStatus, input$pPosition, c(input$pXAxis, input$pYAxis), 
                           input$pMinMins, input$pFPts.Mean[1], input$pFPts.Mean[2], input$pFPts.90[1], input$pFPts.90[2],
                           input$pWindow[1], input$pWindow[2])
    
    p <- ggplot(df_temp, aes(colour = Position)) + aes_string(input$pYAxis, input$pXAxis) +
      geom_point() + 
      geom_text(
        aes(label = Player), 
        check_overlap = F,
        adj = -0.1,
        vjust="inward"
      ) + coord_flip(clip = "off") +
      geom_abline(intercept = c(0), slope = 1, color = c("black"), alpha=0.4)
    
    p + theme_classic()
    
  }, res = 90)
  
  output$table = DT::renderDataTable({
    extraCols <- ("FPts.MeanMinusDD")
    df_temp <- Create_Data(input$tTeam, input$tStatus, input$tPosition, c(extraCols, input$tPicker), input$tMinMins,
                           input$tFPts.Mean[1], input$tFPts.Mean[2], input$tFPts.90[1], input$tFPts.90[2],
                           input$tWindow[1], input$tWindow[2])
  })
  
  output$playerPlot <- renderPlot({
    
    df_temp <- Create_Player_Data(input$plPlayer1, input$plPlayer2, input$plMetric, input$plWindow[1], input$plWindow[2])
    
    p <- ggplot(df_temp, aes(x=Gameweek, y=Value, fill=Player)) + 
      geom_area(stat = "smooth", method = "loess", span = 1/5, alpha=0.5 , size=1, colour="white", position = "identity") + 
      scale_fill_brewer(palette = "Set1") +
      ggtitle("Values")
    
    p + theme_classic()
    
  }, res = 90)
  
  output$teamTable = DT::renderDataTable({
    
    cols <- c("Min.Mean", "FPts.Mean", "G.Mean", "A.Mean", "KP.Mean",
              "S.Mean", "SOT.Mean", "AP.Mean", "SFTP.Mean", input$sPicker)
    
    df_temp <- datatable(
      Create_Team_Table(cols),
      options = list(paging = F)
    )
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui, server)