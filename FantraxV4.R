library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(shinyWidgets)
library(stats)
library(PerformanceAnalytics)

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
#Some players on certin gameweeks could potentially go higher or lower than the filters
#
#Add a FPts.90MnsMean
#----------------------- SETUP -----------------------#

rm(list = ls())

#WILL NEED TO DELETE THE OLD DATA FROM THE V4 FOLDER WHEN THE SEASON BEGINS
setwd("C:/Users/OEM/OneDrive/Documents/R/Fantrax/FantraxV4")

#----------------------- LOADING DATA -----------------------#

dataStatus <- "Incomplete"

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
  for(j in c("SD", "Mean", "Med", "MAD", "DownDev", "90", "MeanMnsDD", "LQ", "SubMeanMean")){
    varCombos <- c(varCombos, paste(i, j, sep = ".")) 
  }
}

#Identify double gameweeks
doubleGwRows <- subset(gwdf, GP == 2)
doubleGws <- unique(doubleGwRows$Gameweek)

#----------------------- FIX DOUBLE GAMEWEEKS -----------------------#

DGWRows <- gwdf[gwdf$GP == 2, ]
newRows <- gwdf[gwdf$GP == 2, ]
newRows$Gameweek <- newRows$Gameweek + 0.5

DGWRows[, numericColumns] <- DGWRows[, numericColumns] / 2
newRows[, numericColumns] <- newRows[, numericColumns] / 2

gwdf <- subset(gwdf, GP != 2)
DGWRowsAndNewRows <- rbind(DGWRows, newRows)
gwdf <- rbind(gwdf, DGWRowsAndNewRows)

#----------------------- FUNCTIONS -----------------------#

Add_Columns <- function(df, cols, startGW, endGW){
  
  gwWindow <- subset(gwdf, Gameweek >= startGW & Gameweek <= endGW)
  
  for(i in cols){
    
    if(!(i %in% colnames(df))){
      
      #if it doesn't contain a . then it must be just the var without a calc (e.g. the sum of the var)
      if (!(grepl("\\.", i))) {
        df <- left_join(df, summarise(group_by(gwWindow, Player), "{i}" := sum(get(i))), by = "Player")
      }
      else{
        var <- strsplit(i, ".", fixed = TRUE)[[1]][1]
        calc <- strsplit(i, ".", fixed = TRUE)[[1]][2]
        
        df <- do.call(paste0("Add_", calc), list(df, gwWindow, var))
      }
    }
  }
  return(df)
}

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


Create_BoxPlot_Data <- function(metric, selector){
  
  df <- template
  df <- Add_Columns(df, c(metric), 1, max(gwNumbers))
  
  #Some of the values in the new calculated column can be NA if they dont have enough data. So remove them
  df <- filter(df, !(is.na(get(metric))))
  
  #Get rid of the Waiver and FA Statuses if you are looking at status
  if(selector == "Status"){
    df <- df[!grepl("^W \\(", df$Status), ]
    df <- df[!grepl("FA", df$Status), ]
  }

  return(df)
}

#----------------------- CALCULATION FUNCTIONS -----------------------#

Add_SD <- function(df, gwWindow, var){
  df <- left_join(df, summarise(group_by(gwWindow, Player), "{var}.SD" := round(sd(get(var), na.rm = TRUE),2)), by = "Player")
  return(df)
}

Add_Mean <- function(df, gwWindow, var){
  df <- left_join(df, summarise(group_by(gwWindow, Player), "{var}.Mean" := round(mean(get(var), na.rm = TRUE),2)), by = "Player")
  return(df)
}

Add_Med <- function(df, gwWindow, var){
  df <- left_join(df, summarise(group_by(gwWindow, Player), "{var}.Med" := median(get(var), na.rm = TRUE)), by = "Player")
  return(df)
}

Add_LQ <- function(df, gwWindow, var){
  df <- left_join(df, summarise(group_by(gwWindow, Player), "{var}.LQ" := quantile(get(var), na.rm = TRUE)[[2]]), by = "Player")
  return(df)
}

Add_MAD <- function(df, gwWindow, var){
  df <- left_join(df, summarise(group_by(gwWindow, Player), "{var}.MAD" := mad(get(var), constant = 1, na.rm = TRUE)), by = "Player")
  return(df)
}

Add_DownDev <- function(df, gwWindow, var){
  df <- left_join(df,  summarise(group_by(gwWindow, Player), "{var}.DownDev" := round(DownsideDeviation(get(var), MAR = mean(get(var)), na.rm = TRUE),2)), by = "Player")
  df[, ncol(df)] <- as.vector(df[, ncol(df)])
  return(df)
}

Add_MeanMnsDD <- function(df, gwWindow, var){
  removeMean <- FALSE
  removeDD <- FALSE
  if (!(paste(var, "Mean", sep = ".") %in% colnames(df))) {
    df <- Add_Mean(df, gwWindow, var)
    removeMean <- TRUE
  }
  if (!(paste(var, "DownDev", sep = ".") %in% colnames(df))) {
    df <- Add_DownDev(df, gwWindow, var)
    removeDD <- TRUE
  }
  df <- mutate(df, !!paste0(var, ".MeanMnsDD") := round(get(paste0(var, ".Mean")) - get(paste0(var, ".DownDev")), 2))
  if(removeMean == TRUE){
    df <- subset(df, select = -get(paste0(var, ".Mean")))
  }
  if(removeDD == TRUE){
    df <- subset(df, select = -get(paste0(var, ".DownDev")))
  }
  return(df)
}

Add_90 <- function(df, gwWindow, var){
  removeVar = FALSE
  removeMin = FALSE
  if (!(var %in% colnames(df))) {
    df <- left_join(df, summarise(group_by(gwWindow, Player), "{var}" := sum(get(var))), by = "Player")
    removeVar = TRUE
  }
  if (!("Min" %in% colnames(df))) {
    df <- left_join(df, summarise(group_by(gwWindow, Player), Min := sum(Min)), by = "Player")
    removeMin = TRUE
  }
  df <- mutate(df, "{var}.90" := round(((get(var) / Min)*90),2))
  if(removeVar == TRUE){
    df <- subset(df, select = -get(var))
  }
  if(removeMin == TRUE){
    df <- subset(df, select = -Min)
  }
  return(df)
}

Add_SubMeanMean <- function(df, gwWindow, var){
  df <- left_join(df, summarise(group_by(gwWindow, Player), "{var}.SubMeanMean" := round(mean(get(var)[get(var) < mean(get(var), na.rm = TRUE)], na.rm = TRUE), 2)), by = "Player")
  return(df)
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
  
  tags$style(HTML("
    .container-fluid > .nav > li > a[data-value='Incomplete'] {color:red}
    .container-fluid > .nav > li > a[data-value='INCOMPLETE'] {color:red}
    .container-fluid > .nav > li > a[data-value='Complete'] {color:green}
    container-fluid > .nav > li > a[data-value='COMPLETE'] {color:green}
  ")),
  
  
  theme = shinytheme("flatly"),
  
  navbarPage("Fantrax",
             tabPanel("Plot",
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          width = "2",
                          
                          selectInput("pTeam","Choose a Team", choices = c("All",unique(sort(overall$Team))), selected = "All"),
                          selectInput("pStatus","Choose a Status", choices = c("All", "All Available", "All Taken", "Waiver", fantraxTeams), selected = "All Available"),
                          selectInput("pPosition","Choose a Position", choices = c("All", "D", "M", "F"), selected = "All"),
                          selectInput("pXAxis","Choose the X Axis", choices = sort(varCombos), selected = "FPts.90"),
                          selectInput("pYAxis","Choose the Y Axis", choices = sort(varCombos), selected = "FPts.Mean"),
                          sliderInput("pMinMins", "Minimum Total Minutes", min = min(overall$Min, na.rm = TRUE), max = max(overall$Min, na.rm = TRUE), value = min(10, na.rm = TRUE)),
                          sliderInput("pFPts.Mean", "FPts.Mean", min = min(gwdf$FPts, na.rm = TRUE), max = max(gwdf$FPts, na.rm = TRUE), value = c(min(gwdf$FPts, na.rm = TRUE), max(gwdf$FPts, na.rm = TRUE))),
                          sliderInput("pFPts.90", "FPts per 90", min = 0, max = 100, value = c(min(overall$FPts.90, na.rm = TRUE), max(overall$FPts.90, na.rm = TRUE))),
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
                          sliderInput("tFPts.Mean", "FPts.Mean", min = min(gwdf$FPts, na.rm = TRUE), max = max(gwdf$FPts, na.rm = TRUE), value = c(min(gwdf$FPts, na.rm = TRUE), max(gwdf$FPts, na.rm = TRUE))),
                          sliderInput("tFPts.90", "FPts per 90", min = min(overall$FPts.90, na.rm = TRUE), max = max(overall$FPts.90, na.rm = TRUE), value = c(min(overall$FPts.90, na.rm = TRUE), max(overall$FPts.90, na.rm = TRUE))),
                          pickerInput("tPicker", "Columns", choices = sort(varCombos), options = list(`actions-box` = TRUE), selected=NULL, multiple=TRUE),
                          sliderInput("tWindow", "Gameweek Window", min = min(gwNumbers), max = max(gwNumbers), value = c(min(gwNumbers), max(gwNumbers)))
                        ),
                        
                        mainPanel(
                          DT::dataTableOutput("table"),
                          div(style="margin-bottom:10px"),
                          DT::dataTableOutput("selectTable")
                        )
                      )
             ),
             tabPanel("Box Plot",
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          width = "2",
                          
                          selectInput("bpSelector","Choose a Team Type", choices = c("Status", "Team"), selected = "Status"),
                          selectInput("bpMetric","Choose a Metric", choices = sort(varCombos), selected = "FPts.Mean")
                          # sliderInput("bpWindow", "Gameweek Window", min = min(gwNumbers), max = max(gwNumbers), value = c(min(gwNumbers), max(gwNumbers)))
                        ),
                        
                        mainPanel(
                          plotOutput(outputId = "boxPlot",width = "1500px", height = "900px")
                        )
                      )
             ),
             tabPanel(dataStatus,
             )
  )
)

#----------------------- Server -----------------------#

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

  tableDF <- data.frame()
  selectTableDF <- data.frame()
  selectedPlayers <- c()
  extraCols <- c("Min.Mean", "FPts.MeanMnsDD", "SFTP.90", "Pts.90", "CS.90", "KP.90", "GS.Mean")
  
  output$table = DT::renderDataTable({
    df_temp <- Create_Data(input$tTeam, input$tStatus, input$tPosition, c(extraCols, input$tPicker), input$tMinMins,
                           input$tFPts.Mean[1], input$tFPts.Mean[2], input$tFPts.90[1], input$tFPts.90[2],
                           input$tWindow[1], input$tWindow[2])

    tableDF <<- df_temp
  }, options = list(pageLength = 10))
  
  output$selectTable = DT::renderDataTable({
    df_temp <- Create_Data("All", "All", "All", c(extraCols, input$tPicker), 1,
                                  min(gwdf$FPts, na.rm = TRUE), max(gwdf$FPts, na.rm = TRUE), min(overall$FPts.90, na.rm = TRUE),
                                  max(overall$FPts.90, na.rm = TRUE), input$tWindow[1], input$tWindow[2])

    df_temp <- subset(df_temp, Player %in% selectedPlayers)

    selectTableDF <<- df_temp
  }, options = list(dom = 't'))
  
  observeEvent(input$table_row_last_clicked, {
    #add the player to the player list
    playerToAdd <- tableDF$Player[input$table_row_last_clicked]
    selectedPlayers <<- c(selectedPlayers, playerToAdd)
    
    #clear the table selection
    selectRows(dataTableProxy("table"), "none")
    
    #replace the data on the select table
    df_temp <- Create_Data("All", "All", "All", c(extraCols, input$tPicker), 1,
                           min(overall$FPts.Mean, na.rm = TRUE), max(overall$FPts.Mean, na.rm = TRUE), min(overall$FPts.90, na.rm = TRUE),
                           max(overall$FPts.90, na.rm = TRUE), input$tWindow[1], input$tWindow[2])
    df_temp <- subset(df_temp, Player %in% selectedPlayers)
    replaceData(dataTableProxy("selectTable"), df_temp, resetPaging = FALSE)
    
    #update select table
    selectTableDF <<- df_temp
  })
  
  observeEvent(input$selectTable_row_last_clicked, {
    #remove the player from the player list
    playerToRemove <- selectTableDF$Player[input$selectTable_row_last_clicked]
    selectedPlayers <<- selectedPlayers[!selectedPlayers == playerToRemove]
    
    #clear the select table selection
    selectRows(dataTableProxy("selectTable"), "none")

    #replace the data on the select table
    df_temp <- Create_Data("All", "All", "All", c(extraCols, input$tPicker), 1,
                           min(overall$FPts.Mean, na.rm = TRUE), max(overall$FPts.Mean, na.rm = TRUE), min(overall$FPts.90, na.rm = TRUE),
                           max(overall$FPts.90, na.rm = TRUE), input$tWindow[1], input$tWindow[2])
    df_temp <- subset(df_temp, Player %in% selectedPlayers)
    replaceData(dataTableProxy("selectTable"), df_temp, resetPaging = FALSE)
    
    #update select table
    selectTableDF <<- df_temp
  })
  
  output$boxPlot <- renderPlot({
    
    df_temp <- Create_BoxPlot_Data(input$bpMetric, input$bpSelector)
    
    p <- ggplot(df_temp, aes(x = reorder(get(input$bpSelector), get(input$bpMetric), FUN=mean), y = get(input$bpMetric), fill = get(input$bpSelector))) +
      geom_boxplot() +
      stat_summary(
        fun = mean,
        geom = "point",
        shape = 23,
        size = 3,
        fill = "white",
        color = "black"
      ) +
      labs(title = "Distributions",
           x = input$bpSelector,
           y = input$bpMetric)
    
    p + theme_classic()
    
  }, res = 90)
  
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui, server)
