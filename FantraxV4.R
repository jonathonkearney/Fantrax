library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(shinyWidgets)
library(stats)
library(PerformanceAnalytics)

#----------------------- SETUP -----------------------#

rm(list = ls())

#WILL NEED TO DELETE THE OLD DATA FROM THE V4 FOLDER WHEN THE SEASON BEGINS
setwd("C:/Users/OEM/OneDrive/Documents/R/Fantrax/FantraxV4")

#----------------------- LOADING DATA -----------------------#

gws <- list(
  merge(x = read.csv("FT_GW1.csv", header = TRUE), y = read.csv("FS_GW1.csv", header = TRUE)),
  merge(x = read.csv("FT_GW2.csv", header = TRUE), y = read.csv("FS_GW2.csv", header = TRUE)),
  merge(x = read.csv("FT_GW3.csv", header = TRUE), y = read.csv("FS_GW3.csv", header = TRUE)),
  merge(x = read.csv("FT_GW4.csv", header = TRUE), y = read.csv("FS_GW4.csv", header = TRUE)),
  merge(x = read.csv("FT_GW5.csv", header = TRUE), y = read.csv("FS_GW5.csv", header = TRUE)),
  merge(x = read.csv("FT_GW6.csv", header = TRUE), y = read.csv("FS_GW6.csv", header = TRUE)),
  # merge(x = read.csv("FT_GW7.csv", header = TRUE), y = read.csv("FS_GW7.csv", header = TRUE))
  merge(x = read.csv("FT_GW8.csv", header = TRUE), y = read.csv("FS_GW8.csv", header = TRUE)),
  merge(x = read.csv("FT_GW9.csv", header = TRUE), y = read.csv("FS_GW9.csv", header = TRUE)),
  merge(x = read.csv("FT_GW10.csv", header = TRUE), y = read.csv("FS_GW10.csv", header = TRUE)),
  merge(x = read.csv("FT_GW11.csv", header = TRUE), y = read.csv("FS_GW11.csv", header = TRUE)),
  merge(x = read.csv("FT_GW12.csv", header = TRUE), y = read.csv("FS_GW12.csv", header = TRUE)),
  merge(x = read.csv("FT_GW13.csv", header = TRUE), y = read.csv("FS_GW13.csv", header = TRUE)),
  merge(x = read.csv("FT_GW14.csv", header = TRUE), y = read.csv("FS_GW14.csv", header = TRUE)),
  merge(x = read.csv("FT_GW15.csv", header = TRUE), y = read.csv("FS_GW15.csv", header = TRUE)),
  merge(x = read.csv("FT_GW16.csv", header = TRUE), y = read.csv("FS_GW16.csv", header = TRUE)),
  merge(x = read.csv("FT_GW17.csv", header = TRUE), y = read.csv("FS_GW17.csv", header = TRUE)),
  merge(x = read.csv("FT_GW18.csv", header = TRUE), y = read.csv("FS_GW18.csv", header = TRUE)),
  merge(x = read.csv("FT_GW19.csv", header = TRUE), y = read.csv("FS_GW19.csv", header = TRUE)),
  merge(x = read.csv("FT_GW20.csv", header = TRUE), y = read.csv("FS_GW20.csv", header = TRUE)),
  merge(x = read.csv("FT_GW21.csv", header = TRUE), y = read.csv("FS_GW21.csv", header = TRUE)),
  merge(x = read.csv("FT_GW22.csv", header = TRUE), y = read.csv("FS_GW22.csv", header = TRUE)),
  merge(x = read.csv("FT_GW23.csv", header = TRUE), y = read.csv("FS_GW23.csv", header = TRUE)),
  merge(x = read.csv("FT_GW24.csv", header = TRUE), y = read.csv("FS_GW24.csv", header = TRUE)),
  merge(x = read.csv("FT_GW25.csv", header = TRUE), y = read.csv("FS_GW25.csv", header = TRUE)),
  merge(x = read.csv("FT_GW26.csv", header = TRUE), y = read.csv("FS_GW26.csv", header = TRUE)),
  merge(x = read.csv("FT_GW27.csv", header = TRUE), y = read.csv("FS_GW27.csv", header = TRUE)),
  merge(x = read.csv("FT_GW28.csv", header = TRUE), y = read.csv("FS_GW28.csv", header = TRUE)),
  merge(x = read.csv("FT_GW29.csv", header = TRUE), y = read.csv("FS_GW29.csv", header = TRUE)),
  merge(x = read.csv("FT_GW30.csv", header = TRUE), y = read.csv("FS_GW30.csv", header = TRUE)),
  merge(x = read.csv("FT_GW31.csv", header = TRUE), y = read.csv("FS_GW31.csv", header = TRUE)),
  merge(x = read.csv("FT_GW32.csv", header = TRUE), y = read.csv("FS_GW32.csv", header = TRUE)),
  merge(x = read.csv("FT_GW33.csv", header = TRUE), y = read.csv("FS_GW33.csv", header = TRUE)),
  merge(x = read.csv("FT_GW34.csv", header = TRUE), y = read.csv("FS_GW34.csv", header = TRUE)),
  merge(x = read.csv("FT_GW35.csv", header = TRUE), y = read.csv("FS_GW35.csv", header = TRUE)),
  merge(x = read.csv("FT_GW36.csv", header = TRUE), y = read.csv("FS_GW36.csv", header = TRUE)),
  merge(x = read.csv("FT_GW37.csv", header = TRUE), y = read.csv("FS_GW37.csv", header = TRUE)),
  merge(x = read.csv("FT_GW38.csv", header = TRUE), y = read.csv("FS_GW38.csv", header = TRUE))
)
#----------------------- TEMPLATE -----------------------#
#Template needs to be made first before you remove rows with 0 mins
template <- as.data.frame(tail(gws, n=1))
template <- select(template, c(Player, Team, Position, Status))

#----------------------- DATA CLEANING -----------------------#

#remove comma from data$Min and AP and convert to numeric 
gws <- lapply(gws, function(x) mutate(x, Min = as.numeric(gsub("\\,", "", Min))))
gws <- lapply(gws, function(x) mutate(x, Min = as.numeric(as.character(Min))))
gws <- lapply(gws, function(x) mutate(x, AP = as.numeric(gsub("\\,", "", AP))))
gws <- lapply(gws, function(x) mutate(x, AP = as.numeric(as.character(AP))))

#Split out Opponent and HomeAway
#NOTE - Opponent might not be correct for each game week. It depends when the data was extracted
gws <- lapply(gws, function(x) mutate(x, HomeOrAway = ifelse(startsWith(x$Opponent, "@"), "Away", "Home")))
gws <- lapply(gws, function(x) mutate(x, Opponent = ifelse(startsWith(x$Opponent, "@"), substring(x$Opponent,2,4), substring(x$Opponent,1,3))))

#Remove the row if they didnt play e.g. Min == 0 (because 0's mess up SD)
gws <- lapply(gws, function(x) subset(x, Min != 0))

#----------------------- GLOBAL VARIABLES -----------------------#

#Statuses
statuses <- c("W (Mon)", "W (Tue)", "W (Wed)", "W (Thu)", "W (Fri)", "W (Sat)", "W (Sun)", "FA")

#Gameweek numbers
gwNumbers <- list.files(pattern = "^FS.*", full.names = FALSE)
gwNumbers <- lapply(gwNumbers, function(s) substr(s, 6, nchar(s) - 4))
gwNumbers <- sort(as.numeric(gwNumbers))

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

#create basic overall dataframe, so that you have min/max for sliders
overall <- template
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise("FPts.Mean" := round(mean(FPts, na.rm = TRUE),2)), by = "Player")
for (i in c("FPts", "Min")) {
  overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise("{i}" := sum(get(i))), by = "Player")
}
overall <- mutate(overall, "FPts.90" := round(((FPts / Min)*90),2))

#List of our teams for the status dropdowns
fantraxTeams <- unique(overall$Status)
fantraxTeams <- sort(fantraxTeams[!grepl("^W \\(|^FA", fantraxTeams)])
                       
#----------------------- NEW GW COLUMNS -----------------------#

#new columns
gws <- lapply(gws, function(x) mutate(x, TkWAndIntAndCLR = TkW + Int + CLR))
gws <- lapply(gws, function(x) mutate(x, SOTAndKP = SOT + KP))
gws <- lapply(gws, function(x) mutate(x, CoSMinusDIS = CoS - DIS))
gws <- lapply(gws, function(x) mutate(x, SOTMinusG = SOT - G))
gws <- lapply(gws, function(x) mutate(x, KPMinusA = KP - A))

#Make .90 columns for all columns minus Min, GP, and GS
for (i in numericColumns) {
  if(i != "Min" & i != "GP" & i != "GS"){
    gws <- lapply(gws, function(x) mutate(x, "{i}.90" := round(((get(i) / Min)*90),2)))
  }
}
#----------------------- FIX DOUBLE GAMEWEEKS -----------------------#

#Has to happen after gw columns have been created
#Divide double gw columns by 2
for (i in numericColumns) {
  if(i != "GP" & i != "GS"){
    gws <- lapply(gws, function(x) mutate(x, "{i}" := ifelse(x$GP == 2, get(i)/2, get(i))))
  }
}

#----------------------- CREATE DATA FUNCTION -----------------------#

Create_Data <- function(team, status, position, vars, minMins, minFPts.mean, maxFPts.mean, minFPts.90, maxFPts.90, startGW, endGW) {
  
  #CALCULATE THE TWO COLUMNS WE NEED, ADD THEM TO THE DF, THEN FILTER THE DF?
  
  #should always be the last df
  df <- template
  
  #this might play up in the unlikely event of a cancelled gameweek
  gwWindow <- gws[startGW:endGW]
  
  #compute FPts.Mean and FPts.90 for the sliders
  df <- left_join(df, bind_rows(gwWindow) %>% group_by(Player) %>% summarise("FPts.Mean" := round(mean(FPts, na.rm = TRUE),2)), by = "Player")
  for (i in c("FPts", "Min")) {
    df <- left_join(df, bind_rows(gwWindow) %>% group_by(Player) %>% summarise("{i}" := sum(get(i))), by = "Player")
  }
  df <- mutate(df, "FPts.90" := round(((FPts / Min)*90),2))
  
  #calculate and add the new columns
  for(i in vars){
    
    #If the new column to be added is already in the dataframe then skip to the end of the function
    if(!(i %in% colnames(df))){
      
      #if it doesn't contain a . then it must be just the var without a calc (e.g. the sum of the var)
      if (!(grepl("\\.", i))) {
        df <- left_join(df, bind_rows(gwWindow) %>% group_by(Player) %>% summarise("{i}" := sum(get(i))), by = "Player")
      }
      else{
        var <- strsplit(i, ".", fixed = TRUE)[[1]][1]
        calc <- strsplit(i, ".", fixed = TRUE)[[1]][2]
        
        if(calc == "SD"){
          df <- left_join(df, bind_rows(gwWindow) %>% group_by(Player) %>% summarise("{var}.SD" := sd(get(var), na.rm = TRUE)), by = "Player")
        }
        else if(calc == "Mean"){
          if (var != "FPts") {
            df <- left_join(df, bind_rows(gwWindow) %>% group_by(Player) %>% summarise("{var}.Mean" := round(mean(get(var), na.rm = TRUE),2)), by = "Player")
          }
        }
        else if(calc == "Med"){
          df <- left_join(df, bind_rows(gwWindow) %>% group_by(Player) %>% summarise("{var}.Med" := median(get(var), na.rm = TRUE)), by = "Player")
        }
        else if(calc == "MAD"){
          df <- left_join(df, bind_rows(gwWindow) %>% group_by(Player) %>% summarise("{var}.MAD" := mad(get(var), constant = 1, na.rm = TRUE)), by = "Player")
        }
        else if(calc == "DownDev"){
          df <- left_join(df, bind_rows(gwWindow) %>% group_by(Player) %>% summarise("{var}.DownDev" := round(DownsideDeviation(get(var), MAR = mean(get(var)), na.rm = TRUE),2)), by = "Player")
          df[, ncol(df)] <- as.vector(df[, ncol(df)])
        }
        else if(calc == "MeanMinusDD"){
          if (!(paste(var, "Mean", sep = ".") %in% colnames(df))) {
            df <- left_join(df, bind_rows(gwWindow) %>% group_by(Player) %>% summarise("{var}.Mean" := round(mean(get(var), na.rm = TRUE),2)), by = "Player")
          }
          if (!(paste(var, "DownDev", sep = ".") %in% colnames(df))) {
            df <- left_join(df, bind_rows(gwWindow) %>% group_by(Player) %>% summarise("{var}.DownDev" := round(DownsideDeviation(get(var), MAR = mean(get(var)), na.rm = TRUE),2)), by = "Player")
          }
          df <- mutate(df, !!paste0(var, ".MeanMinusDD") := round(get(paste0(var, ".Mean")) - get(paste0(var, ".DownDev")), 2))
        }
        else if(calc == "90"){
          for (i in c(var, "Min")) {
            df <- left_join(df, bind_rows(gwWindow) %>% group_by(Player) %>% summarise("{var}" := sum(get(var))), by = "Player")
          }
          df <- mutate(df, "{var}.90" := round(((get(var) / Min)*90),2))
        }
      }
    }
  }
  
  #Sidebar Filters
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

# test <- Create_Data("All", "All", "All", c("KP.Mean", "G.DownDev"), 10, 1, 30, 1, 30, "1", "38")

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
                          # actionButton("pButton", "Apply")
                          
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
             )
  )
)

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    df_temp <- Create_Data(input$pTeam, input$pStatus, input$pPosition, c(input$pXAxis, input$pYAxis), 
                                 input$pMinMins, input$pFPts.Mean[1], input$pFPts.Mean[2], input$pFPts.90[1], input$pFPts.90[2],
                                 input$pWindow[1], input$pWindow[2])
    
    test <- df_temp
    
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
    
  }, res = 90) #the resolution of the plot
  
  output$table = DT::renderDataTable({
    extraCols <- ("FPts.MeanMinusDD")
    df_temp <- Create_Data(input$tTeam, input$tStatus, input$tPosition, c(extraCols, input$tPicker), input$tMinMins,
                             input$tFPts.Mean[1], input$tFPts.Mean[2], input$tFPts.90[1], input$tFPts.90[2],
                             input$tWindow[1], input$tWindow[2])
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui, server)