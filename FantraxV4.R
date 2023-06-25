library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
# library(stringi)
# library(shinyWidgets)
library(stats)
# library(PerformanceAnalytics)

#This version will compute the data in real time, not before shiny has loaded. It will be slower but more customizable
#It might be useful to pre compute all the overall stats (oldest gw to latest), then recompute custom gw windows on the fly
#Ideally I'd like to compute some data beforehand, like FPts.mean and FPts.90, but I suppose I would have to do so for every gw combination?

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

Create_Data <- function(team, status, position, x, y, minMins, minFPts.mean, maxFPts.mean, minFPts.90, maxFPts.90, startGW, endGW) {
  
  columns <- c()
  gwWindow <- gws[startGW:endGW]
  df <- gws[[endGW]]
  df <- select(template, c(ID, Player, Team, Position, Status))
  
  if(startGW == min(gwNumbers) & endGW == max(gwNumbers)){
    columns <- numericColumns
  }
  else{
    columns <- c(x, y)
    
    
    # gwWindow <- lapply(gwWindow, function(x) subset(x, Team == team, Status == ))
    
  }
  
  
  #SUM each (summable) GW column
  for (i in columns) {
    df <- left_join(overall, bind_rows(gwWindow) %>% group_by(Player) %>% summarise("{i}" := sum(get(i))), by = "Player")
  }
  
}


#Use the latest GW as a starting template
template <- as.data.frame(tail(gws, n=1))
template <- select(template, c(ID, Player, Team, Position, Status))
overall <- template

#SUM each (summable) GW column to create the overall columns
for (i in numericColumns) {
  overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise("{i}" := sum(get(i))), by = "Player")
}

#Remove the row if they didnt play e.g. Min == 0 (because 0's mess up SD)
gwsMinus0Min <- lapply(gws, function(x) subset(x, Min != 0))

for (i in numericColumns) {
  overall <- left_join(overall, bind_rows(gwsMinus0Min) %>% group_by(Player) %>% summarise("{i}.Mean" := round(mean(get(i), na.rm = TRUE),2)), by = "Player")
}

for (i in numericColumns) {
  if(i != "Min" & i != "GP" & i != "GS"){
    overall <- mutate(overall, "{i}.90" := round(((get(i) / Min)*90),2))
  }
}
#----------------------- CREATE OVERALL DATAFRAME -----------------------#
# overall <- Create_Data()





#----------------------- UI -----------------------#
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  navbarPage("Fantrax",
             tabPanel("Plot",
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          width = "2",
                          
                          selectInput("pTeam","Choose a Team", choices = c("All",unique(sort(overall$Team))), selected = "All"),
                          selectInput("pStatus","Choose a Status", choices = c("All", "All Available", "All Taken", unique(sort(overall$Status)), "Waiver"), selected = "All Available"),
                          selectInput("pPosition","Choose a Position", choices = c("All", "D", "M", "F"), selected = "All"),
                          selectInput("pXAxis","Choose the X Axis", choices = sort(names(overall)), selected = "FPts.MeanMinusDD"),
                          selectInput("pYAxis","Choose the Y Axis", choices = sort(names(overall)), selected = "Min.Mean"),
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
                          
                          # selectInput("tTeam","Choose a team", choices = c("All",unique(overall$Team)), selected = "All"),
                          # selectInput("tStatus","Choose a Status", choices = c("All", "All Available", "All Taken", unique(overall$Status), "Waiver"), selected = "All"),
                          # selectInput("tPosition","Choose a Position", choices = c("All", "D", "M", "F"), selected = "All"),
                          # sliderInput("tMinMinsPerGP", "Minimum Minutes Per GP", min = min(overall$Min.Mean, na.rm = TRUE), max = max(overall$Min.Mean, na.rm = TRUE), value = min(overall$Min.Mean, na.rm = TRUE)),
                          # sliderInput("tMinMins", "Minimum Total Minutes", min = min(overall$Min, na.rm = TRUE), max = max(overall$Min, na.rm = TRUE), value = min(overall$Min, na.rm = TRUE)),
                          # pickerInput("tPicker", "Columns", choices = sort(names(overall)), options = list(`actions-box` = TRUE), selected=NULL, multiple=TRUE),
                          # radioButtons("tLast5", "Overall or Last 5", choices = list("Overall" = 1, "Last 5" = 2),selected = 1),
                        ),
                        
                        mainPanel(
                          # DT::dataTableOutput("table")
                        )
                      )
             )
  )
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    df_temp <- Scatter_Plot_Data(input$pTeam, input$pStatus, input$pPosition, input$pXAxis, input$pYAxis, 
                                 input$pMinMins, input$pFPts.Mean[1], input$pFPts.Mean[2], input$pFPts.90[1], input$pFPts.90[2],
                                 input$pWindow[1], input$pWindow[2])
    
    
    
  }, res = 90) #the resolution of the plot
  
  output$table = DT::renderDataTable({
    
  })
}

shinyApp(ui, server)