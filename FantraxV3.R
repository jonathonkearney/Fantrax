library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(rvest)
library(stringi)
library(shinyWidgets)
library(stats)
library(PerformanceAnalytics)


# *********************** LOAD & CLEAN DATA ***************************
rm(list = ls())

setwd("C:/Users/OEM/OneDrive/Documents/R/Fantrax/FantraxV3")

#Create a list of dataframes for each game week
#CHECK THESE AGAINST FANTRAX. THE FP.G SHOULD MATCH Fpts.Mean
#e.g. go through each gameweek in Fantrax and check it against the gameweeks here
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
  merge(x = read.csv("FT_GW20.csv", header = TRUE), y = read.csv("FS_GW20.csv", header = TRUE)) #INCOMPLETE
)

#Statuses
statuses <- c("W (Mon)", "W (Tue)", "W (Wed)", "W (Thu)", "W (Fri)", "W (Sat)", "W (Sun)", "FA")

#remove comma from data$Min and AP and convert to numeric 
gws <- lapply(gws, function(x) mutate(x, Min = as.numeric(gsub("\\,", "", Min))))
gws <- lapply(gws, function(x) mutate(x, Min = as.numeric(as.character(Min))))
gws <- lapply(gws, function(x) mutate(x, AP = as.numeric(gsub("\\,", "", AP))))
gws <- lapply(gws, function(x) mutate(x, AP = as.numeric(as.character(AP))))

#Split out Opponent and HomeAway
#NOTE - Opponent might not be correct for each game week. It depends when the data was extracted
gws <- lapply(gws, function(x) mutate(x, HomeOrAway = ifelse(startsWith(x$Opponent, "@"), "Away", "Home")))
gws <- lapply(gws, function(x) mutate(x, Opponent = ifelse(startsWith(x$Opponent, "@"), substring(x$Opponent,2,4), substring(x$Opponent,1,3))))

# *********************** ADD NEW COLUMNS TO GW DATA ***************************

#new columns
gws <- lapply(gws, function(x) mutate(x, TkWAndIntAndCLR = TkW + Int + CLR))
gws <- lapply(gws, function(x) mutate(x, SOTAndKP = SOT + KP))
gws <- lapply(gws, function(x) mutate(x, CoSMinusDIS = CoS - DIS))
gws <- lapply(gws, function(x) mutate(x, SOTMinusG = SOT - G))
gws <- lapply(gws, function(x) mutate(x, KPMinusA = KP - A))

#Dynamically create .90 columns for the gws
per90Columns <-  c("FPts", "G", "A", "Pts", "S", "SOT", "YC", "RC", "A2","KP",
                   "AT", "TkW", "DIS", "ErG", "AP", "SFTP", "ACNC", "Int",
                   "CLR", "CoS", "AER", "PKM", "OG", "GAD", "CSD", "CSM",
                   "FC", "FS", "DPt", "Off", "CS",  "TkWAndIntAndCLR",
                   "SOTAndKP", "CoSMinusDIS", "SOTMinusG", "KPMinusA")

for (i in per90Columns) {
  gws <- lapply(gws, function(x) mutate(x, "{i}.90" := round(((get(i) / Min)*90),2)))
}

# *********************** CREATE OVERALL AND LAST5 DATAFRAMES ***************************

#Use the latest GW as a starting template
template <- as.data.frame(tail(gws, n=1))
template <- select(template, c(ID, Player, Team, Position, Status, Opponent))
overall <- template
last5 <- template

GWColumns <-  c("Min", "FPts", "GP", "GS", "G", "A", "Pts", "S", "SOT", "YC", "RC", "A2","KP",
                 "AT", "TkW", "DIS", "ErG", "AP", "SFTP", "ACNC", "Int",
                 "CLR", "CoS", "AER", "PKM", "OG", "GAD", "CSD", "CSM",         
                 "FC", "FS", "DPt", "Off", "CS",  "TkWAndIntAndCLR",
                 "SOTAndKP", "CoSMinusDIS", "SOTMinusG", "KPMinusA")


#SUM each (summable) GW column to create the overall/last 5 columns
for (i in GWColumns) {
  overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise("{i}" := sum(get(i))), by = "Player")
  last5 <- left_join(last5, bind_rows(tail(gws, n=5)) %>% group_by(Player) %>% summarise("{i}" := sum(get(i))), by = "Player")
}

# *********************** DYNAMICALLY CREATE NEW COLUMNS ***************************

#Remove the row if they didnt play e.g. Min == 0 (because 0's mess up SD)
gwsMinus0Min <- lapply(gws, function(x) subset(x, Min != 0))

for (i in GWColumns) {
  #Standard Deviation
  overall <- left_join(overall, bind_rows(gwsMinus0Min) %>% group_by(Player) %>% summarise("{i}.SD" := sd(get(i), na.rm = TRUE)), by = "Player")
  last5 <- left_join(last5, bind_rows(tail(gwsMinus0Min, n=5)) %>% group_by(Player) %>% summarise("{i}.SD" := sd(get(i), na.rm = TRUE)), by = "Player")
  
  #Minimum
  overall <- left_join(overall, bind_rows(gwsMinus0Min) %>% group_by(Player) %>% summarise("{i}.Min" := min(get(i), na.rm = TRUE)), by = "Player")
  last5 <- left_join(last5, bind_rows(tail(gwsMinus0Min, n=5)) %>% group_by(Player) %>% summarise("{i}.Min" := min(get(i), na.rm = TRUE)), by = "Player")
  
  #Maximum
  overall <- left_join(overall, bind_rows(gwsMinus0Min) %>% group_by(Player) %>% summarise("{i}.Max" := max(get(i), na.rm = TRUE)), by = "Player")
  last5 <- left_join(last5, bind_rows(tail(gwsMinus0Min, n=5)) %>% group_by(Player) %>% summarise("{i}.Max" := max(get(i), na.rm = TRUE)), by = "Player")

  #Mean
  # overall <- left_join(overall, bind_rows(gwsMinus0Min) %>% group_by(Player) %>% summarise("{i}.Mean" := mean(get(i), na.rm = TRUE)), by = "Player")
  # last5 <- left_join(last5, bind_rows(tail(gwsMinus0Min, n=5)) %>% group_by(Player) %>% summarise("{i}.Mean" := mean(get(i), na.rm = TRUE)), by = "Player")
  overall <- left_join(overall, bind_rows(gwsMinus0Min) %>% group_by(Player) %>% summarise("{i}.Mean" := round(mean(get(i), na.rm = TRUE),2)), by = "Player")
  last5 <- left_join(last5, bind_rows(tail(gwsMinus0Min, n=5)) %>% group_by(Player) %>% summarise("{i}.Mean" := round(mean(get(i), na.rm = TRUE),2)), by = "Player")
  
  #Median
  overall <- left_join(overall, bind_rows(gwsMinus0Min) %>% group_by(Player) %>% summarise("{i}.Med" := median(get(i), na.rm = TRUE)), by = "Player")
  last5 <- left_join(last5, bind_rows(tail(gwsMinus0Min, n=5)) %>% group_by(Player) %>% summarise("{i}.Med" := median(get(i), na.rm = TRUE)), by = "Player")
  
  #Lower Quartile 
  overall <- left_join(overall, bind_rows(gwsMinus0Min) %>% group_by(Player) %>% summarise("{i}.LQ" := quantile(get(i), .25, na.rm = TRUE)), by = "Player")
  last5 <- left_join(last5, bind_rows(tail(gwsMinus0Min, n=5)) %>% group_by(Player) %>% summarise("{i}.LQ" := quantile(get(i), .25, na.rm = TRUE)), by = "Player")  
  
  #Median Absolute Deviation - needs to have constant = 1
  overall <- left_join(overall, bind_rows(gwsMinus0Min) %>% group_by(Player) %>% summarise("{i}.MAD" := mad(get(i), constant = 1, na.rm = TRUE)), by = "Player")
  last5 <- left_join(last5, bind_rows(tail(gwsMinus0Min, n=5)) %>% group_by(Player) %>% summarise("{i}.MAD" := mad(get(i), constant = 1, na.rm = TRUE)), by = "Player")
  
  #Downside Deviation
  overall <- left_join(overall, bind_rows(gwsMinus0Min) %>% group_by(Player) %>% summarise("{i}.DownDev" := round(DownsideDeviation(get(i), MAR = mean(get(i)), na.rm = TRUE),2)), by = "Player")
  last5 <- left_join(last5, bind_rows(tail(gwsMinus0Min, n=5)) %>% group_by(Player) %>% summarise("{i}.DownDev" := round(DownsideDeviation(get(i), MAR = mean(get(i)), na.rm = TRUE),2)), by = "Player")
  
  
}

MADColumns <-  c("FPts.90", "G.90", "A.90", "Pts.90", "S.90", "SOT.90", "YC.90", "RC.90", "A2.90","KP.90",
                   "AT.90", "TkW.90", "DIS.90", "ErG.90", "AP.90", "SFTP.90", "ACNC.90", "Int.90",
                   "CLR.90", "CoS.90", "AER.90", "PKM.90", "OG.90", "GAD.90", "CSD.90", "CSM.90",
                   "FC.90", "FS.90", "DPt.90", "Off.90", "CS.90",  "TkWAndIntAndCLR.90",
                   "SOTAndKP.90", "CoSMinusDIS.90", "SOTMinusG.90", "KPMinusA.90")

#Create MAD columns for the .90 metrics
for (i in MADColumns) {
  overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise("{i}.MAD" := mad(get(i), constant = 1, na.rm = TRUE)), by = "Player")
  last5 <- left_join(last5, bind_rows(tail(gws, n=5)) %>% group_by(Player) %>% summarise("{i}.MAD" := mad(get(i), constant = 1, na.rm = TRUE)), by = "Player")
}

# *********************** CREATE COLUMNS THAT CAN ONLY BE ADDED AT THE OVERALL/LAST5 STAGE ***************************

#.90 Columns
for (i in per90Columns) {
  overall <- mutate(overall, "{i}.90" := round(((get(i) / Min)*90),2))
  last5 <- mutate(last5, "{i}.90" := round(((get(i) / Min)*90),2))
}

overall <- mutate(overall, "KPConversion" = round((AT / KP),2))
last5 <- mutate(last5, "KPConversion" = round((AT / KP),2))

overall <- mutate(overall, "SOTConversion" = round((G / SOT),2))
last5 <- mutate(last5, "SOTConversion" = round((G / SOT),2))

overall <- mutate(overall, "ShotAcc" = round((SOT / S),2))
last5 <- mutate(last5, "ShotAcc" = round((SOT / S),2))

overall <- mutate(overall, "InFinal3rd" = round((SFTP / AP),2))
last5 <- mutate(last5, "InFinal3rd" = round((SFTP / AP),2))

overall <- mutate(overall, "FPts.MedMinusMean" = round((FPts.Med - FPts.Mean),2))
last5 <- mutate(last5, "FPts.MedMinusMean" = round((FPts.Med - FPts.Mean),2))

#Would these be the best measure for picking the draft for next season?
overall <- mutate(overall, "FPts.MeanMinusMAD" = round((FPts.Mean - FPts.MAD),2))
last5 <- mutate(last5, "FPts.MeanMinusMAD" = round((FPts.Mean - FPts.MAD),2))
overall <- mutate(overall, "FPts.MedMinusMAD" = round((FPts.Med - FPts.MAD),2))
last5 <- mutate(last5, "FPts.MedMinusMAD" = round((FPts.Med - FPts.MAD),2))

#This formula will need to change if the points system changes
overall <- mutate(overall, "GhostPts.90" = round(((FPts - ( (CSD * 6) + (CSM * 1) + (G * 9) + (AT * 6))) / Min) * 90,2))
last5 <-  mutate(last5, "GhostPts.90" = round(((FPts - ( (CSD * 6) + (CSM * 1) + (G * 9) + (AT * 6))) / Min) * 90,2))

#GhostPts.90.PC - Percent of points that are Ghost points
overall <- mutate(overall, "GhostPts.90.PC" = round(GhostPts.90 / FPts.90,2))
last5 <-  mutate(last5, "GhostPts.90.PC" = round(GhostPts.90 / FPts.90,2))

#mean - Downwards Deviation
overall <- mutate(overall, "FPts.MeanMinusDD" = round(FPts.Mean - FPts.DownDev,2))
last5 <-  mutate(last5, "FPts.MeanMinusDD" = round(FPts.Mean - FPts.DownDev,2))


# *********************** ADD TOP10 TO DATAFRAMES ***************************

#Mark the top 10 players for each Status by their regular FPts.Mean score
top10 <- subset(overall, !(overall$Status %in% statuses))
top10 <- top10 %>%                                      
  arrange(desc(FPts.Mean)) %>% 
  group_by(Status) %>%
  slice(1:10)
top10$Top10 <- 1
top10 <- subset(top10, select = c(Player, Top10))
overall <- full_join(overall, top10, by = "Player")
overall$Top10 <- ifelse(is.na(overall$Top10), 0, overall$Top10)

#Mark the top 10 players for each Status by their regular FPts.Mean score
top10 <- subset(last5, !(last5$Status %in% statuses))
top10 <- top10 %>%                                      
  arrange(desc(FPts.Mean)) %>% 
  group_by(Status) %>%
  slice(1:10)
top10$Top10 <- 1
top10 <- subset(top10, select = c(Player, Top10))
last5 <- full_join(last5, top10, by = "Player")
last5$Top10 <- ifelse(is.na(last5$Top10), 0, last5$Top10)

# ****************************************************************************************************

#find the highest values between both last5 and overall for use below
maxFPts.Mean <- max(c(max(overall$FPts.Mean, na.rm = TRUE),max(last5$FPts.Mean, na.rm = TRUE)),na.rm = TRUE)
maxFPts.90 <- max(c(max(overall$FPts.90, na.rm = TRUE),max(last5$FPts.90, na.rm = TRUE)),na.rm = TRUE)
minFPts.Mean <- min(c(min(overall$FPts.Mean, na.rm = TRUE),min(last5$FPts.Mean, na.rm = TRUE)),na.rm = TRUE)
minFPts.90 <- min(c(min(overall$FPts.90, na.rm = TRUE),min(last5$FPts.90, na.rm = TRUE)),na.rm = TRUE)

# ****************************************************************************************************
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
                          selectInput("pYAxis","Choose the Y Axis", choices = sort(names(overall)), selected = "FPts.Mean"),
                          selectInput("pXAxis","Choose the X Axis", choices = sort(names(overall)), selected = "Min.MAD"),
                          sliderInput("pMinMins", "Minimum Total Minutes", min = min(overall$Min, na.rm = TRUE), max = max(overall$Min, na.rm = TRUE), value = min(10, na.rm = TRUE)),
                          # sliderInput("pMinMinsPerGP", "Minimum Minutes Per GP", min = min(overall$Min.Mean, na.rm = TRUE), max = max(overall$Min.Mean, na.rm = TRUE), value = min(overall$Min.Mean, na.rm = TRUE)),
                          # sliderInput("pMinFPts.Mean", "Minimum FPts.Mean", min = min(overall$FPts.Mean, na.rm = TRUE), max = max(overall$FPts.Mean, na.rm = TRUE), value = min(overall$FPts.Mean, na.rm = TRUE)),
                          sliderInput("pMinFPts.Mean", "Minimum FPts.Mean", min = minFPts.Mean, max = maxFPts.Mean, value = c(minFPts.Mean, maxFPts.Mean)),
                          # sliderInput("pMinFPts.90", "Minimum FPts per 90", min = min(overall$FPts.90, na.rm = TRUE), max = max(overall$FPts.90, na.rm = TRUE), value = min(overall$FPts.90, na.rm = TRUE)),
                          sliderInput("pMinFPts.90", "Minimum FPts per 90", min = minFPts.90, max = maxFPts.90, value = c(minFPts.90, maxFPts.90)),
                          checkboxInput("pAddLines", "Add Lines", value = FALSE, width = NULL),
                          radioButtons("pLast5", "Overall or Last 5", choices = list("Overall" = 1, "Last 5" = 2),selected = 1),
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
                          
                          selectInput("tTeam","Choose a team", choices = c("All",unique(overall$Team)), selected = "All"),
                          selectInput("tStatus","Choose a Status", choices = c("All", "All Available", "All Taken", unique(overall$Status), "Waiver"), selected = "All"),
                          selectInput("tPosition","Choose a Position", choices = c("All", "D", "M", "F"), selected = "All"),
                          sliderInput("tMinMinsPerGP", "Minimum Minutes Per GP", min = min(overall$Min.Mean, na.rm = TRUE), max = max(overall$Min.Mean, na.rm = TRUE), value = min(overall$Min.Mean, na.rm = TRUE)),
                          sliderInput("tMinMins", "Minimum Total Minutes", min = min(overall$Min, na.rm = TRUE), max = max(overall$Min, na.rm = TRUE), value = min(overall$Min, na.rm = TRUE)),
                          pickerInput("tPicker", "Columns", choices = sort(names(overall)), options = list(`actions-box` = TRUE), selected=NULL, multiple=TRUE),
                          radioButtons("tLast5", "Overall or Last 5", choices = list("Overall" = 1, "Last 5" = 2),selected = 1),
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
                          
                          selectInput("bYAxis","Choose the Y Axis", choices = sort(names(overall)), selected = "FPts.LQ"),
                          selectInput("bXAxis","Choose the X Axis", choices = c("Status", "Team"), selected = "Status"),
                          selectInput("bPlotType","Plot Type", choices = c("Box", "Violin"), selected = "Box"),
                          radioButtons("bLast5", "Overall or Last 5", choices = list("Overall" = 1, "Last 5" = 2),selected = 1),
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
    if (input$pLast5 == 1) {
      df_temp <- overall
    }else{
      df_temp <- last5
    }
    #input$range[1] for min and input$range[2] for max
    # df_temp <- filter(df_temp, FPts.Mean >= input$pMinFPts.Mean)
    df_temp <- filter(df_temp, FPts.Mean >= input$pMinFPts.Mean[1])
    df_temp <- filter(df_temp, FPts.Mean <= input$pMinFPts.Mean[2])
    df_temp <- filter(df_temp, Min >= input$pMinMins)
    # df_temp <- filter(df_temp, FPts.90 >= input$pMinFPts.90)
    df_temp <- filter(df_temp, FPts.90 >= input$pMinFPts.90[1])
    df_temp <- filter(df_temp, FPts.90 <= input$pMinFPts.90[2])
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
      ) + coord_flip(clip = "off") +
      geom_abline(intercept = c(0), slope = 1, color = c("black"), alpha=0.4)
    
    if(input$pAddLines == TRUE) {
      p + theme_classic() + geom_smooth(method="lm", se=F)
    }
    else{
      p + theme_classic()
    }
    
  }, res = 90) #the resolution of the plot
  
  
  output$table = DT::renderDataTable({
    if (input$tLast5 == 1) {
      df_temp <- overall
    }else{
      df_temp <- last5
    }
    df_temp <- filter(df_temp, Min.Mean >= input$tMinMinsPerGP)
    df_temp <- filter(df_temp, Min >= input$tMinMins)
    
    columns <- c("Player", "Team", "Status", "Position", "FPts.Mean", "FPts.Med", "FPts.MAD", "FPts.90", "G", "A", "Min",
                 "Min.Mean", "GhostPts.90", "GhostPts.90.PC", "FPts.MeanMinusDD")
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
    
    if (input$bLast5 == 1) {
      df_temp <- overall
    }else{
      df_temp <- last5
    }
    #NAs mess up box plots, specifically their order, so we need to remove them
    df_temp <- df_temp[!is.na(df_temp[[input$bYAxis]]), ]
    
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

