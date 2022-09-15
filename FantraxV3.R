library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(rvest)
library(stringi)
library(shinyWidgets)
library(forecast)

# *********************** LOAD & CLEAN DATA ***************************
rm(list = ls())

setwd("C:/Users/OEM/OneDrive/Documents/R/Fantrax/FantraxV3")

#Create a list of dataframes for each game week
gws <- list(
  merge(x = read.csv("FT_GW1.csv", header = TRUE), y = read.csv("FS_GW1.csv", header = TRUE)),
  merge(x = read.csv("FT_GW2.csv", header = TRUE), y = read.csv("FS_GW2.csv", header = TRUE)),
  merge(x = read.csv("FT_GW3.csv", header = TRUE), y = read.csv("FS_GW3.csv", header = TRUE)),
  merge(x = read.csv("FT_GW4.csv", header = TRUE), y = read.csv("FS_GW4.csv", header = TRUE)),
  merge(x = read.csv("FT_GW5.csv", header = TRUE), y = read.csv("FS_GW5.csv", header = TRUE)),
  merge(x = read.csv("FT_GW6.csv", header = TRUE), y = read.csv("FS_GW6.csv", header = TRUE))
  # merge(x = read.csv("FT_GW7.csv", header = TRUE), y = read.csv("FS_GW7.csv", header = TRUE))
)

#create an overall dataframes
# OLDOverall <- merge(x = read.csv("FT.csv", header = TRUE), y = read.csv("FS.csv", header = TRUE))

#Statuses
statuses <- c("W (Mon)", "W (Tue)", "W (Wed)", "W (Thu)", "W (Fri)", "W (Sat)", "W (Sun)", "FA")

#remove comma from data$Min and AP and convert to numeric 
gws <- lapply(gws, function(x) mutate(x, Min = as.numeric(gsub("\\,", "", Min))))
gws <- lapply(gws, function(x) mutate(x, Min = as.numeric(as.character(Min))))
gws <- lapply(gws, function(x) mutate(x, AP = as.numeric(gsub("\\,", "", AP))))
gws <- lapply(gws, function(x) mutate(x, AP = as.numeric(as.character(AP))))

# *********************** ADD NEW COLUMNS ***************************

#new columns
gws <- lapply(gws, function(x) mutate(x, TkWAndIntAndCLR = TkW + Int + CLR))
gws <- lapply(gws, function(x) mutate(x, SOTAndKP = SOT + KP))
gws <- lapply(gws, function(x) mutate(x, Min.GP = round((Min / GP) ,2)))
gws <- lapply(gws, function(x) mutate(x, CoSMinusDIS = CoS - DIS))
gws <- lapply(gws, function(x) mutate(x, SOTMinusG = SOT - G))
gws <- lapply(gws, function(x) mutate(x, KPMinusA = KP - A))

#Dynamically create .90 columns to gws
# per90Columns <-  c("FPts", "G", "A", "Pts", "S", "SOT", "YC", "RC", "A2","KP",
# "AT", "TkW", "DIS", "ErG", "AP", "SFTP", "ACNC", "Int",
# "CLR", "CoS", "AER", "PKM", "OG", "GAD", "CSD", "CSM",
# "A", "FC", "FS", "DPt", "Off", "CS",  "TkWAndIntAndCLR",
# "SOTAndKP", "CoSMinusDIS", "SOTMinusG", "KPMinusA")
# 
# for (i in per90Columns) {
#   gws <- lapply(gws, function(x) mutate(x, "{i}.90" := round(((x[[i]] / Min)*90),2)))
# }

# *********************** CREATE OVERALL AND LAST5 DATAFRAMES ***************************

#Use the latest GW as a starting template
template <- as.data.frame(tail(gws, n=1))
template <- select(template, c(ID, Player, Team, Position, Status, Opponent))
overall <- template
last5 <- template

sumColumns <-  c("Min", "FPts", "GP", "GS", "G", "A", "Pts", "S", "SOT", "YC", "RC", "A2","KP",
                 "AT", "TkW", "DIS", "ErG", "AP", "SFTP", "ACNC", "Int",
                 "CLR", "CoS", "AER", "PKM", "OG", "GAD", "CSD", "CSM",         
                 "FC", "FS", "DPt", "Off", "CS",  "TkWAndIntAndCLR",
                 "SOTAndKP", "CoSMinusDIS", "SOTMinusG", "KPMinusA")


for (i in sumColumns) {
  overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise("{i}" := sum(get(i))), by = "Player")
  last5 <- left_join(last5, bind_rows(tail(gws, n=5)) %>% group_by(Player) %>% summarise("{i}" := sum(get(i))), by = "Player")
}

#Dynamically create .90 columns 
per90Columns <-  c("FPts", "G", "A", "Pts", "S", "SOT", "YC", "RC", "A2","KP",
"AT", "TkW", "DIS", "ErG", "AP", "SFTP", "ACNC", "Int",
"CLR", "CoS", "AER", "PKM", "OG", "GAD", "CSD", "CSM",
"A", "FC", "FS", "DPt", "Off", "CS",  "TkWAndIntAndCLR",
"SOTAndKP", "CoSMinusDIS", "SOTMinusG", "KPMinusA")

for (i in per90Columns) {
  overall <- mutate(overall, "{i}.90" := round(((get(i) / Min)*90),2))
  last5 <- mutate(last5, "{i}.90" := round(((get(i) / Min)*90),2))
}

#Min.GP
overall <- mutate(overall, Min.GP = round((Min / GP),2))
last5 <- mutate(last5, Min.GP = round((Min / GP),2))
#FP.G
overall <- mutate(overall, FP.G = round((FPts / GP),2))
last5 <- mutate(last5, FP.G = round((FPts / GP),2))

# *********************** ADD SD AND FLOOR/CEILING TO DATAFRAMES ***************************

#Remove the row if they didnt play e.g. Min == 0 (because 0's mess up SD)
gwsMinus0Min <- lapply(gws, function(x) subset(x, Min != 0))


#Dynamically create SD, Min, and Max columns
SDColumns <-  c("Min", "Min.GP", "FPts", "FP.G", "G", "A", "Pts", "S", "SOT", "FC", "FS", "YC",
                "RC", "DPt", "Off", "CS", "A2", "KP",
                "AT", "TkW", "DIS", "ErG", "AP", "SFTP", "ACNC", "Int",
                "CLR", "CoS", "AER", "PKM", "OG", "GAD", "CSD", "CSM",         
                "TkWAndIntAndCLR", "SOTAndKP")

for (i in SDColumns) {
  overall <- left_join(overall, bind_rows(gwsMinus0Min) %>% group_by(Player) %>% summarise("{i}.SD" := sd(get(i), na.rm = TRUE)), by = "Player")
  last5 <- left_join(last5, bind_rows(tail(gwsMinus0Min, n=5)) %>% group_by(Player) %>% summarise("{i}.SD" := sd(get(i), na.rm = TRUE)), by = "Player")
  
  overall <- left_join(overall, bind_rows(gwsMinus0Min) %>% group_by(Player) %>% summarise("{i}.Min" := min(get(i), na.rm = TRUE)), by = "Player")
  last5 <- left_join(last5, bind_rows(tail(gwsMinus0Min, n=5)) %>% group_by(Player) %>% summarise("{i}.Min" := min(get(i), na.rm = TRUE)), by = "Player")
  
  overall <- left_join(overall, bind_rows(gwsMinus0Min) %>% group_by(Player) %>% summarise("{i}.Max" := max(get(i), na.rm = TRUE)), by = "Player")
  last5 <- left_join(last5, bind_rows(tail(gwsMinus0Min, n=5)) %>% group_by(Player) %>% summarise("{i}.Max" := max(get(i), na.rm = TRUE)), by = "Player")
}

#Add Floor/Ceiling. e.g. floor means 90% of results above it
overall <- mutate(overall, FP.G.Floor = (FP.G - (FP.G.SD * 1.29)))
last5 <- mutate(last5, FP.G.Floor = (FP.G - (FP.G.SD * 1.29)))
overall <- mutate(overall, FP.G.Ceiling = (FP.G + (FP.G.SD * 1.29)))
last5 <- mutate(last5, FP.G.Ceiling = (FP.G + (FP.G.SD * 1.29)))

#Standard Error - How far the sample mean is from the true mean
overall <- mutate(overall, FP.G.SE = FP.G.SD / sqrt(GP))
last5 <- mutate(last5, FP.G.SE = FP.G.SD / sqrt(GP))

#Median - This should get more accurate as more GWs happen
overall <- left_join(overall, bind_rows(gwsMinus0Min) %>% group_by(Player) %>% summarise(FP.G.Med = median(FP.G, na.rm = TRUE)), by = "Player")
last5 <- left_join(last5, bind_rows(tail(gwsMinus0Min, n=5)) %>% group_by(Player) %>% summarise(FP.G.Med = median(FP.G, na.rm = TRUE)), by = "Player")

# *********************** ADD TOP10 TO DATAFRAMES ***************************

#Mark the top 10 players for each Status by their regular FP.G score
top10 <- subset(overall, !(overall$Status %in% statuses))
top10 <- top10 %>%                                      
  arrange(desc(FP.G)) %>% 
  group_by(Status) %>%
  slice(1:10)
top10$Top10 <- 1
top10 <- subset(top10, select = c(Player, Top10))
overall <- full_join(overall, top10, by = "Player")
overall$Top10 <- ifelse(is.na(overall$Top10), 0, overall$Top10)

#Mark the top 10 players for each Status by their regular FP.G score
top10 <- subset(last5, !(last5$Status %in% statuses))
top10 <- top10 %>%                                      
  arrange(desc(FP.G)) %>% 
  group_by(Status) %>%
  slice(1:10)
top10$Top10 <- 1
top10 <- subset(top10, select = c(Player, Top10))
last5 <- full_join(last5, top10, by = "Player")
last5$Top10 <- ifelse(is.na(last5$Top10), 0, last5$Top10)

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
                          selectInput("pYAxis","Choose the Y Axis", choices = sort(names(overall)), selected = "FP.G"),
                          selectInput("pXAxis","Choose the X Axis", choices = sort(names(overall)), selected = "FP.G.SD"),
                          sliderInput("pMinMinsPerGP", "Minimum Minutes Per GP", min = min(overall$Min.GP, na.rm = TRUE), max = max(overall$Min.GP, na.rm = TRUE), value = min(overall$Min.GP, na.rm = TRUE)),
                          sliderInput("pMinMins", "Minimum Total Minutes", min = min(overall$Min, na.rm = TRUE), max = max(overall$Min, na.rm = TRUE), value = min(10, na.rm = TRUE)),
                          sliderInput("pMinFPts.90", "Minimum FPts per 90", min = min(overall$FPts.90, na.rm = TRUE), max = max(overall$FPts.90, na.rm = TRUE), value = min(overall$FPts.90, na.rm = TRUE)),
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
                          sliderInput("tMinMinsPerGP", "Minimum Minutes Per GP", min = min(overall$Min.GP, na.rm = TRUE), max = max(overall$Min.GP, na.rm = TRUE), value = min(overall$Min.GP, na.rm = TRUE)),
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
                          
                          selectInput("bYAxis","Choose the Y Axis", choices = sort(names(overall)), selected = "FP.G.SD"),
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
    df_temp <- filter(df_temp, Min.GP >= input$pMinMinsPerGP)
    df_temp <- filter(df_temp, Min >= input$pMinMins)
    df_temp <- filter(df_temp, FPts.90 >= input$pMinFPts.90)
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
    df_temp <- filter(df_temp, Min.GP >= input$tMinMinsPerGP)
    df_temp <- filter(df_temp, Min >= input$tMinMins)
    
    columns <- c("Player", "Team", "Status", "Position", "FP.G", "FP.G.SD", "FPts.90", "G", "A")
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

