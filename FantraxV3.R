library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(rvest)
library(stringi)
library(shinyWidgets)
library(forecast)

# **************************************************
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
)

#create an overall dataframes
overall <- merge(x = read.csv("FT.csv", header = TRUE), y = read.csv("FS.csv", header = TRUE))

#remove all players with less than a certain amount of mins. 
#Don't do this for gws because some gws will have the player and some wont
minMins <- 10
overall <- subset(overall, Min > minMins)

#Statuses
statuses <- c("W (Mon)", "W (Tue)", "W (Wed)", "W (Thu)", "W (Fri)", "W (Sat)", "W (Sun)", "FA")

#remove comma from data$Min and AP and convert to numeric 
gws <- lapply(gws, function(x) mutate(x, Min = as.numeric(gsub("\\,", "", Min))))
gws <- lapply(gws, function(x) mutate(x, Min = as.numeric(as.character(Min))))
gws <- lapply(gws, function(x) mutate(x, AP = as.numeric(gsub("\\,", "", AP))))
gws <- lapply(gws, function(x) mutate(x, AP = as.numeric(as.character(AP))))
overall$Min <- as.numeric(gsub("\\,", "", overall$Min))
overall$Min <- as.numeric(as.character(overall$Min))
overall$AP <- as.numeric(gsub("\\,", "", overall$AP))
overall$AP <- as.numeric(as.character(overall$AP))

#new columns
gws <- lapply(gws, function(x) mutate(x, TkWAndIntAndCLR = TkW + Int + CLR))
gws <- lapply(gws, function(x) mutate(x, SOTAndKP = SOT + KP))
gws <- lapply(gws, function(x) mutate(x, Min.GP = round((Min / GP) ,2)))
gws <- lapply(gws, function(x) mutate(x, CoSMinusDIS = CoS - DIS))
gws <- lapply(gws, function(x) mutate(x, SOTMinusG = SOT - G))
gws <- lapply(gws, function(x) mutate(x, KPMinus = KP - A))
overall <- mutate(overall, GSPercentage = round((GS / max(overall$GP)),2))
overall <- mutate(overall, GPPercentage = round((GP / max(overall$GP)),2))
overall <- mutate(overall, TkWAndIntAndCLR = TkW + Int + CLR)
overall <- mutate(overall, SOTAndKP = SOT + KP)
overall <- mutate(overall, Min.GP = round((Min / GP) ,2))
overall <- mutate(overall, CoSMinusDIS = CoS - DIS)
overall <- mutate(overall, SOTMinusG = SOT - G)
overall <- mutate(overall, KPMinus = KP - A)


#Dynamically create last5 columns
last5Columns <-  c("FPts", "G", "A", "Pts", "S", "SOT", "YC", "RC", "A2","KP",
                   "AT", "TkW", "DIS", "ErG", "AP", "SFTP", "ACNC", "Int", "PKG",
                   "CLR", "CoS", "AER", "PKM", "OG", "GAD", "CSD", "CSM",         
                   "A", "FC", "FS", "DPt", "Off", "PKG", "CS", "TkWAndIntAndCLR",
                   "SOTAndKP", "Min.GP", "CoSMinusDIS", "SOTMinusG", "KPMinus")

for (i in last5Columns) {
  #.l5
}

#Dynamically create .90 columns
per90Columns <-  c("FPts", "G", "A", "Pts", "S", "SOT", "YC", "RC", "A2","KP",
"AT", "TkW", "DIS", "ErG", "AP", "SFTP", "ACNC", "Int", "PKG",
"CLR", "CoS", "AER", "PKM", "OG", "GAD", "CSD", "CSM",         
"A", "FC", "FS", "DPt", "Off", "PKG", "CS",  "TkWAndIntAndCLR",
"SOTAndKP", "Min.GP", "CoSMinusDIS", "SOTMinusG", "KPMinus")

for (i in per90Columns) {
  gws <- lapply(gws, function(x) mutate(x, "{i}.90" := round(((x[[i]] / Min)*90),2)))
  overall <- mutate(overall, "{i}.90" := round(((overall[[i]] / Min)*90),2))
}

#Potential
gws <- lapply(gws, function(x) mutate(x, Potential = round((FPts.90 / FP.G),2)))
overall <- mutate(overall, Potential = round((FPts.90 / FP.G),2))

#When a player doesnt play it gives 0's for everything and those 0's are counted in the SD, which is not ideal
#Remove the row if they didnt play e.g. Min == 0
gws <- lapply(gws, function(x) subset(x, Min != 0))

#Dynamically create SD columns
SDColumns <-  c("Min", "FPts", "FP.G", "G", "A", "Pts", "S", "SOT", "FC", "FS", "YC",
                "RC", "DPt", "Off", "PKG", "CS", "A2", "KP",
                "AT", "TkW", "DIS", "ErG", "AP", "SFTP", "ACNC", "Int", "PKG",
                "CLR", "CoS", "AER", "PKM", "OG", "GAD", "CSD", "CSM",         
                "TkWAndIntAndCLR", "SOTAndKP", 
                "FPts.90", "G.90", "A.90", "Pts.90", "S.90", "SOT.90", "FC.90", "FS.90", "YC.90",
                "RC.90", "DPt.90", "Off.90", "CS.90", "A2.90", "KP.90",
                "AT.90", "TkW.90", "DIS.90", "ErG.90", "AP.90", "SFTP.90", "ACNC.90", "Int.90", "PKG.90",
                "CLR.90", "CoS.90", "AER.90", "PKM.90", "OG.90", "GAD.90", "CSD.90", "CSM.90",         
                "TkWAndIntAndCLR", "SOTAndKP")

for (i in SDColumns) {
  overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise("{i}.SD" := sd(get(i), na.rm = TRUE)), by = "Player")
}

#create the LQ (lower Quartile) columns
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(FP.G.LQ = quantile(FP.G, prob = c(.25), na.rm = TRUE)), by = "Player")

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


# **************************************************

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
                          sliderInput("pMinMinsPerGP", "Minimum Minutes Per GP", min = min(overall$Min.GP), max = max(overall$Min.GP), value = min(overall$Min.GP)),
                          sliderInput("pMinMins", "Minimum Total Minutes", min = min(overall$Min), max = max(overall$Min), value = min(overall$Min)),
                          sliderInput("pMinFPts.90", "Minimum FPts per 90", min = min(overall$FPts.90), max = max(overall$FPts.90), value = min(overall$FPts.90)),
                          sliderInput("pPCofGamesStarted", "% of Games Started", min = min(overall$GSPercentage), max = max(overall$GSPercentage), value = min(overall$GSPercentage)),
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
                          
                          selectInput("tTeam","Choose a team", choices = c("All",unique(overall$Team)), selected = "All"),
                          selectInput("tStatus","Choose a Status", choices = c("All", "All Available", "All Taken", unique(overall$Status), "Waiver"), selected = "All"),
                          selectInput("tPosition","Choose a Position", choices = c("All", "D", "M", "F"), selected = "All"),
                          sliderInput("tMinMinsPerGP", "Minimum Minutes Per GP", min = min(overall$Min.GP), max = max(overall$Min.GP), value = min(overall$Min.GP)),
                          sliderInput("tMinMins", "Minimum Total Minutes", min = min(overall$Min), max = max(overall$Min), value = min(overall$Min)),
                          pickerInput("tPicker", "Columns", choices = sort(names(overall)), options = list(`actions-box` = TRUE), selected=NULL, multiple=TRUE),
                          sliderInput("tPCofGamesStarted", "% of Games Started", min = min(overall$GSPercentage), max = max(overall$GSPercentage), value = min(overall$GSPercentage)),
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
    df_temp <- overall
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
    df_temp <- overall
    df_temp <- filter(df_temp, Min.GP >= input$tMinMinsPerGP)
    df_temp <- filter(df_temp, Min >= input$tMinMins)
    df_temp <- filter(df_temp, df_temp$GSPercentage >= input$tPCofGamesStarted)
    
    columns <- c("Player", "Team", "Status", "Position", "FP.G", "FP.G.SD", "FPts.90", "G", "A", "GSPercentage")
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
    
    df_temp <- overall
    
    #If a player has only played one game then they wil have NAs for all their SD columns
    #This messes up box plots, specifically their order
    #If input$bYAxis ends with "SD" then filter out all NAs
    if(str_sub(input$bYAxis, start= -2) == "SD"){
      df_temp <- df_temp[!is.na(df_temp[[input$bYAxis]]), ]
    }
    
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

