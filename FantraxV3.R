library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(rvest)
library(stringi)
library(shinyWidgets)

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

#add in the .90 columns
gws <- lapply(gws, function(x) mutate(x, FPts.90 = round(((FPts / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, G.90 = round(((G / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, A.90 = round(((A / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, Pts.90 = round(((Pts / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, S.90 = round(((S / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, SOT.90 = round(((SOT / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, FC.90 = round(((FC / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, FS.90 = round(((FS / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, YC.90 = round(((YC / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, RC.90 = round(((RC / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, DPt.90 = round(((DPt / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, Off.90 = round(((Off / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, PKG.90 = round(((PKG / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, CS.90 = round(((CS / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, A2.90 = round(((A2 / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, KP.90 = round(((KP / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, AT.90 = round(((AT / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, TkW.90 = round(((TkW / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, DIS.90 = round(((DIS / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, ErG.90 = round(((ErG / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, ErG.90 = round(((ErG / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, AP.90 = round(((AP / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, SFTP.90 = round(((SFTP / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, ACNC.90 = round(((ACNC / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, Int.90 = round(((Int / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, CLR.90 = round(((CLR / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, CoS.90 = round(((CoS / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, AER.90 = round(((AER / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, PKM.90 = round(((PKM / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, OG.90 = round(((OG / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, GAD.90 = round(((GAD / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, CSD.90 = round(((CSD / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, CSM.90 = round(((CSM / Min)*90),2)))
overall <- mutate(overall, FPts.90 = round(((FPts / Min)*90),2))
overall <- mutate(overall, G.90 = round(((G / Min)*90),2))
overall <- mutate(overall, A.90 = round(((A / Min)*90),2))
overall <- mutate(overall, Pts.90 = round(((Pts / Min)*90),2))
overall <- mutate(overall, S.90 = round(((S / Min)*90),2))
overall <- mutate(overall, SOT.90 = round(((SOT / Min)*90),2))
overall <- mutate(overall, FC.90 = round(((FC / Min)*90),2))
overall <- mutate(overall, FS.90 = round(((FS / Min)*90),2))
overall <- mutate(overall, YC.90 = round(((YC / Min)*90),2))
overall <- mutate(overall, RC.90 = round(((RC / Min)*90),2))
overall <- mutate(overall, DPt.90 = round(((DPt / Min)*90),2))
overall <- mutate(overall, Off.90 = round(((Off / Min)*90),2))
overall <- mutate(overall, PKG.90 = round(((PKG / Min)*90),2))
overall <- mutate(overall, CS.90 = round(((CS / Min)*90),2))
overall <- mutate(overall, A2.90 = round(((A2 / Min)*90),2))
overall <- mutate(overall, KP.90 = round(((KP / Min)*90),2))
overall <- mutate(overall, AT.90 = round(((AT / Min)*90),2))
overall <- mutate(overall, TkW.90 = round(((TkW / Min)*90),2))
overall <- mutate(overall, DIS.90 = round(((DIS / Min)*90),2))
overall <- mutate(overall, ErG.90 = round(((ErG / Min)*90),2))
overall <- mutate(overall, AP.90 = round(((AP / Min)*90),2))
overall <- mutate(overall, SFTP.90 = round(((SFTP / Min)*90),2))
overall <- mutate(overall, ACNC.90 = round(((ACNC / Min)*90),2))
overall <- mutate(overall, Int.90 = round(((Int / Min)*90),2))
overall <- mutate(overall, CLR.90 = round(((CLR / Min)*90),2))
overall <- mutate(overall, CoS.90 = round(((CoS / Min)*90),2))
overall <- mutate(overall, AER.90 = round(((AER / Min)*90),2))
overall <- mutate(overall, PKM.90 = round(((PKM / Min)*90),2))
overall <- mutate(overall, OG.90 = round(((OG / Min)*90),2))
overall <- mutate(overall, GAD.90 = round(((GAD / Min)*90),2))
overall <- mutate(overall, CSD.90 = round(((CSD / Min)*90),2))
overall <- mutate(overall, CSM.90 = round(((CSM / Min)*90),2))

#Potential
gws <- lapply(gws, function(x) mutate(x, Potential = round((FPts.90 / FP.G),2)))
overall <- mutate(overall, Potential = round((FPts.90 / FP.G),2))

#Game Start Perc
gws <- lapply(gws, function(x) mutate(x, GSPercentage = round((GS / max(overall$GP)),2)))
gws <- lapply(gws, function(x) mutate(x, GPPercentage = round((GP / max(overall$GP)),2)))
overall <- mutate(overall, GSPercentage = round((GS / max(overall$GP)),2))
overall <- mutate(overall, GPPercentage = round((GP / max(overall$GP)),2))

#new columns
gws <- lapply(gws, function(x) mutate(x, TkWAndIntAndCLR = TkW + Int + CLR))
gws <- lapply(gws, function(x) mutate(x, SOTAndKP = SOT + KP))
gws <- lapply(gws, function(x) mutate(x, Min.GP = round((Min / GP) ,2)))
gws <- lapply(gws, function(x) mutate(x, TkWAndIntAndCLR.90 = round((((TkW + Int + CLR) / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, SOTAndKP.90 = round((((SOT + KP) / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, CoSMinusDIS.90 = round((((CoS - DIS) / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, SOTMinusG.90 = round((((SOT - G) / Min)*90),2)))
gws <- lapply(gws, function(x) mutate(x, KPMinusA.90 = round((((KP - A) / Min)*90),2)))
overall <- mutate(overall, Min.GP = round((Min / GP) ,2))
overall <- mutate(overall, TkWAndIntAndCLR.90 = round((((TkW + Int + CLR) / Min)*90),2))
overall <- mutate(overall, SOTAndKP.90 = round((((SOT + KP) / Min)*90),2))
overall <- mutate(overall, CoSMinusDIS.90 = round((((CoS - DIS) / Min)*90),2))
overall <- mutate(overall, SOTMinusG.90 = round((((SOT - G) / Min)*90),2))
overall <- mutate(overall, KPMinusA.90 = round((((KP - A) / Min)*90),2))

#Create the SD columns
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(Min.SD = sd(Min, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(FPts.SD = sd(FPts, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(G.SD = sd(G, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(A.SD = sd(A, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(Pts.SD = sd(Pts, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(S.SD = sd(S, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(SOT.SD = sd(SOT, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(FC.SD = sd(FC, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(FS.SD = sd(FS, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(YC.SD = sd(YC, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(RC.SD = sd(RC, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(DPt.SD = sd(DPt, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(Off.SD = sd(Off, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(PKG.SD = sd(PKG, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(CS.SD = sd(CS, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(A2.SD = sd(A2, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(KP.SD = sd(KP, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(AT.SD = sd(AT, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(TkW.SD = sd(TkW, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(DIS.SD = sd(DIS, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(ErG.SD = sd(ErG, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(AP.SD = sd(AP, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(SFTP.SD = sd(SFTP, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(ACNC.SD = sd(ACNC, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(Int.SD = sd(Int, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(CLR.SD = sd(CLR, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(CoS.SD = sd(CoS, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(AER.SD = sd(AER, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(PKM.SD = sd(PKM, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(OG.SD = sd(OG, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(GAD.SD = sd(GAD, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(CSD.SD = sd(CSD, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(CSM.SD = sd(CSM, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(TkWAndIntAndCLR.SD = sd(TkWAndIntAndCLR, na.rm = TRUE)), by = "Player")
overall <- left_join(overall, bind_rows(gws) %>% group_by(Player) %>% summarise(SOTAndKP.SD = sd(SOTAndKP, na.rm = TRUE)), by = "Player")

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
                          selectInput("pYAxis","Choose the Y Axis", choices = sort(names(overall)), selected = "SFTP.90"),
                          selectInput("pXAxis","Choose the X Axis", choices = sort(names(overall)), selected = "KP.90"),
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
                          
                          selectInput("bYAxis","Choose the Y Axis", choices = sort(names(overall)), selected = "MatchupScore"),
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
    df_temp <- filter(df_temp, overall$GSPercentage >= input$tPCofGamesStarted)
    
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
    
    df_temp <- overall
    
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

