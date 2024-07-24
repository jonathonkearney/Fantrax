library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(shinyWidgets)
library(stats)
library(PerformanceAnalytics)

rm(list = ls())

setwd("C:/Users/OEM/OneDrive/Documents/R/Fantrax/FantraxV6")

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

#----------------------- LOAD API DATA -----------------------#


#----------------------- DATA CLEANING -----------------------#

#remove comma from data$Min and AP and convert to numeric 
gwdf <- gwdf %>% 
  mutate(Min = as.numeric(gsub("\\,", "", Min))) %>% 
  mutate(Min = as.numeric(as.character(Min))) %>% 
  mutate(AP = as.numeric(gsub("\\,", "", AP))) %>% 
  mutate(AP = as.numeric(as.character(AP))) %>% 
  #Split out Opponent and HomeAway
  mutate(HomeOrAway = ifelse(startsWith(gwdf$Opponent, "@"), "Away", "Home")) %>% 
  #Clean up opponent column
  mutate(Opponent = str_replace(Opponent, Team, "")) %>% 
  mutate(Opponent = ifelse(startsWith(gwdf$Opponent, " "), substring(gwdf$Opponent,9,11), substring(gwdf$Opponent,1,3))) %>% 
  #Remove the row if they didnt play e.g. Min == 0
  filter(Min != 0)

#----------------------- NEW GW COLUMNS -----------------------#

#new columns
gwdf <- gwdf %>% 
  mutate(TkWAndIntAndCLR = TkW + Int + CLR) %>% 
  mutate(SOTAndKP = SOT + KP) %>% 
  mutate(CoSMinusDIS = CoS - DIS) %>% 
  mutate(SOTMinusG = SOT - G) %>% 
  mutate(KPMinusA = KP - A)

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

#Variable and calculation combos for dropdowns
varCombos <- numericColumns
for(i in numericColumns){
  for(j in c("SD", "Mean", "Med", "MAD", "DownDev", "90", "MeanMnsDD", "LQ")){
    varCombos <- c(varCombos, paste(i, j, sep = "."))
  }
}

#List of our teams for the status dropdowns
fantraxTeams <- unique(gwdf$Status)
fantraxTeams <- sort(fantraxTeams[!grepl("^W \\(|^FA", fantraxTeams)])

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

#Filter the dataframe
Pre_Filter <- function(df, team, status, position, startGW, endGW){
  
  df <- df %>% 
    filter(Gameweek >= startGW & Gameweek <= endGW)
  
  if (team != "All") {
    df <- df %>% filter(Team == team)
  }
  if (status != "All") {
    if (status == "Waiver") {
      df <- df %>% filter(str_detect(df$Status, "^W \\("))
    }
    else if (status == "All Available") {
      df <- df %>% filter(str_detect(df$Status, "^W \\(") | str_detect(df$Status, "^FA"))
    }
    else if (status == "All Taken") {
      df <- df %>% filter(!Status %in% statuses)
    }
    else{
      df <- df %>% filter(Status == status)  
    }
  }
  if (position != "All") {
    if(position == "D"){
      df <- df %>% filter(str_detect(df$Position, "D"))
    }
    else if(position == "M"){
      df <- df %>% filter(str_detect(df$Position, "M"))
    }
    else if(position == "F"){
      df <- df %>% filter(str_detect(df$Position, "F"))
    }
  }
  
  return(df)
  
}

Create_Data <- function(filtered_gwdf, cols){
  
  df <- baseDF
  
  for(i in cols){
    if(!(i %in% colnames(df))){
      var <- ""
      stat <- ""
      if (!(grepl("\\.", i))) {
        var <- i
        stat <- "Sum"
      }
      else{
        var <- strsplit(i, ".", fixed = TRUE)[[1]][1]
        stat <- strsplit(i, ".", fixed = TRUE)[[1]][2]
      }
      df <- Add_Statistic(df, filtered_gwdf, var, stat)
    }
  }
  return(df)
}


Add_Statistic <- function(df, filtered_gwdf, var, stat){
  
  stat_function <- switch(stat,
                          Sum = function(df) sum(df[[var]], na.rm = TRUE),
                          SD = function(df) round(sd(df[[var]], na.rm = TRUE), 2),
                          Mean = function(df) round(mean(df[[var]], na.rm = TRUE), 2),
                          Med = function(df) median(df[[var]], na.rm = TRUE),
                          LQ = function(df) quantile(df[[var]], na.rm = TRUE)[[2]],
                          MAD = function(df) mad(df[[var]], constant = 1, na.rm = TRUE),
                          DownDev = function(df) round(DownsideDeviation(df[[var]], MAR = mean(df[[var]], na.rm = TRUE), na.rm = TRUE), 2),
                          MeanMnsDD = function(df) {
                            mean_val <- mean(df[[var]], na.rm = TRUE)
                            downDev_val <- DownsideDeviation(df[[var]], MAR = mean_val, na.rm = TRUE)
                            round(mean_val - downDev_val, 2)
                          },
                          `90` = function(df) {
                            minutes <- sum(df$Min, na.rm = TRUE)
                            round((sum(df[[var]], na.rm = TRUE) / minutes) * 90, 2)
                          },
                          stop("Invalid statistic"))
  
  colName <- ifelse(stat != "Sum", paste0(var, ".", stat), var)
  
  df <- df %>%
    left_join(
      filtered_gwdf %>%
        group_by(Player) %>%
        summarise(!!sym(colName) := stat_function(pick(everything()))),
      by = "Player"
    )
  
  return(df)
}

Post_Filter <- function(df, minMins){
  
  df <- df %>% filter(Min >= minMins)
  #   filter(FPts.Mean >= minFPts.mean) %>% 
  #   filter(FPts.Mean <= maxFPts.mean) %>% 
  #   filter(FPts.90 >= minFPts.90) %>% 
  #   filter(FPts.90 <= maxFPts.90)
  
  return(df)
}

#----------------------- CREATE BASE SUMMARY DF -----------------------#
#Both as a template to add columns to, and for the sliders
baseDF <- gwdf %>%
  #get the data from the latest gameweek and select only certain columns
  filter(Gameweek == max(gwdf$Gameweek)) %>% 
  select(c(Player, Team, Position, Status)) %>%
  #Add the specific statistic columns
  Add_Statistic(gwdf, "Min", "Sum") %>%
  Add_Statistic(gwdf, "FPts", "Mean") %>%
  Add_Statistic(gwdf, "FPts", "90")
  
#---------------------------------------------- UI ----------------------------------------------#

ui <- fluidPage(
  
  theme = shinytheme("flatly"), 
  navbarPage("Fantrax",
             tabPanel("Plot",
                      sidebarLayout(
                        sidebarPanel(
                          width = "2",
                          selectInput("pTeam","Choose a Team", choices = c("All", unique(sort(gwdf$Team))), selected = "All"),
                          selectInput("pStatus","Choose a Status", choices = c("All", "All Available", "All Taken", "Waiver", fantraxTeams), selected = "All Available"),
                          selectInput("pPosition","Choose a Position", choices = c("All", "D", "M", "F"), selected = "All"),
                          selectInput("pXVar", "Select X-axis:", choices = sort(varCombos), selected = "G.Mean"),
                          selectInput("pYVar", "Select Y-axis:", choices = sort(varCombos), selected = "AT.Mean"),
                          sliderInput("pWindow", "Gameweek Window", min = min(gwNumbers), max = max(gwNumbers), value = c(min(gwNumbers), max(gwNumbers))),
                          sliderInput("pMinMins", "Minimum Total Minutes", min = 0, max = max(baseDF$Min, na.rm = TRUE), value = min(10, na.rm = TRUE)),
                          sliderInput("pFPts.Mean", "FPts.Mean", min = min(baseDF$FPts.Mean, na.rm = TRUE), max = max(baseDF$FPts.Mean, na.rm = TRUE), value = c(0, max(baseDF$FPts.Mean, na.rm = TRUE))),
                          sliderInput("pFPts.90", "FPts per 90", min = min(baseDF$FPts.90, na.rm = TRUE), max = max(baseDF$FPts.90, na.rm = TRUE), value = c(0, max(baseDF$FPts.90, na.rm = TRUE))),
                          
                         
                          
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
                          # selectInput("tTeam","Choose a Team", choices = c("All", unique(sort(df$Team))), selected = "All"),
                          # selectInput("tStatus","Choose a Status", choices = c("All", "All Available", "All Taken", unique(na.omit(df$`Team Name`))), selected = "All Available"),
                          # selectInput("tPosition","Choose a Position", choices = c("All", "DF", "MF", "FW" ), selected = "All"),
                          # sliderInput("tMinSlider", "Select Minutes range:", min = min(df$`Play - Min`, na.rm = TRUE), max = max(df$`Play - Min`, na.rm = TRUE),
                          #             value = c(0, max(df$`Play - Min`, na.rm = TRUE))),
                          # pickerInput("tVars", "Select Columns", choices = sort(names(df)), options = list(`actions-box` = TRUE), multiple=TRUE,
                          #             selected = c("Player", "Team", "Team Name", "Play - Min", "Pos", "P90 - Gls", "P90 - xG", "P90 - Ast",
                          #                          "P90 - xAG", "P90 - KP", "P90 - Tou - Att 3rd", "P90 - xG+xAG", "P90 - PrgPassRec",
                          #                          "P90 - PassCmp"))
                        ),
                        
                        mainPanel(
                          DT::dataTableOutput("table"),
                        )
                      )
             ),
             tabPanel("Box Plot",
                      sidebarLayout(
                        sidebarPanel(
                          width = "2",
                          # selectInput("bpTeamType", "Choose a Team Type", choices = c("Team", "Team Name"), selected = "Team Name"),
                          # selectInput("bpVar", "Choose a Variable", choices = sort(names(df)), selected = "P90 - Tou - Att 3rd")
                        ),
                        
                        mainPanel(
                          plotOutput(outputId = "boxPlot",width = "1500px", height = "900px")
                        )
                      )
             )
  )
)

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    
    plotData <- gwdf %>% 
      Pre_Filter(input$pTeam, input$pStatus, input$pPosition, input$pWindow[1], input$pWindow[2]) %>% 
      Create_Data(c(input$pXVar, input$pYVar)) %>% 
      Post_Filter(input$pMinMins)
      
    
    ggplot(plotData, aes(colour = Position)) + 
      aes(!!sym(input$pXVar), !!sym(input$pYVar)) +
      geom_point() + 
      geom_text(
        aes(label = Player), 
        check_overlap = F,
        adj = -0.1,
        vjust="inward"
      ) + coord_flip(clip = "off") +
      geom_abline(intercept = c(0), slope = 1, color = c("black"), alpha=0.4) + 
      theme_classic()
    
  }, res = 90)
  
  # output$table = DT::renderDataTable({
  #   
  #   first_cols <- c("Player", "Team", "Team Name", "Pos", "Play - Min", "Play - Min/MP", "P90 - Tou - Att 3rd")
  #   selected_cols <- c(input$tVars)
  #   
  #   tableData <- Filter_Table_Data(input$tTeam, input$tStatus, input$tPosition, input$tMinSlider[1], input$tMinSlider[2]) %>% 
  #     select(first_cols, setdiff(selected_cols, first_cols) )
  #   
  #   
  #   tableDF <<- tableData
  # }, options = list(pageLength = 10), rownames = FALSE)
  
  # output$boxPlot <- renderPlot({
  #   
  #   boxPlotData <- df
  #   
  #   boxPlotData <- na.omit(boxPlotData[, c(input$bpTeamType, input$bpVar)])
  #   
  #   p <- ggplot(boxPlotData, aes(x = reorder(get(input$bpTeamType), get(input$bpVar), FUN=mean), y = get(input$bpVar), fill = get(input$bpTeamType))) +
  #     geom_boxplot() +
  #     stat_summary(
  #       fun = mean,
  #       geom = "point",
  #       shape = 23,
  #       size = 3,
  #       fill = "white",
  #       color = "black"
  #     ) +
  #     labs(title = "Distributions",
  #          x = input$bpTeamType,
  #          y = input$bpVar)
  #   
  #   p + theme_classic()
  #   
  # }, res = 90)
}

# Run the app
shinyApp(ui = ui, server = server)
