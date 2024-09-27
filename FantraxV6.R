library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(shinyWidgets)
library(stats)
library(PerformanceAnalytics)
library(httr)
library(broom)
library(moments)

rm(list = ls())

setwd("C:/Users/OEM/OneDrive/Documents/R/Fantrax/FantraxV6")

#----------------------- ISSUES -----------------------#

#Will need to change the league ID in the url for the Fantrax API
#Will need to adjust the teamnames in the API data

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

json_rosters <- GET("https://www.fantrax.com/fxea/general/getTeamRosters?leagueId=vg93n1omlzf9qj69")
json_rosters_list <- jsonlite::fromJSON(rawToChar(json_rosters$content))$rosters
json_eligibility <- GET("https://www.fantrax.com/fxea/general/getLeagueInfo?leagueId=vg93n1omlzf9qj69")
json_eligibility_list <- jsonlite::fromJSON(rawToChar(json_eligibility$content))$playerInfo

# Convert player_status JSON to df
normalise_eligibility_list <- function(lst) {
  keys <- c("eligiblePos", "status")
  sapply(keys, function(k) ifelse(is.null(lst[[k]]), NA, lst[[k]]), simplify = FALSE)
}
normalised_eligibility_list <- lapply(json_eligibility_list, normalise_eligibility_list)

# Convert the lists to dataframes
eligibility <- do.call(rbind, lapply(normalised_eligibility_list, as.data.frame)) %>% 
  rownames_to_column(var = "ID") %>% 
  `colnames<-`(c("ID", "position", "Status")) %>% 
  select(ID, Status)

# Function to extract and combine dataframes
combine_roster_items <- function(lst) {
  teamName <- lst$teamName
  rosterItems <- lst$rosterItems
  rosterItems$teamName <- teamName
  return(rosterItems)
}

teamNames <- c("Whakarongo Wanderers",
                "the rats",
                "Big Mahnchester Utd",
                "Leeton Orient",
                "Blakeburn Rovers",
                "Bede Cosgrove FC",
                "BenSigmund420 FC",
                "BigUps FC", 
                "Jovil Town",
                "Splitting United",
                "Lemony Snickets FC",
                "Stevensage")

shortTeamNames <- c("WHUKS",
               "ratzzz",
               "Chur",
               "LTO",
               "Blkeburn",
               "BCFC",
               "BS420FC",
               "BigUps", 
               "Joe",
               "splitt1n",
               "Snickets",
               "STV")

# Apply the function to each list and combine the dataframes
rosters <- do.call(rbind, lapply(json_rosters_list, combine_roster_items)) %>% 
  rownames_to_column(var = "TeamID") %>% 
  `colnames<-`(c("TeamID", "ID", "position", "oldStatus", "Status")) %>% 
  select("ID", "Status") %>% 
  mutate(Status = case_when(
    Status == teamNames[1] ~ shortTeamNames[1],
    Status == teamNames[2] ~ shortTeamNames[2],
    Status == teamNames[3] ~ shortTeamNames[3],
    Status == teamNames[4] ~ shortTeamNames[4],
    Status == teamNames[5] ~ shortTeamNames[5],
    Status == teamNames[6] ~ shortTeamNames[6],
    Status == teamNames[7] ~ shortTeamNames[7],
    Status == teamNames[8] ~ shortTeamNames[8],
    Status == teamNames[9] ~ shortTeamNames[9],
    Status == teamNames[10] ~ shortTeamNames[10],
    Status == teamNames[11] ~ shortTeamNames[11],
    Status == teamNames[12] ~ shortTeamNames[12],
    TRUE ~ Status  # Keep other values unchanged
  ))

latestStatuses <- eligibility %>%
  left_join(rosters, by = "ID", suffix = c(".old", "")) %>%
  mutate(Status = ifelse(is.na(Status), Status.old, Status)) %>%
  select(-Status.old)

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
  mutate(Opponent = ifelse(grepl("<br/>", Opponent), Opponent, sub(".*([A-Z]{3}).*", "\\1", Opponent))) %>%
  #remove quotes from ID column
  mutate(ID = gsub("^\\*|\\*$", "", ID)) %>%
  #remove the extra team name for the players who have moved teams
  mutate(Team = sub(".*/", "", Team))

#----------------------- UPDATE TEAMS USING FANTRAX API DATA -----------------------#

#This needs to be done before making the Template
gwdf <- gwdf %>%
  left_join(latestStatuses, by = "ID", suffix = c(".old", "")) %>%
  mutate(Status = ifelse(is.na(Status), Status.old, Status)) %>%
  select(-Status.old)

#----------------------- TEMPLATE -----------------------#

#Template needs to be made first before you remove rows with 0 mins
template <- subset(gwdf, Gameweek == max(gwdf$Gameweek)) %>% 
  select(c(Player, Team, Position, Status, Opponent)) %>% 
  #remove the extra team name for the players who have moved teams
  mutate(Team = sub(".*/", "", Team)) 


#----------------------- REMOVE ROWS WITH 0 MINS -----------------------#
  
  #this has to be done last. After the template is made
  #Remove the row if they didnt play e.g. Min == 0
  gwdf <- gwdf %>% 
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
statuses <- c("W (Mon)", "W (Tue)", "W (Wed)", "W (Thu)", "W (Fri)", "W (Sat)", "W (Sun)", "FA", "WW")

characterColumns <- c("Gameweek", "ID", "Player", "Team", "Position", "RkOv",
                      "Status", "Opponent", "Ros..", "X...", "PC.", "HomeOrAway")

numericColumns <-  c("Min", "FPts", "GP", "GS", "G", "A", "Pts", "S", "SOT", "YC", "RC", "A2","KP",
                     "AT", "TkW", "DIS", "ErG", "AP", "SFTP", "ACNC", "Int",
                     "CLR", "CoS", "AER", "PKM", "OG", "GAD", "CSD", "CSM",
                     "FC", "FS", "DPt", "Off", "CS", "TLM", "LBA", "CLO", "BS",
                     "TkWAndIntAndCLR", "SOTAndKP", "CoSMinusDIS", "SOTMinusG", "KPMinusA")

#Variable and calculation combos for dropdowns
varCombos <- numericColumns
for(i in numericColumns){
  for(j in c("SD", "Mean", "Med", "MAD", "DownDev", "90", "MeanMnsDD", "LQ", "Skew", "FormAdj", "Form")){
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

Add_Points <- function(df){
  

}

#Filter the dataframe
Pre_Filter <- function(df, team, status, position, startGW, endGW){
  
  df <- df %>% 
    filter(Gameweek >= startGW & Gameweek <= endGW)
  
  if (team != "All") {
    df <- df %>% filter(Team == team)
  }
  if (status != "All") {
    if (status == "Waiver") {
      df <- df %>% filter(str_detect(df$Status, "^W \\(") | df$Status == "WW")
    }
    else if (status == "All Available") {
      df <- df %>% filter(str_detect(df$Status, "^W \\(") | str_detect(df$Status, "^FA") | df$Status == "WW")
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
  
  #create base summary df to add cols to
  df <- template
  
  #add in base cols
  cols <- c("Min", "Min.Mean", "FPts.Mean", "FPts.90",  cols)
  
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
                          Skew = function(df) round(skewness(df[[var]], na.rm = TRUE), 3),
                          MeanMnsDD = function(df) {
                            mean_val <- mean(df[[var]], na.rm = TRUE)
                            downDev_val <- DownsideDeviation(df[[var]], MAR = mean_val, na.rm = TRUE)
                            round(mean_val - downDev_val, 2)
                          },
                          `90` = function(df) {
                            minutes <- sum(df$Min, na.rm = TRUE)
                            round((sum(df[[var]], na.rm = TRUE) / minutes) * 90, 2)
                          },
                          FormAdj = function(df) {
                            df <- df[order(df$Gameweek, decreasing = TRUE), ]  # Sort by recent gameweek
                            decay_start <- 1
                            decay_step <- 0.2 #This dictates how aggresively it weights gameweeks
                            decay_factor <- pmax(decay_start - ((1:nrow(df) - 1) * decay_step), 0)  # Weights: 1, 0.9, 0.8, ...
                            weighted_sum <- sum(df[[var]] * decay_factor, na.rm = TRUE)
                            total_weight <- sum(decay_factor[!is.na(df[[var]])])  # Adjust for NAs
                            round(weighted_sum / total_weight, 2)  # Return weighted average
                          },
                          Form = function(df) {
                            df <- df[order(df$Gameweek, decreasing = TRUE), ]  # Sort by recent gameweek
                            decay_start <- 1
                            decay_step <- 0.2 #This dictates how aggresively it weights gameweeks
                            decay_factor <- pmax(decay_start - ((1:nrow(df) - 1) * decay_step), 0)  # Weights: 1, 0.9, 0.8, ...
                            weighted_sum <- sum(df[[var]] * decay_factor, na.rm = TRUE)
                            total_weight <- sum(decay_factor[!is.na(df[[var]])])  # Adjust for NAs
                            formAdj <- round(weighted_sum / total_weight, 2)
                            
                            mean_val <- mean(df[[var]], na.rm = TRUE)
                            if (mean_val == 0) return(rep(0, nrow(df)))  # If no variation, return 0 for all players
                            round(formAdj / mean_val, 2)
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

Post_Filter <- function(df, minMins, minMin.mean, minFPts.mean, maxFPts.mean, minFPts.90, maxFPts.90){
  
  
  df <- df %>%
    filter(Min >= minMins) %>% 
    filter(FPts.Mean >= minFPts.mean & FPts.Mean <= maxFPts.mean) %>%
    filter(FPts.90 >= minFPts.90 & FPts.90 <= maxFPts.90) %>% 
    filter(Min.Mean >= minMin.mean)
  
  return(df)
}

#----------------------- CREATE SLIDERS DF -----------------------#

#for the sliders
sliderDF <- template %>% 
  Add_Statistic(gwdf, "Min", "Sum") %>%
  Add_Statistic(gwdf, "FPts", "Mean") %>%
  Add_Statistic(gwdf, "FPts", "90") %>%
  # Add_Statistic(gwdf, "Min", "Mean") %>%
  #Filter out players with like 1 minute that push the max FPts.90 to like 200
  filter(Min > 10)
#---------------------------------------------- RANDOM ----------------------------------------------#


# modelData <- gwdf %>%
#   filter(Min > 25)
# 
# model <- lm(data = modelData, FPts~Min+G+A+S+SOT+YC+RC+A2+KP+AT+TkW+DIS+ErG+AP+SFTP+ACNC+Int+CLR+CoS+AER+OG+GAD+CSD+CSM+FS+DPt+CS)
# modeldf <- tidy(model)
# modeldf

PointMeans <- gwdf %>% 
  filter(Status != "FA" & Status != "WW") %>% 
  summarise(TKW_Pts = (sum(TkW)*1.5)/length(unique(Status)),
            KP_Pts = (sum(KP)*2)/length(unique(Status)),
            AT_Pts = (sum(AT)*6)/length(unique(Status)),
            SOT_Pts = (sum(SOT)*2)/length(unique(Status)),
            G_Pts = (sum(G)*9)/length(unique(Status)),
            CLR_Pts = (sum(CLR)*1)/length(unique(Status)),
            Int_Pts = (sum(Int)*1)/length(unique(Status)),
            SFTP_Pts = ((sum(SFTP)%/%6)*1)/length(unique(Status)),
            )

# teamRanking <- gwdf %>%
#   filter(Min > 30) %>%
#   group_by(Team) %>%
#   summarise(FPts.90 = mean((FPts/Min)*90)) %>%
#   mutate(AdjValue = ((FPts.90 - mean(FPts.90)) / mean(FPts.90) * -1) + 1)



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
                          selectInput("pXVar", "Select X-axis:", choices = sort(varCombos), selected = "FPts.Mean"),
                          selectInput("pYVar", "Select Y-axis:", choices = sort(varCombos), selected = "FPts.90"),
                          sliderInput("pWindow", "Gameweek Window", min = min(gwdf$Gameweek), max = max(gwdf$Gameweek), value = c(min(gwdf$Gameweek), max(gwdf$Gameweek))),
                          sliderInput("pMinMins", "Minimum Total Minutes", min = 0, max = max(sliderDF$Min, na.rm = TRUE), value = min(10, na.rm = TRUE)),
                          sliderInput("pFPts.Mean", "FPts.Mean", min = 0, max = max(gwdf$FPts, na.rm = TRUE), value = c(0, max(gwdf$FPts, na.rm = TRUE))),
                          sliderInput("pFPts.90", "FPts per 90", min = 0, max = 100, value = c(0, 100)),
                          
                         
                          
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
                          selectInput("tTeam","Choose a Team", choices = c("All", unique(sort(gwdf$Team))), selected = "All"),
                          selectInput("tStatus","Choose a Status", choices = c("All", "All Available", "All Taken", "Waiver", fantraxTeams), selected = "All Available"),
                          selectInput("tPosition","Choose a Position", choices = c("All", "D", "M", "F"), selected = "All"),
                          pickerInput("tPicker", "Columns", choices = sort(varCombos), options = list(`actions-box` = TRUE), selected=NULL, multiple=TRUE),
                          sliderInput("tWindow", "Gameweek Window", min = min(gwdf$Gameweek), max = max(gwdf$Gameweek), value = c(min(gwdf$Gameweek), max(gwdf$Gameweek))),
                          sliderInput("tMinMins", "Minimum Total Minutes", min = 0, max = max(sliderDF$Min, na.rm = TRUE), value = min(10, na.rm = TRUE)),
                          sliderInput("tMin.Mean", "Minimum Avg Mins", min = 0, max = 90, value = 0),
                          sliderInput("tFPts.Mean", "FPts.Mean", min = 0, max = max(gwdf$FPts, na.rm = TRUE), value = c(0, max(gwdf$FPts, na.rm = TRUE))),
                          sliderInput("tFPts.90", "FPts per 90", min = 0, max = 100, value = c(0, 100)),
                          
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
                          selectInput("bTeamType", "Choose a Team Type", choices = c("Team", "Status"), selected = "Status"),
                          selectInput("bVar", "Choose a Variable", choices = sort(varCombos), selected = "FPts.Mean"),
                          sliderInput("bWindow", "Gameweek Window", min = min(gwdf$Gameweek), max = max(gwdf$Gameweek), value = c(min(gwdf$Gameweek), max(gwdf$Gameweek))),
                        ),
                        
                        mainPanel(
                          plotOutput(outputId = "boxPlot",width = "1500px", height = "900px")
                        )
                      )
             )
  )
)

server <- function(input, output, session) {
  
  if (any(!rosters$teamName %in% shortTeamNames)) {
    shiny::stopApp()
    print("There is an issue with the team names. Stopping execution.")
  }
  
  output$plot <- renderPlot({
    
    plotData <- gwdf %>% 
      Pre_Filter(input$pTeam, input$pStatus, input$pPosition, input$pWindow[1], input$pWindow[2]) %>% 
      Create_Data(c(input$pXVar, input$pYVar)) %>% 
      #Added in 0 for Min.Mean because the Plot doesn't filter by average Mins
      Post_Filter(input$pMinMins, 0, input$pFPts.Mean[1], input$pFPts.Mean[2], input$pFPts.90[1], input$pFPts.90[2])
      
    
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
  
  output$table = DT::renderDataTable({
  
    extra_cols <- c("FPts.MeanMnsDD", "FPts.DownDev", "FPts.Form", "FPts.FormAdj", "FPts.Skew", "Pts.90")
    
    tableData <- gwdf %>% 
      Pre_Filter(input$tTeam, input$tStatus, input$tPosition, input$tWindow[1], input$tWindow[2]) %>% 
      Create_Data(c(extra_cols, input$tPicker)) %>% 
      Post_Filter(input$tMinMins, input$tMin.Mean, input$tFPts.Mean[1], input$tFPts.Mean[2], input$tFPts.90[1], input$tFPts.90[2])

  }, options = list(pageLength = 12), rownames = FALSE)
  
  output$boxPlot <- renderPlot({

    boxPlotData <- gwdf %>% 
      Pre_Filter("All", "All", "All", input$bWindow[1], input$bWindow[2]) %>% 
      Create_Data(c(input$bVar)) %>% 
      filter(!(Status %in% c("FA")) & !grepl("^W \\(", Status) & Status != "WW")
    
    p <- ggplot(boxPlotData, aes(x = reorder(get(input$bTeamType), get(input$bVar), FUN=mean, na.rm = T ), y = get(input$bVar), fill = get(input$bTeamType))) +
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
           x = input$bTeamType,
           y = input$bVar,
           fill = input$bTeamType)

    p + theme_classic()

  }, res = 90)
}

# Run the app
shinyApp(ui = ui, server = server)
