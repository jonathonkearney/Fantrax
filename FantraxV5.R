library(tidyverse)
library(httr)
library(jsonlite) 
library(stringi)
library(fuzzyjoin)
library(rvest)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(janitor)



##############################  Notes ############################## 
#I am pulling the team sheets from Fantrax and the stats data from FBref
#When I join the two (by Player) I may be left with some rows that are empty
#other than the player name. In most cases the names will need to be fixed before
#joining them. In some other cases the player might not exist in the FBRef data
#which is probably due to them not having any minutes. In this situation
#you can just leave those players. When they get minutes FBRef will add them to
#the dataset and they should join correctly next time.


#Maybe make it so that I get a prompt in the console asking whether I want to reload the data or not. 

##############################  Misc ############################## 
setwd("C:/Users/OEM/OneDrive/Documents/R/Fantrax/FantraxV5")

rm(list = ls())

year <- 2023

##############################  User Input ############################## 

get_new_stats <-  F

##############################  Get JSONS ############################## 
json_players <- GET("https://www.fantrax.com/fxea/general/getPlayerIds?sport=EPL")
json_eligibility <- GET("https://www.fantrax.com/fxea/general/getLeagueInfo?leagueId=oped79b5lk6a6edu")
json_rosters <- GET("https://www.fantrax.com/fxea/general/getTeamRosters?leagueId=oped79b5lk6a6edu")

json_players_list <- jsonlite::fromJSON(rawToChar(json_players$content))
json_eligibility_list <- jsonlite::fromJSON(rawToChar(json_eligibility$content))$playerInfo
json_rosters_list <- jsonlite::fromJSON(rawToChar(json_rosters$content))$rosters

############################## Create Players df ##############################

# Convert players JSON to df
normalise_players_list <- function(lst) {
  keys <- c("rotowireId", "name", "fantraxId", "team", "position")
  sapply(keys, function(k) ifelse(is.null(lst[[k]]), NA, lst[[k]]), simplify = FALSE)
}
normalised_players_list <- lapply(json_players_list, normalise_players_list)

# Convert the lists to dataframes
players <- do.call(rbind, lapply(normalised_players_list, as.data.frame)) %>% 
  rownames_to_column(var = "ID")

players <- players %>% 
  mutate(name = str_replace(name, "^(.*),\\s*(.*)$", "\\2 \\1"))

##############################  Create eligibility df ############################## 

# Convert player_status JSON to df
normalise_eligibility_list <- function(lst) {
  keys <- c("eligiblePos", "status")
  sapply(keys, function(k) ifelse(is.null(lst[[k]]), NA, lst[[k]]), simplify = FALSE)
}
normalised_eligibility_list <- lapply(json_eligibility_list, normalise_eligibility_list)

# Convert the lists to dataframes
eligibility <- do.call(rbind, lapply(normalised_eligibility_list, as.data.frame)) %>% 
  rownames_to_column(var = "ID")

############################## create rosters df ############################## 

# Function to extract and combine dataframes
combine_roster_items <- function(lst) {
  teamName <- lst$teamName
  rosterItems <- lst$rosterItems
  rosterItems$teamName <- teamName
  return(rosterItems)
}

# Apply the function to each list and combine the dataframes
rosters <- do.call(rbind, lapply(json_rosters_list, combine_roster_items)) %>% 
  rownames_to_column(var = "TeamID") %>% 
  `colnames<-`(c("TeamID", "ID", "position", "status", "teamName"))

rosters <- players %>% 
  select(ID, name) %>% 
  right_join(rosters, by = join_by(ID))

rosters$name <- stri_trans_general(rosters$name, "Latin-ASCII")

############################## Player name fixes ############################## 

rosters$name[rosters$name == "Iyenoma Destiny Udogie"] <- "Destiny Udogie"
rosters$name[rosters$name == "Djordje Petrovic"] <- "Dorde Petrovic"
rosters$name[rosters$name == "Pape Sarr"] <- "Pape Matar Sarr"
rosters$name[rosters$name == "Hwang Hee-Chan"] <- "Hwang Hee-chan"
rosters$name[rosters$name == "Jan-Paul van Hecke"] <- "Jan Paul van Hecke"
rosters$name[rosters$name == "Vitalii Mykolenko"] <- "Vitaliy Mykolenko"
rosters$name[rosters$name == "Son Heung-Min"] <- "Son Heung-min"


############################## FBRef Scrape ############################## 

#function to identify duplicate columns and combine the tables of a specific team
combine_ind_team_tables <- function(df1, df2){
  common_columns <- intersect(names(df1), names(df2))
  
  df2_unique <- df2 %>% select(-all_of(setdiff(common_columns, "Player")))
  
  result <- full_join(df1, df2_unique, by = join_by(Player))
  
  return(result)
}

get_stats <- function(){
  
  URLs <- c("https://fbref.com/en/squads/18bb7c10/Arsenal-Stats",
            "https://fbref.com/en/squads/8602292d/Aston-Villa-Stats",
            "https://fbref.com/en/squads/4ba7cbea/Bournemouth-Stats",
            "https://fbref.com/en/squads/cd051869/Brentford-Stats",
            "https://fbref.com/en/squads/d07537b9/Brighton-and-Hove-Albion-Stats",
            "https://fbref.com/en/squads/943e8050/Burnley-Stats",
            "https://fbref.com/en/squads/cff3d9bb/Chelsea-Stats",
            "https://fbref.com/en/squads/47c64c55/Crystal-Palace-Stats",
            "https://fbref.com/en/squads/d3fd31cc/Everton-Stats",
            "https://fbref.com/en/squads/fd962109/Fulham-Stats",
            "https://fbref.com/en/squads/822bd0ba/Liverpool-Stats",
            "https://fbref.com/en/squads/e297cd13/Luton-Town-Stats",
            "https://fbref.com/en/squads/b8fd03ef/Manchester-City-Stats",
            "https://fbref.com/en/squads/19538871/Manchester-United-Stats",
            "https://fbref.com/en/squads/b2b47a98/Newcastle-United-Stats",
            "https://fbref.com/en/squads/e4a775cb/Nottingham-Forest-Stats",
            "https://fbref.com/en/squads/1df6b87e/Sheffield-United-Stats",
            "https://fbref.com/en/squads/361ca564/Tottenham-Hotspur-Stats",
            "https://fbref.com/en/squads/7c21e445/West-Ham-United-Stats",
            "https://fbref.com/en/squads/8cec06e1/Wolverhampton-Wanderers-Stats"
            )
  
  team_names <- c("Arsenal",
                  "Aston Villa",
                  "Bournemouth",
                  "Brentford",
                  "Brighton",
                  "Burnley",
                  "Chelsea",
                  "Crystal Palace",
                  "Everton",
                  "Fulham",
                  "Liverpool",
                  "Luton Town",
                  "Manchester City",
                  "Manchester United",
                  "Newcastle United",
                  "Nottingham Forest",
                  "Sheffield United",
                  "Tottenham",
                  "West Ham United",
                  "Wolverhampton"
  )
  
  #Download the team HTML pages and store them in a list
  team_html_list <- list()
  for (i in seq_along(URLs)) {
    
    team_html_list[[i]] <- read_html(URLs[i])
    cat("Read", URLs[i], "\n")
    # Introduce a delay of 3 seconds
    Sys.sleep(runif(1, min = 3, max = 5))
  }
  
  #Create a list of dataframes with one combined dataframe for each team
  team_df_list <- list()
  for (i in seq_along(team_html_list)) {
    
    # Read the HTML content of the webpage
    webpage <- team_html_list[[i]]
    
    # Extract all tables on the page
    tables <- webpage %>% html_nodes("table") %>% html_table()
    
    # Remove the specified data frames
    tables <- tables[-c(2, 3, 4, 13, 14)]
    
    #clean up each of the dataframes for that team
    for (j in seq_along(tables)) {
      
      #remove the bottom two TOTAL rows
      tables[[j]] <- as.data.frame(tables[[j]]) %>% 
        head(-2)
      
      #Rename the columns to include their sub headers
      colnames(tables[[j]]) <- paste0(colnames(tables[[j]]), " - ", tables[[j]][1, ])
      colnames(tables[[j]]) <- sub("^ - ", "", colnames(tables[[j]]))
      tables[[j]] <- tables[[j]][-1, ]
      
      #remove the Matches column
      tables[[j]] <- tables[[j]] %>% 
        select(-c(Matches))
      
    }
    
    # Combine data frames into a single team dataframe
    team_df <- reduce(tables, combine_ind_team_tables)
    
    #add a team column so we know which team each player plays for
    team_df$Team <- team_names[i]
    
    #add the dataframe to the list
    team_df_list[[i]] <- team_df
    
  }
  
  # Combine all dataframes into one
  stats <- bind_rows(team_df_list)
  
  #remove accents from names so it joins better
  stats$Player <- stri_trans_general(stats$Player, "Latin-ASCII")
  
  #convert total playing time to numeric and remove commas
  stats$`Playing Time - Min` <- as.numeric(gsub(",", "", stats$`Playing Time - Min`))
  
  #Overwrite stats file
  write.csv(stats, file = "stats.csv", row.names = FALSE)
  
  return(stats)
}

if (get_new_stats == F) {
  stats <- read.csv("stats.csv", check.names = FALSE)
} else if (get_new_stats == T) {
  stats <- get_stats()
}

############################## create main df ############################## 

df <- rosters %>%
  left_join(eligibility, by = join_by(ID)) %>% 
  select(name, teamName) %>% 
  `colnames<-`(c("Player", "Team Name")) %>% 
  full_join(stats, by = join_by(Player))

unjoined <- df %>% 
  filter(is.na(Nation))

############################## clean up main df ############################## 

colnames(df) <- colnames(df) %>%
  str_replace("^Expected", "Exp") %>% 
  str_replace("^Performance", "Perf") %>%
  str_replace("^Per 90 Minutes", "P90")%>%
  str_replace("^Standard", "Std") %>%
  str_replace("^Total", "Tot") %>%
  str_replace("^Short", "Sht") %>%
  str_replace("^Medium", "Med") %>%
  str_replace("^Long", "Lon") %>%
  str_replace("^Pass Types", "Pass") %>%
  str_replace("^Corner Kicks", "Cnr") %>%
  str_replace("^Outcomes", "Out") %>%
  str_replace("^SCA Types", "SCA") %>% 
  str_replace("^GCA Types", "GCA") %>%
  str_replace("^Tackles", "Tac") %>%
  str_replace("^Challenges", "Chal") %>%
  str_replace("^Blocks", "Blo") %>%
  str_replace("^Touches", "Tou") %>% 
  str_replace("^Take-Ons", "TkOn") %>% 
  str_replace("^Carries", "Car") %>% 
  str_replace("^Receiving", "Rec") %>% 
  str_replace("^Playing Time", "Play") %>% 
  str_replace("^Starts", "Star") %>% 
  str_replace("^Team Success", "Team") %>% 
  str_replace("^Aerial Duels", "Aer")

#Remove players who haven't played many minutes so it doesnt screw up the sliders
df <- df %>% 
  filter(df$`Play - Min` >= 20)

############################## Make new variables ############################## 

df <- df %>% mutate(`P90 - KP` = round( ((df$KP / df$`Play - Min`)*90),2))
df <- df %>% mutate(`P90 - Tou - Att 3rd` = round( ((df$`Tou - Att 3rd` / df$`Play - Min`)*90),2))


############################## Filter data ############################## 

Filter_Plot_Data <- function(team, status, position, xVar, yVar, xMin, xMax, yMin, yMax, minMins, maxMins){
  
  plotData <- df
  
  #Football team
  if(team != "All"){
    plotData <- plotData %>% filter(Team == team)
  }
  
  #Status
  if (status == "All") {
    plotData <- plotData
  }
  else if (status == "All Available") {
    plotData <- plotData %>% filter(is.na(`Team Name`))
  }
  else if (status == "All Taken") {
    plotData <- plotData %>% filter(!is.na(`Team Name`))
  }
  else{
    plotData <- plotData %>% filter(grepl(status, `Team Name`))
  }
  
  #Position
  if (position != "All") {
    plotData <- plotData %>% filter(grepl(position, Pos))
  }
  
  #Sliders
  plotData <- plotData %>% filter(get(xVar) >= xMin & get(xVar) <= xMax)
  plotData <- plotData %>% filter(get(yVar) >= yMin & get(yVar) <= yMax)
  plotData <- plotData %>% filter(plotData$`Play - Min` >= minMins & plotData$`Play - Min` <= maxMins)
  
  return(plotData)
}

Filter_Table_Data <- function(team, status, position, minMins, maxMins){
  
  tableData <- df
  
  #Football team
  if(team != "All"){
    tableData <- tableData %>% filter(Team == team)
  }
  
  #Status
  if (status == "All") {
    tableData <- tableData
  }
  else if (status == "All Available") {
    tableData <- tableData %>% filter(is.na(`Team Name`))
  }
  else if (status == "All Taken") {
    tableData<- tableData %>% filter(!is.na(`Team Name`))
  }
  else{
    tableData <- tableData %>% filter(grepl(status, `Team Name`))
  }
  
  #Position
  if (position != "All") {
    tableData <- tableData %>% filter(grepl(position, Pos))
  }
  
  #Sliders
  tableData <- tableData %>% filter(tableData$`Play - Min` >= minMins & tableData$`Play - Min` <= maxMins)
  
  return(tableData)
}

#---------------------------------------------- UI ----------------------------------------------#

ui <- fluidPage(
  
  theme = shinytheme("flatly"), 
  navbarPage("Fantrax",
             tabPanel("Plot",
                      sidebarLayout(
                        sidebarPanel(
                          width = "2",
                          selectInput("pTeam","Choose a Team", choices = c("All", unique(sort(df$Team))), selected = "All"),
                          selectInput("pStatus","Choose a Status", choices = c("All", "All Available", "All Taken", unique(na.omit(df$`Team Name`))), selected = "All Available"),
                          selectInput("pPosition","Choose a Position", choices = c("All", unique(na.omit(df$Pos))), selected = "All"),
                          selectInput("pXVar", "Select X-axis:", choices = sort(names(df)), selected = "Tou - Att 3rd"),
                          selectInput("pYVar", "Select Y-axis:", choices = sort(names(df)), selected = "P90 - KP"),
                          sliderInput("pXSlider", "Select X range:", min = 0, max = 100, value = c(0, 100)),
                          sliderInput("pYSlider", "Select Y range:", min = 0, max = 100, value = c(0, 100)),
                          sliderInput("pMinSlider", "Select Minutes range:", min = min(df$`Play - Min`, na.rm = TRUE), max = max(df$`Play - Min`, na.rm = TRUE),
                                      value = c(0, max(df$`Play - Min`, na.rm = TRUE)))
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
                          selectInput("tTeam","Choose a Team", choices = c("All", unique(sort(df$Team))), selected = "All"),
                          selectInput("tStatus","Choose a Status", choices = c("All", "All Available", "All Taken", unique(na.omit(df$`Team Name`))), selected = "All Available"),
                          selectInput("tPosition","Choose a Position", choices = c("All", unique(na.omit(df$Pos))), selected = "All"),
                          sliderInput("tMinSlider", "Select Minutes range:", min = min(df$`Play - Min`, na.rm = TRUE), max = max(df$`Play - Min`, na.rm = TRUE),
                                      value = c(0, max(df$`Play - Min`, na.rm = TRUE))),
                          pickerInput("tVars", "Select Columns", choices = sort(names(df)), options = list(`actions-box` = TRUE), multiple=TRUE,
                                             selected = c("Player", "Team", "Team Name", "Play - Min", "Pos", "Star - Mn/Start", "P90 - Gls", "P90 - xG", "P90 - Ast",
                                                          "P90 - xAG", "P90 - KP", "P90 - Tou - Att 3rd", "Play - Min%"))
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
                          selectInput("bpTeamType", "Choose a Team Type", choices = c("Team", "Team Name"), selected = "Team Name"),
                          selectInput("bpVar", "Choose a Variable", choices = sort(names(df)), selected = "Tou - Att 3rd")
                        ),
                        
                        mainPanel(
                          plotOutput(outputId = "boxPlot",width = "1500px", height = "900px")
                        )
                      )
             ),
             tabPanel("Unjoined",
                      sidebarLayout(
                        sidebarPanel(
                          width = "2",
                        ),
                        
                        mainPanel(
                          DT::dataTableOutput("unjoinedTable"),
                        )
                      )
             )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  observeEvent(input$pXVar, {
    xVar <- input$pXVar
    xMinVal <- min(df[[xVar]], na.rm = TRUE)
    xMaxVal <- max(df[[xVar]], na.rm = TRUE)
    updateSliderInput(session, "pXSlider", min = xMinVal, max = xMaxVal, value = c(xMinVal, xMaxVal), step = xMaxVal/20)
  })
  
  observeEvent(input$pYVar, {
    yVar <- input$pYVar
    yMinVal <- min(df[[yVar]], na.rm = TRUE)
    yMaxVal <- max(df[[yVar]], na.rm = TRUE)
    updateSliderInput(session, "pYSlider", min = yMinVal, max = yMaxVal, value = c(yMinVal, yMaxVal), step = yMaxVal/20)
  })
  
  output$plot <- renderPlot({
    
    plotData <- Filter_Plot_Data(input$pTeam, input$pStatus, input$pPosition, input$pXVar, input$pYVar, input$pXSlider[1], input$pXSlider[2],
                            input$pYSlider[1], input$pYSlider[2], input$pMinSlider[1], input$pMinSlider[2])
    
    ggplot(plotData, aes(colour = Pos)) + aes_string(x = as.name(input$pXVar), y = as.name(input$pYVar)) +
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
    
    first_cols <- c("Player", "Team", "Team Name", "Play - Min", "Pos", "Star - Mn/Start", "Play - Min%", "P90 - Tou - Att 3rd")
    selected_cols <- c(input$tVars)
    
    tableData <- Filter_Table_Data(input$tTeam, input$tStatus, input$tPosition, input$tMinSlider[1], input$tMinSlider[2]) %>% 
      select(first_cols, setdiff(selected_cols, first_cols) )
      
    
    tableDF <<- tableData
  }, options = list(pageLength = 10), rownames = FALSE)
  
  output$boxPlot <- renderPlot({
    
    boxPlotData <- df
    
    boxPlotData <- na.omit(boxPlotData[, c(input$bpTeamType, input$bpVar)])
    
    p <- ggplot(boxPlotData, aes(x = reorder(get(input$bpTeamType), get(input$bpVar), FUN=mean), y = get(input$bpVar), fill = get(input$bpTeamType))) +
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
           x = input$bpTeamType,
           y = input$bpVar)
    
    p + theme_classic()
    
  }, res = 90)
  
  output$unjoinedTable = DT::renderDataTable({
    
    unjoinedTableData <- unjoined %>% 
      select(Player)
    
    tableDF <<- unjoinedTableData
  }, options = list(pageLength = 10), rownames = FALSE)
}

# Run the app
shinyApp(ui = ui, server = server)


