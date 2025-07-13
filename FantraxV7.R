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
library(jsonlite)

rm(list = ls())

setwd("C:/Users/OEM/OneDrive/Documents/R/Fantrax/FantraxV7")

#----------------------- GLOBAL VARIABLES -----------------------#

statuses <- c("W (Mon)", "W (Tue)", "W (Wed)", "W (Thu)", "W (Fri)", "W (Sat)", "W (Sun)", "FA", "WW")

character_variables <- c("Gameweek", "ID", "Player", "Team", "Position",
                      "Status", "Opponent", "home_or_away")

template_variables <- c("Player", "Team", "Position", "Status", "Opponent")

single_game_count_variables <- c("Min", "FPts", "GP", "GS", "G", "A", "Pts", "S", "SOT", "YC", "RC", "A2","KP",
                               "AT", "TkW", "DIS", "ErG", "AP", "SFTP", "ACNC", "Int",
                               "CLR", "CoS", "AER", "PKM", "OG", "GAD", "CSD", "CSM",
                               "FC", "FS", "DPt", "Off", "CS", "TLM", "LBA", "CLO", "BS")

new_single_game_count_variables <- c("TkWAndIntAndCLR", "SOTAndKP", "CoSMinusDIS", "SOTMinusG", "KPMinusA")

single_game_percentage_variables <- c("Ros", "X.D", "X...", "PC.")

#Variable and calculation combos for dropdowns
#outer() generates all combinations of the two vectors using paste().
varCombos <- c(
  single_game_count_variables,
  new_single_game_count_variables,
  c(outer(c(single_game_count_variables, new_single_game_count_variables),
          c("SD", "Mean", "Med", "MAD", "DownDev", "90", "MeanMnsDD", "LQ", "Skew", "FormAdj", "Form"),
          paste, sep = "."))
)

#----------------------------------------------------------------#

load_gameweek_data <- function(){
  
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
  
  df <- bind_rows(gws, .id = "Gameweek")
  df$Gameweek <- as.numeric(df$Gameweek)
  
  return(df)
  
}

update_eligibility <- function(df){
  
  json_rosters <- GET("https://www.fantrax.com/fxea/general/getTeamRosters?leagueId=vg93n1omlzf9qj69") %>%
    content(as = "parsed", type = "application/json")
  json_players <- GET("https://www.fantrax.com/fxea/general/getAdp?sport=EPL") %>%
    content(as = "parsed", type = "application/json")
  json_eligibility <- GET("https://www.fantrax.com/fxea/general/getLeagueInfo?leagueId=vg93n1omlzf9qj69") %>%
    content(as = "parsed", type = "application/json")

  #extract Fantrax teams info
  first_matchups <- pluck(json_eligibility, "matchups", 1, "matchupList")
  fantrax_teams <- do.call(rbind, lapply(first_matchups, function(x) {
    rbind(
      data.frame(name = x$home$name, id = x$home$id, shortName = x$home$shortName, stringsAsFactors = FALSE),
      data.frame(name = x$away$name, id = x$away$id, shortName = x$away$shortName, stringsAsFactors = FALSE)
    )
  }))

  #extract player eligibility status
  player_eligibility <- do.call(rbind, lapply(json_eligibility$playerInfo, function(x) {
      rbind(
        data.frame(status = x$status, stringsAsFactors = FALSE)
      )
  })) %>%
    rownames_to_column("id")

  #extract Fantrax team rosters
  fantrax_team_rosters <- do.call(rbind, lapply(json_rosters$rosters, function(team) {
    do.call(rbind, lapply(team[[2]] , function(player) {
      data.frame(
        team_name = team[[1]],
        id = player$id,
        position = player$position,
        status = player$status,
        stringsAsFactors = FALSE
      )
    }))
  })) %>%
    rownames_to_column("team_id")

  #join the dfs to create master eligibility df
  eligibility <- player_eligibility %>%
    left_join(
      fantrax_team_rosters %>%
        select("id", "team_name"),
      by = "id"
    ) %>%
    left_join(
      fantrax_teams %>%
        select("name", "shortName"),
      by = join_by(team_name == name)
    ) %>% 
    mutate(
      status = case_when(
        status == "T" ~ shortName,
        TRUE ~ status
      )
    ) %>% 
    select(-c("team_name", "shortName")) %>% 
    rename(
      "ID" = "id",
      "Status" = "status"
    ) %>% 
    mutate(
      ID = paste0("*", ID, "*")
    )
  
  #Update main df with latest eligibility data
  df <- df %>%
    left_join(
      eligibility,
      by = "ID",
      suffix = c(".old", "")
    ) %>%
    mutate(
      Status = case_when(
        is.na(Status) ~ Status.old,
        TRUE ~ Status
      )
    ) %>%
    select(-Status.old)
  
  #add fantrax_teams to the global environment
  #Generally discouraged coding behaviour but ahwell
  assign("fantrax_teams", fantrax_teams, envir = .GlobalEnv)
  
  return(df)
  
}

#----------------------- DATA CLEANING -----------------------#

clean_data <- function(df){
  
  df <- df %>% 
    mutate(
      #remove comma from Min and AP and convert to numeric 
      Min = as.numeric(gsub("\\,", "", Min)),
      Min = as.numeric(as.character(Min)),
      AP = as.numeric(gsub("\\,", "", AP)),
      AP = as.numeric(as.character(AP)),
      #Split out Opponent and HomeAway
      home_or_away = ifelse(startsWith(df$Opponent, "@"), "Away", "Home"),
      #Clean up opponent column
      Opponent = str_replace(Opponent, Team, ""),
      Opponent = str_replace(Opponent, "F$", ""),
      Opponent = str_replace_all(Opponent, "[^A-Z]", ""),
      Opponent = str_replace(Opponent, "MAM$", ""),
      #Remove quotes from ID column
      ID = gsub("^\\*|\\*$", "", ID),
      #remove the extra team name for the players who have moved teams
      Team = sub(".*/", "", Team)
    ) %>%
    #Keep only the character variables and the variables that are stats for that particular gameweek
    #This removes all accumulative stats like FP.G. I will calculate them myself
    select(
      c(character_variables, single_game_count_variables)
    )
  
  return(df)
}

NA_0_min_rows <- function(df){
  
  df <- df %>%
    mutate(
      across(all_of(single_game_count_variables), ~ if_else(Min == 0, NA_real_, .))
    )
  
  return(df)
}

add_new_columns <- function(df){
  
  df <- df %>% 
    mutate(
      TkWAndIntAndCLR = TkW + Int + CLR,
      SOTAndKP = SOT + KP,
      CoSMinusDIS = CoS - DIS,
      SOTMinusG = SOT - G,
      KPMinusA = KP - A
    )
  
  return(df)
}


#This grabs the latest gameweek, which should have the most up to date status, opponent, etc.
#...and uses that as the template to add stats to
create_template <- function(df){
  
  template <- df %>% 
    filter(
      Gameweek == max(Gameweek)
    ) %>% 
    select(
      all_of(template_variables)
    )
  
  return(template)
}

fix_double_gameweeks <- function(df) {
  
  # Part 1: rows with GP == 2 (or any GP > 1)
  double_gw_rows <- df %>%
    filter(GP > 1) %>%
    mutate(row_id = row_number()) %>%
    #split the row into the number in GP.
    uncount(weights = GP, .id = "game_index", .remove = FALSE) %>%
    mutate(across(all_of(c(single_game_count_variables, new_single_game_count_variables)), ~ . / GP)) %>%
    select(-row_id, -game_index)
  
  # Part 2: rows where GP is 1 or NA (leave as-is)
  single_gw_rows <- df %>%
    filter(is.na(GP) | GP <= 1)
  
  # Combine both parts
  df <- bind_rows(single_gw_rows, double_gw_rows)
  
  return(df)
}

#Filters the gwdf as opposed to post filter which filters the dashboard
filter_data <- function(df, input_team, input_status, input_position, start_gameweek, end_gameweek){
  
  #filter within the specified gameweek window
  df <- df %>% 
    filter(
      Gameweek >= start_gameweek & Gameweek <= end_gameweek
    )
  
  if (input_team != "All") {
    df <- df %>%
      filter(
        Team == input_team
      )
  }
  
  if(input_status != "All"){
    df <- df %>%
      filter(
        switch(
          input_status,
          "Waiver" = str_detect(Status, "^W \\(") | Status == "WW",
          "All Available" = str_detect(Status, "^W \\(") | str_detect(Status, "^FA") | Status == "WW",
          "All Taken" = !Status %in% statuses,
          Status == input_status
        )
      )
  }
  
  if (input_position != "All") {
    df <- df %>%
      filter(
        switch(
          input_position,
          "D" = str_detect(Position, "D"),
          "M" = str_detect(Position, "M"),
          "F" = str_detect(Position, "F"),
          TRUE                   # default if status is something else
        )
      )
  }
    
  return(df)
}

create_dashboard_data <- function(filtered_df, input_cols){
  
  #create base summary df to add cols to
  df <- template
  
  #add in default cols
  default_cols <- c("Min", "Min.Mean", "Min.Form", "FPts.Mean", "FPts.90", input_cols)
  
  for(i in default_cols){
    if(!(i %in% colnames(df))){
      var <- ""
      stat <- ""
      #get the variable and stat from the input
      #If it doesn't contain a "." then its a plain sum stat
      if (!(grepl("\\.", i))) {
        var <- i
        stat <- "Sum"
      }
      #if it does contain a dot grab the characters eithe side of it
      else{
        var <- strsplit(i, ".", fixed = TRUE)[[1]][1]
        stat <- strsplit(i, ".", fixed = TRUE)[[1]][2]
      }
      
      df <- Add_Statistic(df, filtered_df, var, stat)
      
    }
  }
  
  return(df)
}

#Individual Stat functions. So they can call each other
stat_Sum <- function(df, var) sum(df[[var]], na.rm = TRUE)
stat_SD <- function(df, var) round(sd(df[[var]], na.rm = TRUE), 2)
stat_Mean <- function(df, var) round(mean(df[[var]], na.rm = TRUE), 2)
stat_Med <- function(df, var) median(df[[var]], na.rm = TRUE)
stat_LQ <- function(df, var) quantile(df[[var]], na.rm = TRUE)[[2]]
stat_MAD <- function(df, var) mad(df[[var]], constant = 1, na.rm = TRUE)
stat_DownDev <- function(df, var) {
  round(DownsideDeviation(df[[var]], MAR = mean(df[[var]], na.rm = TRUE), na.rm = TRUE), 2)
}
stat_Skew <- function(df, var) {
  round(skewness(df[[var]], na.rm = TRUE), 3)
}
stat_MeanMnsDD <- function(df, var) {
  mean_val <- mean(df[[var]], na.rm = TRUE)
  downDev_val <- DownsideDeviation(df[[var]], MAR = mean_val, na.rm = TRUE)
  if (is.na(mean_val) || is.na(downDev_val)) return(NA)
  round(mean_val - downDev_val, 2)
}
stat_90 <- function(df, var) {
  minutes <- sum(df$Min, na.rm = TRUE)
  round((sum(df[[var]], na.rm = TRUE) / minutes) * 90, 2)
}
stat_FormAdj <- function(df, var) {
  df <- df[order(df$Gameweek, decreasing = TRUE), ]
  decay <- pmax(1 - (seq_len(nrow(df)) - 1) * 0.2, 0)
  weighted_sum <- sum(df[[var]] * decay, na.rm = TRUE)
  total_weight <- sum(decay[!is.na(df[[var]])])
  round(weighted_sum / total_weight, 2)
}
stat_Form <- function(df, var) {
  formAdj <- stat_FormAdj(df, var)
  mean_val <- mean(df[[var]], na.rm = TRUE)
  if (is.na(mean_val) || mean_val == 0) return(NA)
  round(formAdj / mean_val, 2)
}

Add_Statistic <- function(df, filtered_df, var, stat) {
  
  stat_functions <- list(
    Sum = stat_Sum,
    SD = stat_SD,
    Mean = stat_Mean,
    Med = stat_Med,
    LQ = stat_LQ,
    MAD = stat_MAD,
    DownDev = stat_DownDev,
    Skew = stat_Skew,
    MeanMnsDD = stat_MeanMnsDD,
    `90` = stat_90,
    FormAdj = stat_FormAdj,
    Form = stat_Form
  )
  
  stat_function <- stat_functions[[stat]]
  if (is.null(stat_function)) stop("Invalid statistic")
  
  colName <- ifelse(stat != "Sum", paste0(var, ".", stat), var)
  
  df <- df %>%
    left_join(
      filtered_df %>%
        group_by(Player) %>%
        summarise(
          !!sym(colName) := stat_function(pick(where(is.numeric)), var)
        ),
      by = "Player"
    )
  
  return(df)
}

#function for applying the filters inputted by the user in the dashboard
filter_dashboard_data <- function(df, min_mins, min_min.mean, min_FPts.mean, max_FPts.mean, min_FPts.90, max_FPts.90){
  
  df <- df %>%
    filter(
      Min >= min_mins,
      FPts.Mean >= min_FPts.mean & FPts.Mean <= max_FPts.mean,
      FPts.90 >= min_FPts.90 & FPts.90 <= max_FPts.90,
      Min.Mean >= min_min.mean
    )
  
  return(df)
}

#----------------------- CREATE SLIDERS DF -----------------------#

create_sliders_data <- function(filtered_df){
  
  df <- template %>% 
    Add_Statistic(filtered_df, "Min", "Sum") %>%
    Add_Statistic(filtered_df, "FPts", "Mean") %>%
    Add_Statistic(filtered_df, "FPts", "90") %>%
    # Add_Statistic(filtered_df, "Min", "Mean") %>%
    #Filter out players with like 1 minute that push the max FPts.90 to like 200
    filter(Min > 10)
  
  return(df)
}

#for the sliders

#---------------------------------------------- MAIN SETUP ----------------------------------------------#

#setup data
gwdf <- load_gameweek_data() %>% 
  update_eligibility() %>% 
  clean_data() %>% 
  NA_0_min_rows() %>% 
  add_new_columns()

#needs to be done b4 fixing double gameweeks because it may select a double gw as the template
#if that gw was the latest one. So players would end up in there twice. 
template <- create_template(gwdf)

#fix gameweeks
gwdf <- gwdf %>% 
  fix_double_gameweeks()

#Create data for dashboard sliders
sliderdf <- create_sliders_data(gwdf)

#---------------------------------------------- UI ----------------------------------------------#


ui <- fluidPage(
  
  theme = shinytheme("flatly"), 
  navbarPage("Fantrax",
             tabPanel("Plot",
                      sidebarLayout(
                        sidebarPanel(
                          width = "2",
                          selectInput("pTeam","Choose a Team", choices = c("All", unique(sort(gwdf$Team))), selected = "All"),
                          selectInput("pStatus","Choose a Status", choices = c("All", "All Available", "All Taken", "Waiver", fantrax_teams$shortName), selected = "All Available"),
                          selectInput("pPosition","Choose a Position", choices = c("All", "D", "M", "F"), selected = "All"),
                          selectInput("pXVar", "Select X-axis:", choices = sort(varCombos), selected = "Min.Form"),
                          selectInput("pYVar", "Select Y-axis:", choices = sort(varCombos), selected = "FPts.90"),
                          sliderInput("pWindow", "Gameweek Window", min = min(gwdf$Gameweek), max = max(gwdf$Gameweek), value = c(min(gwdf$Gameweek), max(gwdf$Gameweek))),
                          sliderInput("pMinMins", "Minimum Total Minutes", min = 0, max = max(sliderdf$Min, na.rm = TRUE), value = min(10, na.rm = TRUE)),
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
                          selectInput("tStatus","Choose a Status", choices = c("All", "All Available", "All Taken", "Waiver", fantrax_teams$shortName), selected = "All Available"),
                          selectInput("tPosition","Choose a Position", choices = c("All", "D", "M", "F"), selected = "All"),
                          pickerInput("tPicker", "Columns", choices = sort(varCombos), options = list(`actions-box` = TRUE), selected=NULL, multiple=TRUE),
                          sliderInput("tWindow", "Gameweek Window", min = min(gwdf$Gameweek), max = max(gwdf$Gameweek), value = c(min(gwdf$Gameweek), max(gwdf$Gameweek))),
                          sliderInput("tMinMins", "Minimum Total Minutes", min = 0, max = max(sliderdf$Min, na.rm = TRUE), value = min(10, na.rm = TRUE)),
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
  
  output$plot <- renderPlot({
    
    plotData <- gwdf %>% 
      filter_data(input$pTeam, input$pStatus, input$pPosition, input$pWindow[1], input$pWindow[2]) %>% 
      create_dashboard_data(c(input$pXVar, input$pYVar)) %>% 
      #Added in 0 for Min.Mean because the Plot doesn't filter by average Mins
      filter_dashboard_data(input$pMinMins, 0, input$pFPts.Mean[1], input$pFPts.Mean[2], input$pFPts.90[1], input$pFPts.90[2])
      
    #remove NAs before plotting
    plotData <- plotData %>%
      filter(!is.na(.data[[input$pXVar]]), !is.na(.data[[input$pYVar]]))
    
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
  
    extra_cols <- c("FPts.MeanMnsDD", "FPts.DownDev", "FPts.Form", "FPts.FormAdj", "Pts.90")
    
    tableData <- gwdf %>% 
      filter_data(input$tTeam, input$tStatus, input$tPosition, input$tWindow[1], input$tWindow[2]) %>% 
      create_dashboard_data(c(extra_cols, input$tPicker)) %>% 
      filter_dashboard_data(input$tMinMins, input$tMin.Mean, input$tFPts.Mean[1], input$tFPts.Mean[2], input$tFPts.90[1], input$tFPts.90[2])

  }, options = list(pageLength = 12), rownames = FALSE)
  
  output$boxPlot <- renderPlot({

    boxPlotData <- gwdf %>% 
      filter_data("All", "All", "All", input$bWindow[1], input$bWindow[2]) %>% 
      create_dashboard_data(c(input$bVar)) %>% 
      filter(!(Status %in% c("FA")) & !grepl("^W \\(", Status) & Status != "WW")
    
    ggplot(boxPlotData, aes(x = reorder(get(input$bTeamType), get(input$bVar), FUN=mean, na.rm = T ), y = get(input$bVar), fill = get(input$bTeamType))) +
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
           fill = input$bTeamType) +
      theme_classic()

  }, res = 90)
}

#---------------------------------------------- MAIN APP ----------------------------------------------#

# Run the app
shinyApp(ui = ui, server = server)

