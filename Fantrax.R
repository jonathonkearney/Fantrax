library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(corrplot)
library(FPLOptimiseR)
# 
# #get understat data
understat <- fetch_xg_data()

reqd <- as.vector(c("full_name", "xG", "xA", "influence", "creativity", "threat"))
df_xp <- understat[,reqd]

#set the directory as the R folder and save the two tables as variables
setwd("C:/Users/Joe/Documents/R")
FS <- read.csv("FS.csv", header = TRUE)
FT <- read.csv("FT.csv", header = TRUE)

#merge the two tables together
df <- merge(x = FT, y = FS)

df <- merge(x = df, y = df_xp, by.x = "Player", by.y = "full_name")

minMins <- 20

#remove comma from data$Min and AP and convert to numeric 
df$Min <- as.numeric(gsub("\\,", "", df$Min))
df$Min <- as.numeric(as.character(df$Min))
df$AP <- as.numeric(gsub("\\,", "", df$AP))
df$AP <- as.numeric(as.character(df$AP))

#remove all players with less than a certain amount of mins
df <- subset(df, Min > minMins)

#Make new columns for the .90 scores
for(i in 1:ncol(df)){
  x <- colnames(df)[i]
  if(x != "ID" && x != "Player" && x != "Team" && x != "Position"
     && x != "Rk" && x != "Status" && x != "Opponent" && x != "FP.G" && x != "X.D"
     && x != "ADP"&& x != "GP" && x != "PC." && x != "Min" && x != "G.G" && x != "Total" 
     && x != "X..Owned" && x != "X..." && x != "xG" && x != "xA" && x != "influence"
     && x != "creativity" && x != "threat"){
    name <- paste(colnames(df)[i], ".90", sep="")
    df[,name] <- round((df[,i] / df$Min)*90, digits = 2)
  }
}

df$Min.GP <- round((df$Min / df$GP), digits = 2)

#Make a total column that is the sum of all .90 scores (as % of their max scores)
df$Total.90 <- 0
for(i in 1:ncol(df)){
  x <- colnames(df)[i]
  if(endsWith(x, ".90")){
    if(x != "RC.90" || x != "YC.90" || x != "DIS.90" || x != "ErG.90" 
       || x != "OG.90" || x != "Off.90" || x != "FPts.90"){
      df$Total.90 <- df$Total.90 + (df[,i]/ max(df[,i]))
    }
  }
}

df$PotentialFP <- round(df$FPts.90 - df$FP.G, digits = 2)
df$AT.KP <- round(df$AT.90 / df$KP.90, digits = 2)
df$xGandxA <- round(df$xG + df$xA, digits = 2)

ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  navbarPage("Fantrax",
     tabPanel("Plot",
        sidebarLayout(
          
          sidebarPanel(
            
            width = "2",
            
            selectInput("team","Choose a team", choices = c("All",unique(sort(df$Team))), selected = "All"),
            selectInput("status","Choose a Status", choices = c("All", "All Available", unique(sort(df$Status)), "Waiver"), selected = "All"),
            selectInput("position","Choose a Position", choices = c("All", "D", "M", "F"), selected = "All"),
            selectInput("yAxis","Choose the Y Axis", choices = sort(names(df)), selected = "FPts.90"),
            selectInput("xAxis","Choose the X Axis", choices = sort(names(df)), selected = "Min.GP"),
            checkboxInput("addLines", "Add Lines", value = FALSE, width = NULL)
            
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
          
          selectInput("tTeam","Choose a team", choices = c("All",unique(df$Team)), selected = "All"),
          selectInput("tStatus","Choose a Status", choices = c("All", "All Available", unique(df$Status), "Waiver"), selected = "All"),
          selectInput("tPosition","Choose a Position", choices = c("All", "D", "M", "F"), selected = "All"),
          
        ),
        
        mainPanel(
          DT::dataTableOutput("table")
        )
     )
    ),
    tabPanel("Correlations",
     sidebarLayout(
       
       sidebarPanel(
         
         width = "2"
         
       ),
       
       mainPanel(
         plotOutput(outputId = "corrPlot",width = "1500px", height = "150px")
       )
     )
    ),
    tabPanel("Teams",
     sidebarLayout(
       
       sidebarPanel(
         
         width = "2",
         
         selectInput("fTeamY","Choose the Y Axis", choices = sort(names(df)), selected = "FP.G"),
         selectInput("fTeamX","Choose the X Axis", choices = c("Status", "Team"), selected = "Status")
         
       ),
       
       mainPanel(
         plotOutput(outputId = "fTeams",width = "1500px", height = "800px")
       )
     )
    ),
    tabPanel("Teams-Violin",
     sidebarLayout(
       
       sidebarPanel(
         
         width = "2",
         
         selectInput("VTeamY","Choose the Y Axis", choices = sort(names(df)), selected = "FP.G"),
         selectInput("VTeamX","Choose the X Axis", choices = c("Status", "Team"), selected = "Status"),
         checkboxInput("addPlot", "Add BoxPlot", value = TRUE, width = NULL)
         
       ),
       
       mainPanel(
         plotOutput(outputId = "VTeams",width = "1500px", height = "800px")
       )
     )
    )
  )
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    if (input$team != "All") {
      df <- filter(df, Team == input$team)
    }
    if (input$status != "All") {
      if (input$status == "Waiver") {
        df <- filter(df, str_detect(Status, "^W \\("))
      }
      else if (input$status == "All Available") {
        df <- filter(df, str_detect(Status, "^W \\(") | str_detect(Status, "^FA"))
      }
      else{
        df <- filter(df, Status == input$status)  
      }
    }
    if (input$position != "All") {
      if(input$position == "D"){
        df <- filter(df, str_detect(Position, "D"))
      }
      else if(input$position == "M"){
        df <- filter(df, str_detect(Position, "M"))
      }
      else if(input$position == "F"){
        df <- filter(df, str_detect(Position, "F"))
      }
    }
    
    p <- ggplot(df, aes(colour = Position)) + aes_string(input$yAxis, input$xAxis) +
        geom_point() + 
        geom_text(
          aes(label = Player),
          check_overlap = T,
          adj = -0.1,
          vjust="inward"
        ) +
          coord_flip(clip = "off")
    
    if(input$addLines == TRUE) {
      p + theme_classic() + geom_smooth(method="lm", se=F)
    }
    else{
      p + theme_classic()
    }
    
  }, res = 90)
  
  output$table = DT::renderDataTable({
    
    if (input$tTeam != "All") {
      df <- filter(df, Team == input$tTeam)
    }
    if (input$tStatus != "All") {
      if (input$tStatus == "Waiver") {
        df <- filter(df, str_detect(Status, "^W \\("))
      }
      else if (input$tStatus == "All Available") {
        df <- filter(df, str_detect(Status, "^W \\(") | str_detect(Status, "^FA"))
      }
      else{
        df <- filter(df, Status == input$tStatus)  
      }
    }
    if (input$tPosition != "All") {
      if(input$tPosition == "D"){
        df <- filter(df, str_detect(Position, "D"))
      }
      else if(input$tPosition == "M"){
        df <- filter(df, str_detect(Position, "M"))
      }
      else if(input$tPosition == "F"){
        df <- filter(df, str_detect(Position, "F"))
      }
    }
    df %>% select(c("Player", "Position", "Team", "Status", "FP.G", "FPts.90", "AT.KP", "PotentialFP", "Min.GP", "KP.90", "G.90", "A.90"))
  })
  
  output$corrPlot <- renderPlot({
    
    temp <- select(df, ends_with(".90"))
    
    # m = cor(temp)
    # corrplot(m, method = 'color', order = 'alphabet')
    
    m <- cor(x = df$FPts.90, y = temp)
    corrplot(m, method = "number", tl.srt = 25)
  })
  
  output$fTeams <- renderPlot({
    
    if(input$fTeamX == "Status"){
      temp2 <- subset(df, Status!= "W (Fri)" & Status!= "W (Sat)" & Status!= "W (Sun)" & 
                        Status!= "W (Mon)" & Status!= "W (Tue)" & Status!= "W (Wed)" &
                        Status!= "W (Thu)" & Status!= "FA")
      
      #input$fTeamY is a character, so you have to use get() in aes 
      ggplot(temp2, aes(x = reorder(Status, get(input$fTeamY), FUN=mean), fill=Status)) + aes_string(y = input$fTeamY) +
        geom_boxplot(coef = 5) + labs(x = "Teams")
      
    }else{
      temp2 <- df
      
      #input$fTeamY is a character, so you have to use get() in aes 
      ggplot(temp2, aes(x=reorder(Team, get(input$fTeamY), FUN=mean), get(input$fTeamY), fill=Team)) +
        geom_boxplot(coef = 5) + labs(x = "Teams")
    }
  })
  output$VTeams <- renderPlot({
    
    if(input$VTeamX == "Status"){
      temp3 <- subset(df, Status!= "W (Fri)" & Status!= "W (Sat)" & Status!= "W (Sun)" & 
                        Status!= "W (Mon)" & Status!= "W (Tue)" & Status!= "W (Wed)" &
                        Status!= "W (Thu)" & Status!= "FA")
      
      #input$VTeamY is a character, so you have to use get() in aes 
      v <- ggplot(temp3, aes(x = reorder(Status, get(input$VTeamY), FUN=mean), fill=Status)) + aes_string(y = input$VTeamY) +
        geom_violin() + labs(x = "Teams")
      
    }
    else{
      temp3 <- df
      
      #input$VTeamY is a character, so you have to use get() in aes 
      v <- ggplot(temp3, aes(x=reorder(Team, get(input$VTeamY), FUN=mean), get(input$VTeamY), fill=Team)) +
        geom_violin() + labs(x = "Teams")
    }
    if(input$addPlot == TRUE) {
      v + geom_boxplot(width=.1)
    }
    else{
      v
    }
    
  })
}
shinyApp(ui, server)
