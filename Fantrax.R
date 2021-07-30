library(ggplot2)
library(tidyverse)
library(shiny)
library(dplyr)
library(DT)

#set the directory as the R folder and save the two tables as variables
setwd("C:/Users/Joe/Documents/R")
FS <- read.csv("FS.csv", header = TRUE)
FT <- read.csv("FT.csv", header = TRUE)

#merge the two tables together
df <- merge(x = FT, y = FS)

minMins <- 180

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
     && x != "Rk" && x != "Status" && x != "Opponent" && x != "FP.G"
     && x != "GP" && x != "PC." && x != "Min" && x != "G.G" && x != "Total"){
    name <- paste(colnames(df)[i], ".90", sep="")
    df[,name] <- round((df[,i] / df$Min)*90, digits = 2)
  }
}

df$Min.GP <- round((df$Min / df$GP), digits = 2)

#Make a total column that is the sum of all .90 scores (as % of their max scores)
df$Total <- 0
for(i in 1:ncol(df)){
  x <- colnames(df)[i]
  if(endsWith(x, ".90")){
    df$Total <- df$Total + (df[,i]/ max(df[,i]))
  }
}
#Make a score that calculates the total and the Min.90 together
#Not sure if Total*Min.GP is the right calc or not...
df$Total.Min.GP <- df$Total * df$Min.GP

ui <- fluidPage(
  
  # App title ----
  titlePanel("Fantrax"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      width = "2",
      
      selectInput("team","Choose a team", choices = c("All",unique(df$Team)), selected = "All"),
      selectInput("status","Choose a Status", choices = c("All",unique(df$Status)), selected = "All"),
      selectInput("position","Choose a Position", choices = c("All",unique(df$Position)), selected = "All"),
      selectInput("xAxis","Choose the X Axis", choices = names(df), selected = "FP.G"),
      selectInput("yAxis","Choose the Y Axis", choices = names(df), selected = "Min.GP"),
      
    ),
    
    mainPanel(
      plotOutput(outputId = "plot",width = "1540px", height = "900px"),
      br(),
      DT::dataTableOutput("table")
      
    )
  )
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    if (input$team != "All") {
      df <- filter(df, Team == input$team)
    }
    if (input$status != "All") {
      df <- filter(df, Status == input$status)
    }
    if (input$position != "All") {
      df <- filter(df, Position == input$position)
    }
    
    p <- ggplot(df, aes(colour = Position)) + aes_string(x = input$xAxis, y = input$yAxis) +
        geom_point() +
        geom_text(
          aes(label = Player),
          check_overlap = T,
          adj = -0.1,
          vjust="inward",
          hjust="inward"
        ) +
          coord_flip(clip = "off")
    p + theme_minimal()
    
  }, res = 90)
  
  output$table = DT::renderDataTable({
    # df %>% select(!c("ID", "Opponent", "Rk"))
    df %>% select(c("Player", "Team", "Status", "FP.G", "FPts.90", "Total", "Total.Min.GP", "Min.GP", "G.90", "A.90"))
  })
  
}
shinyApp(ui, server)
