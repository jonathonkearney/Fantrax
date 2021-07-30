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
     && x != "GP" && x != "PC" && x != "Min" && x != "G.G"){
    name <- paste(colnames(df)[i], ".90", sep="")
    df[,name] <- round((df[,i] / df$Min)*90, digits = 2)
  }
}

df$Min.GP <- round((df$Min / df$GP), digits = 2)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Fantrax"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      width = "2",
      
      selectInput("team","Choose a team", choices = c("All",unique(df$Team)), selected = "All"),
      selectInput("status","Choose a Status", choices = c("All",unique(df$Status)), selected = "All"),
      selectInput("position","Choose a Position", choices = c("All",unique(df$Position)), selected = "All"),
      selectInput("xAxis","Choose the X Axis", choices = names(df), selected = "A.90"),
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
  
  # output$table = DT::renderDataTable({
  #   df %>% select(!c("ID", "Opponent", "Rk"))
  # })
  
}
shinyApp(ui, server)
