library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(corrplot)

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
     && x != "Rk" && x != "Status" && x != "Opponent" && x != "FP.G" && x != "X.D"
     && x != "ADP"&& x != "GP" && x != "PC." && x != "Min" && x != "G.G" && x != "Total"){
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

ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  navbarPage("Fantrax",
     tabPanel("Plot",
        sidebarLayout(
          
          sidebarPanel(
            
            width = "2",
            
            selectInput("team","Choose a team", choices = c("All",unique(sort(df$Team))), selected = "All"),
            selectInput("status","Choose a Status", choices = c("All",unique(sort(df$Status))), selected = "All"),
            selectInput("position","Choose a Position", choices = c("All",unique(sort(df$Position))), selected = "All"),
            selectInput("yAxis","Choose the Y Axis", choices = sort(names(df)), selected = "FPts.90"),
            selectInput("xAxis","Choose the X Axis", choices = sort(names(df)), selected = "Min.GP"),
            checkboxInput("addLines", "Add Lines", value = TRUE, width = NULL)
            
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
          selectInput("tStatus","Choose a Status", choices = c("All",unique(df$Status)), selected = "All"),
          selectInput("tPosition","Choose a Position", choices = c("All",unique(df$Position)), selected = "All"),
          
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
    tabPanel("Fantrax Teams",
     sidebarLayout(
       
       sidebarPanel(
         
         width = "2"
         
       ),
       
       mainPanel(
         plotOutput(outputId = "fTeams",width = "1500px", height = "800px")
       )
     )
    ),
    tabPanel("Teams",
     sidebarLayout(
       
       sidebarPanel(
         
         width = "2"
         
       ),
       
       mainPanel(
         plotOutput(outputId = "teams",width = "1500px", height = "800px")
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
      df <- filter(df, Status == input$status)
    }
    if (input$position != "All") {
      df <- filter(df, Position == input$position)
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
      df <- filter(df, Status == input$tStatus)
    }
    if (input$tPosition != "All") {
      df <- filter(df, Position == input$tPosition)
    }
    df %>% select(c("Player", "Position", "Team", "Status", "FP.G", "FPts.90", "AT.KP", "PotentialFP", "Min.GP", "KP.90", "G.90", "A.90"))
  })
  
  output$corrPlot <- renderPlot({
    
    temp <- select(df, ends_with(".90"))
    
    # m = cor(temp)
    # corrplot(m, method = 'color', order = 'alphabet')
    
    m <- cor(x = df$FPts.90, y = temp, use="complete.obs")
    corrplot(m, method = "number", tl.srt = 25)
  })
  
  output$fTeams <- renderPlot({
    
    # temp2 <- df[df$Status != "W (Fri)"] 
    temp2 <- subset(df, Status!= "W (Fri)" & Status!= "W (Sat)" & Status!= "W (Sun)" & 
                      Status!= "W (Mon)" & Status!= "W (Tue)" & Status!= "W (Wed)" &
                      Status!= "W (Thu)" & Status!= "FA")
    
    ggplot(temp2, aes(x=Status, y=FP.G, fill=Status)) +
      geom_boxplot()
  })
  
  output$teams <- renderPlot({
    
    
    ggplot(df, aes(x=Team, y=FPts.90, fill=Team)) +
      geom_boxplot()
  })
}
shinyApp(ui, server)
