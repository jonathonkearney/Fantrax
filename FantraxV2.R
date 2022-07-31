library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)

# **************************************************
rm(list = ls())

setwd("C:/Users/OEM/OneDrive/Documents/R/Fantrax")

FS <- read.csv("FS.csv", header = TRUE)
FT <- read.csv("FT.csv", header = TRUE)

#merging the dfs
df <- merge(x = FT, y = FS)

#remove the ID column now that we don't need it
df <- select(df, -ID)

#remove comma from data$Min and AP and convert to numeric 
df$Min <- as.numeric(gsub("\\,", "", df$Min))
df$Min <- as.numeric(as.character(df$Min))
df$AP <- as.numeric(gsub("\\,", "", df$AP))
df$AP <- as.numeric(as.character(df$AP))

#remove all players with less than a certain amount of mins
#this needs to be non-zero so that we don't get any div/0 errors
minMins <- 45
df <- subset(df, Min > minMins)

#add in the .90 columns
df <- mutate(df, FPts.90 = round(((FPts / Min)*90),2))
df <- mutate(df, G.90 = round(((G / Min)*90),2))
df <- mutate(df, A.90 = round(((A / Min)*90),2))
df <- mutate(df, Pts.90 = round(((Pts / Min)*90),2))
df <- mutate(df, S.90 = round(((S / Min)*90),2))
df <- mutate(df, SOT.90 = round(((SOT / Min)*90),2))
df <- mutate(df, FC.90 = round(((FC / Min)*90),2))
df <- mutate(df, FS.90 = round(((FS / Min)*90),2))
df <- mutate(df, YC.90 = round(((YC / Min)*90),2))
df <- mutate(df, RC.90 = round(((RC / Min)*90),2))
df <- mutate(df, DPt.90 = round(((DPt / Min)*90),2))
df <- mutate(df, Off.90 = round(((Off / Min)*90),2))
df <- mutate(df, PKG.90 = round(((PKG / Min)*90),2))
df <- mutate(df, CS.90 = round(((CS / Min)*90),2))
df <- mutate(df, A2.90 = round(((A2 / Min)*90),2))
df <- mutate(df, KP.90 = round(((KP / Min)*90),2))
df <- mutate(df, AT.90 = round(((AT / Min)*90),2))
df <- mutate(df, TkW.90 = round(((TkW / Min)*90),2))
df <- mutate(df, DIS.90 = round(((DIS / Min)*90),2))
df <- mutate(df, ErG.90 = round(((ErG / Min)*90),2))
df <- mutate(df, AP.90 = round(((AP / Min)*90),2))
df <- mutate(df, SFTP.90 = round(((SFTP / Min)*90),2))
df <- mutate(df, ACNC.90 = round(((ACNC / Min)*90),2))
df <- mutate(df, Int.90 = round(((Int / Min)*90),2))
df <- mutate(df, CLR.90 = round(((CLR / Min)*90),2))
df <- mutate(df, CoS.90 = round(((CoS / Min)*90),2))
df <- mutate(df, AER.90 = round(((AER / Min)*90),2))
df <- mutate(df, PKM.90 = round(((PKM / Min)*90),2))
df <- mutate(df, OG.90 = round(((OG / Min)*90),2))
df <- mutate(df, GAD.90 = round(((GAD / Min)*90),2))
df <- mutate(df, CSD.90 = round(((CSD / Min)*90),2))
df <- mutate(df, CSM.90 = round(((CSM / Min)*90),2))

#new columns
df <- mutate(df, Min.GP = round((Min / GP) ,2))
# df <- mutate(df, Def = round((( / Min)*90),2))
# df <- mutate(df, Att = round((( / Min)*90),2))



# **************************************************

ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  navbarPage("Fantrax",
     tabPanel("Plot",
        sidebarLayout(
          
          sidebarPanel(
            
            width = "2",
            
            selectInput("pTeam","Choose a Team", choices = c("All",unique(sort(df$Team))), selected = "All"),
            selectInput("pStatus","Choose a Status", choices = c("All", "All Available", unique(sort(df$Status)), "Waiver"), selected = "All Available"),
            selectInput("pPosition","Choose a Position", choices = c("All", "D", "M", "F"), selected = "All"),
            selectInput("pYAxis","Choose the Y Axis", choices = sort(names(df)), selected = "FPts.90"),
            selectInput("pXAxis","Choose the X Axis", choices = sort(names(df)), selected = "KP.90"),
            sliderInput("pMinMins", "Minimum Minutes", min = minMins, max = max(df$Min), value = minMins),
            sliderInput("pMinMinsPerGP", "Minimum Minutes Per GP", min = min(df$Min.GP), max = max(df$Min.GP), value = min(df$Min.GP)),
            checkboxInput("pAddLines", "Add Lines", value = FALSE, width = NULL)
            
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
            sliderInput("tMinMins", "Minimum Minutes", min = minMins, max = max(df$Min), value = minMins),
            sliderInput("tMinMinsPerGP", "Minimum Minutes Per GP", min = min(df$Min.GP), max = max(df$Min.GP), value = min(df$Min.GP)),
            checkboxGroupInput("tColumns", "Columns:",
                        c("Player" = "Player",
                          "Position" = "Position",
                          "Team" = "Team",
                          "Status" = "Status",
                          "FP.G" = "FP.G",
                          "FPts.90" = "FPts.90",
                          "KP.90" = "KP.90",
                          "Opponent" = "Opponent",
                          "GP" = "GP",
                          "Min" = "Min"
                          ),
                        selected = c("Player","Position","Team","Status","FPts.90", "KP.90")
                        ),
          ),
          
          mainPanel(
            DT::dataTableOutput("table")
          )
        )
     )
  )
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    df_temp <- df
    df_temp <- filter(df_temp, Min > input$pMinMins)
    df_temp <- filter(df_temp, Min.GP > input$pMinMinsPerGP)
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
        check_overlap = T,
        adj = -0.1,
        vjust="inward"
      ) +
      coord_flip(clip = "off")
    
    if(input$pAddLines == TRUE) {
      p + theme_classic() + geom_smooth(method="lm", se=F)
    }
    else{
      p + theme_classic()
    }
    
  }, res = 90) #the resolution of the plot
  
  output$table = DT::renderDataTable({
    df_temp <- df
    df_temp <- filter(df_temp, Min > input$tMinMins)
    df_temp <- filter(df_temp, Min.GP > input$tMinMinsPerGP)
    df_temp <- df_temp[, which((names(df_temp) %in% input$tColumns)==TRUE)]
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
  
}

shinyApp(ui, server)

