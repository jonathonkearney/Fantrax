library(shiny)

setwd("C:/Users/OEM/OneDrive/Documents/R/Fantrax")
df_DB <- read.csv("Fantrax_Data.csv", header = TRUE)
FTeams_DB <- read.csv("Fantrax_Agg_Data.csv", header = TRUE)

ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  navbarPage("Fantrax",
             tabPanel("Plot",
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          width = "2",
                          
                          selectInput("team","Choose a team", choices = c("All",unique(sort(df_DB$Team))), selected = "All"),
                          selectInput("status","Choose a Status", choices = c("All", "All Available", unique(sort(df_DB$Status)), "Waiver"), selected = "All"),
                          selectInput("position","Choose a Position", choices = c("All", "D", "M", "F"), selected = "All"),
                          selectInput("yAxis","Choose the Y Axis", choices = sort(names(df_DB)), selected = "FPts.90"),
                          selectInput("xAxis","Choose the X Axis", choices = sort(names(df_DB)), selected = "SFTP.90"),
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
                          
                          selectInput("tTeam","Choose a team", choices = c("All",unique(df_DB$Team)), selected = "All"),
                          selectInput("tStatus","Choose a Status", choices = c("All", "All Available", unique(df_DB$Status), "Waiver"), selected = "All"),
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
                          
                          selectInput("fTeamY","Choose the Y Axis", choices = sort(names(df_DB)), selected = "FP.G"),
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
                          
                          selectInput("VTeamY","Choose the Y Axis", choices = sort(names(df_DB)), selected = "FP.G"),
                          selectInput("VTeamX","Choose the X Axis", choices = c("Status", "Team"), selected = "Status"),
                          checkboxInput("addPlot", "Add BoxPlot", value = TRUE, width = NULL)
                          
                        ),
                        
                        mainPanel(
                          plotOutput(outputId = "VTeams",width = "1500px", height = "800px")
                        )
                      )
             ),
             tabPanel("PosDif",
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          width = "2",
                          selectInput("PosDifY","Choose the Y Axis", choices = sort(c("Starting11_GDAdjFP.G", "Starting11_PosAdjFP.G",
                                                                                      "Total_OppPos", "Top10_Total_GDAdjFP.G", "Total_GDAdjFP.G",
                                                                                      "Total_PosAdjFP.G", "Top10_Total_PosAdjFP.G", "Total_PosDif")),
                                                                                      selected = "Total_OppPos")
                        ),
                        
                        mainPanel(
                          plotOutput(outputId = "PosDif",width = "1500px", height = "800px")
                        )
                      )
             )
  )
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    if (input$team != "All") {
      df_DB <- filter(df_DB, Team == input$team)
    }
    if (input$status != "All") {
      if (input$status == "Waiver") {
        df_DB <- filter(df_DB, str_detect(Status, "^W \\("))
      }
      else if (input$status == "All Available") {
        df_DB <- filter(df_DB, str_detect(Status, "^W \\(") | str_detect(Status, "^FA"))
      }
      else{
        df_DB <- filter(df_DB, Status == input$status)  
      }
    }
    if (input$position != "All") {
      if(input$position == "D"){
        df_DB <- filter(df_DB, str_detect(Position, "D"))
      }
      else if(input$position == "M"){
        df_DB <- filter(df_DB, str_detect(Position, "M"))
      }
      else if(input$position == "F"){
        df_DB <- filter(df_DB, str_detect(Position, "F"))
      }
    }
    
    p <- ggplot(df_DB, aes(colour = Position)) + aes_string(input$yAxis, input$xAxis) +
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
      df_DB <- filter(df_DB, Team == input$tTeam)
    }
    if (input$tStatus != "All") {
      if (input$tStatus == "Waiver") {
        df_DB <- filter(df_DB, str_detect(Status, "^W \\("))
      }
      else if (input$tStatus == "All Available") {
        df_DB <- filter(df_DB, str_detect(Status, "^W \\(") | str_detect(Status, "^FA"))
      }
      else{
        df_DB <- filter(df_DB, Status == input$tStatus)  
      }
    }
    if (input$tPosition != "All") {
      if(input$tPosition == "D"){
        df_DB <- filter(df_DB, str_detect(Position, "D"))
      }
      else if(input$tPosition == "M"){
        df_DB <- filter(df_DB, str_detect(Position, "M"))
      }
      else if(input$tPosition == "F"){
        df_DB <- filter(df_DB, str_detect(Position, "F"))
      }
    }
    df_DB %>% select(c("Player", "Position", "Team", "Status", "FP.G", "FPts.90", "GDAdjFP.G",
                       "PosAdjFP.G", "PosDif", "Opponent", "OppGD",  
                      "GP", "Min.GP", "KP.90", "G.90", "A.90"))
  })
  
  output$corrPlot <- renderPlot({
    
    temp <- select(df_DB, ends_with(".90"))
    
    m <- cor(x = df_DB$FPts.90, y = temp)
    corrplot(m, method = "number", tl.srt = 25)
  })
  
  output$fTeams <- renderPlot({
    
    if(input$fTeamX == "Status"){
      temp2 <- subset(df_DB, Status!= "W (Fri)" & Status!= "W (Sat)" & Status!= "W (Sun)" & 
                        Status!= "W (Mon)" & Status!= "W (Tue)" & Status!= "W (Wed)" &
                        Status!= "W (Thu)" & Status!= "FA")
      
      #input$fTeamY is a character, so you have to use get() in aes 
      ggplot(temp2, aes(x = reorder(Status, get(input$fTeamY), FUN=mean), fill=Status)) + aes_string(y = input$fTeamY) +
        geom_boxplot(coef = 5) + labs(x = "Teams")
      
    }else{
      temp2 <- df_DB
      
      #input$fTeamY is a character, so you have to use get() in aes 
      ggplot(temp2, aes(x=reorder(Team, get(input$fTeamY), FUN=mean), get(input$fTeamY), fill=Team)) +
        geom_boxplot(coef = 5) + labs(x = "Teams")
    }
  })
  output$VTeams <- renderPlot({
    
    if(input$VTeamX == "Status"){
      temp3 <- subset(df_DB, Status!= "W (Fri)" & Status!= "W (Sat)" & Status!= "W (Sun)" & 
                        Status!= "W (Mon)" & Status!= "W (Tue)" & Status!= "W (Wed)" &
                        Status!= "W (Thu)" & Status!= "FA")
      
      #input$VTeamY is a character, so you have to use get() in aes 
      v <- ggplot(temp3, aes(x = reorder(Status, get(input$VTeamY), FUN=mean), fill=Status)) + aes_string(y = input$VTeamY) +
        geom_violin() + labs(x = "Teams")
      
    }
    else{
      temp3 <- df_DB
      
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
  output$PosDif <- renderPlot({
    
    #input$PosDifY is a character, so you have to use get() in aes 
    ggplot(FTeams_DB, aes(x=reorder(Team, get(input$PosDifY), FUN=mean), get(input$PosDifY), fill=Team)) +
      geom_bar(stat="identity") + labs(x = "Teams", y = "Sum", title = input$PosDifY) +
      geom_text(aes(label = round(get(input$PosDifY),2)), position=position_dodge(width=0.9), vjust=-0.25)
    
  })
}
shinyApp(ui, server)
