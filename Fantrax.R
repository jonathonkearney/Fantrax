library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(corrplot)
library(rvest)

url_data <- "https://en.wikipedia.org/wiki/2021%E2%80%9322_Premier_League"
css_selector <- "#mw-content-text > div.mw-parser-output > table:nth-child(25)"
league_table <- url_data %>% 
  read_html() %>% 
  html_element(css = css_selector) %>% 
  html_table()

#abbreviate the Team name column in the league table
league_table$Team <- sub("Tottenham Hotspur", "TOT", league_table$Team)
league_table$Team <- sub("West Ham United", "WHU", league_table$Team)
league_table$Team <- sub("Manchester United", "MUN", league_table$Team)
league_table$Team <- sub("Chelsea", "CHE", league_table$Team)
league_table$Team <- sub("Liverpool", "LIV", league_table$Team)
league_table$Team <- sub("Everton", "EVE", league_table$Team)
league_table$Team <- sub("Manchester City", "MCI", league_table$Team)
league_table$Team <- sub("Brighton & Hove Albion", "BHA", league_table$Team)
league_table$Team <- sub("Leicester City", "LEI", league_table$Team)
league_table$Team <- sub("Brentford", "BRF", league_table$Team)
league_table$Team <- sub("Aston Villa", "AVL", league_table$Team)
league_table$Team <- sub("Watford", "WAT", league_table$Team)
league_table$Team <- sub("Southampton", "SOU", league_table$Team)
league_table$Team <- sub("Crystal Palace", "CRY", league_table$Team)
league_table$Team <- sub("Leeds United", "LEE", league_table$Team)
league_table$Team <- sub("Burnley", "BUR", league_table$Team)
league_table$Team <- sub("Newcastle United", "NEW", league_table$Team)
league_table$Team <- sub("Wolverhampton Wanderers", "WOL", league_table$Team)
league_table$Team <- sub("Norwich City", "NOR", league_table$Team)
league_table$Team <- sub("Arsenal", "ARS", league_table$Team)

#set the directory as the R folder and save the two tables as variables
setwd("C:/Users/Joe/Documents/R")
FS <- read.csv("FS.csv", header = TRUE)
FT <- read.csv("FT.csv", header = TRUE)

#merge the two tables together
df <- merge(x = FT, y = FS)

minMins <- 20

#clean up the opponent column in df
df$Opponent <- ifelse(startsWith(df$Opponent, "@"), substring(df$Opponent, 2), df$Opponent)
df$Opponent <- substr(df$Opponent, 1, 3) 

#add in the OppGA and OPPGF and OppRank columns
df$OppGA <- league_table$GA[match(df$Opponent, league_table$Team)]
df$OppGA <- as.numeric(as.character(df$OppGA))
df$OppGF <- league_table$GF[match(df$Opponent, league_table$Team)]
df$OppGF <- as.numeric(as.character(df$OppGF))
df$OppGD <- df$OppGF - df$OppGA
df$OppPos <- league_table$Pos[match(df$Opponent, league_table$Team)]
df$TeamPos <- league_table$Pos[match(df$Team, league_table$Team)]
MaxOppGF <- max(df$OppGF)
MaxOppGA <- max(df$OppGA)
MaxOppGD <- max(df$OppGD)
MinOppGF <- min(df$OppGF)
MinOppGA <- min(df$OppGA)
MinOppGD <- min(df$OppGD)
AvgOppGF <- mean(df$OppGF)
AvgOppGA <- mean(df$OppGA)
AvgOppGD <- mean(df$OppGD)
RangeOppGA <- MaxOppGA - MinOppGA
RangeOppGF <- MaxOppGF - MinOppGF
RangeOppGD <-MaxOppGD - MinOppGD

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
     && x != "ADP"&& x != "GP" && x != "PC." && x != "Min" && x != "G.G"
     && x != "X..Owned" && x != "X..." && x != "xG" && x != "xA" && x != "influence"
     && x != "creativity" && x != "threat" && x != "X..." && x != "Ros." && x != "OppGA" 
     && x != "OppGF" && x != "OppGD" && x != "OppGDxFP.G" && x != "OppPos"
     && x != "PosDifxFP.G"){
    name <- paste(colnames(df)[i], ".90", sep="")
    df[,name] <- round((df[,i] / df$Min)*90, digits = 2)
  }
}

df$Min.GP <- round((df$Min / df$GP), digits = 2)

df$PotentialFP <- round(df$FPts.90 - df$FP.G, digits = 2)
df$AT.KP <- round(df$AT.90 / df$KP.90, digits = 2)

#create adjusted scores based off the league table position differences
AdjFactor <- 0.3 #I added the *.3 because I dont think they vary that much. and the score is an adjusted avg, not a upper or lower limit 

df$PosDif <- df$OppPos - df$TeamPos
df$PosAdjFP.G <- round(df$FP.G * (1 + (((1/19) * df$PosDif)*AdjFactor)), 2)
df$PosAdjFP.90 <- round(df$FPts.90 * (1 + (((1/19) * df$PosDif)*AdjFactor)), 2)

#create adjusted scores by calculating position against OppGA, OppGF, OppGA
df$GDAdjFP.G <- ifelse(grepl("F", df$Position), round(df$FP.G * (1 + (((1/ (RangeOppGA/2) ) * (df$OppGA - (RangeOppGA/2)) )*AdjFactor)), 2), 
                     ifelse(grepl("D", df$Position), round(df$FP.G * (1 + (((1/ (RangeOppGF/2) ) * ((RangeOppGF/2) - df$OppGF) )*AdjFactor)), 2), 
                            round(df$FP.G * (1 + (((1/ (RangeOppGD/2) ) * (0 - df$OppGD) )*AdjFactor)), 2)
                              ))

#add in the percetage games played variable
# gameweeks <-max(df$GP)
# df$GPxPosAdjFP.G<- round(df$PosAdjFP.G *(df$GP / gameweeks), 2)

PosPred <-  df %>%
  group_by(Status) %>%
  arrange(PosAdjFP.G, .by_group = TRUE) %>%
  top_n(10)
PosPred <- select(PosPred, Player, Status, PosAdjFP.G)

GDPred <-  df %>%
  group_by(Status) %>%
  arrange(GDAdjFP.G, .by_group = TRUE) %>%
  top_n(10)
GDPred <- select(GDPred, Player, Status, GDAdjFP.G)

AggTemp1 <- aggregate(df$PosAdjFP.G, by=list(Team=df$Status), FUN=sum)
colnames(AggTemp1)[2] <- "Total_PosAdjFP.G"
AggTemp2 <- aggregate(df$PosDif, by=list(Team=df$Status), FUN=sum)
colnames(AggTemp2)[2] <- "Total_PosDif"
AggTemp3 <- aggregate(df$GDAdjFP.G, by=list(Team=df$Status), FUN=sum)
colnames(AggTemp3)[2] <- "Total_GDAdjFP.G"

AggTemp4 <- aggregate(PosPred$PosAdjFP.G, by=list(Team=PosPred$Status), FUN=sum)
colnames(AggTemp4)[2] <- "Top10_Total_PosAdjFP.G"
AggTemp5 <- aggregate(GDPred$GDAdjFP.G, by=list(Team=GDPred$Status), FUN=sum)
colnames(AggTemp5)[2] <- "Top10_Total_GDAdjFP.G"

temp <- merge(AggTemp1, AggTemp2, by="Team")
temp <- merge(temp, AggTemp3, by="Team")
temp2 <- merge(AggTemp4, AggTemp5, by="Team")
FTeams <- merge(temp, temp2, by="Team")

FTeams <- FTeams[!startsWith(FTeams$Team, "W (") & FTeams$Team != "FA",]

Top10 <- function(Data, Team, Metric) {
  TheTop10 <- c()
  MaxD <- 5
  MaxM <- 5
  MaxF <- 3
  DCount <- 0
  MCount <- 0
  FCount <- 0
  Total <- 0
  
  Players <- filter(Data, Data$Status == {{Team}}) #Needs 2x Curly for some reason
  Players <- select(Players, Status, Player, Position, Metric)
  Players <- arrange(Players, desc(Players[,Metric]))
  
  for (row in 1:nrow(Players)) {
    if(Total < 11){
      if(grepl("F", Players[i,Position])){
        
      }
      else if(grepl("D", Players[i,Position])){
        
      }
      else{
        
      }
      Total <- Total +1
    }
  }

  TheTop10 <- append(TheTop10, x)
  
  return(Players)
}

Top10(df, "Joe", "PosAdjFP.G")

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
            selectInput("xAxis","Choose the X Axis", choices = sort(names(df)), selected = "SFTP.90"),
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
    ),
    tabPanel("PosDif",
     sidebarLayout(
       
       sidebarPanel(
         
         width = "2",
         selectInput("PosDifY","Choose the Y Axis", choices = sort(c("Top10_Total_GDAdjFP.G", "Total_GDAdjFP.G", "Total_PosAdjFP.G", "Top10_Total_PosAdjFP.G", "Total_PosDif")), selected = "Total_PosDif")
         
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
    df %>% select(c("Player", "Position", "Team", "Status", "FP.G", "FPts.90", "GDAdjFP.G",
                    "PosAdjFP.G", "PosAdjFP.90", "PosDif", "Opponent", "OppGD", "OppGF", "OppGA",  
                    "AT.KP", "Min.GP", "KP.90", "G.90", "A.90"))
  })
  
  output$corrPlot <- renderPlot({
    
    temp <- select(df, ends_with(".90"))
    
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
  output$PosDif <- renderPlot({
    
    #input$PosDifY is a character, so you have to use get() in aes 
    ggplot(FTeams, aes(x=reorder(Team, get(input$PosDifY), FUN=mean), get(input$PosDifY), fill=Team)) +
      geom_bar(stat="identity") + labs(x = "Teams", y = "Sum", title = input$PosDifY) +
      geom_text(aes(label = round(get(input$PosDifY),2)), position=position_dodge(width=0.9), vjust=-0.25)
    
  })
}
shinyApp(ui, server)

