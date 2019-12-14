################################################################################################
########  ST 558 Project 3 server code for Shiny app - NBA Player stats and Analytics ##############
########  Student : Sridhar Tutika                                                    ############
########  Uses package SportsAnalytics to fetch NBA player Data                       ############
########  Also uses csv down loaded data from https://www.kaggle.com/schmadam97/nba-regular-season-stats-20182019#
########  Produces player stats compare Barcharts that can be downloaded in csv or pdf format ##
################################################################################################

library(shiny)
require(shinydashboard)
library(SportsAnalytics)
library(ggplot2)
require(car)
require(DT)
require(caret)
require(maptools)
require(rgdal)
require(raster)
require(sp)
require(rgeos)
require(leaflet)
require(shinyBS)
require(RColorBrewer)
library(readr)
library(dplyr)
library(tidyverse)
library(shinyjs)
library(hrbrthemes)
library(stringi)
options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output, session) {

  ##########################
  ## TAB CHANGE EVENTS
  ##########################
  observeEvent(input$tabs, {
    ## If Player stats tab is pressed 
    if (input$tabs == "pcompare") {
      # --- Retrieve nba data by season based on users' input using SportsAnalytics function 
      selectedDataAll <- reactive({
        temp <- fetch_NBAPlayerStatistics(input$season)
        temp[order(-temp$TotalPoints, -temp$GamesPlayed),]
      })
      # --- Subset data based on input (player & stats)
      plotData <- reactive({
        tempdf = data.frame()
        if (input$dataType == "Total") {
          playerA <- subset(selectedDataAll(), Name == input$PlayerA, select = c("Name", "GamesPlayed", input$stats))
          playerB <- subset(selectedDataAll(), Name == input$PlayerB, select = c("Name", "GamesPlayed",input$stats))
          tempdf <- rbind(playerA, playerB)
        } 
        else if (input$dataType == "Average") 
        {
          playerA <- subset(selectedDataAll(), Name == input$PlayerA, select = c("Name", "GamesPlayed",input$stats))
          playerA[, input$stats] <- playerA[, input$stats]/playerA$GamesPlayed
          playerB <- subset(selectedDataAll(), Name == input$PlayerB, select = c("Name", "GamesPlayed",input$stats))
          playerB[, input$stats] <- playerB[, input$stats]/playerB$GamesPlayed
          tempdf <- rbind(playerA, playerB)
        }
        
        # reshape the date from "wide" to "long"
        newdf <- reshape(tempdf, varying = input$stats,
                         v.names = "NBA_Stats",
                         timevar = "Stats_Type",
                         times = input$stats,
                         new.row.names = 1:10000,
                         direction = "long")
        newdf
      })
      
      # --- Create Bar Charts
      output$plot <- renderPlot({
        p <- ggplot(plotData(), aes(x=Stats_Type, y=NBA_Stats, fill=Name)) +
          geom_bar(position="dodge", stat="identity") +  coord_flip() +
          xlab("") + 
          ylab("") +
          theme(axis.text=element_text(size=12),
                axis.title=element_text(size=12,face="bold"),
                legend.text=element_text(size=15),legend.position = "top") +
          geom_text(aes(label=sprintf("%.02f", NBA_Stats)), 
                    position=position_dodge(width=0.9), 
                    vjust=-0.25, size=4)
        
        print(p)
      })
      
      
      # --- Download Data
      output$downloadData <- downloadHandler(
        filename = function() { 
          paste(paste(input$PlayerA, input$PlayerB, sep = " VS "), 
                ".csv", sep = "")
        },
        content = function(file) {
          write.csv(plotData(), file)
        }
      )
      
      # --- Download Plot
      output$downloadPlot <- downloadHandler(
        filename = function() { 
          paste(paste(input$PlayerA, input$PlayerB, sep = " VS "),
                ".pdf", sep = "")
        },
        content = function(file) {
          pdf(file)  # open the pdf device
          thisplot <- plotData()
          
          p <- ggplot(thisplot, aes(x=Stats_Type, y=NBA_Stats, fill=Name)) +
            geom_bar(position="dodge", stat="identity") +
            geom_text(aes(label=sprintf("%.02f", NBA_Stats)), position=position_dodge(width=0.9), vjust=-0.25)
          print(p)
          
          dev.off()  # turn the device off
        }
      )
      ## If Team stats tab is pressed
    } else if (input$tabs == "tstats"){
      
       TableData_ref <- reactive({
         tmp_rf <- read_csv("nba_team_stats_00_to_18.csv") 
         tmp1 <- mutate(tmp_rf,winper= paste(as.character(tmp_rf$`WIN%`*100), "%"))   
       })
       
         TableData <- reactive({ 
         tmp2 <- TableData_ref()
         if (input$seasont == "ALL" & input$TeamA == "ALL" ){
           df_tabledata <- tmp2
         }else if (input$seasont == "ALL") {df_tabledata <- filter(tmp2, tmp2$TEAM == input$TeamA )
         }else if (input$TeamA == "ALL") {df_tabledata <- filter(tmp2, tmp2$SEASON == input$seasont )
         }else {
           df_tabledata <- filter(tmp2, tmp2$TEAM == input$TeamA & tmp2$SEASON == input$seasont)
         }
         df_tabledata_2 <- dplyr::select(df_tabledata,TEAM, SEASON, 
                                         GP, winper,PTS, `+/-`)
         
         attributes(df_tabledata_2)$names[1] <- "Team"
         attributes(df_tabledata_2)$names[2] <- "Season"
         attributes(df_tabledata_2)$names[3] <- "Games Played"
         attributes(df_tabledata_2)$names[4] <- "Win %"
         attributes(df_tabledata_2)$names[5] <- "Points"
         attributes(df_tabledata_2)$names[6] <- "Player/Team Impact"
         df_tabledata_2
      })
         ## Send the table based on selection
         output$table <- renderDT(TableData())
         
         ## Plot 2 data
         # 
          output$plot2 <- renderPlot({
         #   get the data
            tmp_plot1 <- TableData_ref() 
            tmp_plot2 <- filter(tmp_plot1, TEAM == input$TeamA)
         #   
         #   #create a scatter plot, set it to all seasons
            
            observe({updateSelectInput(session, "seasont", selected = "ALL")})
             
             p2 <- ggplot(tmp_plot2, aes(x = stri_sub(SEASON,-5,-1), y = `WIN%`*100))+
                   geom_line(color="#69b3a2") +
                    geom_point(color="#69b3a2", size=4) +
               labs(x = "Season", y = "Win %", title(paste(tmp_plot2$TEAM, " ", "Team Win % Visual")))
             print(p2)
            #theme_ipsum() 
          })
      
    }
    
  })
  
#################################################################################################
#### Defautls to Overview Tab ###################################################################
######################################################################################   
    output$instructions <- renderText({
              includeHTML("help1r.rhtml")
            })
    observeEvent(input$help1, {
      output$instructions <- renderText({
        includeHTML("help1r.rhtml")
      })
    })
    observeEvent(input$help2, {
      output$instructions <- renderText({
        includeHTML("Supervised_model.html")
      })
    })
    observeEvent(input$help3, {
      output$instructions <- renderText({
        includeHTML("UnSupervised_model_PCA.html")
      })
    })
})

###################### END of Server code   ######################