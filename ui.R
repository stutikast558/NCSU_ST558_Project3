################################################################################################
########  ST 558 Project 3 ui code for Shiny app - NBA Player stats and Analytics ##############
########  Student : Sridhar Tutika                                                ##############
########  Uses package SportsAnalytics to fetch NBA player Data                   ##############
########  Also uses csv down loaded data from https://www.kaggle.com/schmadam97/nba-regular-season-stats-20182019#
################################################################################################
require(shiny)
require(shinydashboard)
require(leaflet)
require(shinyBS)
library(SportsAnalytics)
library(shinyjs)
library(readr)
library(DT)

header <- dashboardHeader(title = "NBA Stas Dashboard ")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem(
      "Overview", icon = icon("info", lib = "font-awesome"), tabName = "overview"
    ),
    menuItem(
      "Player Stats Compare", icon = icon("user-friends", lib = "font-awesome"), tabName = "pcompare"
    ),
    menuItem(
      "Team Stats", icon = icon("basketball-ball", lib = "font-awesome"), tabName = "tstats"
    ),
    menuItem(
      "Supervised Model", icon = icon("flash", lib = "font-awesome"), tabName = "smodel"
    )
  #  menuItem(
  #    "Unsupervised Model", icon = icon("magic", lib = "font-awesome"), tabName = "umodel"
  #  )
  )
)
########## function from SportsAnalytics to get player data
selectedDataAll1 <- function(){
  temp1 <- fetch_NBAPlayerStatistics(season = "18-19")
  temp1[order(-temp1$TotalPoints, -temp1$GamesPlayed),]
}
########## Reading csv downloaded data from 
######## https://www.kaggle.com/schmadam97/nba-regular-season-stats-20182019#
######## for team stats
nba_teams <- function(){
  tmp2 <- read_csv("nba_team_stats_00_to_18.csv")
}

body <- dashboardBody(
                  tags$head(tags$style(
                  HTML('.skin-blue .wrapper{background-color:#ecf0f5}')
                  )),
        ## code for player stats menu          
        tabItems(
          tabItem(
          tabName = "pcompare",
          fluidRow(
           column(
                width = 4,
                 box(  
              title = "Player stats", status = "danger", width = 14, solidHeader = TRUE,
              selectInput("season","NBA Season",
            # select Season
                  c(
                        "01-02" = "01-02",
                        "02-03" = "02-03",
                        "03-04" = "03-04",
                        "04-05" = "04-05",
                        "05-06" = "05-06",
                        "06-07" = "06-07",
                        "07-08" = "07-08",
                        "08-09" = "08-09",
                        "09-10" = "09-10",
                        "10-11" = "10-11",
                        "11-12" = "11-12",
                        "12-13" = "12-13",
                        "13-14" = "13-14",
                        "14-15" = "14-15",
                        "15-16" = "15-16",
                        "16-17" = "16-17",
                        "17-18" = "17-18",
                        "18-19" = "18-19"
                                        ),
                        selected = "18-19"
                        ),
                          selectInput("PlayerA", label = h4("Select Player A"),
                            choices = as.list(as.character(unique(selectedDataAll1()$Name))),
                        #    select 1st player
                            selected = as.list(as.character(unique(selectedDataAll1()$Name)))[1],
                            selectize = TRUE,
                            multiple = FALSE),
                          selectInput("PlayerB", label = h4("Select Player B"),
                            choices = as.list(as.character(unique(selectedDataAll1()$Name))),
                        # # select 2nd player
                            selected = as.list(as.character(unique(selectedDataAll1()$Name)))[2],
                            selectize = TRUE,
                            multiple = FALSE),
        
                          radioButtons(
                            "dataType",
                            label = "Stat Type",
                            choices = c("Average", "Total"),
                            selected = "Average"
                          ),
          # select data type
                              checkboxGroupInput(
                                "stats",
                                label = "Stats to compare",
                                choices = c(
                                  "Points" = "TotalPoints",
                                  "Threes Made" = "ThreesMade",
                                  "Rebounds" = "TotalRebounds",
                                  "Assists" = "Assists",
                                  "Steals" = "Steals",
                                  "Blocks" = "Blocks",
                                  "Turnovers" = "Turnovers"
                                ),
                                selected = c(
                                  "TotalPoints",
                                  "ThreesMade",
                                  "TotalRebounds",
                                  "Assists",
                                  "Steals",
                                  "Blocks",
                                  "Turnovers"
                                ),
                                inline = "TRUE"
                              ),
                              # select data type
                              
                              downloadButton('downloadData', 'Download Data'),
                              
                              downloadButton('downloadPlot', 'Download Plot')
                               ),
          ),
       box(width = 8,  plotOutput("plot", height=500))
      )),
    #####code for Team stats menu
    tabItem(
      tabName = "tstats",
      #bsAlert("alert0"),
      fluidRow(
        column(
          width = 3,
          box(  
            title = "Team stats 2018-19", status = "primary", width = 14, solidHeader = TRUE,
            selectInput("seasont","NBA Season",
                        # select Season
                        c(
                          "2001-02" = "2001-02",
                          "2002-03" = "2002-03",
                          "2003-04" = "2003-04",
                          "2004-05" = "2004-05",
                          "2005-06" = "2005-06",
                          "2006-07" = "2006-07",
                          "2007-08" = "2007-08",
                          "2008-09" = "2008-09",
                          "2009-10" = "2009-10",
                          "2010-11" = "2010-11",
                          "2011-12" = "2011-12",
                          "2012-13" = "2012-13",
                          "2013-14" = "2013-14",
                          "2014-15" = "2014-15",
                          "2015-16" = "2015-16",
                          "2016-17" = "2016-17",
                          "2017-18" = "2017-18",
                          "2018-19" = "2018-19",
                          "ALL" = "ALL"
                        ),
                        selected = "ALL"
            ),
            selectInput("TeamA", label = h4("Select a Team"),
                        choices = as.list(as.character(unique(nba_teams()$TEAM))),
                        selected = as.list(as.character(unique(nba_teams()$TEAM[1]))),
                        selectize = TRUE,
                        multiple = FALSE),
            ),
        ),
        tabBox(
          side = "left", width=9, height = "500",
          selected = "Team_Table",
          tabPanel("Team_Table",  DTOutput("table")),
          tabPanel("Team_Performance",plotOutput("plot2")))
        ##### this is working below
            #  box(width = 8,  DTOutput("table"))
        ####### this was working above
      )),
    #### Attach new code for Modeling below ####
    
    tabItem(
      tabName = "smodel",
      fluidRow(
        column(
          width = 4,
          box(  
            title = "Regression Model for Player Salary", status = "primary", width = 14, solidHeader = TRUE,
            selectInput("PlayerC", label = h4("Select a Player"),
                        choices = as.list(as.character(unique(selectedDataAll1()$Name))),
                        #    select 1st player
                        selected = as.list(as.character(unique(selectedDataAll1()$Name)))[1],
                        selectize = TRUE,
                        multiple = FALSE),
            radioButtons(
              "ModelType",
              label = "Supervised Model Type",
              choices = c("Linear_Regression", "Boosted_Tress"),
              selected = "Linear_Regression"
            ),
            # select data type
            checkboxGroupInput(
              "stats2",
              label = "Stats to use for the model",
              choices = c(
                "Points" = "TotalPoints",
                "Threes Made" = "ThreesMade",
                "Rebounds" = "TotalRebounds",
                "Assists" = "Assists",
                "Steals" = "Steals",
                "Blocks" = "Blocks",
                "Turnovers" = "Turnovers"
              ),
              selected = c(
                "TotalPoints",
                "ThreesMade",
                "TotalRebounds",
                "Assists",
                "Steals",
                "Blocks",
                "Turnovers"
              ),
              inline = "TRUE"
            )
          ),
        ),
        box(width = 8,  plotOutput("plotm"))
      )),
    
    #### Attach new code above ####
    ### Code for Introduction and Information Menu
    tabItem(tabName = "overview",
            fluidRow(column(
              4,
              box(
                title = "Contents", status = "primary", width = 12, solidHeader = TRUE,
                div(actionLink("help1", "Introduction"), style = "font-size: 125%"),
                div(actionLink("help2", "Supervised Learning Models"), style = "font-size: 125%"),
                div(actionLink("help3", "Principal Components analysis"), style = "font-size: 125%")
              )
            )
            ,
            column(
              8, box(htmlOutput('instructions'),status = "primary", width = 12, solidHeader = TRUE)
            )))
))
ui <- dashboardPage(skin="red", header,sidebar, body)
##############################################################
############  End of UI Code  ################################
##############################################################