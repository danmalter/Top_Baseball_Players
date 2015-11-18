## ui.R
library(Lahman)
library(shiny)
library(shinydashboard)
library(rCharts)

shinyUI(dashboardPage(skin="black",
                      dashboardHeader(title = "Baseball Players"),
                      dashboardSidebar(
                        sidebarMenu(
                          menuItem("Individual Batters", tabName = "playerStats", icon = icon("star-o")),
                          menuItem(list(selectInput("player_statistic", label = h5("Individual Batters"),
                                             list("Albert Pujols" = "pujolal01",
                                                  "Babe Ruth" = "ruthba01", 
                                                  "Barry Bonds" = "bondsba01",  
                                                  "Lou Gehrig" = "gehrilo01",
                                                  "Honus Wagner" = "wagneho01",
                                                  "Mickey Mantle" = "mantlmi01",
                                                  "Ty Cobb" = "cobbty01",
                                                  "Ted Williams" = "willite01",
                                                  "Joe DiMaggio" = "dimagjo01",
                                                  "Willie Mays" = "mayswi01"))
                              )
                          ),
                          menuItem("Individual Pitchers", tabName = "ind_pitchingStats", icon = icon("glyphicon glyphicon-hand-up", lib = "glyphicon")),
                          menuItem(list(selectInput("pitch_player_statistic", label = h5("Individual Pitchers"),
                                             list("Walter Johnson" = "johnswa01",
                                                  "Lefty Grove" = "grovele01", 
                                                  "Roger Clemens" = "clemero02",  
                                                  "Bob Gibson" = "gibsobo01",
                                                  "Don Drysdale" = "drysddo01",
                                                  "Pete Alexander" = "alexape01",
                                                  "Christy Mathewson" = "mathech01",
                                                  "Eddie Plank" = "planked01",
                                                  "Tom Seaver" = "seaveto01",
                                                  "Cy Young" = "youngcy01"))
                              )
                          ),
                          menuItem("Compare Batters", tabName = "offensiveStats", icon = icon("star-o")),
                          menuItem(list(selectInput("statistic", label = h5("Offensive Statistics"),
                                             list("Runs" = "R",
                                                  "Hits" = "H", 
                                                  "Doubles" = "X2B",
                                                  "Triples" = "X3B",  
                                                  "Home Runs" = "HR",
                                                  "RBIs" = "RBI",
                                                  "Stolen Bases" = "SB",
                                                  "Caught Stealing" = "CS",
                                                  "Walks" = "BB",
                                                  "Strikeouts" = "SO",
                                                  "Intentional Base on Balls" = "IBB",
                                                  "Hit by Pitch" = "HBP",
                                                  "Sacrifice Hits" = "SH",
                                                  "Sacrifice Flys" = "SF",
                                                  "Grounders into Double Plays" = "GIDP",
                                                  "Batting Average" = "BA",
                                                  "Plate Appearances" = "PA",
                                                  "Total Bases" = "TB",
                                                  "Slugging Percentage" = "SlugPct",
                                                  "On Base Percentage" = "OBP",
                                                  "On Base Plus Slugging" = "OPS",
                                                  "Batting Avg. on Balls in Play" = "BABIP"))
                            )
                          ),
                          menuItem("Compare Pitchers", tabName = "pitchingStats", icon = icon("glyphicon glyphicon-hand-up", lib = "glyphicon")),
                          menuItem(list(selectInput("pitch_statistic", label = h5("Pitchers Statistics"),
                                                    list("Wins" = "W",
                                                         "Losses" = "L", 
                                                         "Games" = "G",
                                                         "Games Started" = "GS",  
                                                         "Complete Games" = "CG",
                                                         "Shutouts" = "SHO",
                                                         "Outs Pitched" = "IPouts",
                                                         "Hits" = "H",
                                                         "Earned Runs" = "ER",
                                                         "Home Runs" = "HR",
                                                         "Walks" = "BB",
                                                         "Oppenents Batting Avg." = "BAOpp",
                                                         "Earned Run Average" = "ERA",
                                                         "Intenional Walks" = "IBB",
                                                         "Games Finished" = "GF",
                                                         "Runs" = "R"))
                            )
                          ),
                          menuItem("About", tabName = "about", icon = icon("gear")),
                          menuItem("Source code", icon = icon("file-code-o"), 
                                   href = "https://github.com/danmalter/Top_Baseball_Players")
                        )
                      ),
                      
                      dashboardBody(
                        tags$head(
                          tags$style(type="text/css", "select { max-width: 360px; }"),
                          tags$style(type="text/css", ".span4 { max-width: 360px; }"),
                          tags$style(type="text/css",  ".well { max-width: 360px; }")
                        ),
                        
                        tabItems(  
                          tabItem(tabName = "about",
                                  h2("About this App"),
                                  
                                  HTML('<br/>'),
                                  
                                  fluidRow(
                                    box(title = "Author: Danny Malter", background = "black", width=7, collapsible = TRUE,
                                        
                                        helpText(p(strong("This application is designed to compare statistics between the top 20 players in MLB history."))),
                                        
                                        helpText(p("Please contact",
                                                   a(href ="https://twitter.com/danmalter", "Danny on twitter",target = "_blank"),
                                                   " or at my",
                                                   a(href ="http://danmalter.github.io/", "personal page", target = "_blank"),
                                                   ", for more information, to suggest improvements or report errors.")),
                                        
                                        helpText(p("All code and data is available at ",
                                                   a(href ="https://github.com/danmalter/", "my GitHub page",target = "_blank"),
                                                   "or click the 'source code' link on the sidebar on the left."
                                        ))
                                        
                                    )
                                  )
                          ),
                          tabItem(tabName = "playerStats",
                              HTML('<br/>'),
                              box(showOutput("plot3", "highcharts"), width=13, collapsible = TRUE),
                              HTML('<br/>')
                              #box(dataTableOutput("pitcherTable"), title = "Table of Pitchers", width=11, collapsible = TRUE)
                          ),
                          tabItem(tabName = "ind_pitchingStats",
                                  HTML('<br/>'),
                                  box(showOutput("plot4", "highcharts"), width=13, collapsible = TRUE),
                                  HTML('<br/>')
                                  #box(dataTableOutput("pitcherTable"), title = "Table of Pitchers", width=11, collapsible = TRUE)
                          ),
                          tabItem(tabName = "offensiveStats",
                              HTML('<br/>'),
                              showOutput("plot", "highcharts"), width=11, collapsible = TRUE,
                              HTML('<br/>'),
                              box(dataTableOutput("batterTable"), title = "Table of Batters", width=11, collapsible = TRUE)
                      ),
                          tabItem(tabName = "pitchingStats",
                              HTML('<br/>'),
                              showOutput("plot2", "highcharts"), width=11, collapsible = TRUE,
                              HTML('<br/>'),
                              box(dataTableOutput("pitcherTable"), title = "Table of Pitchers", width=11, collapsible = TRUE)
                      )
                        )
                      
                          )

    )
)