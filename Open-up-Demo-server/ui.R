################################################################################
# # Property of Polytechnique Montréal
################################################################################
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
list.of.packages <- c("shiny","shinythemes","devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(shinythemes)
library(devtools)

# 
source_url("https://raw.githubusercontent.com/jamorafo/Open-Up-Demo-/master/src/RuN.R")

################################################################################
# # Shiny presentation
################################################################################

# Define UI for application that draws a histogram
shinyUI(navbarPage(title="", theme=shinytheme("united"),
                   tabPanel(style = "padding-bottom: 20px;",
                            title = "Training",
                            tags$head(
                              tags$style(HTML("
                                              @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                                              
                                              h1 {
                                              font-family: 'Lobster', cursive;
                                              font-weight: 500;
                                              line-height: 1.1;
                                              color: #48ca3b;
                                              }
                                              body {
                                              background : img('CIMAR.png')
                                              }
                                              h3 {
                                              color: #FF0000;
                                              }
                                              "))
                              ),
                            headerPanel("Open Up"),
                            sidebarLayout(
                              # Sidebar with a slider input
                              sidebarPanel(width = 3,
                        checkboxInput("checkbox", label = "BOZ", value = TRUE),
                                      sliderInput("i",
                                          "Normal observations",
                                          min = 1,
                                          max = sum(y==0),
                                          value = 1),
                                      sliderInput("bad",
                                        "Outliers",
                                        min = 0,
                                        max = sum(y==1),
                                        value = 0)
                            ,
                            checkboxInput("emptybox", label = "Empty zones", value = TRUE), 
                                      sliderInput("e.l",
                                                  "BOZ limits",
                                                  min = 0,
                                                  max = 0.1,
                                                  value = 0),
                                       sliderInput("e.s",
                                               "Empty Zones tolerance",
                                               min = 0,
                                               max = 0.1,
                                               value = 0.1)
                            ,
                           fluidRow(h6(icon("copyright"),"Polytechnique Montréal")),
                            tags$img(src='poly.png',width = "80%")
                            ),
                            mainPanel(fluidRow(column(3,br()),column(7,tableOutput('acctable'))),
                                      plotOutput("boz",width = "105%"),
                                      br(),
                                      br(),
                                      fluidRow(column(5,br()),
                                        column(3,offset=0,tags$img(src='CIMARLAB.png',width = "150%")),
                                    column(3,offset=1,img(src='CIMAR.png',width = "100%")))
                            )
                            )
                   ),
                   tabPanel("On going",
                            #sidebarLayout(
                              # Sidebar with a slider input
                             # sidebarPanel(width = 5, tags$img(src='1600.gif',width = "100%"),
                            #div(style="display:flex",actionButton("anomaly4","Anomaly 4",icon("cogs")), actionButton("anomaly3","Anomaly 3",icon("cogs")),
                            fluidRow(column(6,headerPanel("Open Up")), column(6,offset=0,h2(textOutput("currentTime") ))),
                            #  actionButton("anomaly1","Anomaly 1",icon("cogs"))
                              #  )),
                            # Show a plot of the generated distribution
                              fluidRow(column(6,offset=0,tags$img(src='1600.gif',width = "92%"),
                                              column(2,offset=0,actionButton("anomaly4","Sector 4", icon = icon("exclamation-circle"))),
                                              column(2,offset=1,actionButton("anomaly3","Sector 3", icon = icon("exclamation-circle"))),
                                              column(2,offset=1,actionButton("anomaly2","Sector 2", icon = icon("exclamation-circle"))),
                                              column(1,offset=1,actionButton("anomaly1","Sector 1", icon = icon("exclamation-circle")))
                                              ),
                                     column(5,offset=0,tags$img(src='normalobs.gif',width = "120%"))),
                            
                            fluidRow(column(6,"http://www.zabalgarbi.com/"),column(6,offset=0, h6(icon("copyright"),"Polytechnique Montréal"))),
                             h3(textOutput("anomalyTime")), 
                            plotOutput("anomaly",width = "100%")
                   )
))


