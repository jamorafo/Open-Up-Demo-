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
list.of.packages <- c("shiny","shinythemes","devtools","shinyjs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(shinythemes)
library(devtools)
#library(leaflet)
library(shinyWidgets)
library(shinyjs)
#library(shinydashboard)
# 

source_url("https://raw.githubusercontent.com/jamorafo/Open-Up-Demo-/master/src/RuN.R")
#setwd("/Users/andresmorales/Google_Drive_gmail/CIMARLAB/Open\ Up/Open-Up-Demo/src/")
#source("RuN.R")


################################################################################
# # Shiny presentation
################################################################################

# Define UI for application that draws a histogram
shinyUI(navbarPage(title="", theme=shinytheme("united"), 
                   ################################################################################
                   # # First Panel
                   ################################################################################
                   tabPanel("On going",
                            style = "padding-bottom: 20px;",
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
                                              h2 {
                                              font-family: 'Lobster', cursive;
                                              color: #48ca3b;
                                              }
                                              h3 {
                                              color: #FF0000;
                                              }
                                              "))
                              ),
                            tags$style("#openup {
                                              font-family: 'Lobster', cursive;
                                              font-size: 40px;
                                              position:absolute;
                                              line-height: 0;
                                              width: 100%;
                                              padding-top:70px;
                                              left:80px;
                                              color: #48ca3b; }"),
                            tags$style("#currentTime2 {
                                       font-size:20px;
                                       padding-top:70px;
                                       color:blue;
                                       display:block; }"),
                            tags$style("#currentTime {
                                       font-size:20px;
                                       padding-top:70px;
                                       color:blue;
                                       display:block; }"),
                            tags$style("#anomalyTime2 {
                                       font-size:20px;
                                       padding-top:70px;
                                       color:red;
                                       display:block; }"),


                            fluidRow(column(12,
                                            fluidRow(column(6,textOutput("openup")) ,column(3,textOutput("currentTime2")),column(3,uiOutput("anomalyTime2")),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            fluidRow(column(4, 
                                            fluidRow( 
                                              column(1, br()),
                                              column(9,
                                              tags$img(src='1600.gif',width = "130%"),
                                                               fluidRow(
                                                                 column(2,offset=1,actionButton(inputId = "button4", label = "Sector 4")),
                                                                 column(2,offset=1,actionButton(inputId = "button3", label = "Sector 3")),
                                                                 column(2,offset=1,actionButton(inputId = "button2", label = "Sector 2")),
                                                                 column(2,offset=1,actionButton(inputId = "button1", label = "Sector 1"))
                                                                 )
                                              ),
                                              column(2,offset=0,
                                                               br(),
                                                               actionBttn(inputId = "outlier",label = "", icon = icon("bolt"),color = "danger"),
                                                               br(),
                                                               br(),
                                                               actionBttn("reset","", icon = icon("recycle") ,color = "success")
                                                               )
                                            )
                                            ),
                            column(8,offset=0,
                                   plotOutput("bozz",width = "100%")
                                   )
                            )))),
                            br(),
                            br(),
                            br(),
                            fluidRow(column(3,tags$img(src='poly.png',width = "80%")), 
                                     column(4,br()),
                                     column(2,offset=0, br(),
                                            br(),
                                                      tags$img(src='CIMARLAB.png',width = "110%")),
                            column(3,offset=0,img(src='CIMAR.png',width = "60%"))),
                            br(),
                            column(12,h6("http://www.zabalgarbi.com/")),
                            column(12,offset=0, h6(icon("copyright"),"Polytechnique Montréal")),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            tags$script("$(document).ready(function(){
                                        setTimeout(function() {$('.slider-animate-button').click()},10);
                                        });"),
                            
                            
                            # useShinyjs() has to be call for the clicking count
                            useShinyjs(),
                           #textOutput("clickCount"),
                           

                           sliderInput("time",
                                       "",
                                       min = 1,
                                       max = length(id_good),
                                       value = 1, animate = animationOptions(interval = 1000, loop = TRUE),
                                       ticks = FALSE,
                                       width='0%')
                           
                           
                   ),
                   ################################################################################
                   # # Second Panel
                   ################################################################################
                   
                   tabPanel("Demo",
                            tags$style("#anomalyTime {
                                       font-size:20px;
                                       padding-top:70px;
                                       color:red;
                                       display:block; }"),
                            tags$style("#openup2 {
                                              font-family: 'Lobster', cursive;
                                       font-size: 40px;
                                       position:absolute;
                                       line-height: 0;
                                       width: 100%;
                                       padding-top:70px;
                                       left:80px;
                                       color: #48ca3b; }"),
                            
                            
                            fluidRow(column(12,
                                            fluidRow(column(6,textOutput("openup2")) ,column(3,textOutput("currentTime")),column(3,uiOutput("anomalyTime")),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     fluidRow(column(4, 
                                                                     fluidRow( 
                                                                       column(1, br()),
                                                                       column(9,
                                                                              tags$img(src='1600.gif',width = "130%"),
                                                                              fluidRow(
                                                                                column(2,offset=1,actionButton(inputId = "anomaly4", label = "Sector 4")),
                                                                                column(2,offset=1,actionButton(inputId = "anomaly3", label = "Sector 3")),
                                                                                column(2,offset=1,actionButton(inputId = "anomaly2", label = "Sector 2")),
                                                                                column(2,offset=1,actionButton(inputId = "anomaly1", label = "Sector 1"))
                                                                              )
                                                                       )#,
                                                                       #column(2,offset=0,
                                                                       #    br(),
                                                                       #     actionBttn(inputId = "outlier",label = "", icon = icon("bolt"),color = "danger"),
                                                                       #     br(),
                                                                       #      br(),
                                                                       #      actionBttn("reset","", icon = icon("recycle") ,color = "success")
                                                                       #)
                                                                     )
                                                     ),
                                                     column(8,offset=0,
                                                            plotOutput("bozz2",width = "100%")
                                                     )
                                                     )))),
                            fluidRow(column(3,
                                            tags$img(src='poly.png',width = "80%"),
                                            h6("http://www.zabalgarbi.com/"),
                                            h6(icon("copyright"),"Polytechnique Montréal"),
                                            tags$img(src='cimarlab.jpeg',width = "60%")
                                            ), 
                                     column(1,br()),
                                     column(8,offset=0, 
                                            plotOutput("anomaly",width = "100%")
                                            ))
                               
                    ),
                   ################################################################################
                   # # Third Panel
                   ################################################################################
                   tabPanel(title = "Training",
                            
                            tags$style("#acctable{
                                       font-family:'Courier New', Courier, monospace;
                                       font-weight:bold;
                                       vertical-align:middle;
                                       text-align:center;
                                       color:#438EC8;
                                       background-color:#FFFFFF;
                                       border:2px solid #B28DFF;
                                       xl-wrap-text:wrap;
                                       display:block; }"),
                            
                            
                            headerPanel("Open Up"),
                            sidebarLayout(
                              # Sidebar with a slider input
                              sidebarPanel(width = 3,
                        checkboxInput("checkbox", label = "BOZ", value = TRUE),
                                      sliderInput("i",
                                          "Normal observations",
                                          min = 1,
                                          max = length(id_good),
                                          value = 1),
                                      sliderInput("bad",
                                        "Outliers",
                                        min = 0,
                                        max = length(id_bad),
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
                            mainPanel(fluidRow(column(3,br()),column(4,tableOutput('acctable'))), column(3,br()),
                                      plotOutput("boz",width = "105%"),
                                      br(),
                                      br(),
                                      fluidRow(column(5,br()),
                                        column(3,offset=0,tags$img(src='CIMARLAB.png',width = "150%")),
                                    column(3,offset=1,img(src='CIMAR.png',width = "100%")))
                            )
                            )
                   )

))


