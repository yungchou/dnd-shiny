library(shiny)
library(dygraphs)
library(shinythemes)

fluidPage(theme = shinytheme("flatly"),
  navbarPage("IoT",
    tabPanel("Data Analytics"),
    tabPanel("More Plots", plotOutput('more')),
    tabPanel("Table", DT::dataTableOutput("table")),
    tabPanel("Summary", verbatimTextOutput("summary")),
#    tabPanel("Yung's Blog"),

#  titlePanel("IoT Deployment"),
fluidRow(
  column(4,leafletOutput('devLocMap')),
  column(8,dygraphOutput("ts"))),
fluidRow(div(style='height:20px')),
fluidRow(div(style="margin-top:50px 0 50px"),
  column(4,dygraphOutput("rp1")),
  column(4,dygraphOutput("rp2")),
  column(4,dygraphOutput("rp3")),
fluidRow(div(style='height:20px'))
)))
