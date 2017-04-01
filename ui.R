# load required library
library(shiny)
library(shinydashboard)
library(plotly)


dashboardPage(
  # name a title to the project
  dashboardHeader(title = "Airline On-Time Performance Analysis",
                  titleWidth = 400),
  # create dynamic airport dropdown list
  dashboardSidebar(sidebarMenu(
    uiOutput("originmenu"),
    uiOutput("destmenu")
  )),
  
  dashboardBody(
    # add the Jetblue company logo
    tags$img(align = "right", src = "logo.jpg", width = 150),
    # used for custom local image files as icon
    tags$head(# must include css
      tags$style(
        HTML(
          "
          .img-local {
          }
          
          .small-box .img-local {
          position: absolute;
          top: auto;
          bottom: 5px;
          right: 5px;
          z-index: 0;
          font-size: 70px;
          color: rgba(0, 0, 0, 0.15);
          }"
        )
        )),
    tabsetPanel(
      # create dynamic plot output and statistical summaries result
      tabPanel(
        "Departure and Arrival Delay Analysis",
        fluidRow(
          box(plotOutput("hist1"), br()),
          box(
            uiOutput("vBoxesLight1"),
            status = "primary",
            width = 6
          )
        ),
        fluidRow(
          box(plotOutput("hist2"), br()),
          box(
            uiOutput("vBoxesLight2"),
            status = "primary",
            width = 6
          )
        )
      ),
      # create par chart for the analysis of most common cause for delay
      tabPanel("Delay Cause Analysis",
               fluidRow(box(
                 plotlyOutput("par1")
               ))),
      # create x-bar chart for process control analysis
      tabPanel("Process Control Analysis",
               fluidRow(box(
                 plotOutput("xbarchart")
               )))
      
    )
        )
    )
