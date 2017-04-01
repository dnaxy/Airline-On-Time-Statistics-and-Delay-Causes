# load required library
library(shiny)
library(purrr)
library(dplyr)
library(reshape)
library(plotly)
library(qcc)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  source("override.R", local = TRUE) # override 'icon' and 'valueBox'
  # create logic for dynamic airport list output, once the origin airport is
  # selected, only the destination airport has air route from that origin airport
  # can been shown
  origin <- reactive({
    selectInput(
      inputId = "origin",
      label = "Origin Airport",
      choices = airport.list$OriginCityName,
      selected = TRUE,
      multiple = FALSE
    )
  })
  
  dest <- reactive({
    selectInput(
      inputId = "dest",
      label = "Destination Airport",
      choices = c(airport.list$DestCityName[which(airport.list$OriginCityName ==
                                                    input$origin)]),
      selected = TRUE,
      multiple = FALSE
    )
  })
  output$originmenu <- renderUI(origin())
  output$destmenu <- renderUI(dest())
  # logic for histogram output with custom icon
  # custom icon code is forked from Matt Leonawicz,
  # on https://gist.github.com/leonawicz/0fab3796b02a62b7f3bd0c02a171f0b7
  # the code has been modified to used in this project
  output$hist1 <- renderPlot({
    # get the origin airport, dest airport from user input
    originCity <- as.character (input$origin)
    destCity <- as.character (input$dest)
    # get the query result based on the user input
    DepDelay.df <-
      jetblue %>%
      filter(DepDelayMinutes > 0,
             OriginCityName ~ originCity,
             DestCityName ~ destCity) %>%
      select(DepDelayMinutes) %>% as.data.frame() %>% collect()
    depDelay <- DepDelay.df$DepDelayMinutes
    # plot the histogram
    hist(depDelay, breaks = 50)
    # caculate statistical summaries reslut on the databse level
    depDelay.summary <-
      jetblue %>%
      filter(DepDelayMinutes > 0,
             OriginCityName ~ originCity,
             DestCityName ~ destCity) %>%
      select(DepDelayMinutes) %>% as.data.frame() %>%
      summarise(
        mean = round(mean(DepDelayMinutes), 2),
        median = median(DepDelayMinutes),
        min = min(DepDelayMinutes),
        max = max(DepDelayMinutes),
        IQR = IQR(DepDelayMinutes),
        sd = round(sd(DepDelayMinutes), 2)
      ) %>% collect()
    # mask the statistical summaries result with custom icon and plot the result
    clrs <- c("yellow",
              "orange",
              "purple",
              "red",
              "blue",
              "navy")
    
    pTextSize <-
      function(depDelay, value.d)
        tags$p(depDelay, style = paste0("font-size: ", value.d, "%;"))
    
    vbox <- function(vb) {
      # taglist around all 12 value boxes
      tagList(fluidRow(
        tags$head(tags$style(HTML(
          ".small-box {height: 100px}"
        ))),
        column(6, vb[[1]], vb[[5]], vb[[3]]),
        column(6, vb[[2]], vb[[6]], vb[[4]])
      ))
    }
    
    # image files
    fileparts1 <-
      c(paste0("normal_", c("mean", "sd", "min", "max", "median"), "_"), "boxplot_iqr_")
    files_white <- paste0("stat_icon_", fileparts1, "white.png")
    
    val.d <-
      c(
        depDelay.summary$mean,
        depDelay.summary$sd,
        depDelay.summary$min,
        depDelay.summary$max,
        depDelay.summary$median
      )
    val.d <-
      c(val.d, paste(depDelay.summary$IQR))
    
    text <-
      map(c("Mean", "Std Dev", "Min", "Max", "Median", "IQR"),
          ~ pTextSize(.x, 150))
    output$vBoxesLight1 <- renderUI({
      vb <- map(1:6,
                ~ valueBox(
                  val.d[[.x]],
                  text[[.x]],
                  icon = icon(list(src = files_white[.x], width = "80px"), lib = "local"),
                  color = clrs[.x],
                  width = NULL
                ))
      vbox(vb)
    })
    output$distLight <- renderValueBox({
      x <- "stat_icon_normal_dist_white.png"
      valueBox(
        "Data",
        "light image icon color",
        icon = icon(list(src = x, width = "80px"), lib = "local"),
        color = "black",
        width = NULL
      )
    })
  })
  # the same logic as the first histogram plot
  output$hist2 <- renderPlot({
    originCity <- as.character (input$origin)
    destCity <- as.character (input$dest)
    arvDelay.df <-
      jetblue %>%
      filter(ArrDelayMinutes > 0,
             OriginCityName ~ originCity,
             DestCityName ~ destCity) %>%
      select(ArrDelayMinutes) %>% as.data.frame() %>% collect()
    arvDelay <- arvDelay.df$ArrDelayMinutes
    hist(arvDelay, breaks = 50)
    
    arvDelay.summary <-
      jetblue %>%
      filter(ArrDelayMinutes > 0,
             OriginCityName ~ originCity,
             DestCityName ~ destCity) %>%
      select(ArrDelayMinutes) %>% as.data.frame() %>%
      summarise(
        mean = round(mean(ArrDelayMinutes), 2),
        median = median(ArrDelayMinutes),
        min = min(ArrDelayMinutes),
        max = max(ArrDelayMinutes),
        IQR = IQR(ArrDelayMinutes),
        sd = round(sd(ArrDelayMinutes), 2)
      ) %>% collect()
    
    
    clrs <- c("yellow",
              "orange",
              "purple",
              "red",
              "blue",
              "navy")
    pTextSize <-
      function(arvDelay, value.a)
        tags$p(arvDelay, style = paste0("font-size: ", value.a, "%;"))
    
    vbox <- function(vb) {
      # taglist around all 12 value boxes
      tagList(fluidRow(
        tags$head(tags$style(HTML(
          ".small-box {height: 100px}"
        ))),
        column(6, vb[[1]], vb[[5]], vb[[3]]),
        column(6, vb[[2]], vb[[6]], vb[[4]])
      ))
    }
    
    # image files
    fileparts1 <-
      c(paste0("normal_", c("mean", "sd", "min", "max", "median"), "_"), "boxplot_iqr_")
    files_white <- paste0("stat_icon_", fileparts1, "white.png")
    
    val.a <-
      c(
        arvDelay.summary$mean,
        arvDelay.summary$sd,
        arvDelay.summary$min,
        arvDelay.summary$max,
        arvDelay.summary$median
      )
    val.a <-
      c(val.a, paste(arvDelay.summary$IQR))
    
    text <-
      map(c("Mean", "Std Dev", "Min", "Max", "Median", "IQR"),
          ~ pTextSize(.x, 150))
    output$vBoxesLight2 <- renderUI({
      vb <- map(1:6,
                ~ valueBox(
                  val.a[[.x]],
                  text[[.x]],
                  icon = icon(list(src = files_white[.x], width = "80px"), lib = "local"),
                  color = clrs[.x],
                  width = NULL
                ))
      vbox(vb)
    })
    output$distLight <- renderValueBox({
      x <- "stat_icon_normal_dist_white.png"
      valueBox(
        "Data",
        "light image icon color",
        icon = icon(list(src = x, width = "80px"), lib = "local"),
        color = "black",
        width = NULL
      )
    })
  })
  # make the par chart for the anlysis of the most common reason caused delay
  output$par1 <- renderPlotly({
    # get the airport input from user
    originCity <- as.character (input$origin)
    destCity <- as.character (input$dest)
    # caculate how many flights got delay because of the carrierdelay,weatherdelay,
    # NASdelay,securitydelay, and lateaircraftdelay
    count.CarrierDelay <-
      jetblue %>%
      filter(CarrierDelay > 0,
             OriginCityName ~ originCity,
             DestCityName ~ destCity) %>% summarise(count = count(CarrierDelay)) %>%  collect()
    
    count.WeatherDelay <-
      jetblue %>%
      filter(WeatherDelay > 0,
             OriginCityName ~ originCity,
             DestCityName ~ destCity) %>% summarise(count = count(WeatherDelay)) %>%  collect()
    
    count.NASDelay <-
      jetblue %>%
      filter(NASDelay > 0,
             OriginCityName ~ originCity,
             DestCityName ~ destCity) %>% summarise(count = count(NASDelay)) %>%  collect()
    
    count.SecurityDelay <-
      jetblue %>%
      filter(SecurityDelay > 0,
             OriginCityName ~ originCity,
             DestCityName ~ destCity) %>% summarise(count = count(SecurityDelay)) %>%  collect()
    
    count.LateAircraftDelay <-
      jetblue %>%
      filter(LateAircraftDelay > 0,
             OriginCityName ~ originCity,
             DestCityName ~ destCity) %>% summarise(count = count(LateAircraftDelay)) %>%  collect()
    
    # combine the result as dataframe, and give the coloumn with names
    delayCause <-
      as.data.frame(
        c(
          count.CarrierDelay,
          count.WeatherDelay,
          count.NASDelay,
          count.SecurityDelay,
          count.LateAircraftDelay
        ),
        col.names = c(
          "CarrierDelay",
          "WeatherDelay",
          "NASDelay",
          "SecurityDelay",
          "LateAircraftDelay"
        )
      )
    # melt the data for future use
    delayCause.df <- melt(delayCause)
    
    
    # use plotly package to make the par chart
    colors <-
      c(
        'rgb(211,94,96)',
        'rgb(128,133,133)',
        'rgb(144,103,167)',
        'rgb(171,104,87)',
        'rgb(114,147,203)'
      )
    
    
    plot_ly(
      delayCause.df,
      labels = ~ variable,
      values = ~ value,
      type = 'pie',
      textposition = 'inside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      marker = list(
        colors = colors,
        line = list(color = '#FFFFFF', width = 1)
      ),
      #The 'pull' attribute can also be used to create space between the sectors
      showlegend = TRUE
    ) %>%
      layout(
        title = 'Delay Cause Analysis',
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        yaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        )
      )
  })
  # make x-bart chart
  output$xbarchart <- renderPlot({
    # get airport from user choose
    originCity <- as.character (input$origin)
    destCity <- as.character (input$dest)
    # get variable value from database
    DepDelay.df <-
      jetblue %>%
      filter(DepDelayMinutes > 0,
             OriginCityName ~ originCity,
             DestCityName ~ destCity) %>%
      select(DepDelayMinutes) %>% as.data.frame() %>% collect()
    # use qcc package to make the chart
    qcc(DepDelay.df$DepDelayMinutes,
        type = "xbar.one",
        nsigmas = 3)
    
  })
})
