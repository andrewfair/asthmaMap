library(shiny)
library(leaflet)
library(rgdal)
library(rgeos)
library(dplyr)
library(RColorBrewer)
library(ggplot2)

asthmaDf <- read.csv('fakeAsthmaData.csv')

# read shapefile
uhfShapeFile <- readOGR('./uhfShapes/UHF42BaseMap', 'shp')
uhfs <- spTransform(uhfShapeFile, CRS("+proj=longlat +datum=WGS84 +no_defs"))

subsetter <- function(anySelect, severitySelect, sexSelect, ageSelect, raceSelect){
  df <- asthmaDf
  df <- filter(df, sex %in% sexSelect, age %in% ageSelect, race %in% raceSelect)
  if(anySelect=='Any'){
    df <- filter(df, asthma=='Any')
  } else{
    df <- filter(df, asthma %in% severitySelect)
  }
  dens <- setNames(aggregate(den ~ UHF, df, sum), c('UHF', 'den'))
  nums <- setNames(aggregate(num ~ UHF, df, sum), c('UHF', 'num'))
  uhfData <- merge(merge(uhfs, nums, by='UHF'), dens, by='UHF')
  uhfData$rate <- uhfData$num / uhfData$den
  return(uhfData)
}

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width='80%', height='70%'),
  plotOutput('plot', width='70%', height='30%'),
  absolutePanel(top=10, right=-120,
                selectInput("measure", "Color by:",
                            c('num', 'den', 'rate'), 'rate',
                            width='50%'
                ),
                checkboxGroupInput("sex", "Sex",
                                   unique(asthmaDf$sex), unique(asthmaDf$sex)
                ),
                checkboxGroupInput("age", "Age",
                                   unique(asthmaDf$age), unique(asthmaDf$age)
                ),
                checkboxGroupInput("race", "Race",
                                   unique(asthmaDf$race), unique(asthmaDf$race)
                ),
                radioButtons('any', 'Asthma Dx',
                             choices=c('Any', 'by Severity')),
                checkboxGroupInput("severity", "Asthma Severity",
                                   unique(asthmaDf[asthmaDf$asthma!='Any', 'asthma']),
                                   unique(asthmaDf[asthmaDf$asthma!='Any', 'asthma'])
                )
  )

)

server <- function(input, output, session) {

  subDf <- reactive({
    validate(
      need(input$sex, 'Select at least one sex category.'),
      need(input$age, 'Select at least one age category.'),
      need(input$race, 'Select at least one race category.')
    )
    if(input$any=='by Severity'){
      validate(
        need(input$severity, 'Select at least one severity category, or select "Any".')
      )      
    }

    subsetter(input$any, input$severity, input$sex, input$age, input$race)
  })
    
  colorPal <- reactive({
    colorQuantile('YlOrRd', domain=subDf()$input$measure)
  })

  labels <- reactive({
    sprintf(
      "<strong>%s</strong><br/>num: %d<br/>den: %d<br/>rate: %f",
      subDf()$UHFNAME, subDf()$num, subDf()$den, subDf()$rate
    ) %>% lapply(htmltools::HTML)
  })
  
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedShape=NULL)
  
  output$mymap <- renderLeaflet({
    leaflet(subDf()) %>%
      setView(-73.9, 40.7, zoom=11) %>%
      addTiles()
      # addProviderTiles("MapBox", options = providerTileOptions(
      #   id = "mapbox.light",
      #   accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
  })
  
  observe({

    pal <- colorPal()
    leafletProxy('mymap', data=subDf()) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(get(input$measure)),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        label = labels(),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        layerId=~UHF
      )
  })
  
  # store the click
  observeEvent(input$mymap_shape_click,{
    data_of_click$clickedShape <- input$mymap_shape_click
  })
  
  output$plot=renderPlot({
    selectUHF=data_of_click$clickedShape$id
    row=subset(subDf(), UHF==selectUHF)
    rowRate <- row$num/row$den
    totNum <- sum(subDf()$num, na.rm=TRUE)
    totDen <- sum(subDf()$den, na.rm=TRUE)
    totRate <- totNum/totDen
    # plotData <- data.frame(geography=c(as.character(row$UHFNAME), 'NYC - overall'),
    #                        rate=c(rowRate, totRate))
    plotData <- data.frame(geography=factor(c(as.character(row$UHFNAME), 'NYC - overall'), 
                                            levels=c(as.character(row$UHFNAME), 'NYC - overall')),
                           rate=c(rowRate, totRate))
    ggplot(data=plotData, aes(x=geography, y=rate)) + 
      geom_bar(stat='identity') +
      geom_text(aes(label=round(rate, digits=2)), vjust=-0.3, size=3.5) +
      theme_minimal()
    # myPlot <- barplot(c(rowRate, totRate), 
    #         names.arg=c(as.character(row$UHFNAME), 'NYC - overall'),
    #         ylim=c(0,0.25),
    #         ylab='asthma rate',
    #         col='steelblue1')
    # text(x = myPlot, y = c(rowRate, totRate), label = c(round(rowRate, digits=2), round(totRate, digits=2)), pos = 3, cex = 0.8, col = "red")
  })
}

shinyApp(ui, server)
