library(shiny)
library(tidyverse)
library(bs4Dash)
library(leaflet)
library(leaflet.extras)
library(glue)
library(sf)
library(plotly)
library(shinycssloaders)

spinner_colour <- "#FE8A02"
spinner_type = "circle"

lsoa_sampled <- read_rds('lsoa_sampled.RDS')
msoa_sampled <- read_rds('msoa_sampled.RDS')
msoa_correlations <- read_rds('msoa_correlations.RDS')

#-- UI ---------------------------------------------------------------------
ui <- bs4DashPage(bs4DashBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "hilltop-bs4dash.css"),
    tags$script(
      "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
                  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
                  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
                  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

                  ga('create', 'UA-59111745-1', 'auto');
                  ga('send', 'pageview');"
    )
  ),
  tags$style(HTML(".content-wrapper {margin-left: 0px}")),
  #hide the left sidebar space
  
  bs4TabItems(bs4TabItem(
    hr(),
                  fluidRow(column(
                    12,
                    
                    bs4Card(
                      width = 12,
                      collapsible	= FALSE,
                      closable = FALSE,
                      maximizable = TRUE,
                      title = textOutput('mapTitle'),
                      height = '85vh',
                      enable_sidebar = TRUE,
                      sidebar_start_open = TRUE,
                      sidebar_content = fluidRow(
                        column(
                          12,
                          h4('Select Week Ending Date'),
                          fluidRow(
                            column(2,
                                   uiOutput('prevDate')),
                            column(8,
                                   uiOutput('datePicker', width = 12)),
                            column(2,
                                   uiOutput('nextDate'))
                          ),
                          radioButtons(
                            'uiViewLevel',
                            'Area Size',
                            choices = list(
                              'LSOA (best zoom, less up to date, shows fewer cases)' = 'lsoa',
                              'MSOA (less zoom, more up to date, shows more cases)' = 'msoa'
                            ),
                            selected = 'msoa'
                          ),
                          actionButton('uiShowHistory', 'Show Case History for Highlighted Area'),
                          br(),
                          br(),
                          actionButton('uiShowCorrelations', 'Show Top Demographic Correlations for Selected Date'),
                          br(),
                          br(),
                          h4('Map Scale Controls'),
                          p(
                            "Use these sliders to control the size of the
                                  map blobs and the number of cases within a
                                  blob that will display as maximum 'heat'."
                          ),
                          p(
                            "Changing settings often helps when you zoom
                                and cause the map blobs cover a different sized area.
                                  You're likely to want smaller circles and a
                                  higher max as you zoom out."
                          ),
                          fluidRow(uiOutput('maxValue')),
                          fluidRow(uiOutput('circleRadius')),
                          br(),
                          p(
                            "Source: ",
                            tags$a(href = "https://coronavirus.data.gov.uk/about-data#cases-by-lower-super-output-area-lsoa", "UK Government Lower Super Output Area Statistics"),
                            br(),
                            "Note that since the UK Gov source data sets areas with only 1-2 cases to NULL,
                               an area will only display as heat if it had >2 cases within a week. This means that LSOA (the most detailed level) shows fewer cases overall.",
                            br(),
                            "To distribute cases, the map takes each individual recorded case and randomly assigns it an exact location within its reported area."
                          )
                        )
                      ),
                      dropdownIcon = "wrench",
                      leafletOutput('heatmap', height = '100%', width = '100%')
                    ),
                  )
    )
  )),
  bs4DashFooter(p(
    tags$a(href = "https://twitter.com/neilcharles_uk", "@neilcharles_uk")
  )),
  
))

#-- Server ---------------------------------------------------------------------
server <- function(input, output, session) {

  case_points <- reactive({
    if(input$uiViewLevel=='lsoa') lsoa_sampled else msoa_sampled
  })
  
  date_range <- reactive({
    case_points() %>%
      group_by(week_ending) %>%
      summarise() %>%
      arrange(week_ending) %>%
      pull(week_ending)
  })  
  
  # Base map -------------------------------------------------------------------
  output$heatmap <- renderLeaflet({
    map <- leaflet() %>%
      addDrawToolbar(
        targetGroup = 'draw',
        polylineOptions = FALSE,
        polygonOptions = FALSE,
        circleOptions = FALSE,
        rectangleOptions = drawRectangleOptions(),
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(edit = FALSE, remove = TRUE),
        singleFeature = TRUE
      ) %>%
      setView(lat = 53.6,
              lng = -1.98,
              zoom = 8)
    
    if(input$uiViewLevel == 'lsoa'){
      map <- map %>% 
        addProviderTiles(
          provider = providers$CartoDB.Positron,
          options = providerTileOptions(minZoom = 6, maxZoom = 12)
        )
    } else {
      map <- map %>% 
        addProviderTiles(
        provider = providers$CartoDB.Positron,
        options = providerTileOptions(minZoom = 6, maxZoom = 11)
      )
    }
    
    map
        
  })
  
  # Drawn area -----------------------------------------------------------------
  map_rectangle <- reactiveValues()
  
  drawn_rectangle <- observe({
    
    req(input$heatmap_draw_new_feature)
    
    #Return rectangle as sf polygon
    rect <- as_tibble(matrix(
      unlist(input$heatmap_draw_new_feature$geometry$coordinates[[1]]),
      ncol = 2,
      byrow = TRUE,
      dimnames = list(NULL, c('lon', 'lat'))
    )) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      summarise(geometry = st_combine(geometry)) %>%
      st_cast("POLYGON")
    
    map_rectangle$rect <- rect
  })
  
  deleted_rectangle <- observe({

    input$heatmap_draw_deleted_features
    map_rectangle$rect <- NULL
  })
    
  
  points_in_rectangle <- reactive({

    validate(need(!is.null(map_rectangle$rect),
                  'Use the map controls to draw an area'))
    
    calc_within <- case_points() %>% 
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      st_within(map_rectangle$rect)
    
    points_filtered <- case_points()
    
    points_filtered$in_drawn_area <- as.integer(calc_within)
    
    points_filtered %>% 
      filter(in_drawn_area==1)
  })
  
  # Modal history chart --------------------------------------------------------
  observeEvent(input$uiShowHistory, {
      showModal(
        modalDialog(size = 'l',
          title = "Cases by Week Ending Date",
            withSpinner(plotlyOutput('chart_history'), color = spinner_colour),
          footer = p('Weekly case numbers are an estimate because UK government
                     statistics only quote case numbers for LSOA and MSOA
                     geographic areas that have >2 cases in a week'),
          easyClose = TRUE
      ))
  })
  
  output$chart_history <- renderPlotly({

    points_in_rectangle() %>% 
      group_by(week_ending) %>% 
      summarise(cases_count = n()) %>% 
      plot_ly() %>% 
      add_bars(x = ~week_ending, y = ~cases_count, marker = list(color = "#D87B0D")) %>% 
      layout(
        yaxis = list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE
      ),
      xaxis = list(
        title = "Week Ending",
        showgrid = FALSE
      ),
      margin = list(l = 0, r = 0))
  })

  # Modal Demographic Correlations ---------------------------------------------
  observeEvent(input$uiShowCorrelations, {
    showModal(
      modalDialog(size = 'l',
        title = glue("Top Demographic Correlations with Cases {input$uiDatePicker}"),
        withSpinner(plotlyOutput('chart_correlations'), color = spinner_colour),
        footer = p('MSOA level Correlation between cases % of population and 2011 UK Census Key Statistics https://www.nomisweb.co.uk/census/2011/bulk/r2_2'),
        easyClose = TRUE
      ))
  })
  
  output$chart_correlations <- renderPlotly({
    
    chart_data <- msoa_correlations %>% 
      filter(week_ending==input$uiDatePicker) %>% 
      select(week_ending, correlations) %>% 
      unnest(cols = c(correlations))
    
    chart_data %>% 
      arrange(correlation) %>% 
      top_n(20, correlation) %>% 
      mutate(label = fct_inorder(label)) %>% 
      plot_ly() %>% 
        add_bars(y = ~label, x = ~correlation, marker = list(color = "#D87B0D"), orientation = 'h') %>% 
        layout(
          xaxis = list(
            title = "",
            zeroline = FALSE,
            showline = FALSE,
            showticklabels = FALSE,
            showgrid = FALSE
          ),
          yaxis = list(
            title = "",
            showgrid = FALSE
          ),
          margin = list(l = 600, r = 0))
  })
  
    
  # Title ----------------------------------------------------------------------
  output$mapTitle <- renderText({
    req(input$uiDatePicker)
    glue('Week Ending {format(as.Date(input$uiDatePicker), "%d %b %Y")}')
  })
  
  # Heatmap layer --------------------------------------------------------------
  observe({
    req(input$uiDatePicker)
    
    leafletProxy('heatmap',
                 data = filter(case_points(), week_ending == input$uiDatePicker)) %>%
      removeHeatmap('heatmap') %>%
      addHeatmap(
        layerId = 'heatmap',
        lng = ~ lon,
        lat = ~ lat,
        minOpacity = 0.25,
        max = input$uiMaxValue,
        radius = input$uiCircleRadius
      )
  })
  
  # Controls -------------------------------------------------------------------
  output$datePicker <- renderUI({
    selectInput("uiDatePicker", NULL, date_range(), max(date_range()))
  })
  
  output$nextDate <- renderUI({
    actionButton('uiNextDate', 'Next')
  })
  
  output$prevDate <- renderUI({
    actionButton('uiPrevDate', 'Prev')
  })
  
  #Iterate dates
  observeEvent(input$uiNextDate, {
    current <- which(date_range() == input$uiDatePicker)
    if (current < length(date_range())) {
      updateSelectInput(session,
                        "uiDatePicker",
                        choices = date_range(),
                        selected = date_range()[current + 1])
    }
  })
  
  observeEvent(input$uiPrevDate, {
    current <- which(date_range() == input$uiDatePicker)
    if (current > 1) {
      updateSelectInput(session,
                        "uiDatePicker",
                        choices = date_range(),
                        selected = date_range()[current - 1])
    }
  })
  
  #Leaflet format controls
  output$circleRadius <- renderUI({
    sliderInput(
      'uiCircleRadius',
      'Circle Radius',
      min = 10,
      max = 30,
      value = 20
    )
  })
  
  output$maxValue <- renderUI({
    sliderInput(
      'uiMaxValue',
      'Scale Max (Number of cases to display as red)',
      min = 1,
      max = 250,
      value = 100
    )
  })
  
}

#-- Run ------------------------------------------------------------------------
shinyApp(ui = ui,
         server = server,
         enableBookmarking = "url")
