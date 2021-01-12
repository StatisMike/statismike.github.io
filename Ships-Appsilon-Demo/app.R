#### loading libraries and data ####

library(shiny)
library(shinybusy)
library(shiny.semantic)
library(semantic.dashboard)
library(geosphere)
library(leaflet)
library(tidyverse)
library(lubridate)
library(DT)
library(shinyjs)


# DATA CLEANING
# outside of this app I've cleaned the data: it seemed like there were some
# incorrect SHIPNAMES (. PRINCE OF WAVES, .WLA-311, [SAT-AIS]).
# There are also some ships with numbers as SHIPNAMEs, but their SHIP_id
# doesn't seem to align with any other ship with legit-looking names.
# I've decided to keep their data in the app.
# # # # # # # # # # # # # # # # # # # # # # 
# # DATA PREPARATION - firstly I intended to do it inside shinyapp. Unfortunately,
# # it gave me "code 137, signal 9 (SIGKILL)" error in shinyapps.io, most possibly
# # due to too much memory usage or too big database. Below are the steps
# # I took to prepare the data - saved it as "data_joined.csv" then, and the app
# # reads only this file
# # # # # # # # # # # # # # # # # # # # # # 
# # I need to first bind the start and end localization of each vessel in each trip
# # I presumed that each trip starts with first observation when the is_parked == 0
# # and each trip ends with first observation when the is_parked == 1
# 
# data_filtered <- read.csv("data_checked.csv") %>%
#     mutate(startend = if_else(
#         is_parked != lag(is_parked, 1), #lag() lets check if the variable is changed
#         1, 
#         0)) %>%
#     filter(startend %in% c(NA, 1))
# 
# # create sail_id to identify each trip
# 
# data_filtered["sail_id"] <- rep(
#     seq(
#         from = 1, 
#         to = nrow(data_filtered) / 2, # each trip consists of start and end localization
#         by = 1), 
#     each = 2)
# 
# # left join to create a df where each observation is one trip
# # selecting only necessary variables: location, datetime (for arranging by most recent),
# # sail_id for joining, ship_type and SHIPNAME for filtering
# 
# data_joined <- left_join(
#     select(filter(data_filtered, is_parked == 0), LAT, LON, DATETIME, sail_id, ship_type, SHIPNAME),
#     select(filter(data_filtered, is_parked == 1), LAT, LON, DATETIME, sail_id, ship_type, SHIPNAME),
#     suffix = c("_start", "_end"),
#     by = c("sail_id", "ship_type", "SHIPNAME"))
# 
# write.csv(data_joined, "data_joined.csv")

data_joined <- read.csv("data_joined.csv")

#### custom function for distance calculations ####
# for calculating distance between two points I've used distGeo (faster) and 
# distVincentyEllipsoid (more accurate) from geosphere package.
# It seems though the calculated distances are the same, though
# I will keep that functionality as it may be beneficial in future
# It computes both the trip with longest distance and alll trips

distances <- function(data, vessel, method){
    
    require(geosphere)
    require(tidyverse)
    
    data <- filter(data, SHIPNAME == vessel)
    
    if(method == "fast"){
        
        data$distance <- distGeo(
            p1 = tibble(longitude = data$LON_start, latitude = data$LAT_start),
            p2 = tibble(longitude = data$LON_end, latitude = data$LAT_end)
        )
        
    } else {
        
        data$distance <- distVincentyEllipsoid(
            p1 = tibble(longitude = data$LON_start, latitude = data$LAT_start),
            p2 = tibble(longitude = data$LON_end, latitude = data$LAT_end)
        )
    }
    
    
    longest <- data %>%
        arrange(desc(distance, DATETIME_start)) %>%
        slice_head()
    
    
    all <- data %>%
        arrange(desc(distance, DATETIME_start)) %>%
        mutate(dur_temp = as_datetime(DATETIME_end) - as_datetime(DATETIME_start),
               duration = as.character(as.period(dur_temp)),
               duration_s = as.numeric(dur_temp, units = "secs"),
               mean_speed = (distance/1000)/as.numeric(dur_temp, units = "hours")
        )
        
    list("longest" = longest, 
         "all" = all)
}

#### creating module UI and server ####

# UI module with 4 inputs:
# 1. the one to choose vessel type (defaults to any)
# 2. the one to choose vessel name
# 3. for choosing computation method (look above at function distances())
# 4. is an action button to calculate (I find isolating inputs for rendering
#    as a good practice personally)

vessel_choose <- function(id) {

    ns <- NS(id)

    tagList(
    
        selectInput(NS(id,"vessel_type"),
                    "Choose a vessel type to search for:",
                    choices = c("any", unique(data_joined$ship_type)),
                    multiple = FALSE),

        selectInput(NS(id, "vessel_name"),
                    "Choose a vessel for visualization:",
                    choices = sort(unique(data_joined$SHIPNAME)),
                    multiple = FALSE),
        
        selectInput(NS(id, "dist_calc"),
                    "Choose a distance calculation method",
                    c("Fast (default)" = "fast",
                      "Accurate" = "acc"),
                    selected = c("Fast (default)" = "fast")),
        
        actionButton(NS(id, "search_ves"), "Search for vessel")
    )
}

# module server for updating input ship_name
# also calculates distance and everything needed for leaflet creation

select_server <- function(id){
    
    moduleServer(
        id,
        function(input, output, session){
            
    # updated input choices
            
        observe({
          
          req(!is.null(input$vessel_type))
            
            vessel_type <- input$vessel_type
            
            if(input$vessel_type == "any"){
                
                updateSelectInput(session, "vessel_name",
                                  label = "Choose a vessel for visualization",
                                  choices = sort(unique(data_joined$SHIPNAME)))
                
            } else {
            
            updateSelectInput(session, "vessel_name",
                              label = "Choose a vessel for visualization",
                              choices = sort(unique(data_joined[data_joined$ship_type == input$vessel_type,]$SHIPNAME)))
            }
                
                })
            
            
        # after button push the parameters() reactive is returned to main app
            
            parameters <- eventReactive(input$search_ves, {
                
                distances(data = data_joined,
                          vessel = input$vessel_name,
                          method = input$dist_calc) })
            
            return(parameters)
            
        }
    )
}



#### UI code ####

ui <- semanticPage(
    useShinyjs(),
    sidebar_layout(
    
    # sidebar definition
    
    sidebar_panel(
        h1("Ship trips search engine"),
        div(class = "ui raised segment",
            div(p("There you can find some routes the ships have taken.",
                  "The trip showed will be of longest distance. If two trips made by that ship was the same, the most recent one will be showed."),
                p("To use the application as it is meant to be used, take this three steps:",
                tags$ol(
                    tags$li("Choose type of the vessel that you want to find"),
                    tags$li("Choose name of the vessel of that type"),
                    tags$li("Choose the distance calculation method (optional)"),
                    tags$li("Click the button to make all calculations")
                )),
                p("On the panel of the right the map indicating route the ship has taken should appear.",
                  "Additionally, below the map there should be some more information about that trip.")
                  )),
        div(class = "ui horizontal divider",
            "Inputs"),
        
        vessel_choose(id = "ships")
            
        
    ),
    main_panel(
        
        add_busy_bar(),
        
        leafletOutput("sail_leaflet"),
        
        uiOutput("infoboxes")
        

            )))
    
#### server code #####

server <- shinyServer(function(input, output, session) {
    
    # getting the values from modules
    
    parameters <- select_server("ships")
    
    observe({
        
        parameters()
        shinyjs::runjs("window.scrollTo(0, 0)")
        
    })
    
    # creating leaflet
    
    output$sail_leaflet <- renderLeaflet({
        
        data <- parameters()$longest
        
        leaflet() %>%
            addTiles %>%
            addMarkers(lng = data$LON_start,
                       lat = data$LAT_start,
                       label = "Start") %>%
            addMarkers(lng = data$LON_end,
                       lat = data$LAT_end,
                       label = "End") %>%
            addPolylines(lng = c(data$LON_start,
                                 data$LON_end),
                         lat = c(data$LAT_start,
                                 data$LAT_end))
        
    })
    
    # creating additional values - for display under the leaflet
        
    additional <- reactive({
        
        data <- parameters()
        
        # time_diff will be needed multiple times
        
        time_diff <- as_datetime(data$longest$DATETIME_end) - as_datetime(data$longest$DATETIME_start)
        
    list("data" = data,
        
        # info about start of trip
        
        "trip_start" = list("lat" = data$longest$LAT_start,
                           "lon" = data$longest$LON_start,
                           "time" = as_datetime(data$longest$DATETIME_start)),
        
        #info about end of trip
        
        "trip_end" = list("lat" = data$longest$LAT_end,
                         "lon" = data$longest$LON_end,
                         "time" = as_datetime(data$longest$DATETIME_end)),
        
        #info about distance in time and space
        
        "dist" = list("distance" = paste(
                                as.character(round(data$longest$distance, 2)), 
                                "m", sep = " "),
                     "time" = as.character(as.period(time_diff)),
                     "mean_speed" = paste(
                                as.character(round((data$longest$distance/1000)/as.numeric(time_diff, units = "hours"), 2)),
                                "km/h")),
        
        # numbers of all trips for this ship
        
        "all_trips" = nrow(data$all)
        
    )
    
    })
    
    # infoboxes UI contains everything under the leaflet
    # rendering in renderUI to optimize
    
output$infoboxes <- renderUI({
    
        # list of elements that should render always
    
    always_UI <- list(
        
    div(class = "ui horizontal divider", "Longest vessel trip info"),
        
        # box with details about start point: Latitude, Longitude and time of beginning
        
    box(title = "Start point info",
    div(class = "ui one row stackable grid layout",
        div(class = "three column row",
            div(class = "column",
                infoBox(subtitle = "Latitude",
                        value = additional()$trip_start$lat,
                        size = "tiny")),
            div(class = "column",
                infoBox(subtitle = "Longitude",
                        value = additional()$trip_start$lon,
                        size = "tiny")),
            div(class = "column",
                infoBox(subtitle = "Time of start",
                        value = additional()$trip_start$time,
                        size = "tiny")),
        ))),

        # box with detail about end point: same as above
    
    box(title = "End point info",
    div(class = "ui one row stackable grid layout",
        div(class = "three column row",
            div(class = "column",
                infoBox(subtitle = "Latitude",
                        value = additional()$trip_end$lat,
                        size = "tiny")),
            div(class = "column",
                infoBox(subtitle = "Longitude",
                        value = additional()$trip_end$lon,
                        size = "tiny")),
            div(class = "column",
                infoBox(subtitle = "Time of start",
                        value = additional()$trip_end$time,
                        size = "tiny")),
        ))),

        # box with info about trip: Distance, Duration and Mean speed in km/h
    
    box(title = "Trip info",
    div(class = "ui one row stackable grid layout",
        div(class = "three column row",
            div(class = "column",
                infoBox(subtitle = "Distance",
                        value = additional()$dist$distance,
                        size = "tiny")),
            div(class = "column",
                infoBox(subtitle = "Duration",
                        value = additional()$dist$time,
                        size = "tiny")),
            div(class = "column",
                infoBox(subtitle = "Mean speed",
                        value = additional()$dist$mean_speed,
                        size = "tiny")),
        ))),
    div(class = "ui horizontal divider", "All trips of vessel info"),
    infoBox(subtitle = "Number of trips in database",
            value = additional()$all_trips),
    
    # datatable of all trips by this vessel
    
    box(style = "overflow-y: scroll;overflow-x: scroll;", title = "Datatable",
    
    renderDT({
        
        parameters()$all %>%
            arrange(DATETIME_start) %>%
            
            select("Distance sailed" = distance,
                   "Duration of trip" = duration,
                   "Duration in seconds" = duration_s,
                   "Mean speed" = mean_speed,
                   "Time of start" = DATETIME_start,
                   "Start latitude" = LAT_start,
                   "Start longitude" = LON_start,
                   "Time of end" = DATETIME_end,
                   "End latitude" = LAT_end,
                   "End longitude" = LON_end)

        }),
    
    downloadButton("DTdownload", "Download ship trips database")
    
    )
    
    )
    
    # histogram of distances - different list, at it should be generated
    # conditionally

    histogram <- list(
        
        box(title = "Histogram",
        
        renderPlot({
        
        parameters()$all %>%
            ggplot(aes(distance)) + 
            geom_histogram() + 
            theme_minimal() + 
            xlab("Distance in meters") +
            ylab("Count of trips") + 
            ggtitle("Distances of vessel trips in database")

            })
    
    ))
    
    # conditional: histogram should render only if number of all trips
    # by that vessel is bigger than one: histogram for one value
    # is redundant
    
    if(additional()$all_trips == 1){
        
        always_UI
        
    } else {
        
        list(always_UI,
             histogram)
    }
    
})

    # download handler for .csv of vessel trips

output$DTdownload <- downloadHandler(
    filename = function(){
        paste0(parameters()$all$SHIPNAME[1],
               "sail_data.csv")},
    content = function(file){
        write.csv(select(parameters()$all,
                         SHIPNAME,
                         sail_id,
                         ship_type,
                         LAT_start,
                         LON_start,
                         DATETIME_start,
                         LAT_end,
                         LON_end,
                         DATETIME_end,
                         distance,
                         duration_s,
                         mean_speed), file)
    }
    
    
    
)

    
})

# Run the application 
shinyApp(ui = ui, server = server)
