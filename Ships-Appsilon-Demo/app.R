#### loading libraries and data ####

library(shiny)
library(shiny.semantic)
library(geosphere)
library(leaflet)
library(tidyverse)
library(lubridate)
library(shinybusy)

# # DATA CLEANING
# # outside of this app I've cleaned the data: it seemed like there were some
# # incorrect SHIPNAMES (". PRINCE OF WAVES", ".WLA-311", "[SAT-AIS]").
# # There are also some ships with numbers as SHIPNAMEs, but their SHIP_id
# # doesn't seem to align with any other ship with legit-looking names.
# # I've decided to keep their data in the app.
# #
# # DATA PREPARATION
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
        arrange(desc(distance, DATETIME_start))
        
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
            
            vessel_type <- input$vessel_type
            
            req(!is.null(input$vessel_type))
            
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

ui <- semanticPage(shiny.semantic::sidebar_layout(
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
    main_panel(add_busy_bar(),
        
        div(class = "ui horizontal divider",
            "Mapped beginning and end of longest trip"),
        
        leafletOutput("sail_leaflet"),
        
        div(class = "ui two column centered grid",
        
        div(class = "column",  
        
        div(class = "ui horizontal divider",
            "Additional informations about longest trip"),
        
        flow_layout(
            div(class = "ui raised segment",
                htmlOutput("start")),
            div(class = "ui raised segment",
                htmlOutput("end")),
            div(class = "ui raised segment",
                htmlOutput("duration")))),
        
        div(class = "column",
        
        div(class = "ui horizontal divider",
            "Additional informations about trips in database"),
        flow_layout(
            div(class = "ui raised segment",
                htmlOutput("n_trips")),
            div(class = "ui fluid container",
                plotOutput("all_trips_hist")))
            ))))
        
    
    )
    
#### server code #####

server <- shinyServer(function(input, output, session) {
    
    # getting the values from modules
    
    parameters <- select_server("ships")
    
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
                       label = "End")
        
    })
    
    # creating additional values
        
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
                                "meters", sep = " "),
                     "time" = as.character(as.period(time_diff)),
                     "mean_speed" = paste(
                                as.character(round((data$longest$distance/1000)/as.numeric(time_diff, units = "hours"), 2)),
                                "kilometers per hour")),
        
        # numbers of all trips for this ship
        
        "all_trips" = paste("This vessel made",
            as.character(nrow(data$all)),
            "trips recorded in database", sep = " ")
        
    )
    
    })
    
    output$all_trips_hist <- renderPlot({
        
        parameters()$all %>%
            ggplot(aes(distance)) + 
            geom_histogram() + 
            theme_minimal() + 
            xlab("Distance in meters") +
            ylab("Count of trips")
        
    })
    
    output$start <- renderText({
        
        paste("<h3>Time started:</h3>",
              "<p>", additional()$trip_start$time, "</p>",
              "<h3>Location of start:</h3>",
              "<p>Longitude:",additional()$trip_start$lon,"<br>",
              "Latitude:", additional()$trip_start$lat, "</p>")
         })
    
    output$end <- renderText({
        
        paste("<h3>Time ended:</h3>",
              "<p>", additional()$trip_end$time, "</p>",
              "<h3>Location of end:</h3>",
              "<p>Longitude:",additional()$trip_end$lon,"<br>",
              "Latitude:", additional()$trip_end$lat, "</p>")
    })
    
    output$duration <- renderText({
        
        paste("<h3>Distance</h3><p>",
              additional()$dist$distance,"</p>",
              "<h3>Time of trip</h3><p>",
              additional()$dist$time,"</p>",
              "<h3>Mean speed</h3><p>",
              additional()$dist$mean_speed, "</p>")
    })
    
    output$n_trips <- renderText({
        
        additional()$all_trips
        
    })
    
})

# Run the application 
shinyApp(ui = ui, server = server)
