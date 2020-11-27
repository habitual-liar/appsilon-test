rm(list=ls())

# setwd("D:\\Projects\\Appsilon\\shinyapp")

library(tidyverse)
library(data.table)
library(anytime)
library(janitor)
library(shiny)
library(gmt)
library(leaflet)
library(reactable)
library(bit64)

## Read and process raw data ----
# ships <- fread("./data/ships.csv") %>%
#     clean_names() %>% 
#     # Create date_time from column
#     mutate(datetime = anytime(datetime)) %>%
#     # Arrange by datetime
#     arrange(ship_type, shipname, datetime) %>%
#     # COnvert to standard eglish characters
#     mutate(port = iconv(port_2, from="UTF-8",to="ASCII//TRANSLIT")) %>%
#     # Remove unncessary columns
#     select(-ship_id, -shiptype, -date, -port_2, -week_nb) %>%
#     # Create row index w.r.t to each ship
#     group_by(ship_type, shipname) %>% 
#     mutate(row_index = 1:n()) %>%
#     ungroup() %>% 
#     relocate(row_index) %>%
#     group_by(ship_type, shipname) %>% 
#     rename(datetime_current = "datetime") %>% 
#     mutate(datetime_from = lag(datetime_current)) %>% 
#     ungroup() %>% 
#     relocate(datetime_from, .before = "datetime_current") %>% 
#     # Rename lat and lon
#     rename(lat_current = "lat") %>% 
#     rename(lon_current = "lon") %>% 
#     # Add previous coordinates as separate columns
#     group_by(ship_type, shipname) %>%
#     mutate(lat_from = lag(lat_current)) %>% 
#     mutate(lon_from = lag(lon_current)) %>% 
#     ungroup() %>% 
#     relocate(lat_from, .before = "lat_current") %>% 
#     relocate(lon_from, .after = "lat_from") %>%
#     mutate(lat_from = if_else(is.na(lat_from), lat_current, lat_from)) %>% 
#     mutate(lon_from = if_else(is.na(lon_from), lon_current, lon_from)) %>% 
#     # # Add next coordinates as separate columns
#     group_by(ship_type, shipname) %>%
#     mutate(lat_to = lead(lat_current)) %>%
#     mutate(lon_to = lead(lon_current)) %>%
#     ungroup() %>%
#     relocate(lat_to, .after = "lon_current") %>%
#     relocate(lon_to, .after = "lat_to") %>%
#     mutate(lat_to = if_else(is.na(lat_to), lat_current, lat_to)) %>% 
#     mutate(lon_to = if_else(is.na(lon_to), lon_current, lon_to)) %>% 
#     # Calculate geo distances
#     mutate(dist_covered = geodist(lat_from, lon_from, lat_current, lon_current, units="km") * 1000) %>% 
#     mutate(dist_covered = round(dist_covered, 2)) %>% 
#     mutate(dist_to_be_covered = geodist(lat_current, lon_current, lat_to, lon_to, units="km") * 1000) %>%
#     mutate(dist_to_be_covered = as.numeric(dist_to_be_covered)) %>%
#     relocate(dist_covered, .after = "lon_current") %>%
#     relocate(dist_to_be_covered, .after = "dist_covered") %>%
#     # Check if coordinates remained the same
#     mutate(is_moving = if_else(lat_current == lat_to & lon_current == lon_to, "Not Moving", "Moving")) %>% 
#     relocate(is_moving, .after = "dist_covered") %>%
#     # Mutate parked to yes and no
#     mutate(is_parked = if_else(is_parked == 0, "No", "Yes")) %>% 
#     # Filter rows where ship is not moving and is not parked
#     filter(is_moving == "Moving") %>% 
#     filter(is_parked == "No") %>% 
#     # remove unnecessary columns 
#     select(-lat_to, -lon_to, -dist_to_be_covered, -dwt)
#     
#     
# write.csv(ships, "./data/ships_processed.csv", row.names = F)
    
## Load processed ships data ----
ships <- fread("./data/ships_processed.csv")

## Get unique ship types ----
ship_types <- ships %>% 
    select(ship_type) %>% 
    distinct() %>% 
    pull(ship_type)

## Get unique ship name against each ship ----
ship_names_df <- ships %>% 
    select(ship_type, shipname) %>% 
    distinct()


## Server Functions ----
server <- function(session, input, output) {
    
    # Dropdown to select ship type
    output$ship_type_selector <- renderText({
        ship_types <-  paste0('<option>', ship_types, '</option>')
        ship_types <-  paste(ship_types, collapse = "")
        
        ship_types <- paste0(
            '<div class="form-group">
             <label for="ship_type_selector"><strong>Ship Type</strong></label>
             <select class="form-control" id="ship_type_selector" name="ship_type_selector">',
            ship_types,
            '</select>
             </div>')
        
        return (ship_types)
        
    })
    
    # Dropdown to select ship by name
    output$ship_name_selector <- renderText({
        
        selected_ship_type <- input$ship_type_selector
        
        if(is.null(selected_ship_type)) {
            return("")
        }
        
        ship_names <- ship_names_df %>% 
            filter(ship_type == selected_ship_type) %>% 
            select(shipname) %>%
            pull(shipname)
        
        ship_names <-  paste0('<option>', ship_names, '</option>')
        ship_names <-  paste(ship_names, collapse = "")
        
        ship_names <- paste0(
            '<div class="form-group">
             <label for="ship_name_selector"><strong>Ship Name</strong></label>
             <select class="form-control" id="ship_name_selector" name="ship_name_selector">',
            #'<option class="bg-secondary">All</option>',
            ship_names,
            '</select>
              </div>')
        
        return (ship_names)
        
    })
    
    # Plot longest distance on map
    output$mymap <- renderLeaflet({
        
        # selected_ship_type <- "Tanker"
        # selected_ship_name <- "AALBORG"
        
        # selected_ship_type <- "Pleasure"
        # selected_ship_name <- "BMJ WINDY 37"
        
        # selected_ship_type <- "Cargo"
        # selected_ship_name <- "ADAMAS"
        
        selected_ship_type <- input$ship_type_selector
        selected_ship_name <- input$ship_name_selector
        
        # Return blank is ship name of type is empty
        if(length(selected_ship_type) < 1 | 
           length(selected_ship_name) < 1 ) {
            return(leaflet())
        }
        
        # Filter selected ship name and type
        d <- ships %>% 
            # Filter selected ship name and type
            filter(shipname == selected_ship_name) %>%
            filter(ship_type == selected_ship_type) %>%
            slice(which.max(dist_covered))
        
        # Return blank is no ship is found with selected name and type
        if(nrow(d) != 1) {
            return (leaflet())
        }
        
        
        # Popup labels
        distance_covered_str1 <- paste("<strong>Distance Covered:</strong> ", round(d$dist_covered[1]), " meters", sep = "")
        time_taken <- paste("<strong>Time Taken:</strong> ", round(difftime(d$datetime_current[1], d$datetime_from[1]), 2), "hours")
        info_string <- paste(distance_covered_str1, "<br>", time_taken, "<br>",
                             "<strong>End Date & Time:</strong> ", d$datetime_current[1])
        distance_start_string <- paste("<strong>Start Date & Time:</strong> <br>", d$datetime_from[1])
        
        
        l <- leaflet() %>% 
            addTiles() %>%
            # Marker for starting destinations
            addCircleMarkers(lng = d$lon_from[1], lat=d$lat_from[1],
                             radius = 2, color = "blue", opacity = 1, popup = distance_start_string) %>%
            # Popup for starting destinations
            addPopups(lng = d$lon_from[1], lat=d$lat_from[1], popup = distance_start_string) %>%
            # Marker for ending destinations
            addMarkers(lng = d$lon_current[1], lat=d$lat_current[1],
                       popup = info_string) %>%
            # Popup for ending destinations
            addPopups(lng = d$lon_current[1], lat=d$lat_current[1], popup = info_string) %>%
            # Draw line between starting and ending destinations
            addPolylines( lat = c(d$lat_from[1], d$lat_current[1]), 
                          lng = c(d$lon_from[1], d$lon_current[1]), 
                          color = "blue")
        
        return (l)
    })
    
    # Ship data rows
    output$ship_records <- renderReactable({
        
        # selected_ship_type <- "Tanker"
        # selected_ship_name <- "AALBORG"
        
        selected_ship_type <- input$ship_type_selector
        selected_ship_name <- input$ship_name_selector
        
        # Return blank is ship name of type is empty
        if(length(selected_ship_type) < 1 | 
           length(selected_ship_name) < 1 ) {
            return(leaflet())
        }
        
        # Filter data for selected ship name and type
        d <- ships %>% 
            filter(shipname == selected_ship_name) %>%
            filter(ship_type == selected_ship_type)
        
        if(nrow(d) >= 2) {
        } else {
            return (reactable())
        }
        
        # Subset columns
        d1 <- d %>% 
            select(#ship_type, shipname, 
                lat_current, lon_current,
                speed, course, heading, destination, flag,
                length,
                width, datetime_current, port, dist_covered, is_parked) %>% 
            as.data.frame() 
        
        # Each row style
        row_style <- "text-align: center !important; font-size: 14px;"
        
        # Create table
        rct <- reactable(d1,
                         defaultPageSize = 10,
                         defaultColGroup = colGroup(headerClass = "rt-header"),
                         defaultColDef = colDef(headerClass = "header col-header"),
                         columns = list(
                             lat_current = colDef(
                                 name = "Latitude",
                                 maxWidth = 85,
                                 align = "center",
                                 style = row_style,
                                 class = "border-left"
                             ),
                             lon_current = colDef(
                                 name = "Longitude",
                                 maxWidth = 85,
                                 align = "center",
                                 style = row_style,
                                 class = "border-left"
                             ),
                             speed = colDef(
                                 name = "Speed",
                                 maxWidth = 75,
                                 align = "center",
                                 style = row_style,
                                 class = "border-left"
                             ),
                             course = colDef(
                                 name = "Course",
                                 maxWidth = 75,
                                 align = "center",
                                 style = row_style,
                                 class = "border-left"
                             ),
                             heading = colDef(
                                 name = "Heading",
                                 maxWidth = 75,
                                 align = "center",
                                 style = row_style,
                                 class = "border-left"
                             ),
                             destination = colDef(
                                 name = "Destination",
                                 maxWidth = 100,
                                 align = "center",
                                 style = row_style,
                                 class = "border-left"
                             ),
                             flag = colDef(
                                 name = "Flag",
                                 maxWidth = 75,
                                 align = "center",
                                 style = row_style,
                                 class = "border-left"
                             ),
                             length = colDef(
                                 name = "Length",
                                 maxWidth = 75,
                                 align = "center",
                                 style = row_style,
                                 class = "border-left"
                             ),
                             width = colDef(
                                 name = "Width",
                                 maxWidth = 75,
                                 align = "center",
                                 style = row_style,
                                 class = "border-left"
                             ),
                             datetime_current = colDef(
                                 name = "Date-Time",
                                 minWidth = 100,
                                 maxWidth = 200,
                                 align = "center",
                                 style = row_style,
                                 class = "border-left"
                             ),
                             port = colDef(
                                 name = "Port",
                                 maxWidth = 100,
                                 align = "center",
                                 style = row_style,
                                 class = "border-left"
                             ),
                             dist_covered = colDef(
                                 name = "Distance Covered (meters)",
                                 maxWidth = 100,
                                 align = "center",
                                 style = paste(row_style, "border-right: 1px solid #DEE2E6;"),
                                 class = "border-left"
                             ),
                             is_parked = colDef(
                                 name = "Is Parked?",
                                 maxWidth = 100,
                                 align = "center",
                                 style = paste(row_style, "border-right: 1px solid #DEE2E6;"),
                                 class = "border-left"
                             )
                         )
        )
        
        return (rct)
        
        
        
        
        
        
    })
    
    
}

shinyApp(ui = htmlTemplate("./template/template.html"), server)