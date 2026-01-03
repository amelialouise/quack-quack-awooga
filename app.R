library(shiny)
library(leaflet)
library(plotly)
library(DT)
library(dplyr)
library(duckdb)
library(DBI)

# Connect to DuckDB database
con <- dbConnect(duckdb(), "meteorites.db")

# Load meteorite data from database
meteorites <- dbGetQuery(con, "SELECT * FROM meteorites WHERE mass > 1e6 LIMIT 200;") %>%
  mutate(
    across(where(is.character), ~ gsub('^"|"$', '', .)),
    year = as.integer(year),
    mass = as.numeric(mass),  # Ensure mass is numeric
    mass_kg = mass / 1000,
    mass_tons = mass / 1000000
  ) %>%
  filter(!is.na(mass), !is.na(mass_tons), is.finite(mass_tons)) %>%  # Remove problematic values
  mutate(
    size_category = cut(mass_tons, 
                        breaks = c(0, 5, 20, Inf),  # Use Inf instead of 60 to capture all large meteorites
                        labels = c("Small (< 5 tons)", "Medium (5-20 tons)", "Large (> 20 tons)"),
                        include.lowest = TRUE)  # Include the lowest value
  )


# Close connection (we'll reconnect in reactive contexts if needed)
dbDisconnect(con)

ui <- fluidPage(
  # Custom CSS for styling
  tags$head(
    tags$style(HTML("
      .content-wrapper {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        min-height: 100vh;
        padding: 20px 0;
      }
      .main-title {
        color: white;
        text-align: center;
        font-size: 2.5em;
        margin-bottom: 30px;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
      }
      .panel {
        background: white;
        border-radius: 10px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      .sidebar-panel {
        background: rgba(255,255,255,0.95);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      .stats-box {
        background: #f8f9fa;
        border-left: 4px solid #667eea;
        padding: 15px;
        margin: 15px 0;
        border-radius: 5px;
      }
    "))
  ),
  
  div(class = "content-wrapper",
      div(class = "container-fluid",
          div(class = "main-title", "🌌 Meteorite Explorer: Giants from Space"),
          
          fluidRow(
            column(3,
                   div(class = "sidebar-panel",
                       h4("🔍 Explore Controls"),
                       
                       sliderInput("year_range", 
                                   "Discovery Year Range:",
                                   sep = "",
                                   min = min(meteorites$year, na.rm = TRUE), 
                                   max = max(meteorites$year, na.rm = TRUE),
                                   value = c(min(meteorites$year, na.rm = TRUE), max(meteorites$year, na.rm = TRUE)),
                                   step = 10),
                       
                       textInput("name_search", 
                                 "🔍 Search by Name:",
                                 value = "",
                                 placeholder = "Enter meteorite name..."),
                       
                       selectInput("size_filter", 
                                   "Size Category:",
                                   choices = c("All" = "all", levels(meteorites$size_category)),
                                   selected = "all"),
                       
                       hr(),
                       
                       h5("📊 Quick Stats"),
                       div(class = "stats-box",
                           verbatimTextOutput("quick_stats")
                       ),
                       
                       hr(),
                       
                       p("💡 Click on the bright meteorite markers to see detailed information! 🔥")
                   )
            ),
            
            column(9,
                   fluidRow(
                     column(6,
                            div(class = "panel",
                                h4("🗺️ Global Meteorite Distribution"),
                                leafletOutput("meteorite_map", height = "400px")
                            )
                     ),
                     column(6,
                            div(class = "panel",
                                h4("📈 Discovery Timeline & Mass Distribution"),
                                plotlyOutput("timeline_plot", height = "400px")
                            )
                     )
                   ),
                   
                   fluidRow(
                     column(12,
                            div(class = "panel",
                                h4("🔬 Detailed Meteorite Database"),
                                DTOutput("meteorite_table")
                            )
                     )
                   )
            )
          )
      )
  )
)

server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    data <- meteorites %>%
      filter(year >= input$year_range[1] & year <= input$year_range[2]) 
    
    # Filter by name search (case-insensitive partial matching)
    if (!is.null(input$name_search) && input$name_search != "") {
      data <- data %>% 
        filter(grepl(input$name_search, name, ignore.case = TRUE))
    }
    
    # Filter by size category
    if (input$size_filter != "all") {
      data <- data %>% filter(size_category == input$size_filter)
    }
    
    return(data)
  })
  
  # Quick stats
  output$quick_stats <- renderText({
    data <- filtered_data()
    total_meteorites <- nrow(data)
    total_mass <- sum(data$mass_tons, na.rm = TRUE)
    avg_mass <- round(mean(data$mass_kg, na.rm = TRUE), 1)
    
    # Handle case where no data is available
    if (total_meteorites == 0) {
      return("No meteorites match current filters")
    }
    
    heaviest_name <- if(total_meteorites > 0) {
      data$name[which.max(data$mass_tons)]
    } else {
      "None"
    }
    
    paste0("Meteorites: ", total_meteorites, "\n",
           "Total Mass: ", round(total_mass, 1), " tons\n",
           "Avg Mass: ", avg_mass, " kgs\n",
           "Heaviest: ", heaviest_name)
  })
  
  # Interactive map
  output$meteorite_map <- renderLeaflet({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(leaflet() %>% 
               addTiles() %>% 
               setView(lng = 0, lat = 20, zoom = 2) %>%
               addPopups(lng = 0, lat = 0, "No data matches current filters"))
    }
    
    # Calculate map center based on filtered data
    center_lat <- mean(data$lat, na.rm = TRUE)
    center_lng <- mean(data$long, na.rm = TRUE)
    
    # Calculate appropriate zoom level based on data spread
    lat_range <- max(data$lat, na.rm = TRUE) - min(data$lat, na.rm = TRUE)
    lng_range <- max(data$long, na.rm = TRUE) - min(data$long, na.rm = TRUE)
    zoom_level <- if(max(lat_range, lng_range) > 100) 2 else if(max(lat_range, lng_range) > 50) 3 else 4
    
    # Create a cleaner color palette based on mass categories
    data <- data %>%
      mutate(
        mass_category = cut(mass_tons,
                            breaks = c(0, 10, 50, 200, Inf),
                            labels = c("< 10 tons", "10-50 tons", "50-200 tons", "> 200 tons"),
                            include.lowest = TRUE)
      )
    
    # Create color palette that works with leaflet
    pal <- colorFactor(
      palette = c("#000000", "#F7DC6F", "#F39C12", "#E74C3C"),
      domain = levels(data$mass_category)
    )
    
    # Create popup content with the "More Info" link restored
    popups <- paste0(
      "<div style='font-size: 14px; line-height: 1.4;'>",
      "<h4 style='margin: 0 0 10px 0; color: #2c3e50;'>🌠 ", data$name, "</h4>",
      "<p style='margin: 5px 0;'><strong>Type:</strong> ", data$reclass, "</p>",
      "<p style='margin: 5px 0;'><strong>Mass:</strong> ", format(round(data$mass_tons, 1), big.mark = ","), " kg</p>",
      "<p style='margin: 5px 0;'><strong>Year:</strong> ", data$year, "</p>",
      "<p style='margin: 5px 0;'><strong>Location:</strong> ", round(data$lat, 3), "°, ", round(data$long, 3), "°</p>",
      "<p style='margin: 10px 0 0 0;'><a href='", data$lpi_entry, "' target='_blank' style='color: #667eea;'>🔗 More Info</a></p>",
      "</div>"
    )
    
    # Calculate radius based on mass but keep it reasonable
    radius_vals <- 5 + (log10(pmax(data$mass_tons, 0.1)) / log10(max(data$mass_tons, na.rm = TRUE))) * 10
    radius_vals[!is.finite(radius_vals)] <- 8
    radius_vals <- pmax(pmin(radius_vals, 15), 5)
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~long, ~lat,
        radius = radius_vals,
        color = "white",
        weight = 2,
        fillColor = ~pal(mass_category),  # Use the palette function
        fillOpacity = 0.8,
        popup = popups,
        label = ~paste0(name, " (", round(mass_kg, 1), " kg)")
      ) %>%
      addLegend(
        pal = pal,  # Use the same palette
        values = ~mass_category,  # Use the mass categories
        title = "Mass Categories",
        position = "bottomright",
        opacity = 0.8
      ) %>%
      setView(lng = center_lng, lat = center_lat, zoom = zoom_level)
  })
  
  
  
  # Timeline and mass visualization
  output$timeline_plot <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(plot_ly() %>% 
               add_annotations(text = "No data matches filters", 
                               xref = "paper", yref = "paper",
                               x = 0.5, y = 0.5, showarrow = FALSE))
    }
    
    # Create simple North/South hemisphere categories
    data <- data %>%
      mutate(
        hemisphere = ifelse(lat >= 0, "Northern Hemisphere", "Southern Hemisphere")
      )
    
    # Better color scheme for North/South
    hemisphere_colors <- c(
      "Northern Hemisphere" = "#2E86AB",  # Cool blue for north
      "Southern Hemisphere" = "#A23B72"   # Warm magenta for south
    )
    
    # Calculate dynamic year range from filtered data
    year_min <- min(data$year, na.rm = TRUE)
    year_max <- max(data$year, na.rm = TRUE)
    year_padding <- max(1, (year_max - year_min) * 0.02)
    
    # Create scatter plot with uniform sizes
    p <- plot_ly(data, 
                 x = ~year, 
                 y = ~mass_tons,
                 color = ~hemisphere,
                 colors = hemisphere_colors,
                 text = ~paste("Name:", name, 
                               "<br>Mass:", format(round(mass_tons, 2), big.mark = ","), "tons",
                               "<br>Year:", year,
                               "<br>Hemisphere:", hemisphere,
                               "<br>Location:", round(lat, 2), "°, ", round(long, 2), "°"),
                 hovertemplate = "%{text}<extra></extra>",
                 type = "scatter",
                 mode = "markers",
                 marker = list(
                   size = 8,  # Uniform size for all dots
                   line = list(width = 1, color = "white"),
                   opacity = 0.4
                 )) %>%
      layout(
        title = list(text = "Meteorite Discoveries: Timeline by Hemisphere", font = list(size = 16)),
        xaxis = list(
          title = "Discovery Year", 
          showgrid = TRUE,
          range = c(year_min - year_padding, year_max + year_padding)
        ),
        yaxis = list(
          title = "Mass (tons)", 
          type = "linear",
          showgrid = TRUE
        ),
        showlegend = TRUE,
        legend = list(
          orientation = "h",  # Horizontal legend for cleaner look
          x = 0.3, 
          y = 1.02,
          font = list(size = 12),
          itemclick = "toggle",
          itemdoubleclick = "toggleothers"
        ),
        hovermode = "closest",
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      ) %>%
      config(
        displayModeBar = FALSE,
        showTips = FALSE
      )
    
    p
  })
  
  
  
  
  # Data table
  output$meteorite_table <- renderDT({
    data <- filtered_data() %>%
      select(name, reclass, mass_tons, year, lat, long) %>%
      mutate(
        mass_tons = round(mass_tons, 1),
        lat = round(lat, 3),
        long = round(long, 3)
      )
    
    datatable(data,
              colnames = c("Name", "Classification", "Mass (tons)", "Year", "Latitude", "Longitude"),
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'ltip'
              ),
              rownames = FALSE) %>%
      formatStyle("mass_tons", 
                  background = styleColorBar(data$mass_tons, "lightblue"),
                  backgroundSize = "100% 90%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })
}

shinyApp(ui = ui, server = server)
