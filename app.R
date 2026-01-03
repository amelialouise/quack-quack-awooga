library(shiny)
library(leaflet)
library(plotly)
library(DT)
library(dplyr)
library(duckdb)
library(DBI)

# Connect to local DuckDB database
con <- dbConnect(duckdb(), "meteorites.db")

# Load meteorite data from database
meteorites <- dbGetQuery(con, "SELECT * FROM meteorites WHERE mass > 1e6 LIMIT 23;") %>%
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
          background: linear-gradient(135deg, #6b7280 0%, #4b5563 50%, #374151 100%);
          min-height: 100vh;
          padding: 20px 0;
      }
      .main-title {
        color: #f3f4f6;
        text-align: center;
        font-size: 2.5em;
        margin-bottom: 30px;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
      }
      .panel {
        background: #f3f4f6;
        border-radius: 10px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      .sidebar-panel {
        background: rgba(255,255,255,0.95);
        border-radius: 10px;
        border: none;
        padding: 20px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      .stats-box {
        background: #f8f9fa;
        border: none;
        padding: 15px;
        margin: 15px 0;
        border-radius: 5px;
      }
    "))
  ),
  
  div(class = "content-wrapper",
      div(class = "container-fluid",
          div(class = "main-title", "🌌 Meteorite Explorer: Rocks from Space!"),
          
          fluidRow(
            column(3,
                   div(class = "sidebar-panel",
                       h4("🔍 Search Options"),
                       
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
                       
                       p("Click on the bright meteorite markers to see detailed information! 🔥"),
                       div(class = "text-muted small mt-2",
                           "More Info and Name links connect to official ", 
                           a("Meteoritical Bulletin Database", 
                             href = "https://www.lpi.usra.edu/meteor/metbull.php", 
                             target = "_blank"), " entries."),
                       
                       hr(),
                       
                       h5("📖 About"),
                       div(class = "stats-box",
                           p(style = "margin-bottom: 10px; font-size: 0.9em;", 
                             "Data from ", 
                             a("NASA's Meteorite Database", 
                               href = "https://data.nasa.gov/dataset/meteorite-landings", 
                               target = "_blank",
                               style = "color: #667eea;")),
                           p(style = "margin: 0; font-size: 0.9em;", 
                             a("View Code & Setup", 
                               href = "https://github.com/amelialouise/shiny-meteorites", 
                               target = "_blank",
                               style = "color: #667eea;"), 
                             " on GitHub")
                       )
                   )
            ),
            
            column(9,
                   fluidRow(
                     column(6,
                            div(class = "panel",
                                div(style = "display: flex; justify-content: space-between; align-items: center;",
                                    h4("🗺️ Global Meteorite Distribution"),
                                    actionButton("expand_map", "⛶", 
                                                 class = "btn btn-sm btn-outline-secondary",
                                                 title = "Expand map")
                                ),
                                leafletOutput("meteorite_map", height = "400px")
                            )
                     ),
                     column(6,
                            div(class = "panel",
                                div(style = "display: flex; justify-content: space-between; align-items: center;",
                                    h4("📈 Discovery Timeline & Mass Distribution"),
                                    actionButton("expand_timeline", "⛶", 
                                                 class = "btn btn-sm btn-outline-secondary",
                                                 title = "Expand timeline")
                                ),
                                plotlyOutput("timeline_plot", height = "400px")
                            )
                     )
                   ),
                   
                   fluidRow(
                     column(12,
                            div(class = "panel",
                                h4("🔬 Meteorite Landings"),
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
    avg_mass <- round(mean(data$mass_tons, na.rm = TRUE), 1)
    
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
           "Avg Mass: ", avg_mass, " tons\n",
           "Heaviest: ", heaviest_name)
  })
  
  # Create shared map reactive
  shared_map <- reactive({
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
      "<p style='margin: 5px 0;'><strong>Mass:</strong> ", format(round(data$mass_tons, 1), big.mark = ","), " tons</p>",
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
        fillColor = ~pal(mass_category),
        fillOpacity = 0.8,
        popup = popups,
        label = ~paste0(name, " (", round(mass_tons, 1), " tons)")
      ) %>%
      addLegend(
        pal = pal,
        values = ~mass_category,
        title = "Mass Categories", 
        position = "bottomright",
        opacity = 0.8
      ) %>%
      setView(lng = center_lng, lat = center_lat, zoom = zoom_level)
  })
  
  # Create shared timeline reactive  
  shared_timeline <- reactive({
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
    plot_ly(data, 
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
              size = 14,  # Uniform size for all dots
              line = list(width = 1, color = "white"),
              opacity = 0.7
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
  })
  
  # Now your outputs just call the shared reactives
  output$meteorite_map <- renderLeaflet({
    shared_map()
  })
  
  output$meteorite_map_full <- renderLeaflet({
    shared_map()
  })
  
  output$timeline_plot <- renderPlotly({
    shared_timeline()
  })
  
  output$timeline_plot_full <- renderPlotly({
    shared_timeline()
  })
  
  # Modal event handlers
  observeEvent(input$expand_map, {
    showModal(modalDialog(
      title = "🗺️ Global Meteorite Distribution - Full View",
      leafletOutput("meteorite_map_full", height = "70vh"),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$expand_timeline, {
    showModal(modalDialog(
      title = "📈 Discovery Timeline - Full View", 
      plotlyOutput("timeline_plot_full", height = "70vh"),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Data table
  output$meteorite_table <- renderDT({
    data <- filtered_data() %>%
      mutate(hemisphere = ifelse(lat >= 0, "Northern", "Southern")) %>% 
      select(name, mass_tons, reclass, year, fall, lpi_entry, hemisphere) %>%
      mutate(
        mass_tons = round(mass_tons, 1),
        name = paste0('<a href="', lpi_entry, '" target="_blank">', name, '</a>')
      ) %>% 
      select(-lpi_entry)
    
    datatable(data,
              escape = FALSE,
              colnames = c("Name", "Mass (tons)", "Type", "Year", "Discovery Method", "Hemisphere"),
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                order = list(list(1, 'desc')),
                dom = 'ltip',
                columnDefs = list(
                  list(targets = ncol(data) - 1, width = "100px")  # Set width for link column
                )
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
          