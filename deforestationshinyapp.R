# library imports

library(readr)
library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)
library(rnaturalearth)
library(countrycode)

# importing the dataset from kaggle

download.file(
  "https://www.kaggle.com/api/v1/datasets/download/ashyou09/global-deforestation-and-afforestation-2000-2025",
  destfile = "deforestation.zip",
  mode = "wb"
)

unzipped_files <- unzip("deforestation.zip", list = TRUE)
csv_name <- unzipped_files$Name[grepl("\\.csv$", unzipped_files$Name)][1]
unzip("deforestation.zip", files = csv_name, overwrite = TRUE)
data <- readr::read_csv(csv_name)

# looking at the first few rows to make sure everything is loading correctly

head(data)

# cleaning the data

data = subset(data, !grepl("Country_Gen", Country))  # filtering countries to only include those with a name to help map the country code to it
data$iso3 = countrycode(data$Country, origin = "country.name", destination = "iso3c") # adding a new column that includes the country code for mapping

# creating global variables to be used later in the creating of the visualization

world  = ne_countries(scale = "medium", returnclass = "sf") # loading the world map from the sf package
year_min = min(data$Year, na.rm = TRUE) # setting up the min and max years for the slider feature
year_max = max(data$Year, na.rm = TRUE)

# building the user interface which is what the user will see when using the shiny app

ui = fluidPage(
  titlePanel("Click a country on the map and compare it by category over the years"),
  helpText("Select a country on the map"),
  helpText("Use the dropdown and slider to explore trends over time"),
  fluidRow(
    
    # this line creates the spot for the map output which will be the world map
    
    column(6, leafletOutput("map", height = 550)),
    
    # this line makes the dropdown, so the user can change the features that are being compared over time
    
    column(6, selectInput(
        "metric", "Metric:",
        choices = c(
          "Forest Area (kilometers squared)" = "Forest_Area_km2",
          "Forest Cover Percentage" = "Forest_Cover_Pct",
          "Annual Deforestation Rate" = "Annual_Deforestation_Rate",
          "Annual Afforestation Rate" = "Annual_Afforestation_Rate",
          "Total Carbon Stock (tonnes)" = "Total_Carbon_Stock_Tonnes"
        ), selected = "Forest_Area_km2"),
        
    # this line is for the year slider time range which will allow the user to change the duration that they are looking at
     
     sliderInput("years", "Year range:",
        min = year_min, max = year_max,
        value = c(year_min, year_max), step = 1, sep = ""),
      h4(textOutput("selected")),
      plotOutput("timeline", height = 520))))

# doing the backend work of what will be doing the computations that appear 

server = function(input, output, session) {
  
  latest_year = max(data$Year, na.rm = TRUE)
  
  # making the table of the map information to include on the globe
  
  map_data = data %>%
    filter(Year == latest_year) %>%
    group_by(iso3) %>% # grouping by the country codes to make sure they map correctly on the map
    summarise(value = mean(Forest_Area_km2, na.rm = TRUE), .groups = "drop")
  
  world_map = world %>%
    left_join(map_data, by = c("iso_a3" = "iso3")) # this part lines up the map table with the world map to make sure all the information lines up with one another
    # this is what gives the shapes on the map meaning essentially
  
  pal_colors = colorNumeric(palette = "YlGn",domain = world_map$value,na.color = "grey") # palette colors for the map with grey being the countries on the map with no value
  
  # is this what makes the map and makes sure it only loads once but still allows for new queries
  
  output$map = renderLeaflet({
    leaflet(world_map) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        layerId = ~iso_a3,
        fillColor = ~pal_colors(value),
        fillOpacity = 0.8,
        color = "grey",
        weight = 0.5,
        label = ~paste0(
          name, "<br>",
          "Forest Area (", latest_year, "): ",
          ifelse(is.na(value), "NA", round(value, 0)))) %>%
      addLegend(
        pal = pal_colors,
        values = ~value,
        title = paste0("Forest Area (", latest_year, ")"),
        position = "bottomright")})

  # this click part stores which country is clicked on the map
  
  clicked_iso3 = reactiveVal(NULL)
  
  selected_country_name = reactive({
    req(clicked_iso3())
    world %>%
      filter(iso_a3 == clicked_iso3()) %>%
      pull(name)
  })
  
  # this is the click event and runs this code when a country is clicked
  
  observeEvent(input$map_shape_click, {
    clicked_iso3(input$map_shape_click$id)})
  
  # text that shows what has been selected whether something has been clicked or not
  
  output$selected = renderText({
    if (is.null(clicked_iso3())) {
      "No country selected"
    } else {paste("Selected Country:", selected_country_name())}})

  # this is what runs once the click has occurred and varies depending on what is clicked and the timeline that is selected
  
  selected_data = reactive({
    req(clicked_iso3())
    data %>%
      filter(
        iso3 == clicked_iso3(),
        Year >= input$years[1],
        Year <= input$years[2]
      ) %>%
      arrange(Year)})
  
  # computes and displays the global averages in comparison to the country that was selected
  
  avg_all_countries = reactive({
    data %>% filter(
        Year >= input$years[1],
        Year <= input$years[2]) %>%
      group_by(Year) %>%
      summarise(avg_value = mean(.data[[input$metric]], na.rm = TRUE), .groups = "drop") %>%
      arrange(Year)})

  # this is what creates the timeline plot
  
  output$timeline = renderPlot({
    df = selected_data()
    req(nrow(df) > 0) # requires that each plot has a minimum of 1 row
    
    avg_df = avg_all_countries() # pulls the global average
    req(nrow(avg_df) > 0)
    
    ggplot() + geom_line(data = df, aes(
          x = Year,
          y = .data[[input$metric]],
          color = "Selected country",
          linetype = "Selected country" ),linewidth = 1) +
      geom_point(data = df, aes(
          x = Year,
          y = .data[[input$metric]],
          color = "Selected country"), size = 1.8) +
      geom_line(data = avg_df, aes(
          x = Year,
          y = avg_value,
          color = "Global average",
          linetype = "Global average"),linewidth = 1.1) +
      scale_color_manual(name = "",values = c(
          "Selected country" = "blue",
          "Global average" = "red")) +
      scale_linetype_manual(name = "", values = c(
          "Selected country" = "solid",
          "Global average" = "dashed")) +
      theme_minimal() + labs(
        title = paste("Selected Country vs Global Average (", selected_country_name(), ")", sep = ""),
        x = "Year",
        y = NULL) + theme(
        legend.position = "top",
        legend.text = element_text(size = 11))})}

# launches the shiny app

shinyApp(ui, server)

