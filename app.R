# missouri-deer-report 
# Shiny R application
# Author: John Barry 
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(leaflet)
library(sf)
library(tigris)

### Data Prep ###
# Missouri Counties
shape = tigris::counties(state = "MO", class = "sf")

hunt_data = read.csv("./missouri-deer-report/hunt_data_2023_w_geoid.csv", header=T)
hunt_data$GEOID = as.character(hunt_data$GEOID)


### SHINY Setup 

# Define UI 
ui = fluidPage(
  
  # Application title
  titlePanel("Missouri Deer Hunting Data: 2023-2024 Season"),
  
  # Top panel with county name
  verticalLayout(
    
    wellPanel(
      verbatimTextOutput("cnty"),
      # select color palette 
      fluidRow(
        column(
          width = 4, 
          selectInput(
            "huntType",
            "Choose Hunt Type:",
            choices = c( "All Hunts", "Archery", "Firearms", "Managed Hunt"),
            selected = "All Hunts"
          )
        ), 
        column(
          width = 4, 
          # select summary type 
          selectInput(
            "summaryType",
            "Choose Summary Type:",
            choices = c( "by County", "by County Population", "by County Area"),
            selected = "by County"
          )
        ), 
        column(
          width = 4, 
          selectInput(
            "palette",
            "Choose Color Palette:",
            choices = c("Oranges", "Greens"),
            selected = "Oranges"
          )
        )
      )
    ),
    
    
    # map + top 5 counties plot + data table
    mainPanel(
      leafletOutput("map"),
      plotOutput("topCountiesPlot", height = "400px"),
      dataTableOutput("huntingTable") 
    )
  )
)

# Define server logic       
server <- function(input, output) {
  
  selected_data = reactive({
    hunt_data %>% 
      filter(huntType == input$huntType)
  })
  
  # create reactive dataframe 
  shape_reactive = reactive({
    
    df = shape %>% 
      left_join(selected_data(), by='GEOID') %>% 
      mutate(
        Total = replace_na(Total, 0),
        Antlered.Buck = replace_na(Antlered.Buck, 0),
        Button.Buck = replace_na(Button.Buck, 0),
        Doe = replace_na(Doe, 0),
        area_sqmi = ifelse(as.numeric(st_area(geometry)) > 0,
                           as.numeric(st_area(geometry)) / 2.59e6,
                           NA)
      )
    
    if (input$summaryType == "by County Area") {
      
      df = df %>% 
        mutate(
          Total = Total / area_sqmi,
          Antlered.Buck = Antlered.Buck / area_sqmi,
          Button.Buck = Button.Buck / area_sqmi,
          Doe = Doe / area_sqmi
        )
      
    } else if (input$summaryType == "by County Population") {
      
      df = df %>%
        mutate(
          pop_thousands = ifelse(Population > 0, Population / 1000, NA),
          Total = Total / pop_thousands,
          Antlered.Buck = Antlered.Buck / pop_thousands,
          Button.Buck = Button.Buck / pop_thousands,
          Doe = Doe / pop_thousands
        )
    }
    
    df
  })
  
  # create map for output using shape_reactive 
  output$map = renderLeaflet({
    
    data = shape_reactive()
    
    vals = data$Total
    if (all(vals == 0, na.rm = TRUE)) {
      vals = c(0, 1)  # prevent zero-range palette
    }
    
    # color palette for map 
    pal = colorNumeric(input$palette, domain = vals, na.color = "transparent")
    
    leaflet(data) %>% 
      #addTiles(group = "OSM (default)") %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(Total),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        layerId = ~COUNTYNS,
        # labels adjusted for summary type 
        label = ~paste0(
          NAMELSAD, ": ",
          scales::comma(round(Total, 2)),
          ifelse(input$summaryType == "by County Area", " per sq mi",
                 ifelse(input$summaryType == "by County Population", " per 1,000 residents", ""))
        )
      ) 
  })
  
  # plot of top 5 counties 
  output$topCountiesPlot = renderPlot({
    
    df = shape_reactive() %>%
      st_drop_geometry()
    
    # get top 5 counties by Total
    top5 = df %>%
      arrange(desc(Total)) %>%
      slice_head(n = 5)
    
    # compute mean across all counties
    mean_val = mean(df$Total, na.rm = TRUE)
    
    # label suffix based on summary type
    suffix <- if (input$summaryType == "by County Area") {
      " (per sq mi)"
    } else if (input$summaryType == "by County Population") {
      " (per 1,000 residents)"
    } else {
      ""
    }
    
    # color of bars based on input palette 
    plot_color = if (input$palette == "Oranges"){
      "#fd8d3c"
    } else if (input$palette == "Greens"){
      "#74c476"
    } else {
      ""
    }
    # bar plot using ggplot 
    ggplot(top5, aes(x = reorder(NAMELSAD, Total), y = Total)) +
      geom_col(fill = plot_color, width = 0.4) +
      coord_flip() +
      labs(
        title = "Top 5 Counties",
        x = "County",
        y = "Total"
      ) +
      theme_minimal()
  })
  
  
  # data table 
  output$huntingTable = DT::renderDataTable({
    data = shape_reactive()
    
    table_data = data %>% 
      st_drop_geometry() %>% 
      select(
        County = NAMELSAD, 
        Antlered.Buck, 
        Button.Buck, 
        Doe, 
        Total
      )%>% 
      mutate(
        across(where(is.numeric), ~round(., 2))
      )
    
    DT::datatable(
      table_data, 
      rownames = FALSE
    )
  })
  
  # setup observe -- outputs to panel 
  observe({
    
    data = shape_reactive()
    
    vals = data$Total
    if (all(vals == 0, na.rm = TRUE)) {
      vals <- c(0, 1)  # prevent zero-range palette
    }
    
    pal = colorNumeric(input$palette, domain = vals, na.color = "transparent")
    
    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(Total),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        layerId = ~COUNTYNS,
        label = ~paste0(
          "<strong>", NAMELSAD, "</strong><br/>",
          "Antlered Buck: ", scales::comma(round(Antlered.Buck, 2)), "<br/>",
          "Button Buck: ", scales::comma(round(Button.Buck, 2)), "<br/>",
          "Doe: ", scales::comma(round(Doe, 2)), "<br/>",
          "Total: ", scales::comma(round(Total, 2)),
          ifelse(input$summaryType == "by County Area", " per sq mi",
                 ifelse(input$summaryType == "by County Population", " per 1,000 residents", ""))
        ) %>% lapply(htmltools::HTML)
      )
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
