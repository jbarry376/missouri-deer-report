# missouri-deer-report 
# Shiny R application
# Author: John Barry 

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Missouri Deer Hunting Data : 2023-2024 Season"),
    
    br(), 
    h4("Summary Type by County"),
    p("Shows the total number and type of deer harvested in each county by hunt type."),
    h4("Summary Type by County Population"),
    p("Shows the numbers of deer harvested divided by the number of 
        county residents e.g. 1 deer per 100 residents = 0.01."), 
    h4("Summary Type by County Area"),
    p("Shows the numbers of deer harvested
        divided by the total area of that county in square miles e.g. 100 deer per 50 square miles = 2.0."), 
    
    
    # drop down by hunt type 
    sidebarPanel(
        selectInput(
            inputId = "huntTypeDropdown",
            label = "Choose a Hunt Type:",
            choices = c("Archery", "Managed Hunt", "Firearms", "All Hunts"),
            selected = "Archery" # Optional: default selected value
        )
    ),
    
    # drop down by summary type 
    sidebarPanel(
        selectInput(
            inputId = "summaryTypeDropdown",
            label = "Choose a Summary Type:",
            choices = c("by County", "by County Population", "by County Area"),
            selected = "by County" # Optional: default selected value
        )
    ),
    
    br(), 
    br(),
    br(),
    br(),
    
    plotOutput("topFivePlot"), 
    
    
    br(), 
    br(),
    br(),
    br(),
    
    # totals table - no search, page # options
    dataTableOutput("huntingTotalsTable"),
    
    
    br(),
    br(),
    # summary by county table with filtering options
    dataTableOutput("huntingSummaryTable"),    

)



### read in data used to create dashboard
hunting.data = read.csv("hunting_data_2023.csv")
county.data = read.csv("county_level_data.csv")
total_counts.data = read.csv("hunting_summary_data.csv")
filter.columns = colnames(hunting.data[2:5])
total.population = sum(county.data$Population)
total.area = sum(county.data$Area_sq_mi)




# function to filter data using drop-down inputs 
filter.data = function(hunt_type_to_filter_by){
    df = hunting.data %>% 
        filter(huntType == hunt_type_to_filter_by) %>% 
        select(-huntType)
    
    df 
}

# join to county level data and make summary table 
make.county.summary = function(df, summary_type_to_filter_by){
    if (summary_type_to_filter_by == 'by County'){
        df
    }
    else if (summary_type_to_filter_by == 'by County Population'){
        df_joined = merge(df, county.data, by = 'County')
        df_by_population = df_joined %>%
            mutate(across(filter.columns, ~ . / Population)) %>% 
            mutate(across(filter.columns, ~round(., 3)))
        
        df_by_population[1:5]
    }
    else if (summary_type_to_filter_by == 'by County Area'){
        df_joined = merge(df, county.data, by = 'County')
        df_by_area = df_joined %>%
            mutate(across(filter.columns, ~ . / Area_sq_mi)) %>% 
            mutate(across(filter.columns, ~round(., 3)))
        
        df_by_area[1:5]
    }
    
}

# make totals table 
make.totals.table = function(hunt_type_to_filter_by, summary_type_to_filter_by){
    total.population = sum(county.data$Population)
    total.area = sum(county.data$Area_sq_mi)
    df = total_counts.data
    df_out = df %>% 
        filter(huntType == hunt_type_to_filter_by) %>% 
        rename(
            "Hunt Type" = huntType, 
            "Total Antlered Bucks" = Antlered.Bucks, 
            "Total Does" = Does, 
            "Total Button Bucks" = Button.Bucks, 
            "Total Deer" = Total
        )
    
    if (summary_type_to_filter_by == "by County"){
        df_out
    }
    else if (summary_type_to_filter_by == 'by County Population'){
        df_out = df_out %>% 
            mutate(across(2:5, ~ . / total.population)) %>% 
            mutate(across(2:5, ~ round(., 3)))
        
        df_out
    }
    else if (summary_type_to_filter_by == "by County Area"){
        df_out = df_out %>% 
            mutate(across(2:5, ~ . / total.area)) %>% 
            mutate(across(2:5, ~ round(.,3)))
        df_out
    }
}


top.five.plot = function(hunt_type_to_filter_by, summary_type_to_filter_by){
    
    df = hunting.data
    
    if (summary_type_to_filter_by == "by County"){
        df_filtered = df
        title.string = "Top 5 Counties: Total Deer Harvested"
    }
    else if (summary_type_to_filter_by == "by County Population"){
        title.string = "Top 5 Counties: Total Deer Harvested per County Resident"
        df_joined = merge(df, county.data, by = 'County')
        df_filtered = df_joined %>%
            mutate(across(filter.columns, ~ . / Population)) %>% 
            mutate(across(filter.columns, ~round(., 3)))
        
    }
    else if (summary_type_to_filter_by == "by County Area"){
        title.string = "Top 5 Counties: Total Deer Harvested per Square Mile"
        df_joined = merge(df, county.data, by = 'County')
        df_filtered = df_joined %>%
            mutate(across(filter.columns, ~ . / Area_sq_mi)) %>% 
            mutate(across(filter.columns, ~round(., 3)))
        
    }
    
    
    
    df_filtered %>%
        filter(huntType==hunt_type_to_filter_by)%>%
        arrange(desc(Total)) %>%        # sort by Total descending
        slice(1:5) %>%                  # take top 5
        ggplot(aes(x = reorder(County, Total), y = Total)) +
        geom_col(fill = "steelblue") +
        labs(
            title = title.string,
            x = "County",
        ) +
        theme_minimal()+
        geom_text(aes(label = Total),size = 6, vjust=+2, color='white') 
}


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$huntingSummaryTable <- DT::renderDataTable({
        make.county.summary(
            filter.data(input$huntTypeDropdown), 
            input$summaryTypeDropdown
        )}, 
        options = list(
            lengthChange = FALSE, 
            pageLength=-1,
            paging=FALSE,
            info=FALSE
        )
    )
    
    output$huntingTotalsTable <- DT::renderDataTable({
        make.totals.table(input$huntTypeDropdown, input$summaryTypeDropdown)
    },
    options = list(
        lengthChange = FALSE,
        paging = FALSE, 
        rownames=FALSE, 
        searching=FALSE, 
        info=FALSE
    )
    )
    
    output$topFivePlot <- renderPlot({
        top.five.plot(input$huntTypeDropdown, input$summaryTypeDropdown)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
