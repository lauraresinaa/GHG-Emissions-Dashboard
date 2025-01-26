# Load necessary libraries
library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)

# Load your data
df <- read_excel("df5.xlsx") %>%
  filter(Year >= 1970)

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("GHG Emissions Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h4("GHG Emissions Analysis"),
      helpText("This dashboard provides insights into greenhouse gas emissions (GHG) data over time and across regions."),
      
       
      sliderInput("year_range",            #YEAR SLIDER
                  "Select Year Range:", 
                  min = min(df$Year), 
                  max = max(df$Year), 
                  value = c(1970, 2023),  # Default range
                  step = 1,
                  animate = TRUE,
                  sep = "")
      ,
      selectInput(
        "country", 
        "Select Country:", 
        choices = NULL,  # Will be updated dynamically in the server
        selected = NULL, 
        multiple = TRUE
      ),
      selectInput("data_type", "Select Data Type:", 
                  choices = c("Values" = "GHG emissions", 
                              "Growth Rate" = "GHG emission Growth Rate", 
                              "Per Capita" = "GHG per capita"),
                  selected = "GHG emissions")
    ),
    mainPanel(
      navlistPanel(
        tabPanel(
          "GHG Evolution",
          h4("GHG Evolution Overview"),
          helpText("This graph displays the growth rates of GHG emissions over the selected time range for the European Union (EU27), the Euro Area (EA), and worldwide."),
          plotlyOutput("ghg_evolution")
        ),
        tabPanel(
          "GHG per Capita", 
          h4("GHG per Capita Overview"),
          helpText("This graph provides a per capita perspective on GHG emissions by normalizing the emissions data according to population size, thus offering insights into the average emissions per individual."),
          plotlyOutput("ghg_per_capita")
        ),
        tabPanel(
          "GHG Contribution", 
          h4("GHG Contribution Overview"),
          helpText("This tab offers a detailed breakdown of GHG emissions by continent and country, featuring a treemap alongside a table highlighting each continent's contribution."),
          plotlyOutput("ghg_contribution"),  # Treemap chart
          uiOutput("gradient_legend"),      # Add gradient legend here
          tableOutput("continent_contribution")
        ),
        
        widths = c(3, 9)
      )
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output) {
  # Update the country dropdown dynamically
  observe({
    unique_countries <- unique(df$CountryGHG)  # Extract unique country names
    updateSelectInput(
      inputId = "country",
      choices = c("Select All", unique_countries),  # Add "Select All" option
      selected = "Select All"  # Default to "Select All"
    )
  })
  
  df_filtered <- reactive({
    selected_years <- input$year_range
    
    # Check if "Select All" is chosen
    if ("Select All" %in% input$country || is.null(input$country)) {
      filtered_data <- df  # No filtering; return all countries
    } else {
      filtered_data <- df %>% filter(CountryGHG %in% input$country)
    }
    
    # Further filter by year range
    filtered_data %>%
      filter(Year >= selected_years[1] & Year <= selected_years[2]) %>%
      distinct(Year, CountryGHG, .keep_all = TRUE)
  })
 
  #CHART 1
  output$ghg_evolution <- renderPlotly({
    df_evolution <- df %>%
      filter(
        Region %in% c("EA", "EU", "Worldwide"), 
        Year >= input$year_range[1], 
        Year <= input$year_range[2]  # Use year range slider input
      ) %>%
      group_by(Year, Region) %>%
      summarize(growthrateGHG = round(mean(`GHG emission Growth Rate` * 100, na.rm = TRUE), 3), .groups = 'drop')
    
    p1 <- ggplot(df_evolution, aes(x = Year, y = growthrateGHG, color = Region)) +
      geom_line(size = 1) +  # Thicker line for better visibility
      scale_color_manual(values = c("#003366", "#3399FF", "#A6CEE3")) +  # ECB-style colors
      labs(
        title = "Evolution of GHG growth by region",
        subtitle = "Comparing GHG Emission Growth Rates: Euro Area, EU, and Worldwide",
        y = "Avg. Annual Growth Rate (%)",
        x = "Year",
        color = "Region"
      ) +
      theme_minimal(base_family = "Arial") +  # Minimalist theme with clean font
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        legend.position = "top",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p1) %>% layout(autosize = TRUE, title = list(y = 0.95))
  })
  
#CHART 2
  output$ghg_per_capita <- renderPlotly({
    df_per_capita <- df %>%
      filter(
        Year > 1986,  # Additional hard-coded filter
        Year >= input$year_range[1],  # Use year range slider input
        Year <= input$year_range[2]
      ) %>%
      group_by(Year, `Income Group`) %>%
      summarize(AvgGHGPerCapita = mean(`GHG per capita`, na.rm = TRUE))
    
    p2 <- ggplot(df_per_capita, aes(x = Year, y = AvgGHGPerCapita, color = `Income Group`)) +
      geom_line(size = 1) +  # Thicker line for better visibility
      scale_color_manual(values = c(
        "#003366",  # Assume this is for one income group
        "#6BAED6",  # Assume this is for another income group
        "#9ECAE1",  # Assume this is for yet another income group
        "#C6DBEF",  # Assume this is for another income group
        "grey50"    # Color for NA values
      ), na.value = "grey50") +  # Assign color to NA values
      labs(
        title = "GHG Emissions per Capita by Income Group",
        subtitle = "Analysis Across Different Income Groups",
        y = "Average GHG per Capita",
        x = "Year",
        color = "Income Group"
      ) +
      theme_minimal(base_family = "Arial") +  # Minimalist theme with clean font
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        legend.position = "top",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p2) %>% layout(autosize = TRUE, title = list(y = 0.95))
  })
  
  
  # CHART 3 - Treemap with gradient legend
  
  output$ghg_contribution <- renderPlotly({
    # Filter the data based on the selected year range
    selected_years <- input$year_range
    df_filtered <- df %>% 
      filter(Year >= selected_years[1] & Year <= selected_years[2]) %>%
      distinct(Year, CountryGHG, .keep_all = TRUE) # Keep unique Year-Country pairs
    
    # Summarize values by continent and country
    world_total <- sum(df_filtered$`GHG emissions`, na.rm = TRUE) # World-level total
    continent_summary <- df_filtered %>%
      group_by(Continent) %>%
      summarize(`GHG emissions` = sum(`GHG emissions`, na.rm = TRUE)) %>%
      arrange(desc(`GHG emissions`)) # Sort in descending order
    
    country_summary <- df_filtered %>%
      group_by(CountryGHG, Continent) %>%
      summarize(`GHG emissions` = sum(`GHG emissions`, na.rm = TRUE)) %>%
      arrange(desc(`GHG emissions`)) # Sort in descending order
    
    # Prepare hierarchical data with IDs
    labels <- c("<b>World</b>", continent_summary$Continent, country_summary$CountryGHG)
    parents <- c("", rep("<b>World</b>", nrow(continent_summary)), country_summary$Continent)
    
    # Set values to 0 for "World" and continents, and use actual values for countries
    values <- c(0, rep(0, nrow(continent_summary)), country_summary$`GHG emissions`)
    
    # Prepare custom hover text
    hover_text <- c(
      paste("<b>World Total:</b> ", format(world_total, big.mark = ",")),
      paste0(continent_summary$Continent, ": ", format(continent_summary$`GHG emissions`, big.mark = ",")),
      paste0(country_summary$CountryGHG, ": ", format(country_summary$`GHG emissions`, big.mark = ","))
    )
    
    # Gradient colors based on GHG emissions
    color_values <- c(0, rep(0, nrow(continent_summary)), country_summary$`GHG emissions`) # Match values
    color_scale <- colorRamp(c("#73AF48", "#F0E442", "#D7191C")) # Gradient: Green -> Yellow -> Red
    
    # Create the treemap using plotly
    fig <- plot_ly(
      type = 'treemap',
      labels = labels,
      parents = parents,
      values = values,
      textinfo = 'label+percent parent+percent entry',
      hoverinfo = 'text',
      text = hover_text, # Add custom hover text
      texttemplate = "<b>%{label}</b><br>%{percentParent:.1%} of continent<br>%{percentEntry:.1%} of world",
      marker = list(
        colors = color_values, # Pass the GHG emissions for coloring
        colorscale = list(
          list(0, "#cce5ff"), # Lowest emissions (Green)
          list(0.5, "#3399ff"), # Medium emissions (Yellow)
          list(1, "#003366")  # Highest emissions (Red)
        ),
        line = list(width = 0.5)
      )
    )
    
    # Layout adjustments
    fig <- fig %>% layout(
      title = "Share of GHG Emissions per Continent and Country with Gradient",
      margin = list(l = 0, r = 0, b = 0, t = 30)
    )
    
    fig
  })
  

  # Define a reactive dataset filtered by the selected year range
  df_filtered <- reactive({
    selected_years <- input$year_range
    df %>%
      filter(Year >= selected_years[1] & Year <= selected_years[2]) %>%
      distinct(Year, CountryGHG, .keep_all = TRUE)  # Ensure distinct rows for Year and CountryGHG
  })
  
  # Continent contribution table
  output$continent_contribution <- renderTable({
    filtered_data <- df_filtered()  # Use the reactive dataset
    world_total <- sum(filtered_data$`GHG emissions`, na.rm = TRUE)  # World-level total
    
    # Summarize by continent
    continent_summary <- filtered_data %>%
      group_by(Continent) %>%
      summarize(
        `GHG Emissions` = sum(`GHG emissions`, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(`% of World Total` = (`GHG Emissions` / world_total) * 100) %>%
      arrange(desc(`GHG Emissions`))
    
    # Return the summarized table
    continent_summary
  })
  
  # Gradient Legend
  output$gradient_legend <- renderUI({
    HTML("<b>Gradient Legend:</b> Green (Low Emissions) → Red (High Emissions)")
  })
  
  # Legend or Table for Gradient Colors
  output$gradient_legend <- renderUI({
    tagList(
      h4("Gradient Legend: GHG Emissions Levels"),
      HTML("
    <table style='width:100%; text-align:center; border-collapse:collapse;'>
      <tr>
        <th style='background-color:#cce5ff; padding:10px; border: 1px solid white; color:white;'>Low Emissions</th>
        <th style='background-color:#3399ff; padding:10px; border: 1px solid white; color:white;'>Medium Emissions</th>
        <th style='background-color:#003366; padding:10px; border: 1px solid white; color:white;'>High Emissions</th>
      </tr>
      <tr>
        <td style='border: 1px solid white;'>0 - 1,000,000 GHG</td>
        <td style='border: 1px solid white;'>1,000,001 - 10,000,000 GHG</td>
        <td style='border: 1px solid white;'>10,000,001+ GHG</td>
      </tr>
    </table>
    ")
    )
  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)
