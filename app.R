library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyjs)
# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)  # Make sure this is also loaded if you're using grid.arrange()


# Read the data and prepare the plot
df <- read.csv("C:\\Users\\arnab\\OneDrive\\Desktop\\Visualization Project\\Gold_Cleaned_Dataset.csv")

# Convert 'Dates' to date format
df$Dates <- as.Date(df$Dates, format = "%Y-%m-%d")

# Adding a 'Year-Month' column for time-based analysis
df$`Year_month` <- format(df$Dates, "%Y-%m")

# Summing occurrences of price directions
price_direction_counts <- colSums(df[, c('Price.Direction.Up', 'Price.Direction.Constant', 'Price.Direction.Down')])

# Create a data frame for plotting
price_direction_df <- data.frame(
  Direction = c('Up', 'Constant', 'Down'),
  Frequency = price_direction_counts
)

df$Dates <- as.Date(df$Dates, format = "%Y-%m-%d")

# Adding a 'Year-Month' column for time-based analysis
df$Year_Month <- format(df$Dates, "%Y-%m")

# Adding a 'Year' column for yearly analysis
df$Year <- format(df$Dates, "%Y")
# Convert 'Dates' to date format
unique_years <- unique(df$Year)


# Define UI for the application
ui <- fluidPage(
  # Link to external CSS (Optional if you are using your custom CSS)
  tags$link(rel = "stylesheet", type = "text/css", href = "sidebar.css"),
  
  # Load shinyjs to control UI interactions
  useShinyjs(),
  
  # Embed external HTML sidebar
  includeHTML("www/sidebar.html"),
  
  # Main panel for content
  mainPanel(
    fluidRow(
      column(10, offset = 4,  # This will shift the plot to the right
             box(
               
               status = "primary", 
               solidHeader = TRUE, 
               # Interactive text above the plot
               uiOutput("interactiveTextAbove"),  
               width = 20,
               plotOutput("priceDirectionPlot", height = "350px",width = "400px")
             ),
             
             
             # Adding interactive text below the plot using uiOutput
             uiOutput("interactiveText") , # This will render dynamic text
             box(
               
               status = "primary", 
               solidHeader = TRUE, 
               # Interactive text above the plot
               uiOutput("interactiveTextAboveforPie"),  
               width = 20,
               plotOutput("sentimentPieChart",height="350px",width="350px"))
             
      )
    )
  ),
  titlePanel("Gold Price Movements Over Time"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown for selecting the time interval (monthly or yearly)
      selectInput(
        inputId = "interval",
        label = "Select Time Interval:",
        choices = c("Monthly" = "month", "Yearly" = "year"),
        selected = "month"
      )
    ),
    
    mainPanel(
      # Plot output
      plotOutput("priceMovementPlot")
    )
  ),
  titlePanel("Monthly Price Direction Analysis"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_year", "Select Year:", choices = unique_years, selected = unique_years[1])
    ),
    
    mainPanel(
      plotOutput("monthlyPriceDirectionPlot")
    )
  ),
  # New Box for Sentiment Impact on Price Directions
  column(10,offset = 4,
         box(
           status = "primary",
           solidHeader = TRUE,
           uiOutput("interactiveTextAboveSentimentImpact"),
           width = 20,
           plotOutput("sentimentImpactPlot", height = "350px", width = "400px")
         )
  )
  ,
  # New Box for Yearly and Monthly Price Movement Trends
  column(10,offset = 4,
         box(
           status = "primary",
           solidHeader = TRUE,
           uiOutput("interactiveTextAboveTrends"),
           width = 20,
           plotOutput("yearlyMonthlyTrendPlot", height = "350px", width = "700px")
         )),
  
  
  
)

# Define server logic
# Define server logic
server <- function(input, output, session) {
  
  # Render the plot for price direction analysis
  output$priceDirectionPlot <- renderPlot({
    ggplot(price_direction_df, aes(x = Direction, y = Frequency, fill = Direction)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("blue", "lightblue", "skyblue")) +
      labs(title = 'Gold Price Movement Distribution', x = NULL, y = 'Frequency') +
      theme_minimal()
  })
  
  ## Sentiment analysis
  output$sentimentPieChart <- renderPlot({
    # Count the sentiment distribution
    sentiment_counts <- table(df$Price.Sentiment)
    
    # Convert table to a data frame for plotting
    sentiment_df <- as.data.frame(sentiment_counts)
    
    # Rename columns
    colnames(sentiment_df) <- c('Sentiment', 'Frequency')
    
    # Calculate percentages
    sentiment_df$Percentage <- (sentiment_df$Frequency / sum(sentiment_df$Frequency)) * 100
    
    # Create a pie chart
    ggplot(sentiment_df, aes(x = "", y = Frequency, fill = Sentiment)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y") +
      scale_fill_manual(values = c("deepskyblue", "dodgerblue", "steelblue", "slateblue")) +
      labs(title = "Sentiment Distribution") +
      theme_void() +  # Removes axis labels and ticks for a clean pie chart look
      geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
                position = position_stack(vjust = 0.5), size = 5, color = "white")  # Add percentage labels
  })
  
  # Interactive text above and below the plot
  output$interactiveTextAbove <- renderUI({
    tagList(
      tags$h4("Price Direction Analysis (Up, Constant, Down)",
              style = "font-size:20px; color:#2c3e50; font-weight:bold; margin-bottom:10px;")
    )
  })
  output$interactiveTextAboveforPie <- renderUI({
    tagList(tags$h4("Sentiment Analysis", style = "font-size:20px; color:#2c3e50; font-weight:bold; margin-bottom:10px;"))
  })
  
  output$interactiveText <- renderUI({
    tagList(
      tags$p(
        "Results: The bar plot provides a clear visual representation of the frequency of each price direction. As 
      seen below, the 'Up' direction is the most common, followed by 'Constant,' with 'Down' being the least frequent.",
        style = "font-size:16px; color:#2c3e50; font-weight:normal; margin-top:20px;"
      ),
      tags$p(
        "Key Insights: The data reveals a higher frequency of upward price movements compared to downward movements, indicating potential bullish periods for gold.",
        style = "font-size:15px; color:#16a085; font-weight:normal; margin-top:10px;"
      )
    )
  })
  
  ## Month and yearly analysis
  filtered_data <- reactive({
    if (input$interval == "month") {
      # Group by Year-Month for monthly analysis
      df %>%
        group_by(Year_Month) %>%
        summarise(
          Price_Direction_Up = sum(Price.Direction.Up, na.rm = TRUE),
          Price_Direction_Constant = sum(Price.Direction.Constant, na.rm = TRUE),
          Price_Direction_Down = sum(Price.Direction.Down, na.rm = TRUE)
        ) %>%
        mutate(Date = as.Date(paste0(Year_Month, "-01")))  # Ensure the Date column is the first day of the month
    } else {
      # Group by Year for yearly analysis
      df %>%
        group_by(Year) %>%
        summarise(
          Price_Direction_Up = sum(Price.Direction.Up, na.rm = TRUE),
          Price_Direction_Constant = sum(Price.Direction.Constant, na.rm = TRUE),
          Price_Direction_Down = sum(Price.Direction.Down, na.rm = TRUE)
        ) %>%
        mutate(Date = as.Date(paste0(Year, "-01-01")))  # Ensure the Date column is the first day of the year
    }
  })
  
  # Render plot for price movement (monthly/yearly)
  output$priceMovementPlot <- renderPlot({
    data <- filtered_data()
    
    ggplot(data, aes(x = Date)) +
      geom_line(aes(y = Price_Direction_Up, color = 'Up'), size = 1) +
      geom_line(aes(y = Price_Direction_Constant, color = 'Constant'), size = 1) +
      geom_line(aes(y = Price_Direction_Down, color = 'Down'), size = 1) +
      geom_point(aes(y = Price_Direction_Up, color = 'Up'), size = 2) +
      geom_point(aes(y = Price_Direction_Constant, color = 'Constant'), size = 2) +
      geom_point(aes(y = Price_Direction_Down, color = 'Down'), size = 2) +
      labs(title = 'Gold Price Movements Over Time', 
           y = 'Frequency', 
           color = 'Price Direction') +
      theme_minimal() +
      theme(legend.position = "right")
  })
  
  ## Monthly price direction plot
  output$monthlyPriceDirectionPlot <- renderPlot({
    # Filter data for selected year
    filtered_monthly_data <- df %>%
      filter(Year == input$selected_year) %>%
      group_by(Year_Month) %>%
      summarise(
        Price_Direction_Up = sum(Price.Direction.Up, na.rm = TRUE),
        Price_Direction_Constant = sum(Price.Direction.Constant, na.rm = TRUE),
        Price_Direction_Down = sum(Price.Direction.Down, na.rm = TRUE)
      )
    
    ggplot(filtered_monthly_data, aes(x = as.Date(paste0(Year_Month, "-01")))) +
      geom_line(aes(y = Price_Direction_Up, color = 'Up'), size = 1) +
      geom_line(aes(y = Price_Direction_Constant, color = 'Constant'), size = 1) +
      geom_line(aes(y = Price_Direction_Down, color = 'Down'), size = 1) +
      geom_point(aes(y = Price_Direction_Up, color = 'Up'), size = 2) +
      geom_point(aes(y = Price_Direction_Constant, color = 'Constant'), size = 2) +
      geom_point(aes(y = Price_Direction_Down, color = 'Down'), size = 2) +
      labs(title = paste("Monthly Price Direction in", input$selected_year), 
           x = "Month", 
           y = "Frequency", 
           color = "Price Direction") +
      theme_minimal() +
      theme(legend.position = "right") +
      scale_x_date(labels = scales::date_format("%b-%Y"), breaks = "1 month")  # Formatting x-axis to show Month-Year
  })
  
  
  ## sentiment impact of price 
  # Group by sentiment and aggregate price directions
  sentiment_impact <- df %>%
    group_by(`Price.Sentiment`) %>%
    summarise(
      Price_Direction_Up = sum(Price.Direction.Up, na.rm = TRUE),
      Price_Direction_Constant = sum(Price.Direction.Constant, na.rm = TRUE),
      Price_Direction_Down = sum(Price.Direction.Down, na.rm = TRUE)
    )
  
  # Convert to long format for ggplot
  sentiment_impact_long <- tidyr::pivot_longer(sentiment_impact, 
                                               cols = starts_with("Price_Direction"),
                                               names_to = "Price_Direction", 
                                               values_to = "Frequency")
  
  # Render the Sentiment Impact Plot
  output$sentimentImpactPlot <- renderPlot({
    ggplot(sentiment_impact_long, aes(x = `Price.Sentiment`, y = Frequency, fill = Price_Direction)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c('#B0C4DE', '#AFEEEE', '#87CEFA')) +
      labs(title = 'Sentiment Impact on Price Directions', 
           x = 'Price Sentiment', 
           y = 'Frequency') +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
  })
  
  # Interactive text for Sentiment Impact
  output$interactiveTextAboveSentimentImpact <- renderUI({
    tagList(
      tags$h4("Sentiment Impact on Price Directions", 
              style = "font-size:20px; color:#2c3e50; font-weight:bold; margin-bottom:10px;")
    )
  })
  # Grouping by year and aggregating price directions
  yearly_trends <- df %>%
    group_by(Year) %>%
    summarise(
      Price_Direction_Up = sum(Price.Direction.Up, na.rm = TRUE),
      Price_Direction_Constant = sum(Price.Direction.Constant, na.rm = TRUE),
      Price_Direction_Down = sum(Price.Direction.Down, na.rm = TRUE)
    )
  
  # Reshape the data for easier plotting
  yearly_trends_long <- yearly_trends %>%
    pivot_longer(cols = starts_with("Price_Direction"), 
                 names_to = "Price_Direction", 
                 values_to = "Frequency")
  
  # Plotting yearly trends
  yearly_plot <- ggplot(yearly_trends_long, aes(x = factor(Year), y = Frequency, fill = Price_Direction)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("Price_Direction_Up" ="#00008B", 
                                 "Price_Direction_Constant" = "#4169E1", 
                                 "Price_Direction_Down" = "#ADD8E6"),
                      labels = c("Price_Direction_Up" = "Up", 
                                 "Price_Direction_Constant" = "Constant", 
                                 "Price_Direction_Down" = "Down")) +  # Add labels for the legend
    labs(title = 'Yearly Price Movement Trends', 
         x = 'Year', 
         y = 'Frequency', 
         fill = 'Price Direction') +  # Add label for the legend
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  # Set x-axis text to vertical
  
  
  # Grouping by month and aggregating price directions
  monthly_trends <- df %>%
    group_by(Month) %>%
    summarise(
      Price_Direction_Up = sum(Price.Direction.Up, na.rm = TRUE),
      Price_Direction_Constant = sum(Price.Direction.Constant, na.rm = TRUE),
      Price_Direction_Down = sum(Price.Direction.Down, na.rm = TRUE)
    )
  
  # Reshape the data for monthly trends
  monthly_trends_long <- monthly_trends %>%
    pivot_longer(cols = starts_with("Price_Direction"), 
                 names_to = "Price_Direction", 
                 values_to = "Frequency")
  
  # Plotting monthly trends
  monthly_plot <- ggplot(monthly_trends_long, aes(x = factor(Month), y = Frequency, fill = Price_Direction)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("Price_Direction_Up" ="#00008B", 
                                 "Price_Direction_Constant" =  "#4169E1", 
                                 "Price_Direction_Down" = "#ADD8E6"),
                      labels = c("Price_Direction_Up" = "Up", 
                                 "Price_Direction_Constant" = "Constant", 
                                 "Price_Direction_Down" = "Down")) +  # Add labels for the legend
    labs(title = 'Monthly Price Movement Trends', 
         x = 'Month', 
         y = 'Frequency', 
         fill = 'Price Direction') +  # Add label for the legend
    theme_minimal()
  
  # Render the combined yearly and monthly trend plot
  output$yearlyMonthlyTrendPlot <- renderPlot({
    grid.arrange(yearly_plot, monthly_plot, ncol = 2)
  })
  
  # Interactive text for Yearly and Monthly Trends
  output$interactiveTextAboveTrends <- renderUI({
    tagList(
      tags$h4("Yearly and Monthly Price Movement Trends", 
              style = "font-size:20px; color:#2c3e50; font-weight:bold; margin-bottom:10px;")
    )
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
