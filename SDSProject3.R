
library(shiny)
library(ggplot2)
library(dplyr)

rsconnect::setAccountInfo(name='estherdabin',
                          token='CAC222F6866A91EB3393C54C4B01039E',
                          secret='7gFmcUyaDAZtKZlIZU6HTGQUM2pZzUGss/Tf4pTE')



texas_football_data <- read.csv("SDS313/TexasFootball(Sheet1).csv")

texas_football_data$Year <- as.numeric(texas_football_data$Year)
texas_football_data$W <- as.numeric(texas_football_data$W)
texas_football_data$L <- as.numeric(texas_football_data$L)
texas_football_data$Pct <- as.numeric(texas_football_data$Pct)
texas_football_data$SRS <- as.numeric(texas_football_data$SRS)


ui <- fluidPage(
  
  titlePanel("Texas College Football Stats"),
  
  p("Welcome to the Texas College Football Stats app! This app allows you to explore various statistics about Texas football teams over the years."),
  p("Select a variable to visualize and filter the data by year range. The app will update the plot and provide descriptive statistics based on your selection."),
  
  
  sidebarLayout(
    
    
    sidebarPanel(
      
      selectInput("variable", 
                  label = "Choose a variable", 
                  choices = c("Year", "W", "L", "Pct", "SRS", "Conf")),
      
      
      helpText("Select a variable from the dataset to visualize. You can choose both numeric and categorical variables."),
      
      
      sliderInput("yearRange", 
                  label = "Select Year Range", 
                  min = 1902, max = 2024, 
                  value = c(1902, 2024)),
      
      # Help text for year range filter
      helpText("Filter the data by selecting a range of years."),
      
      # Color choice
      selectInput("color", 
                  label = "Choose Graph Color", 
                  choices = c("red", "blue", "green", "purple", "orange")),
      
      # Help text for color choice
      helpText("Select the color of the graph bars."),
      
      # Action Button to update plot
      actionButton("update", "Update Graph"),
      
      # Checkbox for additional graph options
      checkboxInput("showStats", "Show Descriptive Statistics", value = TRUE),
      
      # Help text for checkbox
      helpText("Check this box to display descriptive statistics below the graph.")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      # Output for the plot
      plotOutput("distPlot"),
      
      # Output for descriptive statistics
      verbatimTextOutput("stats")
    )
  )
)

# Define server logic for the app
server <- function(input, output) {
  
  # Reactive data subset based on year range
  filtered_data <- reactive({
    texas_football_data %>% filter(Year >= input$yearRange[1], Year <= input$yearRange[2])
  })
  
  # Reactive plot for selected variable and color
  output$distPlot <- renderPlot({
    req(input$update)  # Ensure the action button is pressed
    
    selected_data <- filtered_data()
    selected_var <- input$variable
    
    # Check if the selected variable is numeric or categorical
    if (selected_var %in% c("W", "L", "Pct", "SRS")) {
      # Numeric variables, show histogram
      ggplot(selected_data, aes_string(x = selected_var)) +
        geom_histogram(fill = input$color, color = "black", bins = 20) +
        labs(title = paste("Distribution of", selected_var), 
             x = selected_var, y = "Frequency")
    } else {
      # Categorical variables, show bar plot
      ggplot(selected_data, aes_string(x = selected_var)) +
        geom_bar(fill = input$color, color = "black") +
        labs(title = paste("Bar Plot of", selected_var), 
             x = selected_var, y = "Count")
    }
  })
  
  # Reactive statistics output
  output$stats <- renderPrint({
    req(input$showStats)  # Ensure checkbox is selected
    
    selected_data <- filtered_data()
    selected_var <- input$variable
    
    if (selected_var %in% c("W", "L", "Pct", "SRS")) {
      # Numeric variables, show mean, SD, and five-number summary
      paste("Mean:", mean(selected_data[[selected_var]], na.rm = TRUE),
            "SD:", sd(selected_data[[selected_var]], na.rm = TRUE),
            "\nSummary:", summary(selected_data[[selected_var]]))
    } else {
      # Categorical variable, show frequency table
      table(selected_data[[selected_var]])
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

