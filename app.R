library(rvest)
library(dplyr)
library(xml2)
library(shiny)
library(DT)

# Define base URL structure
base_url <- "https://grandreserva.unfccc.int/applications/grandreserva/public/schedule"

# Read the main page to extract the date options
main_page <- read_html("https://grandreserva.unfccc.int/grandreserva/public/schedule")

# Extract the select element's options for dates
date_options <- main_page %>%
  html_nodes("select option") %>%
  html_text()

# Convert date_options to a format compatible with URL parameters, e.g., "2024/11/11"
date_params <- lapply(date_options, function(date_text) {
  as.character(format(as.Date(date_text, format = "%d-%b-%Y, %a"), "%Y/%m/%d"))
})

# Format today's date to match the format in date_options, e.g., "24-Oct-2024, Thu"
today_date <- format(Sys.Date(), "%d-%b-%Y, %a")

# Check if today's date is in the options; if not, default to the first date in the list
selected_date <- ifelse(today_date %in% date_options, today_date, date_options[1])

# Function to scrape data for a given date parameter
scrape_data <- function(date_param, date_text) {
  # Construct the URL with the date parameter
  url <- paste0(base_url, "?time=", date_param, "&conference_id=93&meeting_type=&body=&webcast=0")
  
  # Read the HTML content of the page for this specific date
  page <- read_html(url)
  
  # Scrape the table or event details
  events_data <- page %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  # Convert the data to a data frame if it exists
  if (length(events_data) > 0) {
    events_df <- events_data[[1]]
    events_df <- events_df %>% mutate(Date = date_text)  # Add the date column for reference
    return(events_df)
  } else {
    return(NULL)  # Return NULL if no table was found
  }
}

# Shiny UI and Server
ui <- fluidPage(
  titlePanel("UNFCCC COP29 Baku Event Schedule"),
  
  sidebarLayout(
    sidebarPanel(
      # "Go to Today" button
      actionButton("goToToday", "Go to Today's Date"),
      
      # Dropdown for date selection based on scraped date options
      selectInput("selectedDate", "Select Date:",
                  choices = date_options,
                  selected = selected_date),  # Default to today's date
      
      # Action button to manually refresh the data
      actionButton("refreshData", "Update Page"),
      
      # Vertical space below the "Update Page" button
      tags$br(), tags$br(),
      
      # "Select All" and "Deselect All" buttons for event types
      actionButton("selectAll", "Select All"),
      actionButton("deselectAll", "Deselect All"),
      
      # Vertical space 
      tags$br(), 
      
      # Checkbox group input for selecting event types
      checkboxGroupInput("eventType", "Select Event Types:",
                         choices = c("Official Meeting", "Mandated Event", 
                                     "Coordination Meeting", "Side Event", 
                                     "Press Conference", "Uncertain"),
                         selected = c("Official Meeting", "Mandated Event", 
                                      "Coordination Meeting", "Side Event", 
                                      "Press Conference", "Uncertain"))
    ),
    mainPanel(
      DTOutput("filteredEvents")
    )
  ),
  
  # Footer
  tags$footer(
    tags$hr(),
    tags$p(
      "App created by ", tags$a(href = "https://moritzschwarz.org", "Moritz Schwarz."),"Report any bugs to ", 
      tags$a(href = "https://github.com/moritzpschwarz/UNFCCC_schedule/issues", "GitHub."),
      style = "text-align: center; color: #888; font-size: 0.9em;"
    )
  )
)

server <- function(input, output, session) {
  # Reactive value to store the scraped data for the selected date
  events_data <- reactiveVal()
  
  # Function to load data based on the selected date
  load_data <- function(date_text) {
    date_param <- as.character(format(as.Date(date_text, format = "%d-%b-%Y, %a"), "%Y/%m/%d"))
    data <- scrape_data(date_param, date_text)
    
    if (!is.null(data)) {
      # Categorize the data
      data <- data %>%
        mutate(EventType = case_when(
          grepl("Coordination|Preparatory|G77 & China", Room, ignore.case = TRUE) ~ "Coordination Meeting",
          grepl("Coordination|Preparatory|G77 & China|Coalition for Rainforest Nations", Title, ignore.case = TRUE) ~ "Coordination Meeting",
          grepl("Mandated", Title, ignore.case = TRUE) ~ "Mandated Event",
          grepl("Meeting Room|Plenary", Room, ignore.case = TRUE) ~ "Official Meeting",
          grepl("Side Event|Special Event", Room, ignore.case = TRUE) ~ "Side Event",
          grepl("Press Conference", Room, ignore.case = TRUE) ~ "Press Conference",
          TRUE ~ "Uncertain"  # If it doesn't match any specific category, mark as "Uncertain"
        )) %>%
        mutate(EventType = factor(EventType, levels = c("Official Meeting", "Mandated Event", 
                                                        "Coordination Meeting", "Side Event", 
                                                        "Press Conference", "Uncertain")))
    }
    
    events_data(data)
  }
  
  # Initial load for today's date
  load_data(selected_date)
  
  # Observe the "Go to Today" button click to reset the date to today and load data
  observeEvent(input$goToToday, {
    updateSelectInput(session, "selectedDate", selected = selected_date)
    load_data(selected_date)
  })
  
  # Observe changes in the selected date or when the "Update Page" button is clicked
  observeEvent(list(input$selectedDate, input$refreshData), {
    load_data(input$selectedDate)
  })
  
  # Observe "Select All" button to check all event types
  observeEvent(input$selectAll, {
    updateCheckboxGroupInput(session, "eventType",
                             selected = c("Official Meeting", "Mandated Event", 
                                          "Coordination Meeting", "Side Event", 
                                          "Press Conference", "Uncertain"))
  })
  
  # Observe "Deselect All" button to uncheck all event types
  observeEvent(input$deselectAll, {
    updateCheckboxGroupInput(session, "eventType", selected = character(0))
  })
  
  # Render the filtered table based on the selected event types
  output$filteredEvents <- renderDT({
    req(events_data())  # Ensure data is available
    events_data() %>%
      filter(EventType %in% input$eventType) %>%
      select(-Date) %>% 
      rename(`Event Type` = EventType) %>% 
      datatable(filter = "top", 
                rownames = FALSE,
                options = list(pageLength = nrow(.), paging = FALSE), 
                selection = "single")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
