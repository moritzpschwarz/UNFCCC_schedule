library(rvest)
library(dplyr)
library(xml2)
library(shiny)
library(DT)

# Define base URL structure
base_url <- "https://grandreserva.unfccc.int/applications/grandreserva/public/schedule"

# Read the main page to extract the date options
main_page <- read_html("https://grandreserva.unfccc.int/grandreserva/public/schedule")
date_options <- main_page %>%
  html_nodes("select option") %>%
  html_text()

# Convert date_options to a format compatible with URL parameters
date_params <- lapply(date_options, function(date_text) {
  as.character(format(as.Date(date_text, format = "%d-%b-%Y, %a"), "%Y/%m/%d"))
})

# Today's date
today_date <- format(Sys.Date(), "%d-%b-%Y, %a")
selected_date <- ifelse(today_date %in% date_options, today_date, date_options[1])

# Function to scrape data
scrape_data <- function(date_param, date_text) {
  url <- paste0(base_url, "?time=", date_param, "&conference_id=93&meeting_type=&body=&webcast=0")
  page <- read_html(url)
  
  events_data <- page %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  if (length(events_data) > 0) {
    events_df <- events_data[[1]]
    events_df <- events_df %>% mutate(Date = date_text)
    return(events_df)
  } else {
    return(NULL)
  }
}

# UI and server
ui <- fluidPage(
  titlePanel("UNFCCC COP29 Baku Event Schedule"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("goToToday", "Go to Today's Date"),
      selectInput("selectedDate", "Select Date:", choices = date_options, selected = selected_date),
      actionButton("refreshData", "Update Page"),
      tags$br(), tags$br(),
      actionButton("selectAll", "Select All"),
      actionButton("deselectAll", "Deselect All"),
      tags$br(),
      checkboxGroupInput("eventType", "Select Event Types:",
                         choices = c("Official Meeting", "Mandated Event", "Constituted Body Event", 
                                     "Coordination Meeting", 
                                     "Side Event", "Press Conference", "Uncertain"),
                         selected = c("Official Meeting", "Mandated Event", "Constituted Body Event",
                                      "Coordination Meeting", 
                                      "Side Event", "Press Conference", "Uncertain")),
      
      # Set default choices for Event Topic
      radioButtons("selectedTopic", 'Select Event Topic\n(only for Official Meetings - select  "All" to display all others):',
                   choices = c("All", "Finance", "Loss and Damage", "Mitigation", "Adaptation", 
                               "GST", "Art. 6", "Transparency", "Capacity Building", "Technology", 
                               "Just Transition", "Science", "AFOLU", "Gender", "Compliance", 
                               "Bunkers", "Other/Uncertain"), 
                   selected = "All")
    ),
    mainPanel(
      DTOutput("filteredEvents")
    )
  ),
  
  # Footer
  tags$footer(
    tags$hr(),
    tags$p(
      "App created by ",tags$a(href = "https://moritzschwarz.org","Moritz Schwarz."), " Report any bugs to ", 
      tags$a(href = "https://github.com/moritzpschwarz/UNFCCC_schedule/issues", "GitHub."),
      style = "text-align: center; color: #888; font-size: 0.9em;"
    )
  )
)

server <- function(input, output, session) {
  events_data <- reactiveVal()
  initial_load <- reactiveVal(TRUE)  # Track if initial load is completed
  
  # Function to load data based on the selected date
  load_data <- function(date_text) {
    date_param <- as.character(format(as.Date(date_text, format = "%d-%b-%Y, %a"), "%Y/%m/%d"))
    data <- scrape_data(date_param, date_text)
    
    if (!is.null(data)) {
      data <- data %>%
        mutate(EventType = case_when(
          grepl("Coordination|Preparatory|G77 & China", Room, ignore.case = TRUE) ~ "Coordination Meeting",
          grepl("Coordination|Preparatory|G77 & China|Coalition for Rainforest Nations|GRULAC", Title, ignore.case = TRUE) ~ "Coordination Meeting",
          grepl("Mandated", Title, ignore.case = TRUE) ~ "Mandated Event",
          grepl("Constituted Body|Facilitative Working Group", Title, ignore.case = TRUE) ~ "Constituted Body Event",
          grepl("Meeting Room|Plenary", Room, ignore.case = TRUE) ~ "Official Meeting",
          grepl("Side Event|Special Event", Room, ignore.case = TRUE) ~ "Side Event",
          grepl("Press Conference", Room, ignore.case = TRUE) ~ "Press Conference",
          TRUE ~ "Uncertain"
        )) %>%
        mutate(Topic = case_when(
          EventType == "Official Meeting" & grepl("finance|Article 9|FRLD|GCF|GEF|NCQG|Adaptation Fund|Provision of financial and technical support|Green Climate Fund|Global Environment Facility", Title, ignore.case = TRUE) ~ "Finance",
          EventType == "Official Meeting" & grepl("just transition", Title, ignore.case = TRUE) ~ "Just Transition",
          EventType == "Official Meeting" & grepl("loss and damage|LnD", Title, ignore.case = TRUE) ~ "Loss and Damage",
          EventType == "Official Meeting" & grepl("adaptation|matters relating to the least developed countries", Title, ignore.case = TRUE) ~ "Adaptation",
          EventType == "Official Meeting" & grepl("mitigation", Title, ignore.case = TRUE) ~ "Mitigation",
          EventType == "Official Meeting" & grepl("transparency|Reporting from |Technical review", Title, ignore.case = TRUE) ~ "Transparency",
          EventType == "Official Meeting" & grepl("capacity building|capacity-building", Title, ignore.case = TRUE) ~ "Capacity Building",
          EventType == "Official Meeting" & grepl("technology", Title, ignore.case = TRUE) ~ "Technology",
          EventType == "Official Meeting" & grepl("science|research|Consultative Group of Experts|Greenhouse gas data interface", Title, ignore.case = TRUE) ~ "Science",
          EventType == "Official Meeting" & grepl("Article 6|Clean Development Mechanism", Title, ignore.case = TRUE) ~ "Art. 6",
          EventType == "Official Meeting" & grepl("agriculture", Title, ignore.case = TRUE) ~ "AFOLU",
          EventType == "Official Meeting" & grepl("gender", Title, ignore.case = TRUE) ~ "Gender",
          EventType == "Official Meeting" & grepl("compliance|Article 15", Title, ignore.case = TRUE) ~ "Compliance",
          EventType == "Official Meeting" & grepl("United Arab Emirates dialogue|global stocktake", Title, ignore.case = TRUE) ~ "GST",
          EventType == "Official Meeting" & grepl("international aviation and maritime transport|bunker", Title, ignore.case = TRUE) ~ "Bunkers",
          EventType == "Official Meeting" & grepl("Response measures", Title, ignore.case = TRUE) ~ "RM",
          EventType == "Official Meeting" & grepl("Action for Climate Empowerment", Title, ignore.case = TRUE) ~ "ACE",
          EventType == "Official Meeting" ~ "Other/Uncertain",
          TRUE ~ NA
        ))
      
      # Update topics dynamically
      unique_topics <- unique(data$Topic)
      preferred_order <- c("Finance", "Mitigation", "Adaptation", "Loss and Damage", 
                           "GST", "Art. 6", "Transparency", "Capacity Building", 
                           "Technology", "Just Transition", "RM", "Science", 
                           "AFOLU", "ACE", "Gender", "Compliance", "Bunkers")
      ordered_topics <- preferred_order[preferred_order %in% unique_topics]
      remaining_topics <- setdiff(unique_topics, preferred_order)
      final_topics <- c("All", ordered_topics, remaining_topics, "Other/Uncertain")
      final_topics <- unique(final_topics)
      final_topics <- final_topics[!is.na(final_topics)]
      
      updateRadioButtons(session, "selectedTopic", choices = final_topics, selected = "All")
    }
    
    events_data(data)
  }
  
  # Load initial data
  load_data(selected_date)
  
  # Apply query parameters on app load (only once)
  observe({
    if (initial_load()) {
      query <- parseQueryString(session$clientData$url_search)
      
      if (!is.null(query$date)) {
        date_text <- ifelse(query$date == "today", today_date, query$date)
        updateSelectInput(session, "selectedDate", selected = date_text)
        load_data(date_text)
      }
      
      if (!is.null(query$eventType)) {
        event_types <- unlist(strsplit(query$eventType, ","))
        updateCheckboxGroupInput(session, "eventType", selected = event_types)
      }
      
      if (!is.null(query$topic)) {
        updateRadioButtons(session, "selectedTopic", selected = query$topic)
      }
      
      initial_load(FALSE)  # Mark initial load as completed
    }
  })
  
  # Update URL query string based on inputs
  observe({
    new_query <- paste0(
      "?date=", ifelse(input$selectedDate == today_date, "today", input$selectedDate),
      "&eventType=", paste(input$eventType, collapse = ","),
      "&topic=", input$selectedTopic
    )
    updateQueryString(new_query, mode = "push")
  })
  
  # Select and Deselect All event types
  observeEvent(input$selectAll, {
    updateCheckboxGroupInput(session, "eventType", selected = c("Official Meeting", "Mandated Event", "Constituted Body Event","Coordination Meeting", "Side Event", "Press Conference", "Uncertain"))
  })
  
  observeEvent(input$deselectAll, {
    updateCheckboxGroupInput(session, "eventType", selected = character(0))
  })
  
  # Watch for changes in the selected date
  observeEvent(input$selectedDate, {
    load_data(input$selectedDate)
  })
  
  # Go to today's date
  observeEvent(input$goToToday, {
    updateSelectInput(session, "selectedDate", selected = today_date)
    load_data(today_date)
  })
  
  # Render filtered events
  output$filteredEvents <- renderDT({
    req(events_data())
    
    filtered_data <- events_data() %>%
      filter(EventType %in% input$eventType)
    
    if (input$selectedTopic != "All") {
      filtered_data <- filtered_data %>% filter(Topic == input$selectedTopic)
    }
    
    filtered_data %>%
      select(-Date) %>%
      rename(`Event Type` = EventType) %>%
      datatable(filter = "top", rownames = FALSE, 
                options = list(pageLength = nrow(.), paging = FALSE), selection = "single")
  })
}


shinyApp(ui = ui, server = server)
