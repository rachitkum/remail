# Load necessary libraries
library(shiny)
library(gmailr)
library(readr)
library(DT)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Bulk Email Sender"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("csv_file", "Choose CSV File", accept = ".csv"),
      textInput("subject", "Subject", "Your Subject Here"),
      textAreaInput("message", "Message", "Your message here."),
      actionButton("send_btn", "Send Emails")
    ),
    
    mainPanel(
      DTOutput("log_table")  # Ensure the log table is initialized in the UI
    )
  )
)

# Define server logic for the app
server <- function(input, output) {
  
  # Reactive value to hold the uploaded emails data
  emails <- reactiveVal(NULL)
  
  # Initialize a log reactive value to hold the log entries
  log_data <- reactiveVal(data.frame(Time = character(0), Message = character(0)))
  
  # Function to log messages
  log <- function(message) {
    current_log <- log_data()
    new_log <- data.frame(
      Time = Sys.time(),
      Message = message,
      stringsAsFactors = FALSE
    )
    updated_log <- rbind(current_log, new_log)
    log_data(updated_log)  # Update log data
    output$log_table <- renderDT(updated_log, options = list(pageLength = 5))  # Render log table
  }
  
  # Event to handle CSV file upload
  observeEvent(input$csv_file, {
    req(input$csv_file)
    
    tryCatch({
      # Read the uploaded CSV
      email_data <- read_csv(input$csv_file$datapath, show_col_types = FALSE)
      
      # Normalize column names to lowercase
      colnames(email_data) <- tolower(colnames(email_data))
      
      # Check if the 'email' column exists
      if (!"email" %in% colnames(email_data)) {
        stop("The uploaded CSV file must contain a column named 'email' (case-insensitive).")
      }
      
      # Save data in the reactive variable
      emails(email_data)
      log("CSV Uploaded Successfully.")
    }, error = function(e) {
      log(paste("Error: ", e$message))
      emails(NULL)
    })
  })
  
  # Send emails when the button is clicked
  observeEvent(input$send_btn, {
    req(emails())
    
    # Get the email data from the reactive value
    email_data <- emails()
    
    # Loop through each row and send emails
    for (i in 1:nrow(email_data)) {
      # Prepare the personalized message
      custom_message <- sprintf(
        "Hello,\n\n%s\n\n%s",
        email_data$email[i],
        input$message
      )
      
      # Prepare email
      email <- gm_mime() %>%
        gm_to(email_data$email[i]) %>%
        gm_from("YOUR_EMAIL@gmail.com") %>%
        gm_subject(input$subject) %>%
        gm_text_body(custom_message)
      
      # Send the email
      gm_send_message(email)
      
      # Log the result
      log(paste("Email sent to:", email_data$email[i]))
    }
    
    log("All emails have been sent.")
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
