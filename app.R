library(shiny)
library(dplyr)
library(bslib)

# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}


# UI Start ----------------------------------------------------------------

ui <- fluidPage(
  
  theme = bs_theme(#version = 5, bootswatch = "flatly",
                   version = 5,
                   primary = "#005EA2",   # USWDS primary blue
                   secondary = "#73B3E7",
                   success = "#00A91C",
                   info = "#00BDE3",
                   warning = "#FFBE2E",
                   danger = "#D54309",
                   
                   #spacer = "1rem", 
                   font_size = "16px"),
  
  
  tags$head(
    tags$style(HTML("
    
    .green-result { color: green; font-weight: bold; }
  .yellow-result { color: orange; font-weight: bold; }
  .red-result { color: red; font-weight: bold; }
  .radio-inline { padding-left: 10px; }
  .radio-inline span { padding-left: 2px; }
  
  /* Remove fixed heights and use flex for image container */
  .card {
    overflow: hidden;
  }
  
  .card-body {
    padding: 0.5rem;
    display: flex;
    justify-content: center;
    align-items: center;
  }
  
  #recommendation {
    max-height: 250px;
    width: auto;
    object-fit: contain;
  }
  
  @media screen and (max-width: 768px) {
    #recommendation {
      max-height: 120px;
    }
  }

    "))
  ),
  
navset_pill(#type = "pills",
  nav_panel("Calculator",
  
  titlePanel("Nutrition Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      # Beverage Type Selection
      selectInput("beverage_type", "Select Beverage Type:", 
                  choices = c( "Juice", "Milk", "Other"), selected = "Juice"),
      
      textInput("beverage_name", "Beverage Name:", placeholder = "Optional"),
      
      # Conditional Panels for Different Beverage Types
      conditionalPanel(
        condition = "input.beverage_type == 'Juice'",

        numericInput("juice_serving_size", "Serving Size (oz):", 
                     min = 0, value = NULL),
        radioButtons("is_100_percent", "Is this 100% Juice?", 
                     choices = c("Yes" = TRUE, "No" = FALSE), inline = TRUE)
      ),
      
      conditionalPanel(
        condition = "input.beverage_type == 'Milk'",

        radioButtons("is_flavored", "Is the milk flavored?", 
                     choices = c("Yes" = TRUE, "No" = FALSE), inline = TRUE),
        radioButtons("is_sweetened", "Is the milk sweetened?", 
                     choices = c("Yes" = TRUE, "No" = FALSE), inline = TRUE)
      ),
      
      conditionalPanel(
        condition = "input.beverage_type == 'Other'",

        numericInput("total_sugar", "Total Sugar (grams):", 
                     min = 0, value = NULL),
        numericInput("added_sugar", "Added Sugar (grams):", 
                     min = 0, value = NULL)
      ),
      
      # Submit Button
      
      
      #radioButtons("show_data", "Show table:", choices = c("Yes", "No"), selected = "No", inline = TRUE),
      
      
      
      # Download Button
        
        actionButton("submit", "Submit"),
        
        downloadButton("download_data", "Download table")

    ),
    
    mainPanel(
      
      # Color-coded Recommendation
      #uiOutput("recommendation"),
      
      card(
      imageOutput("recommendation")
      ),#height="25%"),
      card(class ="dt",
     # conditionalPanel(
        #condition = "input.show_data == 'Yes'",
      
      
      #tableOutput("submissions_table")
      
      DT::dataTableOutput("submissions_table", width = "100%")
      #)
      )
    )
  )
),

nav_panel("About",
         
      h2("This is an about page."),
      
      p("Here is some text on the page.")
  
)

)
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  # Reactive values to store submissions
  submissions <- reactiveVal(data.frame(
    Timestamp = character(),
    BeverageType = character(),
    BeverageName = character(),
    Recommendation = character(),
    Reason = character(),
    stringsAsFactors = FALSE
  ))
  
  # Reactive validation function
  validate_beverage <- eventReactive(input$submit, {
    # Check if beverage type is selected
    if (input$beverage_type == "") {
      return(list(
        message = "Error: Please select a beverage type.",
        recommendation = "",
        color = ""
      ))
    }
    
    # Validation for Milk
    if (input$beverage_type == "Milk") {
      # Ensure both flavored and sweetened status are selected
      if (is.null(input$is_flavored) || is.null(input$is_sweetened)) {
        return(list(
          message = "Error: Please specify if milk is flavored and sweetened.",
          recommendation = "",
          color = ""
        ))
      }
      
      # Determine recommendation for milk
      if (input$is_flavored == "FALSE" && input$is_sweetened == "FALSE") {
        recommendation_text <- "www/goforit.png"
        recommendation_color <- "green"
        reason <-  NA
      } else {
        recommendation_text <- "www/maybenot.png"
        recommendation_color <- "red"
        
        if(input$is_flavored == "FALSE" && input$is_sweetened == "TRUE"){
          reason <- "Milk sweetened"
        }else if (input$is_flavored == "TRUE" && input$is_sweetened == "FALSE"){
          reason <- "Milk flavored"
        }else{
          reason <- "Milk flavored and sweetened"
        }
        
      }
      
      # Add submission to data frame
      new_submission <- data.frame(
        Timestamp = as.character(Sys.Date()),
        BeverageType = "Milk",
        BeverageName = input$beverage_name,
        Recommendation = recommendation_color,
        Reason = reason,
        stringsAsFactors = FALSE
      )
      
      # Update submissions
      current_submissions <- submissions()
      submissions(rbind(current_submissions, new_submission))
      
      # Construct milk validation message
      return(list(
        recommendation = recommendation_text,
        color = recommendation_color
      ))
    }
    
    # Validation for Juice
    if (input$beverage_type == "Juice") {
      # Check serving size
      if (is.null(input$juice_serving_size) || input$juice_serving_size <= 0) {
        return(list(
          message = "Error: Please enter a valid serving size for juice (greater than 0 oz).",
          recommendation = "",
          color = ""
        ))
      }

      # Determine recommendation for juice
      if (input$is_100_percent == "TRUE" && input$juice_serving_size <= 12) {
        recommendation_text <- "www/oksometimes.png"
        recommendation_color <- "yellow"
        reason <- NA
      } else {
        recommendation_text <- "www/maybenot.png"
        recommendation_color <- "red"
        
        if(input$is_100_percent == "FALSE" && input$juice_serving_size <= 12){
          reason <- "Not 100% juice"
        }else if (input$is_100_percent == "TRUE" && input$juice_serving_size > 12){
          reason <- "Serving size > 12 oz"
        }else{
          reason <- "Not 100% juice, Serving size > 12 oz"
        }
        
      }
      
      # Add submission to data frame
      new_submission <- data.frame(
        Timestamp = as.character(Sys.Date()),
        BeverageType = "Juice",
        BeverageName = input$beverage_name,
        Recommendation = recommendation_color,
        Reason = reason,
        stringsAsFactors = FALSE
      )
      
      # Update submissions
      current_submissions <- submissions()
      submissions(rbind(current_submissions, new_submission))
      
      # Construct juice validation message
      return(list(
        message = paste(
          "Juice Validation:",
          "\n- Beverage Name:", input$beverage_name,
          "\n- Serving Size:", input$juice_serving_size, "oz",
          "\n- 100% Juice:", ifelse(input$is_100_percent, "Yes", "No")
        ),
        recommendation = recommendation_text,
        color = recommendation_color
      ))
    }
    
    # Validation for Other Drinks
    if (input$beverage_type == "Other") {
      # Check total sugar input
      if (is.null(input$total_sugar) || is.na(input$total_sugar) || input$total_sugar < 0) {
        return(list(
          message = "Error: Please enter a valid total sugar amount (0 or greater).",
          recommendation = "",
          color = ""
        ))
      }
      
      # Check added sugar input
      if (is.null(input$added_sugar) || is.na(input$added_sugar) || input$added_sugar < 0) {
        return(list(
          message = "Error: Please enter a valid added sugar amount (0 or greater).",
          recommendation = "",
          color = ""
        ))
      }
      
      # Check that added sugar doesn't exceed total sugar
      if (input$added_sugar > input$total_sugar) {
        return(list(
          message = "Error: Added sugar cannot be greater than total sugar.",
          recommendation = "",
          color = ""
        ))
      }
      
      # Determine recommendation for other drinks
      if (input$total_sugar <= 12 && input$added_sugar == 0) {
        recommendation_text <- "www/goforit.png"
        recommendation_color <- "green"
        
        reason <- NA
        
      } else if (input$total_sugar <= 24 && input$added_sugar <= 12 ) {
        recommendation_text <- "www/oksometimes.png"
        recommendation_color <- "yellow"
        
        reason <- NA
        
      } else {
        recommendation_text <- "www/maybenot.png"
        recommendation_color <- "red"
        
        if(!(input$total_sugar > 12 && input$total_sugar <= 24) && input$added_sugar <= 12){
          reason <- "Total sugar > 24g"
        }else if ((input$total_sugar > 12 && input$total_sugar <= 24) && !(input$added_sugar <= 12)){
          reason <- "Added sugar > 12g"
        }else{
          reason <- "Total sugar > 24g, Added sugar > 12g"
        }
        
      }
      
      # Add submission to data frame
      new_submission <- data.frame(
        Timestamp = as.character(Sys.Date()),
        BeverageType = "Other",
        BeverageName = input$beverage_name,
        Recommendation = recommendation_color,
        Reason = reason,
        stringsAsFactors = FALSE
      )
      
      # Update submissions
      current_submissions <- submissions()
      submissions(rbind(current_submissions, new_submission))
      
      # Construct other drink validation message
      return(list(
        message = paste(
          "Other Drink Validation:",
          "\n- Beverage Name:", input$beverage_name,
          "\n- Total Sugar:", input$total_sugar, "g",
          "\n- Added Sugar:", input$added_sugar, "g"
        ),
        recommendation = recommendation_text,
        color = recommendation_color
      ))
    }
  })
  
  
  # Render color-coded recommendation
  output$recommendation <- renderImage({
    
    validate_result <- validate_beverage()

      if (!is.null(validate_result$recommendation) && validate_result$recommendation != "") {
        # div(
        #   h3(validate_result$recommendation)
        # )
        
        list(
          src = validate_result$recommendation,
          contentType = "image/png",
          width = "100%",
          height = "auto",
          alt = "Result indicator"#,
          
        # style= " max-width: 300px; object-fit: contain;"
        )
        
      }
    
  }, deleteFile = FALSE)
    
    
    
    

  
  # Render submissions table
  output$submissions_table <- DT::renderDataTable(submissions(), options = list(
      scrollX = TRUE,  # Horizontal scroll on small screens
      responsive = TRUE
    ))
    
    #renderTable({
   # submissions()
  #})
  
  # Download handler for CSV
  output$download_data <- downloadHandler(
    filename = function() {
      paste("beverage_data_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(submissions(), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui, server)