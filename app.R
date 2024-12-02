library(shiny)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    preset = "bootstrap",
    primary = "#005EA2",
    secondary = "#73B3E7",
    success = "#00A91C",
    info = "#00BDE3",
    warning = "#FFBE2E",
    danger = "#D54309"
  ),
  
  tags$style("
    .card{overflow:hidden}.card-body{padding:.5rem;display:flex;justify-content:left;align-items:left;}
    #recommendation{max-height:200px;width:auto;object-fit:contain}
    div#full_guidelines.shiny-image-output.shiny-bound-output img {width:65%}
    @media screen and (max-width:768px){#recommendation{max-height:100px}div#full_guidelines.shiny-image-output.shiny-bound-output img {width:100%}}
    .green-result{color:green;font-weight:700}.yellow-result{color:orange;font-weight:700}
    .red-result{color:red;font-weight:700}.radio-inline{padding-left:10px}
    .radio-inline span{padding-left:2px}
  "),
  
  navset_pill(
    nav_panel("Calculator",
              div(
                class = "mt-3",
                titlePanel("Nutrition Calculator"),
                
                sidebarLayout(
                  sidebarPanel(
                    selectInput("beverage_type", "Select Beverage Type:", 
                                choices = c("Juice", "Milk", "Other")),
                    textInput("beverage_name", "Beverage Name:", 
                              placeholder = "Optional"),
                    uiOutput("dynamic_inputs"),
                    actionButton("submit", "Submit"),
                    downloadButton("download_data", "Download table")
                  ),
                  
                  mainPanel(
                    card(
                      imageOutput("recommendation")
                    ),
                    card(
                      class = "dt",
                      DT::dataTableOutput("submissions_table", width = "100%")
                    )
                  )
                )
              )
    ),
    
    nav_panel("About",
              div(
                class = "mt-3",
                
                #card(class = "about_text",
                card(class="about_page",
                h2("About"),
                
                p("The Sugar Smart Coalition (SSC) is committed to advocacy,
                           education, equitable practice, and policy that improves
                           healthy food and beverage options and choices.
                           SSC's vision is to reduce added sugar consumption 
                           and its negative health impacts on our Michigan communities."),
                p("SSC's beverage guidelines were developed by member
                           dietitians in the Nutrition Guidelines Committee based on standards
                           set by the American Heart Association, ChangeLab Solutions,
                           Healthy Eating Research, and the National Alliance for Nutrition and Activity."),
                p(HTML("To learn more about the Sugar Smart Coalition, visit our "),
                   tags$a(href = "https://www.facebook.com/SugarSmartCoalition", "Facebook page"),
                   "or ",
                   tags$a(href = "mailto:sugarsmartcoalition@gmail.com", "email us.")),
                
              #)
              #),
              
             # div(
               # class = "mt-3",
                
                #card(
                  
                  h2("Full Guidelines"),
                  
                  imageOutput("full_guidelines")
                  
                  
               )
                
              )
              
              
    )
  )
)

server <- function(input, output, session) {
  submissions <- reactiveVal(
    data.frame(
      Timestamp = character(),
      BeverageType = character(),
      BeverageName = character(),
      Recommendation = character(),
      Reason = character(),
      stringsAsFactors = FALSE
    )
  )
  
  output$full_guidelines <- renderImage({
    
    list(
      src = "www/guidelines_full.png",
      contentType = "image/png",
      width = "100%",
     # height = "auto",
      alt = "Result indicator"
    )
  }, deleteFile = FALSE)
    
  
  output$dynamic_inputs <- renderUI({
    req(input$beverage_type)
    
    switch(input$beverage_type,
           "Juice" = tagList(
             numericInput("juice_serving_size", "Serving Size (oz):", 
                          min = 0, value = NULL),
             radioButtons("is_100_percent", "Is this 100% Juice?", 
                          choices = c("Yes" = TRUE, "No" = FALSE), 
                          inline = TRUE)
           ),
           "Milk" = tagList(
             radioButtons("is_flavored", "Is the milk flavored?", 
                          choices = c("Yes" = TRUE, "No" = FALSE), 
                          inline = TRUE),
             radioButtons("is_sweetened", "Is the milk sweetened?", 
                          choices = c("Yes" = TRUE, "No" = FALSE), 
                          inline = TRUE)
           ),
           "Other" = tagList(
             numericInput("total_sugar", "Total Sugar (grams):", 
                          min = 0, value = NULL),
             numericInput("added_sugar", "Added Sugar (grams):", 
                          min = 0, value = NULL)
           )
    )
  })
  
  validate_beverage <- reactive({
    req(input$submit, input$beverage_type)
    
    if (input$beverage_type == "Milk") {
      req(input$is_flavored, input$is_sweetened)
      
      if (input$is_flavored == "FALSE" && input$is_sweetened == "FALSE") {
        recommendation_text <- "www/goforit.png"
        recommendation_color <- "green"
        reason <- NA
      } else {
        recommendation_text <- "www/maybenot.png"
        recommendation_color <- "red"
        reason <- if(input$is_flavored == "FALSE" && input$is_sweetened == "TRUE") {
          "Milk sweetened"
        } else if(input$is_flavored == "TRUE" && input$is_sweetened == "FALSE") {
          "Milk flavored"
        } else {
          "Milk flavored and sweetened"
        }
      }
      
    } else if (input$beverage_type == "Juice") {
      req(input$juice_serving_size, input$is_100_percent)
      
      if (input$juice_serving_size <= 0) {
        return(NULL)
      }
      
      if (input$is_100_percent == "TRUE" && input$juice_serving_size <= 12) {
        recommendation_text <- "www/oksometimes.png"
        recommendation_color <- "yellow"
        reason <- NA
      } else {
        recommendation_text <- "www/maybenot.png"
        recommendation_color <- "red"
        reason <- if(input$is_100_percent == "FALSE" && input$juice_serving_size <= 12) {
          "Not 100% juice"
        } else if(input$is_100_percent == "TRUE" && input$juice_serving_size > 12) {
          "Serving size > 12 oz"
        } else {
          "Not 100% juice, Serving size > 12 oz"
        }
      }
      
    } else if (input$beverage_type == "Other") {
      req(input$total_sugar, input$added_sugar)
      
      if (input$total_sugar < 0 || input$added_sugar < 0 || input$added_sugar > input$total_sugar) {
        return(NULL)
      }
      
      if (input$total_sugar <= 12 && input$added_sugar == 0) {
        recommendation_text <- "www/goforit.png"
        recommendation_color <- "green"
        reason <- NA
      } else if (input$total_sugar <= 24 && input$added_sugar <= 12) {
        recommendation_text <- "www/oksometimes.png"
        recommendation_color <- "yellow"
        reason <- NA
      } else {
        recommendation_text <- "www/maybenot.png"
        recommendation_color <- "red"
        reason <- if(!(input$total_sugar > 12 && input$total_sugar <= 24) && input$added_sugar <= 12) {
          "Total sugar > 24g"
        } else if((input$total_sugar > 12 && input$total_sugar <= 24) && !(input$added_sugar <= 12)) {
          "Added sugar > 12g"
        } else {
          "Total sugar > 24g, Added sugar > 12g"
        }
      }
    }
    
    new_submission <- data.frame(
      Timestamp = as.character(Sys.Date()),
      BeverageType = input$beverage_type,
      BeverageName = input$beverage_name,
      Recommendation = recommendation_color,
      Reason = reason,
      stringsAsFactors = FALSE
    )
    
    submissions(rbind(submissions(), new_submission))
    
    list(
      recommendation = recommendation_text,
      color = recommendation_color
    )
  }) |> bindEvent(input$submit)
  
  output$recommendation <- renderImage({
    result <- validate_beverage()
    req(result)
    
    list(
      src = result$recommendation,
      contentType = "image/png",
      width = "100%",
      height = "auto",
      alt = "Result indicator"
    )
  }, deleteFile = FALSE)
  
  # Modified table output with delete buttons
  output$submissions_table <- DT::renderDataTable({
    df <- submissions()
    req(df)
    
    if (nrow(df) > 0) {
      df$Delete <- paste('<button class="btn btn-danger btn-sm delete-btn" data-row="', 
                         1:nrow(df), 
                         '"><i class="fa fa-trash"></i></button>')
    }
    
    DT::datatable(
      df,
      colnames = c("Date", "Type", "Name", "Result", "Reason", ""),
      escape = FALSE,
      selection = 'none',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        processing = FALSE,
        searching = FALSE,
        dom = 't',
        columnDefs = list(
          list(
            targets = if (nrow(df) > 0) ncol(df) - 1 else NULL,
            className = 'dt-center'
          )
        )
      )
    )
  })
  
  # Handle delete button clicks
  observeEvent(input$submissions_table_cell_clicked, {
    info <- input$submissions_table_cell_clicked
    if (!is.null(info$col) && !is.null(info$row)) {
      if (info$col == ncol(submissions()) + 1) {  # +1 because of the added Delete column
        current_data <- submissions()
        if (info$row <= nrow(current_data)) {
          current_data <- current_data[-info$row, ]
          submissions(current_data)
        }
      }
    }
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("beverage_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data_to_download <- submissions()
      if ("Delete" %in% names(data_to_download)) {
        data_to_download$Delete <- NULL
      }
      write.csv(data_to_download, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)