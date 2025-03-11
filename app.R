library(shiny)
library(bslib)


str_to_sentence <- function(text) {
  # Convert the first letter to uppercase and keep the rest of the string unchanged
  paste0(toupper(substr(text, 1, 1)), tolower(substr(text, 2, nchar(text))))
}

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
    .snack_page img {width:65%;}
    @media screen and (max-width:768px){#recommendation{max-height:100px}div#full_guidelines.shiny-image-output.shiny-bound-output img {width:100%}.snack_page img {width:100%;}}
    .green-result{color:green;font-weight:700}.yellow-result{color:orange;font-weight:700}
    .red-result{color:red;font-weight:700}.radio-inline{padding-left:10px}
    .radio-inline span{padding-left:2px}
  "),
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css")
  ),
  
  navset_pill (
    
    nav_item(
      img(src = "logo_transparent_background.png", height = "50px"),
      style="margin-right:10px"
    ),    
    
    nav_spacer(),
    
    nav_panel("Beverages",
              div(
                class = "mt-3",
                titlePanel("Beverage Nutrition Calculator"),
                sidebarLayout(
                  sidebarPanel(
                    
                    p("K-12 facilities should first use the",
                      tags$a(href="https://foodplanner.healthiergeneration.org/calculator/", 
                             "USDA Smart Snacks in School Product Calculator"),
                      "to determine if a beverage is compliant with the USDA 
                      guidelines. The SSC Beverage Calculator can then be 
                      used to determine if a USDA-compliant beverage is in 
                      the green or yellow category."),
                    
                    selectInput("beverage_type", "Select Beverage Type:", 
                                choices = c("Juice", "Milk", "Other")),
                    textInput("beverage_name", "Beverage Name:", 
                              placeholder = "Optional"),
                    radioButtons("artificial", HTML(paste0("Does this contain artificial sweeteners?", tags$sup("1"))),
                                 choices = c("Yes" = TRUE, "No" = FALSE), inline = TRUE),
                    uiOutput("dynamic_inputs"),
                    
                    h6(HTML(paste0(tags$sup("1"), "Stevia and monk fruit are not considered to be artificial sweeteners.")), 
                       style = "font-size:.8em; font-weight:normal;"),
                    
                    
                    hr(),
                    actionButton("submit", "Submit"),
                    actionButton("save_data", span(icon("download"), "Save data"))#,
                   # downloadButton("save_data", "Save data")
                    
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
    
    nav_panel("Snacks",
              div(
                class = "mt-3",        
      card(class = "snack_page",
      h2("Snack Guidelines"),
      p("To determine if it is a green, yellow, or red category item use the", tags$a(href="https://foodplanner.healthiergeneration.org/calculator/", "USDA Smart Snacks in School Product Calculator.")),
      
      img(src="snack_guidelines.png")
      
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
                p("SSC's beverage guidelines were developed by member dietitians in the 
                  Nutrition Guidelines Committee based on standards set by the American Heart 
                  Association, ChangeLab Solutions, Healthy Eating Research, and the National 
                  Alliance for Nutrition and Activity. Using SSC’s beverage guidelines, drinks 
                  fall into one of three categories: Green / Go For It - no added sugar, 
                  artificial sweeteners, or sugar alcohol. Yellow / OK SOMETIMES - minimal 
                  added sugar, zero-calorie or low-calorie sweeteners). Red / MAYBE NOT - 
                  added sugar and caloric sweeteners."),
                p("To learn more about the Sugar Smart Coalition, or to share any feedback with us, visit our ",
                   tags$a(href = "https://www.facebook.com/SugarSmartCoalition", "Facebook page", target = "_blank"),
                   "or ",
                   tags$a(href = "mailto:sugarsmartcoalition@gmail.com", "email us.", target = "_blank")),


                  h2("Full Guidelines"),

                  imageOutput("full_guidelines")


               )

              )


    )
    
    

    
  )
)

server <- function(input, output, session) {
  
  # if(file.exists("current_data.csv")){
  #   
  #   submissions <- reactiveVal(read.csv("current_data.csv"))
  #   
  # }else{
  #   
  #   submissions <- reactiveVal(
  #     data.frame(
  #       Timestamp = character(),
  #       BeverageType = character(),
  #       BeverageName = character(),
  #       Recommendation = character(),
  #       Reason = character(),
  #       stringsAsFactors = FALSE
  #     )
  #   )
  #   
  # }

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
      req(input$is_flavored, input$is_sweetened, input$artificial)
      
      
      reason_df <- data.frame(
        input_id = c("is_sweetened", "is_flavored", "artificial"),
        reason_full = c("milk sweetened", "milk flavored", "contains artificial sweeteners")
      )
      
      
      if (input$is_flavored == "FALSE" && input$is_sweetened == "FALSE" && input$artificial == "FALSE") {
        recommendation_text <- "www/goforit.png"
        recommendation_color <- "green"
        reason <- NA
      } else {
        recommendation_text <- "www/maybenot.png"
        recommendation_color <- "red"
        
        inputs <- as.logical(c(is_flavored = input$is_flavored, is_sweetened = input$is_sweetened, artificial = input$artificial))
        
        
        reason <-  paste(reason_df[inputs,2], collapse = ", ") |> str_to_sentence()
        
      }
      
    } else if (input$beverage_type == "Juice") {
      req(input$juice_serving_size, input$is_100_percent)
      
      inputs <- as.logical(c(juice_serving_size = ifelse(input$juice_serving_size > 12, FALSE, TRUE), is_100_percent = input$is_100_percent))
      
      reason_df <- data.frame(
        input_id = c("juice_serving_size", "is_100_percent"),
        reason_full = c("serving size > 12oz", "not 100% juice")
      )
      
      
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
        
        
        reason <-  paste(reason_df[!inputs,2], collapse = ", ") |> str_to_sentence()
      
      }
      
    } else if (input$beverage_type == "Other") {
      req(input$total_sugar, input$added_sugar, input$artificial)
      
      inputs = as.logical(c(total_sugar = ifelse(input$total_sugar > 24, TRUE, FALSE), added_sugar = ifelse(input$added_sugar > 12, TRUE, FALSE), artificial = input$artificial))
      reason_df <- data.frame(
        input_id = c("total_sugar", "added_sugar", "artificial"),
        reason_full = c("total sugar > 24g", "added sugar >12g", "contains artificial sweeteners")
      )
      
      
      
      if (input$total_sugar < 0 || input$added_sugar < 0 || input$added_sugar > input$total_sugar) {
        return(NULL)
      }
      
      if (input$total_sugar <= 12 && input$added_sugar == 0 && input$artificial == "FALSE") {
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
        
        reason <- paste(reason_df[inputs,2], collapse = ", ") |> str_to_sentence()
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
  
  # output$download_data <- downloadHandler(
  #   filename = function() {
  #     paste0("beverage_data_", Sys.Date(), ".csv")
  #   },
  #   content = function(file) {
  #     data_to_download <- submissions()
  #     if ("Delete" %in% names(data_to_download)) {
  #       data_to_download$Delete <- NULL
  #     }
  #     write.csv(data_to_download, file, row.names = FALSE)
  #   }
  # )
  
  
  observeEvent(input$save_data, {
    
    
    if(file.exists("current_data.csv")){
      o <- read.csv("current_data.csv")
      updated <- rbind(submissions(), o)
      write.csv(updated, "current_data.csv")
    }
    
    write.csv(submissions(), "current_data.csv")
    
    showModal(modalDialog(
      title = "Success",
      "Your data has been successfully saved.",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
}

shinyApp(ui, server)