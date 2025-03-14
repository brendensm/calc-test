library(shiny)
library(bslib)
library(httr)
library(jsonlite)
library(shinyjs)

str_to_sentence <- function(text) {
  # Convert the first letter to uppercase and keep the rest of the string unchanged
  paste0(toupper(substr(text, 1, 1)), tolower(substr(text, 2, nchar(text))))
}

useShinyjs()

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
  div#full_guidelines.shiny-image-output.shiny-bound-output img {width:100%}
  .snack_page img {width:65%;}
  
  /* Lightbox styling */
  .lightbox-overlay {
    display: none;
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background-color: rgba(0,0,0,0.9);
    z-index: 1000;
    justify-content: center;
    align-items: center;
  }
  .lightbox-content {
    max-width: 90%;
    max-height: 90%;
  }
  .lightbox-close {
    position: absolute;
    top: 15px;
    right: 15px;
    color: white;
    font-size: 30px;
    cursor: pointer;
  }
  .clickable-image {
    cursor: pointer;
  }
  
  @media screen and (max-width:768px){
    #recommendation{max-height:100px}
    div#full_guidelines.shiny-image-output.shiny-bound-output img {width:100%}
    .snack_page img {width:100%;}
  }
  .green-result{color:green;font-weight:700}
  .yellow-result{color:orange;font-weight:700}
  .red-result{color:red;font-weight:700}
  .radio-inline{padding-left:10px}
  .radio-inline span{padding-left:2px}
"),
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
    tags$script(src = "lightbox.js")

  
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
                    
                    h6(HTML(paste0(tags$sup("1"), "Artificial sweeteners include acesulfame potassium, advantame, aspartame, neotame, saccharin, and sucralose. Stevia and monk fruit are not considered to be artificial sweeteners.")), 
                       style = "font-size:.8em; font-weight:normal;"),
                    
                    
                    hr(),
                    actionButton("submit", "Submit"),
                   # actionButton("save_data", span(icon("download"), "Save data"))
                   actionButton("save_data", span(icon("download"), "Save data"))
                  
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
                page_fillable(
                  layout_columns( col_widths = c(7, 5),
                                  card(
                                    card_header("Snack Guidelines"),
                                    card_body(
                                      p("To determine if it is a green, yellow, or red category item use the", tags$a(href="https://foodplanner.healthiergeneration.org/calculator/", "USDA Smart Snacks in School Product Calculator.")),
                                    )
                                  ),
                                  card(
                                    # card_header("Snack Guidelines"),
                                    card_body(
                                      div(
                                        class = "clickable-image",  # Add this class to make it clear it's clickable
                                        imageOutput("snack_guidelines", height = "auto")
                                      )
                                    )
                                  )
                  )
                )
              )
    ),
    
    nav_panel("About",
              div(
                class = "mt-3",
                page_fillable(
                  layout_columns(
                    col_widths = c(7, 5),  
                    
                    card(
                      card_header("About the Sugar Smart Coalition"),
                      card_body(
                        p("The Sugar Smart Coalition (SSC) is committed to advocacy,
              education, equitable practice, and policy that improves
              healthy food and beverage options and choices.
              SSC's vision is to reduce added sugar consumption
              and its negative health impacts on our Michigan communities."),
                        p("SSC's beverage guidelines were developed by member dietitians in the 
              Nutrition Guidelines Committee based on standards set by the American Heart 
              Association, ChangeLab Solutions, Healthy Eating Research, and the National 
              Alliance for Nutrition and Activity. Using SSC's beverage guidelines, drinks 
              fall into one of three categories:"),
                        tags$ul(
                          tags$li(tags$strong("Green / Go For It"), " - no added sugar, artificial sweeteners, or sugar alcohol."),
                          tags$li(tags$strong("Yellow / OK Sometimes"), " - minimal added sugar, zero-calorie or low-calorie sweeteners."),
                          tags$li(tags$strong("Red / Maybe Not"), " - added sugar and caloric sweeteners.")
                        ),
                        p("To learn more about the Sugar Smart Coalition, or to share any feedback with us, visit our ",
                          tags$a(href = "https://www.facebook.com/SugarSmartCoalition", "Facebook page", target = "_blank"),
                          " or ",
                          tags$a(href = "mailto:sugarsmartcoalition@gmail.com", "email us.", target = "_blank"))
                      )
                    ),
                    
                    # Right column for the image
                    card(
                      card_header("Full Guidelines"),
                      card_body(
                        div(
                          class = "clickable-image",  # Add this class to make it clear it's clickable
                          imageOutput("full_guidelines", height = "auto")
                        )
                      )
                    )
                  )
                )
              )
    )
  ),
  div(
    id = "lightbox",
    class = "lightbox-overlay",
    div(class = "lightbox-close", "×"),
    tags$img(id = "lightbox-img", class = "lightbox-content")
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
      height = "auto",
      alt = "Full Guidelines",
      class = "clickable-image"  
    )
  }, deleteFile = FALSE)
  
  output$snack_guidelines <- renderImage({
    list(
      src = "www/snack_guidelines.png",
      contentType = "image/png",
      width = "100%",
      height = "auto",
      alt = "Full Guidelines",
      class = "clickable-image"  # Add this class to enhance user understanding
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
  
  
  observeEvent(input$save_started, {
    showModal(modalDialog(
      title = "Saving...",
      "Saving data to the shared spreadsheet. Please wait.",
      footer = NULL,
      easyClose = FALSE
    ))
  })
  
  observeEvent(input$save_result, {
    removeModal()
    
    if (input$save_result$status == "success") {
      showModal(modalDialog(
        title = "Success",
        input$save_result$message,
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    } else {
      showModal(modalDialog(
        title = "Error",
        input$save_result$message,
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })
  
  
  observeEvent(input$save_data, {
    # Show saving modal
    showModal(modalDialog(
      title = "Saving...",
      "Saving data to the shared spreadsheet. Please wait.",
      footer = NULL,
      easyClose = FALSE
    ))
    
    # Get current data
    current_data <- submissions()
    
    # Create JavaScript with better error handling
    js_code <- paste0(
      "console.log('Starting data save operation');\n",
      "try {\n",
      "  const tableData = ", jsonlite::toJSON(current_data), ";\n",
      "  console.log('Data prepared:', tableData.length, 'rows');\n",
      "  const scriptUrl = 'https://script.google.com/macros/s/AKfycby6D2dpPUHUrPSzl-mXoVWGuhpYOrORQScpEsWN8zHy_01-0NORjVRgtX0VnvAFkHkHeA/exec';\n",
      "  console.log('Using script URL:', scriptUrl);\n",
      "  \n",
      "  fetch(scriptUrl, {\n",
      "    method: 'POST',\n",
      "    headers: { 'Content-Type': 'application/json' },\n",
      "    body: JSON.stringify(tableData)\n",
      "  })\n",
      "  .then(response => {\n",
      "    console.log('Received response with status:', response.status);\n",
      "    if (!response.ok) {\n",
      "      throw new Error('Network response was not ok: ' + response.status);\n",
      "    }\n",
      "    return response.text();\n", 
      "  })\n",
      "  .then(text => {\n",
      "    console.log('Response text:', text);\n",
      "    try {\n",
      "      const data = JSON.parse(text);\n",
      "      console.log('Parsed JSON:', data);\n",
      "      Shiny.setInputValue('save_result', { status: 'success', message: 'Data saved successfully!' });\n",
      "    } catch (e) {\n",
      "      console.error('Error parsing JSON:', e);\n",
      "      Shiny.setInputValue('save_result', { status: 'error', message: 'Error parsing response: ' + e.message });\n",
      "    }\n",
      "  })\n",
      "  .catch(error => {\n",
      "    console.error('Fetch error:', error);\n",
      "    Shiny.setInputValue('save_result', { status: 'error', message: 'Error: ' + error.message });\n",
      "  });\n",
      "} catch (error) {\n",
      "  console.error('JavaScript error:', error);\n",
      "  Shiny.setInputValue('save_result', { status: 'error', message: 'JavaScript error: ' + error.message });\n",
      "}"
    )
    
    # Run the JavaScript
    shinyjs::runjs(js_code)
  })
  
  # Keep this observer for handling the result
  observeEvent(input$save_result, {
    removeModal()
    
    if (input$save_result$status == "success") {
      showModal(modalDialog(
        title = "Success",
        input$save_result$message,
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    } else {
      showModal(modalDialog(
        title = "Error",
        input$save_result$message,
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })

  
}

shinyApp(ui, server)