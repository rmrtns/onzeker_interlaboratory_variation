library(shiny)
library(bslib)


library(shiny)

# Define UI for the application
ui <- fluidPage(
  navset_tab( 
    nav_panel("Invoer",
              sidebarLayout(
                sidebarPanel(
                  fileInput("file", "CSV bestand met patiënten samples",
                            accept = c(".csv")),
                  uiOutput("column_selector")  # Placeholder for dynamic drop-downs
                ),
                mainPanel(
                  h3("Geselecteerde kolommen"),
                  uiOutput("selected_id_column"),
                  uiOutput("selected_outcome_column")
                )
              )
    ),
    nav_panel("Rapport A", "Inhoud rapport A"), 
    nav_panel("Rapport B", "Inhoud rapport B"))
)
  
# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to read the uploaded file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Dynamically generate drop-downs based on column names
  output$column_selector <- renderUI({
    req(data())
    tagList(
      selectInput("id_col", "Selecteer kolom met uniek patiënt ID:", 
                  choices = names(data())),
      selectInput("outcome_col", "Selecteer kolom met uitkomst:", 
                  choices = names(data()))
    )
  })
  
  # Display the selected column names
  output$selected_id_column <- renderUI({
    req(input$id_col)
    HTML(paste("<p>Patiënt ID kolom:<strong>", input$id_col, "</strong><i>", 
               class(data()[[input$id_col]]), "met",
               length(unique(data()[[input$id_col]])), "unieke waarden.</i></p>"))
  })
  output$selected_outcome_column <- renderUI({
    req(input$outcome_col)
    HTML(paste("<p>Uitkomst kolom:<strong>", input$outcome_col, "</strong><i>", 
               class(data()[[input$outcome_col]]), "met",
               length(unique(data()[[input$outcome_col]])), "unieke waarden.</i></p>"))
  })
}

shinyApp(ui, server)
