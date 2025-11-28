
csvFileUI <- function(id, label = "CSV file") {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), label)
  )
}



dataUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    # Give the page a title
    titlePanel("Data initialiseren"),
    # Generate a row with a sidebar
    sidebarLayout(      
      # Define the sidebar with one input
      sidebarPanel(csvFileUI(id, "Kies CSV bestand met samples van centrum")),
      # Create a spot for the table
      mainPanel(
        p("Inhoud bestand"),
        DT::DTOutput("table")
      )
    )
  )
}


csvFileServer <- function(id, stringsAsFactors, skip) {
  moduleServer(id, function(input, output, session) {
      # The selected file, if any
      userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$file, message = FALSE))
        input$file
      })
      
      # The user's data, parsed into a data frame
      dataframe <- reactive({
        read.csv(userFile()$datapath, stringsAsFactors = stringsAsFactors, 
                 skip = input$skip)
      })
      
      # Return the reactive that yields the data frame
      return(dataframe)
    }
  )    
}


dataTableServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$table <- DT::renderDT({
      req(data())
      DT::datatable(data())
    })
  })
}



