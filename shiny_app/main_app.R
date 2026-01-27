library(shiny)
library(data.table)

source("~/R/onzeker_interlaboratory_variation/shiny_app/data_module.R")


# Main --------------------------------------------------------------------


ui <- fluidPage(
  navset_tab( 
    nav_panel("Data", dataUI("datafile")),   
    nav_panel("Rapport 1", "Inhoud rapport 1"), 
    nav_panel("Rapport 2", "Inhoud rapport 2"), 
    id = "page", 
  ) 
)

server <- function(input, output, session) {
  datafile <- csvFileServer("datafile", stringsAsFactors = FALSE, skip = 0)
  output$table <- DT::renderDT({datafile()})
}

shinyApp(ui, server)
