library(shiny)

shinyApp(
  ui = fluidPage(
    tags$head(
      tags$style("
        #my_select_input ~ .selectize-control .option:nth-child(-n+2) {
          background-color: rgba(0,255,0,1);

        }

        #my_select_input ~ .selectize-control .option:nth-child(n+3) {
          background-color: rgba(255,0,0,1);
        }
        "
      )
    ),
    selectInput(
      inputId = "my_select_input",
      label = "Select Letter", 
      choices = c("A", "B", "C", "D")
    )
  ),
  server = function(input, output) {
    
  }
)