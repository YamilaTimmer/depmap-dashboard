library(shiny)
library(bslib)
library(shinyjs)


server <- function(input, output, session) {
    
    # Show/hide filter panels in UI, depending on chosen 'use_case'
    observeEvent(input$use_case, {
        if (input$use_case == "compare_genes") {
            show("genes_accordion")
            hide("cancer_types_accordion")
        } else {
            show("cancer_types_accordion")
            hide("genes_accordion")
        }
    })
    
    
}
