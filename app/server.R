source("functions.R")

server <- function(input, output, session) {
    
    expression_data <- read_feather("../expression_subset.tsv")
    meta_data <- read_feather("../meta_data.tsv")
    
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
    
    selectize_input <- function(ID, choices, selected) {
        updateSelectizeInput(session, ID, 
                             choices = choices, 
                             server = TRUE, 
                             selected = selected)
    }
    
    # Updates all dropdown inputs using server-side selectize
    selectize_input(ID = 'gene_name', 
                    choices = expression_data$gene,
                    selected = sort(expression_data$gene[1]))
    
    selectize_input(ID = 'onco_type', 
                    choices = sort(meta_data$OncotreePrimaryDisease), 
                    selected = "Acute Myeloid Leukemia")
    
    selectize_input(ID = 'sex', 
                    choices = unique(meta_data$Sex), 
                    selected = c("Female", "Male", "Unknown"))
    
    selectize_input(ID = "race", 
                    choices = meta_data$PatientRace, 
                    selected = c("caucasian", "asian", "black_or_african_american",
                          "african", "american_indian_or_native_american", 
                          "east_indian", "north_african", "hispanic_or_latino", "unknown"))
    
    selectize_input(ID = "age_category", 
                    choices = meta_data$AgeCategory, 
                    selected = c("Fetus", "Pediatric", "Adult", "Unknown"))



    # output$plot <- renderPlotly({
    #     
    #     if (input$summary_type == "Box Plot"){ 
    #         # Call function with arguments to generate the plot
    #         merged_data <- reactive_merged()
    # 
    #         plot <- generate_boxplot(merged_data, input$onco_type)
    #     }
    #     
    # })
    
}
