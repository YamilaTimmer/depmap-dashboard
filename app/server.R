source("functions.R")

server <- function(input, output, session) {
    
    # Read in .tsv files with expression data and metadata
    expression_data <- read_feather("../expression_subset.tsv")
    meta_data <- read_feather("../meta_data.tsv")
    
    # Move this to pre-processing (change column name so expression and metadata 
    # can be merged on column with same name)
    colnames(expression_data)[1] <- "ModelID"
    
    # Show/hide filter panels in UI, depending on chosen 'use_case'
    observeEvent(input$use_case, {
        if (input$use_case == "explore_expression") {
            show("genes_accordion")
            show("cancer_types_accordion")
        } else {
            show("genes_accordion")
            hide("cancer_types_accordion")
            
        }
    })
    
    
    # Updates all dropdown inputs using server-side selectize
    updateSelectizeInput(session, 
                         'gene_name', 
                         choices = expression_data$gene, 
                         selected = expression_data$gene[1], 
                         server = TRUE)
    
    updateSelectizeInput(session, 
                         'onco_type', 
                         choices = sort(meta_data$OncotreePrimaryDisease), 
                         selected = "Acute Myeloid Leukemia",
                         server = TRUE)
    
    
    updateSelectizeInput(session, 
                         "sex", 
                         choices = unique(meta_data$Sex), 
                         selected = c("Female", "Male", "Unknown"))
    
    updateSelectizeInput(session, 
                         "race", 
                         choices = meta_data$PatientRace, 
                         selected = c("caucasian", "asian", "black_or_african_american",
                                      "african", "american_indian_or_native_american", 
                                      "east_indian", "north_african", "hispanic_or_latino", "unknown"))
    
    updateSelectizeInput(session, 
                         "age_category", 
                         choices = meta_data$AgeCategory, 
                         selected = c("Fetus", "Pediatric", "Adult", "Unknown"))
    
    # Calls function to filter metadata based on user input for cancer type and other metadata
    filtered_metadata <- reactive({
        filter_metadata(meta_data, input)
    })
    
    # Calls function to merge filtered metadata with the corresponding expression data
    merged_data <- reactive({
        merge_data(filtered_metadata(), expression_data)
    })
    
    # Calls function to filter the merged data on the user-chosen gene(s)
    selected_data <- reactive({
        filter_gene(merged_data(), input)
    })
    
    # Generate output for plot (x/y plots for now)
    output$plot <- renderPlotly({
        
        # Retrieve data from reactive function
        data <- selected_data()
        
        # Prevent error where plot tries to render before data has loaded in
        req(nrow(data) >= 1)
        
        # If statement that determines which plot should be generated (based on user input)
        if (input$summary_type == "Box Plot"){
            
            plot <- generate_boxplot(data)
        }
        
        else if (input$summary_type == "Violin Plot"){
            
            plot <- generate_violinplot(data)
        }
        
        else if (input$summary_type == "Bar Plot"){
            
            plot <- generate_barplot(data)
        }
        
    })
    
    # Generate output for data table
    output$data <- renderDataTable({
      # Retrieve data from reactive function
      data <- selected_data()
      
      # Prevent error where plot tries to render before data has loaded in
      req(nrow(data) >= 1)
      
      generate_datatable(data)
    })
    
    
}
