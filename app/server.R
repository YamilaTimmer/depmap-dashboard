source("functions.R")
source("../config.R")

server <- function(input, output, session) {
  
  # Read in .tsv files with expression data and metadata
  expression_data <- read_feather(paste0(DATA_DIR, "expression_data_subset.tsv"))
  meta_data <- read_feather(paste0(DATA_DIR, "meta_data.tsv"))
  
  # Convert NA to Unknown
  meta_data$Sex[is.na(meta_data$Sex)] <- "Unknown"
  meta_data$PatientRace[is.na(meta_data$PatientRace)] <- "Unknown"
  meta_data$AgeCategory[is.na(meta_data$AgeCategory)] <- "Unknown"
  
  # Show/hide filter panels in UI, depending on chosen 'use_case'
  observeEvent(input$use_case, {
    if (input$use_case == "explore_expression") {
      show("genes_accordion")
      show("cancer_types_accordion")
      hide("singular_cancer_type")
      hide("individual_gene")
      
      nav_show("navcards", "Summary plots")
      nav_show("navcards", "Heatmap")
      nav_hide("navcards", "Gene Clustering")
      nav_hide("navcards", "Correlation Plot")
      
      
    } else {
      hide("genes_accordion")
      hide("cancer_types_accordion")
      show("singular_cancer_type")
      show("individual_gene")
      
      nav_hide("navcards", "Summary plots")
      nav_hide("navcards", "Heatmap")
      nav_show("navcards", "Gene Clustering")
      nav_show("navcards", "Correlation Plot")
      
      
    }
  })
  
  # Select first tab to be shown in the UI, depending on chosen 'use_case'
  observeEvent(input$use_case, {
    if (input$use_case == "gene_clustering") {
      updateTabsetPanel(inputId = "navcards", selected = "Gene Clustering")
    } else if (input$use_case == "explore_expression") {
      updateTabsetPanel(inputId = "navcards", selected = "Summary plots")
    }
  })
  
  # Updates all dropdown inputs using server-side selectize
  updateSelectizeInput(session, 
                       'gene_name', 
                       choices = expression_data$gene, 
                       selected = expression_data$gene[1], 
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       'gene_names', 
                       choices = expression_data$gene, 
                       selected = expression_data$gene[1], 
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       'onco_type', 
                       choices = sort(meta_data$OncotreePrimaryDisease), 
                       selected = "Acute Myeloid Leukemia",
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       'onco_types', 
                       choices = sort(meta_data$OncotreePrimaryDisease), 
                       selected = "Acute Myeloid Leukemia",
                       server = TRUE)
  
  
  updateSelectizeInput(session, 
                       "sex", 
                       choices = unique(meta_data$Sex), 
                       selected = c("Female", "Male", "Unknown"),
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "race", 
                       choices = meta_data$PatientRace, 
                       selected = c("caucasian", "asian", "black_or_african_american",
                                    "african", "american_indian_or_native_american", 
                                    "east_indian", "north_african", "hispanic_or_latino", "Unknown"),
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "age_category", 
                       choices = meta_data$AgeCategory, 
                       selected = c("Fetus", "Pediatric", "Adult", "Unknown"),
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       'correlation_gene', 
                       choices = expression_data$gene, 
                       selected = expression_data$gene[1], 
                       server = TRUE)
  
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
  
  # Generate output for XY plots
  output$plot <- renderPlotly({
    
    # Retrieve data from reactive function
    data <- selected_data()
    
    # Prevent error where plot tries to render before data has loaded in
    req(nrow(data) >= 1)
    
    plot <- xyplots(data, type = switch(input$summary_type, 
                                        "Box Plot" = "boxplot",
                                        "Violin Plot" = "violin",
                                        "Bar Plot" = "bar"))
    
    if (input$summary_type == "Box Plot"){
      output = ggplotly(plot)
      output$x$data[[1]]$marker = list(opacity = 0)
      output}
    else{
      plot
    }
  })
  
  output$heatmap <- renderPlotly({
    
    # Retrieve data from reactive function
    data <- selected_data()
    # Prevent error where plot tries to render before data has loaded in
    req(nrow(data) >= 1)
    
    heatmap <- generate_heatmap(input, data)
    
  })
  
  output$clusterplot <- renderPlotly({
    
    req(selected_data())
    data <- merged_data()
    
    tp <- reformat_cluster_data(data, input)
      
    # Generate plot
    clusterplot <- generate_clusterplot(tp)
  })
  
  output$corr_plot <- renderPlotly({
    
    req(selected_data())
    data <- merged_data()
    wide_exprdata <- reformat_data(data)
    print(wide_exprdata)
    
    #tp <- reformat_cluster_data(data, input)
    
    corr_plot <- generate_corr_plot(input,wide_exprdata)
    
  })
  
  
  output$data <- renderDataTable({
    req(selected_data())  # Ensure data is available
    
    # Retrieve data from reactive function
    data <- selected_data()
    
    # Generate the data table with additional features
    generate_datatable(data, filter = "top")
  })
  
}