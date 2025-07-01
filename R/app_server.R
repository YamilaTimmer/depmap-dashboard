#' @importFrom feather read_feather
#' @importFrom dplyr %>% arrange count filter pull select distinct inner_join
#' @importFrom DT renderDataTable
#' @importFrom plotly renderPlotly ggplotly
#' @importFrom shiny observeEvent updateSelectizeInput updateTabsetPanel renderPlot req debounce
#' @importFrom shinyjs show hide
#' @importFrom bslib nav_show nav_hide

app_server <- function(input, output, session) {

    # Read in .tsv files with expression data and metadata
    expression_path <- system.file("app/data/expression_data.tsv", package = "depmapdashboard")
    expression_data <- read_feather(expression_path)

    meta_path <- system.file("app/data/meta_data.tsv", package = "depmapdashboard")
    meta_data <- read_feather(meta_path)

    # Load human pathways and sort by pathway ID
    pathways_path <- system.file("app/data/human_pathways.tsv", package = "depmapdashboard")
    human_pathways <- read_feather(pathways_path)
    human_pathways <- human_pathways %>%
        arrange(PathwayID)

    # Rename pathway options to include both ID and description in the name
    human_pathways_hsa <- paste0(human_pathways$PathwayID, ":", human_pathways$Description)

    # no longer needed if new version from pre-processing is loaded
    colnames(expression_data)[colnames(expression_data) == "expression"] <- "expr"

    # Convert NA to Unknown
    meta_data$Sex[is.na(meta_data$Sex)] <- "Unknown"
    meta_data$PatientRace[is.na(meta_data$PatientRace)] <- "Unknown"
    meta_data$AgeCategory[is.na(meta_data$AgeCategory)] <- "Unknown"


    # Filter to cancer types with more than 1 row, needed for onco type dropdown menus
    valid_onco_types <- meta_data %>%
        count(OncotreePrimaryDisease) %>%
        filter(n > 2) %>%
        pull(OncotreePrimaryDisease) %>%
        sort()

    # Show/hide filter panels in UI, depending on chosen 'use_case'
    observeEvent(input$use_case, {


      if (input$use_case == "explore_expression") {

        # Select first tab to be shown in the UI, depending on chosen 'use_case'
        updateTabsetPanel(inputId = "navcards", selected = "Summary plots")

        shinyjs::show("genes_accordion")
        shinyjs::show("cancer_types_accordion")
        shinyjs::hide("singular_cancer_type")
        shinyjs::hide("individual_gene")
        shinyjs::hide("pathway")
        shinyjs::hide("compare_pathway_cancertypes")
        shinyjs::hide("p_value_checkbox")

        nav_show("navcards", "Summary plots")
        nav_show("navcards", "Heatmap")
        nav_hide("navcards", "Clustering Plot")
        nav_hide("navcards", "Correlation Plot")
        nav_show("navcards", "help_explore")
        nav_hide("navcards", "help_cluster")
        nav_hide("navcards", "help_compare")


      }

      else if (input$use_case == "compare_pathway") {

        # Select first tab to be shown in the UI, depending on chosen 'use_case'
        updateTabsetPanel(inputId = "navcards", selected = "Heatmap")

        shinyjs::show("p_value_checkbox")
        shinyjs::show("pathway")
        shinyjs::hide("cancer_types_accordion")
        shinyjs::hide("singular_cancer_type")
        shinyjs::hide("individual_gene")
        shinyjs::hide("genes_accordion")
        shinyjs::show("compare_pathway_cancertypes")

        nav_show("navcards", "Heatmap")
        nav_hide("navcards", "Clustering Plot")
        nav_hide("navcards", "Correlation Plot")
        nav_hide("navcards", "Summary plots")
        nav_hide("navcards", "help_explore")
        nav_hide("navcards", "help_cluster")
        nav_show("navcards", "help_compare")


      }


      # for use case "gene_clustering"
      else  {

        # Select first tab to be shown in the UI, depending on chosen 'use_case'
        updateTabsetPanel(inputId = "navcards", selected = "Clustering Plot")

        shinyjs::hide("pathway")
        shinyjs::hide("genes_accordion")
        shinyjs::hide("cancer_types_accordion")
        shinyjs::show("singular_cancer_type")
        shinyjs::show("individual_gene")
        shinyjs::hide("compare_pathway_cancertypes")

        nav_hide("navcards", "Summary plots")
        nav_hide("navcards", "Heatmap")
        nav_show("navcards", "Clustering Plot")
        nav_show("navcards", "Correlation Plot")
        nav_hide("navcards", "help_explore")
        nav_show("navcards", "help_cluster")
        nav_hide("navcards", "help_compare")


      }

    })

    # Updates all dropdown inputs using server-side selectize
    updateSelectizeInput(session = session,
                         'gene_name',
                         choices = expression_data$gene,
                         selected = expression_data$gene[1],
                         server = TRUE)

    updateSelectizeInput(session = session,
                         'gene_names',
                         choices = expression_data$gene,
                         selected = expression_data$gene[1],
                         server = TRUE)


    updateSelectizeInput(session = session,
                         'pathway_name',
                         choices = human_pathways_hsa,
                         server = TRUE,
                         options = list(maxOptions = length(human_pathways_hsa)))

    updateSelectizeInput(session = session,
                         'onco_type',
                         choices = valid_onco_types,
                         selected = "Acute Myeloid Leukemia",
                         server = TRUE,
                         # Enables user to scroll through all options
                         options = list(maxOptions = length(meta_data$OncotreePrimaryDisease)))

    updateSelectizeInput(session = session,
                         'onco_types',
                         choices = valid_onco_types,
                         selected = "Acute Myeloid Leukemia",
                         server = TRUE,
                         options = list(maxOptions = length(meta_data$OncotreePrimaryDisease)))

    updateSelectizeInput(session = session,
                         'compare_pathway_onco_type',
                         choices = valid_onco_types,
                         selected = c("Acute Myeloid Leukemia", "Ampullary Carcinoma"),
                         server = TRUE,
                         options = list(maxOptions = length(meta_data$OncotreePrimaryDisease)))


    updateSelectizeInput(session = session,
                         "sex",
                         choices = unique(meta_data$Sex),
                         selected = c("Female", "Male", "Unknown"),
                         server = TRUE)

    updateSelectizeInput(session = session,
                         "race",
                         choices = meta_data$PatientRace,
                         selected = c("caucasian", "asian", "black_or_african_american",
                                      "african", "american_indian_or_native_american",
                                      "east_indian", "north_african", "hispanic_or_latino", "Unknown"),
                         server = TRUE)

    updateSelectizeInput(session = session,
                         "age_category",
                         choices = meta_data$AgeCategory,
                         selected = c("Fetus", "Pediatric", "Adult", "Unknown"),
                         server = TRUE)

    updateSelectizeInput(session = session,
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
        filter_gene(merged_data(), input, human_pathways)
    })

    # Debounce the selected data so that the plots do not refresh so quickly after selecting new filters.
    debounced_selected_data <- debounce(selected_data, 350)

    # Generate output for XY plots
    output$plot <- renderPlotly({
        set_theme()
        
        # Retrieve data from reactive function
        data <- debounced_selected_data()

        # Prevent error where plot tries to render before data has loaded in
        req(nrow(data) >= 1)

        plot <- xyplots(input, data, type = switch(input$summary_type,
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

    # Call functions needed for heatmap
    output$heatmap <- renderPlotly({
        set_theme()
        
        # Retrieve data from reactive function
        data <- debounced_selected_data()

        if (input$use_case == "compare_pathway"){

            data <- check_significancy(data,input)
        }
        # Prevent error where plot tries to render before data has loaded in
        req(nrow(data) >= 1)

        # Generate plot
        heatmap <- generate_heatmap(input, data)

    })

    # Call functions needed for clusterplot
    output$clusterplot <- renderPlotly({
        set_theme()
        
        req(debounced_selected_data()) # Ensure data is available

        # Load and reformat data for gene clustering
        data <- merged_data()
        wide_exprdata <- reformat_data(data)
        target_matrix  <- wide_exprdata %>% dplyr::select(-gene) %>% as.matrix()
        query_profile <- create_query(wide_exprdata, input)
        all_distances <- determine_distances(data, input, target_matrix,
                                             query_profile, wide_exprdata)

        tp <- determine_top_scoring(input, all_distances, data)

        # Generate plot
        clusterplot <- generate_clusterplot(tp, input)
    })

    # Call functions needed for correlation plot
    output$corr_plot <- renderPlotly({
        set_theme()
        req(debounced_selected_data()) # Ensure data is available

        # Obtain data in correct format
        data <- merged_data()
        wide_exprdata <- reformat_data(data)

        # Generate plot
        corr_plot <- generate_corr_plot(input,wide_exprdata)

    })

    # Call functions needed for datatable
    output$data <- renderDataTable({

        req(debounced_selected_data())  # Ensure data is available

        if (input$use_case == "gene_clustering"){
            data <- merged_data()
            wide_exprdata <- reformat_data(data)
            target_matrix  <- wide_exprdata %>% dplyr::select(-gene) %>% as.matrix()
            query_profile <- create_query(wide_exprdata, input)
            all_distances <- determine_distances(data, input, target_matrix,
                                                 query_profile, wide_exprdata)

            data <- determine_top_scoring(input, all_distances, data)

        } else if (input$use_case == "compare_pathway"){

            # Retrieve data from reactive function
            data <- debounced_selected_data()
            data <- check_significancy(data,input)
            log_fold_data <- calculate_logfold_change(data)

            # Transform log_2 data so that it can be joined to the table data
            log_2 <-  log_fold_data %>%
                dplyr::select(gene, log2_fc) %>%
                distinct(gene, .keep_all = TRUE) # Only keep one row per gene

            # Merge table data with log_fc
            data <- data %>%
                inner_join(log_2, by = "gene")


        }


        else{
            # Retrieve data from reactive function
            data <- debounced_selected_data()

        }

        # Generate the data table with additional features
        generate_datatable(data, filter = "top")
    })

    output$homepage_plot <- renderPlot({
        req(meta_data)
        generate_homepage_viz(meta_data)
    })
}

