#' @importFrom ggplot2 theme_set theme_minimal theme element_text

# Set theme for all plots globally:
theme_set(
    theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            strip.text = element_text(size = 14, face = "bold")
        )
)


#' Filter metadata
#'
#' This function filters the metadata based on the user inputs. For usecase
#' "explore_expression" multiple cancer types can be selected and for usecase
#' "gene_clustering", only one cancer type can be chosen.
#'
#' @param meta_data dataframe with all the metadata
#' @param input user input from filter options in application
#' @return filtered_metadata, dataframe of metadata that is filtered on user inputs
#' @examples
#' filter_metadata(meta_data, input)

filter_metadata <- function(meta_data, input) {

    filtered_metadata <- meta_data %>%
        filter(Sex %in% input$sex
               & PatientRace %in% input$race
               & AgeCategory %in% input$age_category)

    if (input$use_case == 'gene_clustering') {

        # For gene clustering, a different input is used where max 1 onco
        # type can be selected
        filtered_metadata <- filtered_metadata %>%
            filter(OncotreePrimaryDisease == input$onco_type)
    }


    else if (input$use_case == 'compare_pathway'){

        # 2 oncotypes can be selected
        filtered_metadata <- filtered_metadata %>%
            filter(OncotreePrimaryDisease %in% input$compare_pathway_onco_type)
    }


    else {

        # For other use cases multiple onco types can be selected
        filtered_metadata <- filtered_metadata %>%
            filter(OncotreePrimaryDisease %in% input$onco_types)
    }

    return(filtered_metadata)
}


#' Merge data
#'
#' This function merges the filtered metadata with the corresponding expression data,
#' using the model-ID column.
#'
#' @param filtered_metadata dataframe with filtered metadata (based on user input)
#' @param expression_data dataframe with all expression data
#' @return merged_data, dataframe of metadata with corresponding expression data
#' @examples
#' merge_data(filtered_metadata, expression_data)

merge_data <- function(filtered_metadata, expression_data) {

    merged_data <- merge(filtered_metadata,
                         expression_data,
                         by = "ModelID",
                         all = FALSE)

    return(merged_data)

}


#' Filter gene
#'
#' This function filters the merged data based off of the user-chosen gene(s).
#' The function contains an if statement which determines how filtered_gene will
#' be created, based off of two different inputs, one for "explore expression",
#' which allows the user to choose multiple genes and one for "gene clustering",
#' which allows te user to only choose one gene.
#'
#' @param merged_data dataframe of metadata with corresponding expression data
#' @param input user input from filter options in application
#' @param human_pathways dataframe of human pathways with corresponding genes per pathway
#' @return filtered_gene, dataframe of metadata + expression data that is filtered on all user inputs
#' @importFrom dplyr filter
#' @importFrom feather read_feather
#' @examples
#' filter_gene(merged_data, input, human_pathways)
#'

filter_gene <- function(merged_data, input, human_pathways) {

    if (input$use_case == 'explore_expression'){

        filtered_gene <- merged_data %>%
            filter(gene %in% input$gene_names)
    }

    else if (input$use_case == 'compare_pathway'){

        # Chosen ID is everything that comes before ":" in input$pathway_name
        chosen_pathway_ID <- sub(":.*", "", input$pathway_name)

        # Create table with humanpathway ID's and corresponding genes
        pathway_table_data <- system.file("app/data/pathway_table.tsv", package = "depmapdashboard")
        pathway_table <- read_feather(pathway_table_data)

        gene_names <- pathway_table %>% filter(pathway_table$PathwayID == chosen_pathway_ID)

        filtered_gene <- merged_data %>%
            filter(gene %in% gene_names$Symbol)
    }
    else {

        filtered_gene <- merged_data %>%
            filter(gene == input$gene_name)
    }


    return(filtered_gene)
}



#' Significancy checker
#'
#' This function checks if differences in gene expression across cancer types are
#' statistically significant
#'
#' @param filtered_gene dataframe of metadata + expression data that is filtered on all user inputs
#' @param input user input from filter options in application
#' @return data that only contains genes that are significantly differentially expressed over the two cancer types
#' @importFrom stats t.test
#' @importFrom dplyr filter inner_join
#' @examples
#' check_significancy <- function(filtered_gene, input)


check_significancy <- function(filtered_gene, input) {

    data <- filtered_gene

    # Create empty dataframe to save p_value per gene (using expression over 2 cancer types)
    p_value_df <- data.frame(
        gene = character(),
        p_value = numeric()
    )

    genes <- unique(data$gene)


    for (gene_name in genes) {

        # Retrieve data per gene
        gene_data <- data[data$gene == gene_name, ]

        # Only continue if exactly 2 cancer types are selected
        if (length(unique(gene_data$OncotreePrimaryDisease)) == 2) {

            # Perform t test on genes from 2 groups (the 2 selected cancer types)
            # and determines whether the differences are statistically significant
            res <- t.test(expr ~ OncotreePrimaryDisease, data = gene_data)

            # Add gene name + corresponding p_value to df
            p_value_df <- rbind(p_value_df, data.frame(
                gene = gene_name,
                p_value = res$p.value))
        }
    }

    # Vector of only significant gene names
    significant_genes <- p_value_df$gene[p_value_df$p_value <= 0.05]


    if (input$p_value_checkbox == TRUE){
        # Subset data on significant genes
        data <- data[data$gene %in% significant_genes, ]
    }


    # Add p_value as column to data
    data <- data %>%
        inner_join(p_value_df, by = "gene")

    return(data)

}

#' Generate XY plots
#'
#' This function generates different types of expression plots (boxplot, violin plot, or bar plot)
#' using the provided dataset. The plot shows gene expression levels across cancer types.
#'
#' @param data a dataframe containing atleast gene names, expression values, and cancer types.
#' @param type a string specifying the type of plot to generate. Options:
#'   - `"boxplot"`: Boxplot of expression values
#'   - `"violin"`: Violin plot of expression values
#'   - `"bar"`: Bar plot showing mean expression with error bars
#' @return A ggplot2 plot object
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_violin geom_bar stat_summary facet_wrap labs theme element_blank geom_point theme element_rect position_dodge mean_se
#' @importFrom dplyr filter
#' @importFrom plotly ggplotly layout
#' @importFrom paletteer scale_fill_paletteer_d
#' @examples
#' xyplots(merged_data, type = "boxplot")
#' xyplots(merged_data, type = "violin")
#' xyplots(merged_data, type = "bar")

xyplots <- function(input, data, type = "boxplot") {
    p <- ggplot(data, aes(x = OncotreePrimaryDisease,
                          y = expr,
                          fill = OncotreePrimaryDisease
    ))


    chosen_palette <- paletteer::palettes_d_names %>%
        filter(paletteer::palettes_d_names$palette %in% input$xyplot_palette)
    palette <- paste0(chosen_palette$package, "::", chosen_palette$palette)

    # Adjusting settings according to plot type
    if (type == "boxplot") {
        p <- p + geom_boxplot()
    } else if (type == "violin") {
        p <- p + geom_violin()
    } else if (type == "bar") {
        p <- p +
            stat_summary(geom = "bar",
                         fun = "mean",
                         position = "dodge") +

            stat_summary(geom = "errorbar",
                         fun.data = mean_se,
                         width = 0.2,
                         position = position_dodge(0.9))

    } else {
        stop("Invalid plot type. Choose 'boxplot', 'violin', or 'bar'.")
    }

    # Adding titles and labels to plot
    p <- p +
        facet_wrap(~gene, scales = "free_y") +
        labs(
            x = "",
            y = "Expression (log 2TPM)",
            title = "Expression across cancer types",
            fill = "Cancer type:"
        ) +

        # Remove X-axis labels, because they are too long
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) +

        # Apply user-chosen color palette
        scale_fill_paletteer_d(palette)

    if (input$geom_point_checkbox == TRUE){
        p <- p + geom_point()
    }
    if (input$border_checkbox == TRUE){
        p <- p + theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5))
    }
    if (input$y_labs_checkbox == TRUE){
      p <- p + facet_wrap(~gene, scales = "fixed")
    }

    # Makes plot better visible (e.g. y-axis title was cut-off before)
    p <- ggplotly(p,
                  height = input$plot_height,
                  width = input$plot_width) %>%
        layout(margin = list(l = 60, r = 20, b = 0, t = 80))



    return(p)
}

#' Calculate logfold change
#'
#' This function calculates mean expression and logfoldchange, which is a preparatory step
#' needed to visualize the data in the heatmap in the 'compare pathway' usecase.
#'
#' @param data: a dataframe containing atleast gene names, expression values, and cancer types.
#' @return A dataframe containing gene name, onco type, mean expr,
#' total_mean (used for reordering heatmap) and log2_fc
#' @importFrom dplyr group_by summarise mutate ungroup
#' @importFrom tidyr pivot_wider pivot_longer
calculate_logfold_change <- function(data){


    # Convert heatmap data to group cellines by gene, per onco type and
    # calculate mean expr across cellines, to be displayed in heatmap
    heatmap_data <- data %>%
        group_by(gene, OncotreePrimaryDisease) %>%
        summarise(mean_expr = mean(expr), .groups = "drop") %>%

        # Calculate total mean between the two onco types, to reorder genes
        # based on this order in the heatmap
        group_by(gene) %>%
        mutate(total_mean = mean(mean_expr, .groups = "drop")) %>%
        ungroup()

    # Calculate log fold change per gene per cancer type (log2(geneA/geneB))
    log_fold_data <- heatmap_data %>%
        pivot_wider(
            names_from = OncotreePrimaryDisease,
            values_from = mean_expr) %>%
        mutate(log2_fc = log2(.[[2]]/ .[[3]])) %>%
        pivot_longer(
            cols = 3:4,
            names_to = "OncotreePrimaryDisease",
            values_to = "mean_expr"
        )


    return(log_fold_data)
}


#' Generate heatmap
#'
#' This function generates a heatmap that displays the expression per gene per cell line.
#'
#' The generated heat map shows the gene name on the x-axis, the cell line name on the
#' y-axis and shows the expression levels with colour (fill).
#'
#' @param data a dataframe containing atleast gene names, expression values, and cancer types.
#' @return A ggplot2 heat map object
#' @importFrom ggplot2 ggplot aes geom_tile ylab xlab labs theme element_text facet_wrap theme element_rect
#' @importFrom dplyr filter
#' @importFrom plotly ggplotly layout
#' @importFrom paletteer scale_fill_paletteer_c
#' @examples
#' generate_heatmap(merged_data)

generate_heatmap <- function(input, data){

    # Assigns palette to heatmap that aligns with chosen option
    chosen_palette <- paletteer::palettes_c_names %>%
        filter(paletteer::palettes_c_names$palette %in% input$heatmap_palette)

    palette <- paste0(chosen_palette$package, "::", chosen_palette$palette)

    # Generate heatmap for comparing expression of pathway across cancer types
    if (input$use_case == 'compare_pathway'){


        if (length(input$compare_pathway_onco_type) !=2){

            stop("⚠️: Please select 2 cancer types")

        }

        else{

            log_fold_data <- calculate_logfold_change(data)


            p <- ggplot(
                data = log_fold_data,
                aes(x = reorder(gene, total_mean),
                    y = OncotreePrimaryDisease,
                    fill = mean_expr
                )) +
                geom_tile() +
                ylab("Cancer type") +
                xlab("Gene") +
                scale_fill_paletteer_c(palette) +
                theme(axis.text.x = element_text(angle = 90,
                                                 vjust = 0.5,
                                                 hjust=1))

        }
    }

    # Generate heatmap for visualizing gene expression across selected genes/cell lines
    else{
        p <- ggplot(data = data,
                    aes(x = gene,
                        y = StrippedCellLineName,
                        fill = expr)) +
            geom_tile() +
            ylab("Tumor Cell Line") +
            xlab("Gene") +
            labs(fill = "Expression level \n (log2 TPM)") +
            scale_fill_paletteer_c(palette)

        p <- p +  facet_wrap(~OncotreePrimaryDisease,
                             scales = "free_y")

    }


    if (input$border_checkbox_heatmap == TRUE){
        p <- p + theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5))
    }

    p <- ggplotly(p,
                  height = input$heatmap_height,
                  width = input$heatmap_width) %>%
        layout(margin = list(l = 100,
                             r = 10,
                             b = 90,
                             t = 50))



    return(p)
}

#' Generate datatable
# '
#' This function generates a datatable using the merged data.
#'
#' The generated table shows the cell line name, the gene name, and the expression value.
#' @param data: a dataframe containing atleast gene names, expression values, and cancer types.
#' @return a datatable.
#' @importFrom DT datatable
#' @importFrom shiny validate need
#' @examples
#' generate_datatable(merged_data)

generate_datatable <- function(data, filter = "top") {
    validate(
        need(nrow(data) > 0, "No data available for the selected settings.")
    )

    # Create a new column containing a link to PubMed
    data$research <- paste0("<a href='https://pubmed.ncbi.nlm.nih.gov/?term=",
                            URLencode(data$gene, reserved=TRUE),
                            "+cancer' target='_blank'>PubMed</a>")

    # Make the gene symbol clickable, linking to GeneCards
    data$gene <- paste0("<a href='https://www.genecards.org/cgi-bin/carddisp.pl?gene=",
                        data$gene, "' target='_blank'>", data$gene, "</a>")

    data$expr <- round(data$expr, 3)

    # Make the P-value column use scientific notation
    if ("p_value" %in% names(data)) {
      data$p_value <- format(data$p_value, scientific = TRUE, digits = 3)
    }

    if ("log2_fc" %in% names(data)) {
        data$log2_fc <- format(data$log2_fc, scientific = TRUE, digits = 3)
    }
    # Render the table
    dt <- datatable(data,
              rownames = FALSE,
              escape = FALSE,
              filter = filter,
              extensions = c("Buttons"),
              options = list(
                  dom = "Btip",
                  buttons = list(
                      list(extend = "colvis",
                           text = "Select columns"),
                      list(extend = "colvisRestore",
                           text = "Reset to default columns"),
                      list(extend = "collection",
                           buttons = c("csv", "excel"),
                           text = "Download")
                  ),
                  columnDefs = list(
                      # Specify which columns to hide
                      list(targets = c(0:2,4:5,7:42), visible = FALSE)
                  )
              )
    )
}


#' Gene clustering reformat
#'
#' This function reformats the data to make it suited for gene clustering/correlation plot.
#'
#' @param merged_data: dataframe of metadata with corresponding expression data
#' @return wide dataframe based of merged_data
#' @importFrom dplyr select
#' @importFrom tidyr pivot_wider
#' @examples
#' reformat_data(merged_data)

reformat_data <- function(merged_data){
    wide_exprdata <- merged_data %>%
        dplyr::select(StrippedCellLineName, gene, expr) %>%
        pivot_wider(names_from = "StrippedCellLineName", values_from = "expr")


    return(wide_exprdata)
}



#' Create Query Profile
#'
#' This function creates the query profile, which is the expression profile of the query gene,
#' which is needed to compare it to the profiles of other genes
#'
#' @param merged_data: dataframe of metadata with corresponding expression data
#' @param input: input from filter options in application
#' @return numeric vector with expression profile of chosen gene
#' @importFrom dplyr filter select
#' @examples
#' create_query(wide_exprdata, input)
create_query <- function(wide_exprdata, input){
    query_profile <- wide_exprdata %>%
        filter(gene==input$gene_name) %>%
        dplyr::select(-gene) %>%
        as.numeric()

    return(query_profile)
}


#' Calculate distance
#'
#' This function calculates the distance between two target (x and y).
#' @importFrom stats cor

calc_dist <- function(x,y){
    suppressWarnings(cor(x,y))
}


#' Determine distances
#'
#' This function determines the distance for all gene combinations and saves this,
#' in a tibble containing the query gene and target gene (and distance between these two).
#'
#' @param data: a dataframe containing atleast gene names, expression values, and cancer types.
#' @param input: user input from filter options in application
#' @param target_matrix: numeric vector with expression profile of target gene
#' @param query_profile: numeric vector with expression profile of chosen query gene
#' @param wide_exprdata: wide dataframe based of merged_data
#' @return a tibble with 3 columns, containing the query_gene [1], the target_gen [2]
#' and the difference in expression between these two genes (also known as the 'distance') [3]
#' @importFrom dplyr pull bind_rows
#' @importFrom tidyr tibble
#' @examples
#' determine_distances(data, input, target_matrix, query_profile, wide_exprdata)

determine_distances <- function(data, input, target_matrix, query_profile, wide_exprdata){


    # Calculate distances
    all_correlations <- apply(target_matrix,1, function(y){
        calc_dist(x = query_profile, y = y )})

    results<-list()

    # Save distances in tibble as query gene and target gene (and distance between these two)
    results[[input$gene_name]] <- tibble(
        query_gene = input$gene_name,
        target_gene = wide_exprdata %>% pull(gene),
        distance = all_correlations
    )

    # Merge all tibbles by row, making a longer dataframe as result
    all_distances <- bind_rows(results)

    return(all_distances)
}


#' determine_top_scoring
#'
#' This function determines what target genes have the smallest or largest distance
#' to the query gene, where small distance points to positive correlation and large
#' distance points to negative correlation
#'
#' @param input: user input from filter options in application
#' @param all_distances: a tibble with 3 columns, containing the query_gene [1], the target_gen [2]
#' and the difference in expression between these two genes (also known as the 'distance') [3]
#' @param data: a dataframe containing atleast gene names, expression values, and cancer types.
#' @return a reduced version of data, with only the "top-scoring" genes
#' (whether the user chose for most positive/most negative correlation)
#' @importFrom dplyr filter arrange
#' @examples
#' determine_top_scoring(input, all_distances, data)

determine_top_scoring <- function(input, all_distances, data){

    if (input$clustering_options == "Positive correlation"){
        top_scoring <- all_distances %>%
            filter(distance < 0.99, distance > 0.1) %>%
            arrange(-abs(distance)) %>%
            head(input$top_n_genes)
    } else if (input$clustering_options == "Negative correlation"){
        top_scoring <- all_distances %>%
            filter(distance < 0.99, distance > 0.1) %>%
            arrange(-abs(distance)) %>%
            tail(input$top_n_genes)
    }

    # Selects expression data for genes with highest correlation to query gene
    tp <- data %>% filter(gene %in% top_scoring$target_gene)

    # Add selected gene to dataframe
    tp <- data %>% filter(gene %in% c(top_scoring$target_gene, input$gene_name))

    return(tp)
}



#' Gene Clustering Plot
#'
#' This function generates a gene clustering plot that displays up to 10 genes
#' with a similar expression profile compared to a selected gene
#'
#' @param tp: a reduced version of data, with only the "top-scoring" genes
#' @return A ggplot2 line plot object, displaying the top-scoring genes + query gene
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme element_text theme element_rect
#' @importFrom plotly ggplotly
#' @importFrom dplyr n_distinct
#' @examples
#' generate_clusterplot(tp)


generate_clusterplot <- function(tp, input){

    # Warning when only one patient is selected, no satisfactory expression profile
    # comparison can be created
    if (tp %>%
        pull(StrippedCellLineName) %>%
        n_distinct() <= 1) {
        stop("⚠️ Warning: There is only one patient (StrippedCellLineName) for the selected data. No satisfactory plot can be created.")
    }

    #TODO: look into this, as there should always be enough genes available
    if (tp %>%
        pull(gene) %>%
        n_distinct() <= 1){
        stop("⚠️ Warning: There are no similiar genes available for the selected data. No satisfactory plot can be created.")
    }

    # generate the plot
    p <- ggplot(tp,
                aes(x = StrippedCellLineName,
                    y = expr,
                    color = gene))
    p <- p + geom_point()
    p <- p + geom_line(aes(group = gene))
    p <- p + theme(axis.text.x = element_text(angle=270))


    if (input$border_checkbox_cluster == TRUE){
        p <- p + theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5))
    }


    p <- ggplotly(p, height = input$cluster_height, width = input$cluster_width)


    return(p)

}


#' Gene Correlation Plot
#'
#' This function generates a correlation plot displaying a comparison of the expression
#' between two genes across all cell lines.
#'
#' @param input: user input from filter options in application
#' @param wide_exprdata: wide dataframe based of merged_data
#' @return A ggplot2 point plot object, displaying the correlation between one
#' query gene and one target gene
#' @importFrom dplyr pull
#' @importFrom ggplot2 ggplot aes geom_point geom_abline geom_text theme element_rect
#' @examples
#' generate_corr_plot(input,wide_exprdata)


generate_corr_plot <- function(input, wide_exprdata){

    # Transpose the wide dataframe (swapping rows/columns)
    gene_exprdata = as.data.frame(t(wide_exprdata[, -1]))
    colnames(gene_exprdata) <- wide_exprdata$gene


    x <- gene_exprdata %>% pull(input$gene_name)
    y <- gene_exprdata %>% pull(input$correlation_gene)
    Cell_line <- rownames(gene_exprdata)

    mymodel <-lm(x~y+0)
    p <- ggplot(gene_exprdata, aes(x = .data[[input$gene_name]],
                                   y = .data[[input$correlation_gene]],
                                   label = Cell_line))

    p <- p + geom_point(size=5, alpha=0.5)
    p <- p + geom_abline(slope=mymodel$coefficients, intercept=0)


    if (input$border_checkbox_correlation == TRUE){
        p <- p + theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5))
    }

    if (input$label_checkbox == TRUE){
        p <- p + geom_text()
    }

    return(p)
}

#' determine_top_scoring
#'
#' This function determines what target genes have the smallest or largest distance
#' to the query gene, where small distance points to positive correlation and large
#' distance points to negative correlation
#'
#' @param input: user input from filter options in application
#' @param all_distances: a tibble with 3 columns, containing the query_gene [1], the target_gen [2]
#' and the difference in expression between these two genes (also known as the 'distance') [3]
#' @param data: a dataframe containing atleast gene names, expression values, and cancer types.
#' @return a reduced version of data, with only the "top-scoring" genes
#' (whether the user chose for most positive/most negative correlation)
#' @importFrom dplyr filter arrange
#' @examples
#' determine_top_scoring(input, all_distances, data)

determine_top_scoring <- function(input, all_distances, data){

    if (input$clustering_options == "Positive correlation"){
        top_scoring <- all_distances %>%
            filter(distance < 0.99, distance > 0.1) %>%
            arrange(-abs(distance)) %>%
            head(input$top_n_genes)
    } else if (input$clustering_options == "Negative correlation"){
        top_scoring <- all_distances %>%
            filter(distance < 0.99, distance > 0.1) %>%
            arrange(-abs(distance)) %>%
            tail(input$top_n_genes)
    }

    # Selects expression data for genes with highest correlation to query gene
    tp <- data %>% filter(gene %in% top_scoring$target_gene)

    # Add selected gene to dataframe
    tp <- data %>% filter(gene %in% c(top_scoring$target_gene, input$gene_name))

    return(tp)
}



#'generate_homepage_viz
#'
#'This function generates the piechart shown on the homepage of the dashboard.
#'
#'@param data: a dataframe containing the OncotreePrimaryDisease.
#'@return a piechart showing the top 10 cancer types in the dataframe
#'@importFrom ggplot2 coord_polar theme_void
#'@importFrom forcats fct_lump
#'@importFrom paletteer scale_fill_paletteer_d
#'@examples
#'generate_homepage_viz(data)

generate_homepage_viz <- function(data){
    ggplot(data, aes(x = "", fill = fct_lump(OncotreePrimaryDisease, n = 10))) +
        geom_bar(width = 1) +
        coord_polar("y") +
        theme_void() +
        labs(title = "Top 10 cancer types in the dataset", fill = "Cancer type:") +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
        scale_fill_paletteer_d("colorBlindness::PairedColor12Steps")
}

