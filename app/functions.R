# Imports
library(ggplot2) # make plots
library(shiny)
library(bslib)
library(shinyjs)
library(bsicons)
library(shinyjqui)
library(plotly) # make plots interactive
library(feather)
library(DT)
library(tidyr)
library(dplyr)


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
    
    if (input$use_case == "explore_expression"){
        filtered_metadata <- meta_data %>% 
            filter(Sex %in% input$sex
                   & PatientRace %in% input$race
                   & AgeCategory %in% input$age_category
                   & OncotreePrimaryDisease %in% input$onco_types
            )
        
    }
    
    else {
        
        filtered_metadata <- meta_data %>% 
            filter(Sex %in% input$sex
                   & PatientRace %in% input$race
                   & AgeCategory %in% input$age_category
                   & OncotreePrimaryDisease == input$onco_type
            )
        
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
#' @return filtered_gene, dataframe of metadata + expression data that is filtered on all user inputs
#' @examples
#' filter_gene(merged_data, input)
#' 

filter_gene <- function(merged_data, input) {
    
    if (input$use_case == "explore_expression"){
        filtered_gene <- merged_data %>% 
            filter(gene %in% input$gene_names
            )
    }
    else {
        
        filtered_gene <- merged_data %>% 
            filter(gene == input$gene_name
            )
        
    }
    return(filtered_gene)
}


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
#' @examples  
#' xyplots(merged_data, type = "boxplot")  
#' xyplots(merged_data, type = "violin")  
#' xyplots(merged_data, type = "bar")  

xyplots <- function(data, type = "boxplot") {
    p <- ggplot(data, aes(x = OncotreePrimaryDisease, 
                          y = expression, 
                          fill = OncotreePrimaryDisease))
    
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
            y = "Expression level (log 2TPM)",
            title = "Expression of selected genes across cancer types",
            fill = "Cancer type:"
        )
    
    # If multiple cancertypes are selected, axis labels get adjusted
    if (length(unique(data$OncotreePrimaryDisease)) > 1) {
        p <- p + theme(axis.text.x = element_text(angle = -90))
    }
    
    # Hides the legend, cant be given as an argument to ggplot, because this
    # will not work with plotly
    p <- ggplotly(p) %>% layout(showlegend = FALSE)
    
    return(p)
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
#' @examples 
#' generate_heatmap(merged_data) 

generate_heatmap <- function(input, data){
    
    palettes <- list("Grayscale" = "Greys", 
                     "Purple-Green" = "PRGn", 
                     "Blue" = "Blues", 
                     "Red-Blue" = "RdBu")
    
    # Assigns palette to heatmap that aligns with chosen option
    palette = palettes[[input$heatmap_palette]]
    
    p <- ggplot(data = data, 
                aes(x = gene, 
                    y = StrippedCellLineName, 
                    fill = expression)) +
        geom_tile() + 
        ylab("Tumor Cell Line") +
        xlab("Gene") +
        labs(fill = "Expression level (log2 TPM)") +
        scale_fill_distiller(palette = palette)
    
    # Angles x-axis labels to -90 degrees when more than 3 genes are selected
    if (length(unique(data$gene)) > 3) {
        p <- p + theme(axis.text.x = element_text(angle = -90))
        
    }
    return(p)
}

#' Generate datatable
# '
#' This function generates a datatable using the merged data.
#'
#' The generated table shows the cell line name, the gene name, and the expression value.
#' @param data: a dataframe containing atleast gene names, expression values, and cancer types.
#' @return a datatable.
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
    
    data$expression <- round(data$expression, 3)
    
    # Render the table
    datatable(data, 
              rownames = FALSE, 
              escape = FALSE,
              filter = filter, 
              extensions = c("Buttons"),
              options = list(
                  dom = 'Btip',
                  buttons = list(
                      list(extend = 'colvis', 
                           text = 'Select columns'),
                      list(extend = 'csv', 
                           title = 'download.csv', 
                           text = 'Download CSV'),
                      list(extend = 'excel', 
                           title = 'download.xlsx', 
                           text = 'Download Excel')
                  ),
                  columnDefs = list(
                      # Specify which columns to hide
                      list(targets = c(0:2,4:6,8:42), visible = FALSE)  
                  )
              )
    )
}


#' Gene clustering reformat
#' 
#' This function reformats the data to make it suited for gene clustering/correlation plot.
#'



reformat_data <- function(merged_data){
    wide_exprdata <- merged_data %>% 
        select(ModelID, gene, expression) %>%
        pivot_wider(names_from = "ModelID", values_from = "expression")
    
    
    return(wide_exprdata)
}



#' Create Query Profile
#'
#'
create_query <- function(wide_exprdata, input){
    query_profile <- wide_exprdata %>% 
        filter(gene==input$gene_name) %>% 
        select(-gene) %>% 
        as.numeric()
    
    return(query_profile)
}


#' Calculate distance
#' 
#' This function calculates the distance between two target (x and y).

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
#' @return a tibble
#' @examples
#' determine_distances(data, input)

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


#' Gene Clustering Plot
#'  
#' This function generates a gene clustering plot that displays up to 10 genes 
#' with a similar expression profile compared to a selected gene 
#' 
#' @param tp: tp <- determine_top_scoring(input, all_distances, data)
#' @return A ggplot2 line plot object 
#' @examples 
#' generate_clusterplot(tp)


generate_clusterplot <- function(tp){
    
    # Warning when only one patient is selected, no satisfactory expression profile
    # comparison can be created
    if (tp %>% 
        pull(ModelID) %>% 
        n_distinct() <= 1) {
        stop("⚠️ Warning: There is only one patient (ModelID) for the selected data. No satisfactory plot can be created.")
    }
    
    #TODO: look into this, as there should always be enough genes available
    if (tp %>% 
        pull(gene) %>% 
        n_distinct() <= 1){
        stop("⚠️ Warning: There are no similiar genes available for the selected data. No satisfactory plot can be created.")
    }
    
    # generate the plot
    p <- ggplot(tp, 
                aes(x = ModelID, 
                    y = expression, 
                    color = gene))
    p <- p + geom_point()
    p <- p + geom_line(aes(group = gene))
    p <- p + theme(axis.text.x = element_text(angle=270)) 
    
    return(p)
    
}


#' Gene Correlation Plot
#'  
#' This function generates a correlation plot displaying a comparison of the expression
#' between two genes across all cell lines.
#' 
#' @param input: user input from filter options in application
#' @param wide_exprdata: 
#' @return A ggplot2 point plot object 
#' @examples 
#' generate_corr_plot(input,wide_exprdata)


generate_corr_plot <- function(input, wide_exprdata){
    
    # Transpose the wide dataframe (swapping rows/columns)
    gene_exprdata = as.data.frame(t(wide_exprdata[, -1]))
    colnames(gene_exprdata) <- wide_exprdata$gene
    
    
    x <-gene_exprdata %>% pull(input$gene_name)
    y <- gene_exprdata %>% pull(input$correlation_gene)
    
    mymodel <-lm(x~y+0)
    p <- ggplot(gene_exprdata, aes(x = .data[[input$gene_name]], 
                                   y = .data[[input$correlation_gene]], 
                                   label= rownames(gene_exprdata)))
    p <-p + geom_text()
    p <-p + geom_point(size=5, alpha=0.5)
    p <- p + geom_abline(slope=mymodel$coefficients, intercept=0)
    
    return(p)
}

#' determine_top_scoring
#'  
#' This function ...
#' 
#' @param input: user input from filter options in application
#' @param all_distances: 
#' @param data: a dataframe containing atleast gene names, expression values, and cancer types.
#' @return 
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


