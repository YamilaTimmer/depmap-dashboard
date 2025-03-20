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
  p <- ggplot(data, aes(x = OncotreeCode, y = expression, fill = OncotreePrimaryDisease))
  # Adjusting settings according to plot type
  if (type == "boxplot") {
    p <- p + geom_boxplot()
  } else if (type == "violin") {
    p <- p + geom_violin()
  } else if (type == "bar") {
    p <- p + 
      stat_summary(geom = "bar", fun = "mean", position = "dodge") +
      stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2, position = position_dodge(0.9))
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

generate_heatmap <- function(data){
  
  p <- ggplot(data = data, 
              aes(x = gene, 
                  y = StrippedCellLineName, 
                  fill = expression)) +
    geom_tile() + 
    ylab("Tumor Cell Line") +
    xlab("Gene") +
    labs(fill = "Expression level (log2 TPM)")
  
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
                             URLencode(data$gene, reserved=TRUE), "+cancer' target='_blank'>PubMed</a>")
  
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
                list(extend = 'colvis', text = 'Select columns'),
                list(extend = 'csv', title = 'download.csv', text = 'Download CSV'),
                list(extend = 'excel', title = 'download.xlsx', text = 'Download Excel')
              ),
              columnDefs = list(
                list(targets = c(0:2,4:6,8:42), visible = FALSE)  # Specify which columns to hide
              )
            )
  )
}


#' Filter metadata
#'
#' This function filters the metadata based on the user inputs. 
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
           & AgeCategory %in% input$age_category
           & OncotreePrimaryDisease %in% input$onco_type
    )
  
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
#' This function filters the merged data based off of the user-chosen gene(s) 
#'
#' @param merged_data dataframe of metadata with corresponding expression data
#' @param input user input from filter options in application
#' @return filtered_gene, dataframe of metadata + expression data that is filtered on all user inputs
#' @examples
#' filter_gene(merged_data, input)
#' 

filter_gene <- function(merged_data, input) {
  
  filtered_gene <- merged_data %>% 
    filter(gene %in% input$gene_name
    )
  
  return(filtered_gene)
}