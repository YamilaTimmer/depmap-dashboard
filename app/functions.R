# Imports
library(ggplot2) # make plots
library(shiny)
library(bslib)
library(shinyjs)
library(bsicons)
library(shinyjqui)
library(plotly) # make plots interactive
library(feather)


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

#' Generate boxplot
# '
#' This function generates a boxplot using the merged data.
#'
#' The generated plot shows the gene name on the x-axis, and the expression level on the y-axis
#' @param data: a dataframe containing atleast gene names, expression values, and cancer types.
#' @return a ggplot2 boxplot object.
#' @examples
#' generate_boxplot(merged_data)

generate_boxplot <- function(data) {
  ggplot(data, aes(x = OncotreePrimaryDisease, y = expression, fill = OncotreePrimaryDisease)) +
    geom_boxplot() +
    facet_wrap(~gene, scales = "free_y") +
    labs(
      x = "",
      y = "Expression Level (log 2TPM)",
      title = "Expression of selected genes across cancer types",
      fill = "Cancer type:"
    ) + 
    
    if (length(unique(data$OncotreePrimaryDisease)) > 1) {
    theme(axis.text.x = element_text(angle = -90))
    }
}

#' Generate violinplot
# '
#' This function generates a violinplot using the merged data.
#'
#' The generated plot shows the gene name on the x-axis, and the expression level on the y-axis
#' @param data: a dataframe containing atleast gene names, expression values, and cancer types.
#' @return a ggplot2 violinplot object.
#' @examples
#' generate_violinplot(merged_data)

generate_violinplot <- function(data) {
  ggplot(data, aes(x = "", y = expression, fill = OncotreePrimaryDisease)) +
    geom_violin() +
    facet_wrap(~gene, scales = "free_y") +
    labs(
      x = "",
      y = "Expression Level (log 2TPM)",
      title = "Expression of selected genes acros cancer types",
      fill = "Cancer type:"
    )
}

#' Generate barplot
# '
#' This function generates a barplot using the merged data.
#'
#' The generated plot shows the gene name on the x-axis, and the expression level on the y-axis
#' @param data: a dataframe containing atleast gene names, expression values, and cancer types.
#' @return a ggplot2 barplot object.
#' @examples
#' generate_barplot(merged_data)

generate_barplot <- function(data) {
  ggplot(data, aes(x = "", y = expression, fill = OncotreePrimaryDisease)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~gene, scales = "free_y") +
    labs(
      x = "", 
      y = "Expression Level (log 2TPM)",
      title = "Expression of selected genes across cancer types",
      fill = "Cancer type:"
    )
}

#' Filter metadata
#'
#' This function filters the metadata based on the user inputs. 
#'
#' 
#' @param meta_data
#' @param input
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
#' 
#' @param filtered_metadata
#' @param expression_data 
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
#' 
#' @param merged_data
#' @param input
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