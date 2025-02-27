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
  ggplot(data, aes(x = "", y = expression, fill = OncotreePrimaryDisease)) +
    geom_boxplot() +
    facet_wrap(~gene, scales = "free_y") +
    labs(
      x = "",
      y = "Expression Level (log 2TPM)",
      title = "Expression of selected genes across cancer types",
      fill = "Cancer type:"
    )
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

#' Merge data
# '
#' 
#'
#' 
#' @param filtered_metadata
#' @param filtered_expr
#' @return merged_data, dataframe of metadata with corresponding expression data
#' @examples
#' merge_data(filtered_metadata, filtered_expr)

merge_data <- function(filtered_metadata, filtered_expr) {

  merged_data <- merge(filtered_metadata, 
                  filtered_expr, 
                  by = "ModelID", 
                  all = FALSE)
  
  return(merged_data)
  
}