#' Preprocessing
#'
#' This function preprocesses raw .CSV files (model.csv and a DepMap gene 
#' ), the files are present in the package/repo. And will save the processed
#' versions of the csv as .tsv feather encoded files.
#' @return NULL

run_preprocessing <- function(input_path, output_path) {
    
    library(data.table)
    library(dplyr)
    library(tidyr)
    library(feather)
    
    # Load data
    expression_data <- fread("../../OmicsExpressionProteinCodingGenesTPMLogp1.csv")
    meta_data <- fread("../../Model.csv", na.strings = c("", "unknown", "Unknown", "None")) 

    # Change colname in expression data to make merging easier
    colnames(expression_data)[1] <- "ModelID"
    
    # Make expression data tidy:
    tidy_data <- expression_data %>% 
        pivot_longer(
            cols = 2:ncol(expression_data),
            names_to = "gene",
            values_to = "expr")
    
    # Removing entrez ID from the gene column:
    tidy_data$gene <- gsub(" \\(\\d+\\)", "", tidy_data$gene)
    
    
    # Save the two seperate data files:
    write_feather(x = tidy_data, path = "expression_data.tsv")
    write_feather(x = meta_data, path = "meta_data.tsv")
    
}