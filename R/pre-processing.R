#' Preprocessing
#'
#' This function preprocesses raw .CSV files (model.csv and a DepMap gene 
#' ), the files are present in the package/repo. And will save the processed
#' versions of the csv as .tsv feather encoded files.
#' @return NULL
#' @importFrom data.table fread
#' @importFrom feather write_feather
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_longer
#' @param model_path path to Model.csv
#' @param expression_path path to OmicsExpressionProteinCodingGenesTPMLogp1.csv
#' @export
run_preprocessing <- function(model_path, expression_path) {
    
    # Load data
    expression_data <- fread(expression_path)
    meta_data <- fread(model_path, na.strings = c("", "unknown", "Unknown", "None"))

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
    
    pkg_path <- system.file(package = "depmapdashboard")
    
    # Save the two seperate data files:
    write_feather(x = tidy_data, path = paste0(pkg_path, "/app/data/expression_data.tsv"))
    write_feather(x = meta_data, path = paste0(pkg_path, "/app/data/meta_data.tsv"))
    
}