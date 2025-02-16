library(ggplot2) # make plots

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
    facet_wrap(~ gene, scales = "free_y") +
    labs(
      x = "",
      y = "Expression Level (log 2TPM)",
      title = "Expression of Selected Genes Across Cancer Types",
      fill = "Cancer type:"
    )
}