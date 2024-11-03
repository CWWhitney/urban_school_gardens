#' Analyze and Visualize Pareto-Optimal Points for Garden and STEM Options
#'
#' This function identifies Pareto-optimal points for two decision options (STEM and Garden) 
#' based on three objectives: economic return, child health, and biodiversity. It provides 
#' summary statistics, a count of Pareto-optimal points, and a comparative plot visualizing 
#' trade-offs among the objectives for each option.
#'
#' @param economic_return_garden Numeric vector. Economic return values for the Garden option.
#' @param child_health_garden Numeric vector. Child health values for the Garden option.
#' @param biodiversity_garden Numeric vector. Biodiversity values for the Garden option.
#' @param economic_return_STEM Numeric vector. Economic return values for the STEM option.
#' @param child_health_STEM Numeric vector. Child health values for the STEM option.
#' @param biodiversity_STEM Numeric vector. Biodiversity values for the STEM option.
#'
#' @return A list with the following elements:
#'   \item{stem_summary}{Summary statistics of the Pareto-optimal points for the STEM option.}
#'   \item{garden_summary}{Summary statistics of the Pareto-optimal points for the Garden option.}
#'   \item{trade_off_plot}{A `ggplot2` object showing the trade-offs between biodiversity and 
#'     child health, with economic return as the color scale, faceted by decision option (STEM or Garden).}
#'
#' @details The function first identifies Pareto-optimal points for each option by maximizing 
#' all three objectives (economic return, child health, and biodiversity) using the `rPref` package. 
#' It then calculates summary statistics for each option's Pareto-optimal points and plots the 
#' trade-offs among objectives to aid interpretation.
#'
#' @examples
#' \dontrun{
#' pareto_front_posthoc(economic_return_garden, child_health_garden, biodiversity_garden, 
#'                      economic_return_STEM, child_health_STEM, biodiversity_STEM)
#' }
#'
#' @import rPref
#' @import ggplot2
#' @import reshape2
#' @export
pareto_front_posthoc <- function(economic_return_garden, child_health_garden, biodiversity_garden, 
                                 economic_return_STEM, child_health_STEM, biodiversity_STEM) {
  
  # Combine into data frames for each option
  stem_garden_data <- data.frame(
    economic_return = economic_return_garden,
    biodiversity = child_health_garden,
    child_health = biodiversity_garden
  )
  
  garden_data <- data.frame(
    economic_return = economic_return_STEM,
    biodiversity = child_health_STEM,
    child_health = biodiversity_STEM
  )
  
  # Identify Pareto-optimal points
  p_stem <- rPref::high(stem_garden_data$biodiversity) * 
    rPref::high(stem_garden_data$child_health) * 
    rPref::high(stem_garden_data$economic_return)
  pareto_front_stem <- rPref::psel(stem_garden_data, p_stem)
  
  p_garden <- rPref::high(garden_data$biodiversity) * 
    rPref::high(garden_data$child_health) * 
    rPref::high(garden_data$economic_return)
  pareto_front_garden <- rPref::psel(garden_data, p_garden)
  
  # Summary statistics and range of values for each option
  stem_summary <- summary(pareto_front_stem)
  garden_summary <- summary(pareto_front_garden)
  
  # Print out details for interpretation
  cat("Number of Pareto-optimal points for STEM option:", nrow(pareto_front_stem), "\n")
  cat("Number of Pareto-optimal points for Garden option:", nrow(pareto_front_garden), "\n\n")
  cat("Summary of Pareto-optimal points for STEM option:\n")
  print(stem_summary)
  cat("\nSummary of Pareto-optimal points for Garden option:\n")
  print(garden_summary)
  
  library(ggplot2)
  library(reshape2)
  
  # Melt data for easier plotting
  stem_long <- melt(pareto_front_stem, variable.name = "Objective")
  garden_long <- melt(pareto_front_garden, variable.name = "Objective")
  
  # Add a label for the option type
  stem_long$Option <- "STEM"
  garden_long$Option <- "Garden"
  
  # Combine data for a comparative plot
  combined_data <- rbind(stem_long, garden_long)
  
  # Reshape combined_data back to wide format
  combined_data_wide <- dcast(combined_data, Option ~ Objective, value.var = "value")
  
  # Create the plot
  trade_off_plot <- ggplot(combined_data_wide, aes(x = biodiversity, y = child_health, color = economic_return)) +
    geom_point() +
    facet_wrap(~ Option) +
    labs(
      title = "Trade-offs in Pareto-Optimal Points",
      x = "Biodiversity",
      y = "Child Health",
      color = "Economic Return"
    ) +
    theme_minimal()
  
  # Display the plot
  trade_off_plot
  
  # Return a list of outputs
  return(list(
    stem_summary = stem_summary,
    garden_summary = garden_summary,
    trade_off_plot = trade_off_plot
  ))
}