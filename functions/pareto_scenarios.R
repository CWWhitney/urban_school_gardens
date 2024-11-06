# Pareto scenarios

input_table <- read.csv("data/inputs_school_garden.csv")

garden_simulation_data <- cbind(garden_simulation_results$x, garden_simulation_results$y)

# Separate the variable names based on status
uncertain_vars <- variable_list$variable[variable_list$status == "uncertain"]

# Additional uncertain variables from garden_simulation_results$y
additional_uncertain_vars <- c("NPV_garden", "NPV_garden_STEM", 
                               "NPV_garden_public_school", "NPV_garden_STEM_public_school", 
                               "biodiversity", "health", "health_STEM", 
                               "total_costs", "total_costs_STEM")

# Combine both lists to get the final list of uncertain variables
uncertain_vars <- c(uncertain_vars, additional_uncertain_vars)
 
controllable_vars <- variable_list$variable[variable_list$status == "controllable"]

# Filter the Monte Carlo results based on these categories
uncertain_data <- garden_simulation_data[, uncertain_vars]
controllable_data <- garden_simulation_data[, controllable_vars]



# Calculate Expected Values for Uncertain Variables
# For the uncertain variables, calculate the expected value across all simulations 
# to represent the average impact of these variables.
# Calculate expected values (mean) for uncertain variables
expected_values_uncertain <- colMeans(uncertain_data)

# Generate Scenarios for Controllable Variables
# Use the controllable variables to create distinct scenarios. 
# These scenarios can represent feasible configurations of the controllable options.

# Example: Generating two scenarios for controllable variables
scenario_1 <- as.list(expected_values_uncertain)
scenario_1$if_animals_in_garden <- 1
scenario_1$size_of_garden <- 500

scenario_2 <- as.list(expected_values_uncertain)
scenario_2$if_animals_in_garden <- 0
scenario_2$size_of_garden <- 3

scenario_3 <- as.list(expected_values_uncertain)
scenario_3$if_animals_in_garden <- 0
scenario_3$size_of_garden <- 30

# Compute the Pareto Front Based on Achievable Scenarios
# analyze the Pareto front using the achievable configurations 
# generated from the controllable variables and the expected values of uncertain variables.

library(rPref)

# Combine scenarios for Pareto analysis
 scenarios <- data.frame(rbind(scenario_1, scenario_2, scenario_3))

# Convert list columns to numeric vectors in `scenarios`
scenarios <- data.frame(lapply(scenarios, function(x) {
  if (is.list(x)) {
    unlist(x)
  } else {
    x
  }
}))

# Plot single scenario example ####

# Define the objectives to maximize (e.g., biodiversity, child health, economic return)
pareto_criteria <- high(scenarios$biodiversity) * high(scenarios$health) * high(scenarios$NPV_garden)

# Compute the Pareto front for achievable configurations
pareto_front <- psel(scenarios, pareto_criteria)

plot_scatter <- plot_ly(
  pareto_front,
  x = ~biodiversity,
  y = ~health,
  z = ~NPV_garden,
  type = 'scatter3d',
  mode = 'markers',
  marker = list(size = 3, color = 'blue'),
  name = 'STEM Option Pareto Front'
) %>%
  layout(
    title = "Comparison of Pareto Fronts: STEM vs Garden Option",
    scene = list(
      xaxis = list(title = "Biodiversity"),
      yaxis = list(title = "Child Health"),
      zaxis = list(title = "Economic Return")
    )
  )

plot_scatter

### Multiple options ####

# Load necessary library
library(rPref)

# Set up the ranges for controllable binary variables and sample from existing data for continuous variables
controllable_options <- list(
  "if_effective_manage" = c(0, 1),
  "if_effective_teaching" = c(0, 1),
  "if_effective_training" = c(0, 1),
  "if_animals_in_garden" = c(0, 1),
  "if_school_has_canteen" = c(0, 1),
  "if_parking" = c(0, 1),
  "school_event_freq" = quantile(garden_simulation_data$school_event_freq, probs = c(0.1, 0.5, 0.9)),  # Sample low, median, high
  "size_of_garden" = quantile(garden_simulation_data$size_of_garden, probs = c(0.1, 0.5, 0.9)),
  "annual_teacher_training" = quantile(garden_simulation_data$annual_teacher_training, probs = c(0.1, 0.5, 0.9))
)

# Generate all possible combinations of controllable variables
scenarios_grid <- expand.grid(controllable_options)

# Obtain expected values for uncertain variables from previous calculations
expected_values_uncertain <- as.list(colMeans(uncertain_data))

# Create a list of full scenarios by merging uncertain values with each controllable scenario
full_scenarios <- lapply(1:nrow(scenarios_grid), function(i) {
  scenario <- as.list(scenarios_grid[i, ])
  return(c(scenario, expected_values_uncertain))
})

# Convert to a data frame with all scenario combinations
expanded_scenarios <- do.call(rbind, lapply(full_scenarios, as.data.frame))
expanded_scenarios <- data.frame(lapply(expanded_scenarios, unlist))

# Define the Pareto criteria to maximize biodiversity, health, and NPV_garden
pareto_criteria <- high(expanded_scenarios$biodiversity) * 
  high(expanded_scenarios$health) * 
  high(expanded_scenarios$NPV_garden)

# Compute the Pareto front for the expanded scenarios
pareto_front <- psel(expanded_scenarios, pareto_criteria)

# Plotting the Pareto front for biodiversity, health, and NPV_garden
library(plotly)
plot_scatter <- plot_ly(
  pareto_front,
  x = ~biodiversity,
  y = ~health,
  z = ~NPV_garden,
  type = 'scatter3d',
  mode = 'markers',
  marker = list(size = 3, color = 'blue'),
  name = 'Pareto Front'
) %>%
  layout(
    title = "Pareto Front with Expanded Scenarios",
    scene = list(
      xaxis = list(title = "Biodiversity"),
      yaxis = list(title = "Health"),
      zaxis = list(title = "Economic Return")
    )
  )

plot_scatter
