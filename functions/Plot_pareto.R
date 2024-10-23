# install.packages("rPref")
# install.packages("plotly")

# Original data for Pareto front (already computed)
biodiversity <- rnorm(10000, mean = 100, sd = 30)  # Replace with real data
child_health <- rnorm(10000, mean = 80, sd = 20)    # Replace with real data
economic_return <- rnorm(10000, mean = 120, sd = 40) # Replace with real data

# Combine into a data frame for Pareto analysis
original_data <- data.frame(
  biodiversity = biodiversity,
  child_health = child_health,
  economic_return = economic_return
)

# New decision option data
new_biodiversity <- rnorm(10000, mean = 105, sd = 25)  # Replace with real data
new_child_health <- rnorm(10000, mean = 85, sd = 15)    # Replace with real data
new_economic_return <- rnorm(10000, mean = 115, sd = 35) # Replace with real data

# Combine into a data frame for the new decision option
new_data <- data.frame(
  biodiversity = new_biodiversity,
  child_health = new_child_health,
  economic_return = new_economic_return
)

library(rPref)


# Find Pareto-optimal points for original data
p <- high(original_data$biodiversity) * high(original_data$child_health) * high(original_data$economic_return)
pareto_front_original <- psel(original_data, p)

# Find Pareto-optimal points for the new decision option
p_new <- high(new_data$biodiversity) * high(new_data$child_health) * high(new_data$economic_return)
pareto_front_new <- psel(new_data, p_new)


# Plot Pareto front
library(plotly)

# Plot the Pareto front of the original intervention (blue)
plot <- plot_ly(
  pareto_front_original,
  x = ~biodiversity,
  y = ~child_health,
  z = ~economic_return,
  type = 'scatter3d',
  mode = 'markers',
  marker = list(size = 3, color = 'blue'),
  name = 'Original Pareto Front'
)

# Plot the Pareto front of the new decision option (red)
plot <- plot %>%
  add_trace(
    data = pareto_front_new,
    x = ~biodiversity,
    y = ~child_health,
    z = ~economic_return,
    type = 'scatter3d',
    mode = 'markers',
    marker = list(size = 3, color = 'red'),
    name = 'New Decision Option Pareto Front'
  )

# Customize the layout
plot <- plot %>%
  layout(
    title = "Comparison of Pareto Fronts: Original vs New Decision Option",
    scene = list(
      xaxis = list(title = "Biodiversity"),
      yaxis = list(title = "Child Health"),
      zaxis = list(title = "Economic Return")
    )
  )

# Show the plot
plot
