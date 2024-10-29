# install.packages("rPref")
# install.packages("plotly")

# stem data for Pareto front (already computed)

pareto_front <- function(economic_return_garden, child_health_garden, biodiversity_garden, 
  economic_return_STEM, child_health_STEM, biodiversity_STEM){
## combine the input data into two separate data frames, stem_garden_data and
## garden_data, for the two options being compared.
  
# Combine into a data frame for the stem garden decision option
stem_garden_data <- data.frame(
  economic_return = economic_return_garden,
  biodiversity = child_health_garden,
  child_health = biodiversity_garden
)

# Combine into a data frame for the stem garden decision option
garden_data <- data.frame(
  economic_return = economic_return_STEM,
  biodiversity = child_health_STEM,
  child_health = biodiversity_STEM
)

library(rPref)

# Find Pareto-optimal points for stem data
# The function high() in rPref specifies that higher values for a given
# objective are preferred. In other words, high(objective_name) sets a
# preference to maximize that objective.
p_stem <- high(stem_garden_data$biodiversity) * 
  high(stem_garden_data$child_health) * 
  high(stem_garden_data$economic_return)

# rPref package, identifies the Pareto-optimal points for both
# datasets (stem_garden_data and garden_data), aiming to maximize each of the
# objectives: biodiversity, child health, and economic return.
# combines these preferences using the multiplication operator (*), which means
# all objectives—biodiversity, child health, and economic return—are to be
# maximized simultaneously.
pareto_front_stem <- psel(stem_garden_data, p_stem)

# Find Pareto-optimal points for the garden decision option
p_garden <- high(garden_data$biodiversity) * 
  high(garden_data$child_health) * 
  high(garden_data$economic_return)

pareto_front_garden <- psel(garden_data, p_garden)

# pareto_front_stem and pareto_front_garden, contain only the data points where
# no other point in the dataset improves any objective without sacrificing at
# least one of the other objectives.

# extracts only the solutions that represent the best trade-offs among the
# objectives. By focusing on the Pareto-optimal points, the analysis helps
# decision-makers visualize and evaluate only those options that are efficient,
# balancing biodiversity, child health, and economic return effectively. 

# Pareto front

# Plot the Pareto front of the stem intervention (blue)
plot <- plot_ly(
  pareto_front_stem,
  x = ~biodiversity,
  y = ~child_health,
  z = ~economic_return,
  type = 'scatter3d',
  mode = 'markers',
  marker = list(size = 3, color = 'blue'),
  name = 'STEM Option Pareto Front'
)

# Plot the Pareto front of the garden decision option (red)
plot <- plot %>%
  add_trace(
    data = pareto_front_garden,
    x = ~biodiversity,
    y = ~child_health,
    z = ~economic_return,
    type = 'scatter3d',
    mode = 'markers',
    marker = list(size = 3, color = 'red'),
    name = 'Garden Option Pareto Front'
  )

# Customize the layout
plot <- plot %>%
  layout(
    title = "Comparison of Pareto Fronts: STEM vs Garden Option",
    scene = list(
      xaxis = list(title = "Biodiversity"),
      yaxis = list(title = "Child Health"),
      zaxis = list(title = "Economic Return")
    )
  )

# Show the plot
return(plot)

}