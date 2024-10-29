# install.packages("rPref")
# install.packages("plotly")

# stem data for Pareto front (already computed)

# Combine into a data frame for the no garden decision option

paretto_front <- function(economic_return_garden, child_health_garden, biodiversity_garden, 
  economic_return_STEM, child_health_STEM, biodiversity_STEM){

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
p <- high(stem_garden_data$biodiversity) * 
  high(stem_garden_data$child_health) * 
  high(stem_garden_data$economic_return)

pareto_front_stem <- psel(stem_garden_data, p)

# Find Pareto-optimal points for the garden decision option
p_garden <- high(garden_data$biodiversity) * 
  high(garden_data$child_health) * 
  high(garden_data$economic_return)
pareto_front_garden <- psel(garden_data, p_garden)


# Plot Pareto front
library(plotly)

# Plot the Pareto front of the stem intervention (blue)
plot <- plot_ly(
  pareto_front_stem,
  x = ~biodiversity,
  y = ~child_health,
  z = ~economic_return,
  type = 'scatter3d',
  mode = 'markers',
  marker = list(size = 3, color = 'blue'),
  name = 'STEM Pareto Front'
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



