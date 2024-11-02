# install.packages("rPref")
# install.packages("plotly")

# stem data for Pareto front (already computed)

pareto_front <- function(economic_return_garden, child_health_garden, biodiversity_garden, 
  economic_return_STEM, child_health_STEM, biodiversity_STEM, plot_return = plot_scatter){
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

# plse() collects all non-dominated points in the MC results, 
# as a set of Pareto-optimal solutions where it is
# impossible to increase one objective without decreasing another

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

# Pareto front as a 3d scatter plot ###

# Plot the Pareto front of the stem intervention (blue)
plot_scatter <- plot_ly(
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
plot_scatter <- plot_scatter %>%
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
plot_scatter <- plot_scatter %>%
  layout(
    title = "Comparison of Pareto Fronts: STEM vs Garden Option",
    scene = list(
      xaxis = list(title = "Biodiversity"),
      yaxis = list(title = "Child Health"),
      zaxis = list(title = "Economic Return")
    )
  )


### As surface plot ####

# pareto_front_stem and pareto_front_garden contain the Pareto-optimal points

# Interpolating a surface for STEM option
interp_stem <- with(pareto_front_stem, interp(x = biodiversity, y = child_health, z = economic_return, 
                                              duplicate = "mean"))

# Interpolating a surface for Garden option
interp_garden <- with(pareto_front_garden, interp(x = biodiversity, y = child_health, z = economic_return, 
                                                  duplicate = "mean"))

# Creating a plotly plot for STEM option
plot <- plot_ly() %>%
  add_surface(x = interp_stem$x, y = interp_stem$y, z = interp_stem$z, colorscale = 'Blues', 
              showscale = FALSE, opacity = 0.7, name = 'STEM Option Pareto Front') %>%
  layout(scene = list(
    xaxis = list(title = "Biodiversity"),
    yaxis = list(title = "Child Health"),
    zaxis = list(title = "Economic Return")
  ))

# Adding Garden option surface
plot <- plot %>%
  add_surface(x = interp_garden$x, y = interp_garden$y, z = interp_garden$z, colorscale = 'Reds', 
              showscale = FALSE, opacity = 0.7, name = 'Garden Option Pareto Front')

# Assuming pareto_front_stem and pareto_front_garden contain the Pareto-optimal points

# Interpolating a surface for STEM option
interp_stem <- with(pareto_front_stem, interp(x = biodiversity, y = child_health, z = economic_return, 
                                              duplicate = "mean"))

# Interpolating a surface for Garden option
interp_garden <- with(pareto_front_garden, interp(x = biodiversity, y = child_health, z = economic_return, 
                                                  duplicate = "mean"))

# Creating the plotly plot with single color legends
plot_surface <- plot_ly() %>%
  add_surface(x = interp_stem$x, y = interp_stem$y, z = interp_stem$z, 
              colorscale = list(c(0, 'red'), c(1, 'red')),  # STEM option in red
              opacity = 0.7, name = 'STEM Option', showscale = FALSE) %>%
  add_surface(x = interp_garden$x, y = interp_garden$y, z = interp_garden$z, 
              colorscale = list(c(0, 'blue'), c(1, 'blue')),  # Garden option in blue
              opacity = 0.7, name = 'Garden Option', showscale = FALSE) %>%
  
  # Customize the layout
  layout(scene = list(
    xaxis = list(title = "Biodiversity"),
    yaxis = list(title = "Child Health"),
    zaxis = list(title = "Economic Return")
  ))

# Show the plot
return(plot_return)

}