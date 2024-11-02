# Install necessary packages if not already installed
# install.packages("rPref")
# install.packages("plotly")
# install.packages("akima")

library(rPref)
library(plotly)
library(akima)

pareto_front <- function(economic_return_garden, child_health_garden, biodiversity_garden, 
                         economic_return_STEM, child_health_STEM, biodiversity_STEM, plot_return = "scatter") {
  
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
  
  # Find Pareto-optimal points for each option
  p_stem <- high(stem_garden_data$biodiversity) * high(stem_garden_data$child_health) * high(stem_garden_data$economic_return)
  pareto_front_stem <- psel(stem_garden_data, p_stem)
  
  p_garden <- high(garden_data$biodiversity) * high(garden_data$child_health) * high(garden_data$economic_return)
  pareto_front_garden <- psel(garden_data, p_garden)
  
  # Create a 3D scatter plot for the Pareto front
  if (plot_return == "scatter") {
    plot_scatter <- plot_ly(
      pareto_front_stem,
      x = ~biodiversity,
      y = ~child_health,
      z = ~economic_return,
      type = 'scatter3d',
      mode = 'markers',
      marker = list(size = 3, color = 'blue'),
      name = 'STEM Option Pareto Front'
    ) %>%
      add_trace(
        data = pareto_front_garden,
        x = ~biodiversity,
        y = ~child_health,
        z = ~economic_return,
        type = 'scatter3d',
        mode = 'markers',
        marker = list(size = 3, color = 'red'),
        name = 'Garden Option Pareto Front'
      ) %>%
      layout(
        title = "Comparison of Pareto Fronts: STEM vs Garden Option",
        scene = list(
          xaxis = list(title = "Biodiversity"),
          yaxis = list(title = "Child Health"),
          zaxis = list(title = "Economic Return")
        )
      )
    return(plot_scatter)
  }
  
  # Create a 3D surface plot for the Pareto front
  if (plot_return == "surface") {
    # Interpolate the surface for both options
    interp_stem <- with(pareto_front_stem, interp(x = biodiversity, y = child_health, z = economic_return, 
                                                  duplicate = "mean"))
    interp_garden <- with(pareto_front_garden, interp(x = biodiversity, y = child_health, z = economic_return, 
                                                      duplicate = "mean"))
    
    plot_surface <- plot_ly() %>%
      add_surface(x = interp_stem$x, y = interp_stem$y, z = interp_stem$z, 
                  colorscale = list(c(0, 'red'), c(1, 'red')),  # STEM option in red
                  opacity = 0.7, name = 'STEM Option', showscale = FALSE) %>%
      add_surface(x = interp_garden$x, y = interp_garden$y, z = interp_garden$z, 
                  colorscale = list(c(0, 'blue'), c(1, 'blue')),  # Garden option in blue
                  opacity = 0.7, name = 'Garden Option', showscale = FALSE) %>%
      layout(
        scene = list(
          xaxis = list(title = "Biodiversity"),
          yaxis = list(title = "Child Health"),
          zaxis = list(title = "Economic Return")
        )
      )
    return(plot_surface)
  }
  
  # Error message if an incorrect plot_return argument is provided
  stop("Invalid value for plot_return. Use 'scatter' or 'surface'.")
}
