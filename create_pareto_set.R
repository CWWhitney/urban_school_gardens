source("Garden_Model.R")

estimates = read.csv("data/inputs_school_garden.csv")
estimates = estimates[estimates$variable !="", ]

controllable_estimates = estimates[estimates$control_status == "controllable", ]
uncontrollable_estimates = estimates[estimates$control_status != "controllable", ]

n_mc_runs = 500

money = c()
biodiversity = c()
health = c()

#NPV_garden_public_school = c()
#NPV_garden_STEM_public_school = c()
est = decisionSupport::estimate_read_csv(paste("data/inputs_school_garden.csv",sep=""))

mc_draws <- decisionSupport::random(rho=est,n=n_mc_runs)

decision_vars = vector()

lower = c()
upper = c()
is_binary = c()
decision_var_names = c()

for (i in 1:nrow(controllable_estimates)) {
    if(controllable_estimates$distribution[i] == "tnorm_0_1"){
        val = 1
        lower = append(lower, 0)
        upper = append(upper, 1)
        is_binary = append(is_binary, TRUE)
    } else {
        val = (controllable_estimates$upper[i] + controllable_estimates$lower[i]) / 2
        lower = append(lower, controllable_estimates$lower[i])
        upper = append(upper, controllable_estimates$upper[i])
        is_binary = append(is_binary, FALSE)
    } 
    decision_vars = append(decision_vars, val)
    decision_var_names = append(decision_var_names, controllable_estimates$variable[i])
}

fitness <- function(decision_vars) {
    for (mc_i in 1:n_mc_runs){
        for (i in 1:nrow(controllable_estimates)) {
            #if (estimates$control_status[i] == "controllable") {
            val = decision_vars[i]
            assign(controllable_estimates$variable[i], val, envir=.GlobalEnv)
        }
        for (i in 1:nrow(estimates)) {
            if (estimates$control_status[i] != "controllable") {
                val = mc_draws[mc_i, i]
                assign(estimates$variable[i], val, envir=.GlobalEnv)
            }
        }
        res = school_garden_function()
        
        money = append(money, res$NPV_garden)
        biodiversity = append(biodiversity, res$biodiversity)
        health = append(health, res$health)
    }
    res = c(-mean(money)*0.01, -mean(biodiversity), -mean(health)*0.01)
    
    return(res)
}

fitness(decision_vars)

decision_vars

# Custom population function
my_population <- function(object) {
  print("Create population")
  # Validate input dimensions
  if (length(object@lower) != length(object@upper) || length(object@lower) != length(object@upper)) {
    stop("Lengths of lower bounds, upper bounds, and is_binary must be the same.")
  }
  
  # Number of decision variables
  num_vars <- length(object@lower)
  
  # Initialize population matrix
  population <- matrix(NA, nrow = object@popSize, ncol = num_vars)
  
  # Generate each individual
  for (i in 1:object@popSize) {
    individual <- sapply(1:num_vars, function(j) {
      if (is_binary[j]) {
        # Binary variable: Randomly choose 0 or 1
        sample(c(0, 1), size = 1)
      } else {
        # Continuous variable: Random value between lower and upper bound
        runif(1, min = object@lower[j], max = object@upper[j])
      }
    })
    population[i, ] <- individual
  }
  return(population)
}
binary_mutation_rate = 0.1
continuous_mutation_std = 0.1
my_mutation <- function(object, parent_idx) {
  print("Mutate")
  # Extract attributes
  lower_bounds <- object@lower
  upper_bounds <- object@upper
  population <- object@population  # Population matrix
  num_vars <- length(lower_bounds)
  
  parent <- population[parent_idx, ]
  child = rep(0, num_vars)
  # Mutate each individual
    for (j in 1:num_vars) {
        if (is_binary[j]) {
            if (runif(1) < binary_mutation_rate) {  # Apply mutation with given probability
                # For binary: flip the bit
                child[j] <- ifelse(parent[j] == 0, 1, 0)
            } else {
                child[j] <- parent[j]
            }
        } else {
            # For continuous: generate a new value in bounds
            #child[j] <- runif(1, min = lower_bounds[j], max = upper_bounds[j])

            mutation <- rnorm(1, mean = 0, sd = (upper_bounds[j] - lower_bounds[j]) * continuous_mutation_std)
            child[j] <- pmax(lower_bounds[j], pmin(upper_bounds[j], child[j] + mutation))
        }
    }
  return(child)
}

crossover_rate = 0.8
my_crossover <- function(object, parents) {
  # Extract attributes
  lower_bounds <- object@lower
  upper_bounds <- object@upper
  population <- object@population  # Population matrix
  pop_size <- object@popSize
  num_vars <- length(lower_bounds)
  
  # Crossover: Create a new generation
  children <- matrix(NA, nrow = 2, ncol = num_vars)
   
  parent1 <- population[parents[1], ]
  parent2 <- population[parents[2], ]
  if (runif(1) < crossover_rate) {
    print("Crossover")
    
    # Perform gene-by-gene crossover
    child1 <- numeric(num_vars)
    child2 <- numeric(num_vars)
    
    for (i in 1:num_vars) {
      if (runif(1) < 0.5) {  # 50% chance to take gene from each parent
        child1[i] <- parent1[i]
        child2[i] <- parent2[i]
      } else {
        child1[i] <- parent2[i]
        child2[i] <- parent1[i]
      }
    }
  } else {
    # No crossover, copy parents
    child1 <- parent1
    child2 <- parent2
  }

  children_fitness <- c(NA, NA)

  children[1, ] <- child1
  children[2, ] <- child2
  
  
  return(list(
    children=children,
    fitness=children_fitness
    ))
}

result <- rmoo::nsga2(
    type = "real-valued",
    fitness = fitness,
    lower = lower,
    upper = upper,
    selection = rmoo::nsga_tourSelection,
    population = my_population,
    mutation = my_mutation,
    crossover = my_crossover,
    popSize = 200,
    nObj = 3,
    maxiter = 100,
    names = decision_var_names
)




library(plotly)

load(file="data/optimization_results/private_nostem_500_200_100.RData")
rmoo::summary(result)
mat = result@fitness
front1_set = rmoo::non_dominated_fronts(result)$fit[[1]]
mat2 = sweep(-mat, 2, c(100, 1, 100) , `*`) # retransform
set1 = mat2[front1_set, ]

load(file="private_stem_500_200_100.RData")
rmoo::summary(result)
mat = result@fitness
front1_set = rmoo::non_dominated_fronts(result)$fit[[1]]
mat2 = sweep(-mat, 2, c(100, 1, 100) , `*`) # retransform
set2 = mat2[front1_set, ]
set2 = set2[set2[, 1]>0, ]

load(file="public_nostem_500_200_100.RData")
rmoo::summary(result)
mat = result@fitness
front1_set = rmoo::non_dominated_fronts(result)$fit[[1]]
mat2 = sweep(-mat, 2, c(100, 1, 100) , `*`) # retransform
set3 = mat2[front1_set, ]

load(file="public_stem_500_200_100.RData")
rmoo::summary(result)
mat = result@fitness
front1_set = rmoo::non_dominated_fronts(result)$fit[[1]]
mat2 = sweep(-mat, 2, c(100, 1, 100) , `*`) # retransform
set4 = mat2[front1_set, ]

plot_ly() %>%
  add_trace(x = set1[,1], y = set1[,2], z = set1[,3],
            type = "scatter3d", mode = "markers",
            marker = list(color = 'blue', size = 5),
            name = 'private, no STEM') %>%
  add_trace(x = set2[,1], y = set2[,2], z = set2[,3],
            type = "scatter3d", mode = "markers",
            marker = list(color = 'red', size = 5),
            name = 'private, STEM') %>%
  add_trace(x = set3[,1], y = set3[,2], z = set3[,3],
            type = "scatter3d", mode = "markers",
            marker = list(color = 'green', size = 5),
            name = 'public, no STEM') %>%
  add_trace(x = set4[,1], y = set4[,2], z = set4[,3],
            type = "scatter3d", mode = "markers",
            marker = list(color = 'orange', size = 5),
            name = 'public, STEM') %>%
  layout(scene = list(xaxis = list(title = 'economic'),
                      yaxis = list(title = 'biodiversity'),
                      zaxis = list(title = 'health')))

library(ggplot2)
# Convert matrix to a data frame
df <- as.data.frame(set1)
colnames(df) = c("economic", "biodiversity", "health")

# Create pairwise scatterplots
pairs <- expand.grid(names(df), names(df))
pairs <- pairs[pairs$Var1 != pairs$Var2, ]  # Remove diagonal pairs
pairs <- unique(t(apply(pairs, 1, sort)))  # Get unique pairs
pairs <- as.data.frame(pairs)
colnames(pairs) <- c("Col1", "Col2")

# Generate scatterplots
plots <- lapply(1:nrow(pairs), function(i) {
  ggplot(df, aes_string(x = pairs$Col1[i], y = pairs$Col2[i])) +
    geom_point() +
    labs(title = paste(pairs$Col1[i], "vs", pairs$Col2[i]))
})

# Display the plots
library(gridExtra)
combined_plot = do.call(grid.arrange, c(plots, ncol = 3))
ggsave("scatterplot_grid.png", combined_plot, width = 10, height = 6)
