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
    # Perform crossover
    crossover_point <- sample(1:(num_vars-1), 1)
    child1 <- c(parent1[1:crossover_point], parent2[(crossover_point + 1):num_vars])
    child2 <- c(parent2[1:crossover_point], parent1[(crossover_point + 1):num_vars])
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
    population = my_population,
    mutation = my_mutation,
    crossover = my_crossover,
    popSize = 200,
    nObj = 3,
    maxiter = 100,
    names = decision_var_names
)

mat = result@fitness

rmoo::summary(result)

mat = sweep(-mat, 2, c(100, 1, 100) , `*`) # retransform

library(plotly)
plotly::plot_ly(x = mat[,1], y = mat[,2], z = mat[,3], 
        type = "scatter3d", mode = "markers",
        marker = list(color = 'blue', size = 5)) %>%
  layout(scene = list(xaxis = list(title = 'economic'),
                      yaxis = list(title = 'biodiversity'),
                      zaxis = list(title = 'health')))

# Create a 2D scatterplot using the first two columns
plot(mat[,2], mat[,3], 
     xlab = "biodiversity", ylab = "health", 
     main = "2D Scatterplot", 
     pch = 19, col = "blue")
