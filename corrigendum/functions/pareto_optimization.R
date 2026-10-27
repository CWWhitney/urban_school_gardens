# Multi-objective optimisation (NSGA-II, rmoo) of the controllable inputs of the school garden model.
# Port of Pareto/create_pareto_set.R of nifam_codas_school_garden, using the corrected model.
# Differences from the original script:
#  * rmoo 0.3.2 renamed the tournament selection (nsga_tourSelection -> rmooreal_tourSelection);
#  * the original mutation started from child = 0 instead of the parent's value, which pushed every
#    continuous gene to its lower bound; here it starts from the parent.
#
# Objectives per scenario: mean economic return (NPV), biodiversity and child health over
# `n_draws` random draws of the uncontrollable inputs (the same draws for every candidate).
pareto_objective_map <- list(
  nostem_priv = c(econ = "NPV_garden",                    health = "health"),
  stem_priv   = c(econ = "NPV_garden_STEM",               health = "health_STEM"),
  nostem_pub  = c(econ = "NPV_garden_public_school",      health = "health"),
  stem_pub    = c(econ = "NPV_garden_STEM_public_school", health = "health_STEM"))

# `scenario`: nostem_priv | stem_priv | nostem_pub | stem_pub. Paper settings: popSize 80,
# maxiter 80, n_draws 2500 (about an hour per scenario with 3 cores). Saves
# outputs/pareto/result_<scenario>.rds and returns the same list.
run_pareto_optimization <- function(scenario, pop_size = 80L, max_iter = 80L, n_draws = 2500L,
                                    cores = 3L, seed = 2026, out_dir = "outputs/pareto",
                                    input_file = "inputs/school_garden_estimate.csv") {
  obj <- pareto_objective_map[[scenario]]
  stopifnot(!is.null(obj))
  RNGkind("L'Ecuyer-CMRG")
  set.seed(seed)

  # Controllable (decision) and uncontrollable inputs
  tab <- read.csv(input_file, check.names = FALSE)
  names(tab) <- trimws(names(tab))
  tab <- tab[tab$variable != "", ]
  ctrl <- tab[tab$control_status == "controllable", ]
  is_binary <- ctrl$distribution == "tnorm_0_1"   # yes/no decisions, as in the paper
  lower <- ifelse(is_binary, 0, ctrl$lower)
  upper <- ifelse(is_binary, 1, ctrl$upper)
  draws <- suppressWarnings(decisionSupport::random(rho = garden_estimate(input_file), n = n_draws))
  draws <- draws[, !colnames(draws) %in% ctrl$variable, drop = FALSE]
  draw_rows <- lapply(seq_len(nrow(draws)), function(i) as.list(draws[i, ]))

  # Run the model body with the inputs as variables (what mcSimulation does)
  model <- school_garden_function
  run_one <- function(inputs) {
    environment(model) <- list2env(inputs, parent = globalenv())
    model()
  }
  fitness <- function(decision_vars) {
    dec <- as.list(setNames(decision_vars, ctrl$variable))
    chunks <- split(seq_along(draw_rows), cut(seq_along(draw_rows), cores, labels = FALSE))
    res <- parallel::mclapply(chunks, function(idx) {
      t(sapply(idx, function(i) {
        r <- run_one(c(draw_rows[[i]], dec))
        c(r[[obj["econ"]]], r$biodiversity, r[[obj["health"]]])
      }))
    }, mc.cores = cores, mc.set.seed = TRUE)
    m <- colMeans(do.call(rbind, res))
    c(-m[1] * 0.01, -m[2], -m[3] * 0.01)   # NSGA-II minimises: negative, scaled means
  }

  # Genetic operators, as in the paper's script
  my_population <- function(object) {
    n_vars <- length(object@lower)
    t(sapply(seq_len(object@popSize), function(i) {
      sapply(seq_len(n_vars), function(j)
        if (is_binary[j]) sample(c(0, 1), 1) else runif(1, object@lower[j], object@upper[j]))
    }))
  }
  my_mutation <- function(object, parent_idx) {
    parent <- object@population[parent_idx, ]
    child <- parent
    for (j in seq_along(parent)) {
      if (is_binary[j]) {
        if (runif(1) < 0.1) child[j] <- 1 - parent[j]
      } else {
        sd_j <- (object@upper[j] - object@lower[j]) * 0.1
        child[j] <- pmax(object@lower[j], pmin(object@upper[j], parent[j] + rnorm(1, 0, sd_j)))
      }
    }
    child
  }
  my_crossover <- function(object, parents) {
    p1 <- object@population[parents[1], ]
    p2 <- object@population[parents[2], ]
    c1 <- p1; c2 <- p2
    if (runif(1) < 0.8) {
      swap <- runif(length(p1)) >= 0.5
      c1[swap] <- p2[swap]; c2[swap] <- p1[swap]
    }
    list(children = rbind(c1, c2), fitness = c(NA, NA))
  }

  t0 <- Sys.time()
  result <- rmoo::nsga2(
    type = "real-valued", fitness = fitness, lower = lower, upper = upper,
    selection = rmoo::rmooreal_tourSelection, population = my_population,
    mutation = my_mutation, crossover = my_crossover,
    popSize = pop_size, nObj = 3, maxiter = max_iter, names = ctrl$variable, monitor = FALSE)
  out <- list(scenario = scenario, objectives = obj,
              settings = list(popSize = pop_size, maxiter = max_iter, n_draws = n_draws),
              names = ctrl$variable, population = result@population,
              fitness = sweep(-result@fitness, 2, c(100, 1, 100), `*`),   # retransformed
              front = rmoo::non_dominated_fronts(result)$fit[[1]],
              minutes = as.numeric(difftime(Sys.time(), t0, units = "mins")))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(out, file.path(out_dir, paste0("result_", scenario, ".rds")))
  out
}
