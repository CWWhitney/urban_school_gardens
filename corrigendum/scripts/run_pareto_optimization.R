# Launcher for one NSGA-II optimisation (about an hour): the work is done by
# run_pareto_optimization() in functions/pareto_optimization.R. From the repository root, one terminal
# or background job per scenario (3 cores each):
#   Rscript scripts/run_pareto_optimization.R nostem_priv
#   Rscript scripts/run_pareto_optimization.R stem_priv
#   Rscript scripts/run_pareto_optimization.R nostem_pub
#   Rscript scripts/run_pareto_optimization.R stem_pub
# Optional further arguments: population size, generations, draws per candidate, cores (paper: 80 80 2500 3).
args <- commandArgs(trailingOnly = TRUE)
source("functions/load_all.R")
num <- function(i, default) if (length(args) >= i) as.integer(args[i]) else default
res <- run_pareto_optimization(args[1], pop_size = num(2, 80L), max_iter = num(3, 80L),
                               n_draws = num(4, 2500L), cores = num(5, 3L))
cat(sprintf("%s: %d Pareto-optimal of %d, %.1f min\n", res$scenario, length(res$front), nrow(res$fitness), res$minutes))
