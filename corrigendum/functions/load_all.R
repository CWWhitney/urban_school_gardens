# Loads every function of this repository (run from the repository root, or from an Rmd in it):
#   source("functions/load_all.R")
# The published model is NOT loaded here (it would mask the corrected school_garden_function);
# use load_original_model() from functions/run_garden_mc.R.
suppressPackageStartupMessages(library(decisionSupport))
for (f in c("vv", "chance_event", "discount",             # decisionSupport helpers used by the model
            "school_garden_model",                        # the (corrected) model
            "run_garden_mc", "garden_comparison",         # running and comparing model versions
            "pls_vip", "empirical_EVPI_test", "multi_EVPI_test", "per_variable_evpi",
            "voi_garden",                                 # value of information (voi)
            "pareto_optimization", "pareto_analysis"))    # multi-objective optimisation and plots
  source(file.path("functions", paste0(f, ".R")))
