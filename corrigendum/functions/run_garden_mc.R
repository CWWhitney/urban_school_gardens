# Running the school garden model (decisionSupport front end)
#
# The three corrections to the published Garden_Model.R are switched with options() inside
# school_garden_function (functions/school_garden_model.R); these helpers set them, run the
# Monte Carlo simulation and put them back.

# Read the 90% CI input table (inputs/school_garden_estimate.csv) as a decisionSupport estimate
garden_estimate <- function(file = "inputs/school_garden_estimate.csv") {
  suppressWarnings(decisionSupport::estimate_read_csv(file))
}

# `fixes`: TRUE (all three corrections), FALSE (published behaviour), or a named logical vector
# c(land_block = , cost_zeroing = , maint_m2 = ) to switch them one at a time.
fix_flags <- function(fixes = TRUE) {
  if (length(fixes) == 1 && is.null(names(fixes))) {
    fixes <- c(land_block = fixes, cost_zeroing = fixes, maint_m2 = fixes)
  }
  stopifnot(all(c("land_block", "cost_zeroing", "maint_m2") %in% names(fixes)))
  fixes
}

# Monte Carlo simulation of the model. `model` defaults to the corrected model; pass the published
# one (see load_original_model()) to run it unchanged.
run_garden_mc <- function(fixes = TRUE, n = 1e4, seed = 42, estimate = garden_estimate(),
                          model = school_garden_function) {
  f <- fix_flags(fixes)
  old <- options(garden.fix_land_block = f[["land_block"]],
                 garden.fix_cost_zeroing = f[["cost_zeroing"]],
                 garden.fix_maint_m2 = f[["maint_m2"]])
  on.exit(options(old))
  set.seed(seed)
  suppressWarnings(decisionSupport::mcSimulation(
    estimate = estimate, model_function = model, numberOfModelRuns = n,
    functionSyntax = "plainNames"))
}

# The published Garden_Model.R, unchanged, returned as a function (kept out of the global
# environment so that it does not mask the corrected school_garden_function)
load_original_model <- function(file = "functions/school_garden_model_original.R") {
  env <- new.env(parent = globalenv())
  sys.source(file, envir = env)
  env$school_garden_function
}
