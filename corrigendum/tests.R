# Run from this folder: Rscript tests.R
#
# Sanity checks for the school garden analysis (functions in functions/). Approximate where Monte Carlo
# or regression error is involved; tolerances are set accordingly.
suppressPackageStartupMessages(library(voi))
source("functions/load_all.R")
est <- garden_estimate()
original <- load_original_model()

# 1. Every variable in the input table is used by the model.
vars <- rownames(est$marginal)
model_text <- paste(readLines("functions/school_garden_model.R"), collapse = " ")
stopifnot(all(sapply(vars, grepl, x = model_text, fixed = TRUE)))

# 2. With the three corrections switched off the corrected model reproduces the published model exactly
#    (same seed, same draws).
stopifnot(isTRUE(all.equal(run_garden_mc(FALSE, n = 3000, seed = 1, model = original)$y,
                           run_garden_mc(FALSE, n = 3000, seed = 1)$y)))

# 3. With the corrections, land_access now matters for the public-school NPV:
#    a higher chance of having land gives a higher mean NPV.
est_land <- function(lo, hi) { e <- est; e$marginal["land_access", c("lower", "upper")] <- c(lo, hi); e }
mean_public_npv <- function(e) mean(run_garden_mc(TRUE, n = 4000, seed = 2, estimate = e)$y$NPV_garden_public_school_inclusive)
stopifnot(mean_public_npv(est_land(0.9, 0.99)) > mean_public_npv(est_land(0.1, 0.3)))

# 4. Private-school results are not affected by the public-school failure events (the cost-zeroing
#    defect used to leak into them): costs in years 2+ are never zeroed, so total costs are positive in every run.
mc <- run_garden_mc(TRUE, n = 3000, seed = 1)
stopifnot(all(mc$y$total_costs > 0), all(mc$y$total_costs_STEM > 0))

# 5. Health and biodiversity are not changed by the corrections (same seed).
a <- run_garden_mc(FALSE, n = 3000, seed = 1, model = original)$y
b <- run_garden_mc(TRUE, n = 3000, seed = 1)$y
stopifnot(isTRUE(all.equal(a[c("health", "health_STEM", "biodiversity")], b[c("health", "health_STEM", "biodiversity")])))

# 6. VoI sanity: EVPI >= 0, EVPPI of an input <= EVPI (+ regression noise); bootstrap SEs are returned;
#    pop_voi() follows its documented formula; enbs() runs on an evsi-style data frame.
x <- garden_inputs(mc)
pub <- garden_decisions(mc)$public
whole <- voi::evpi(pub)
stopifnot(whole >= 0)
for (p in c("bureaucratic_barriers", "school_event_value", "if_community_likes"))
  stopifnot(voi::evppi(pub, x, pars = p)$evppi <= 1.1 * whole)
stopifnot(voi::evppi(pub, x, pars = list(names(x)), method = "earth")$evppi <= 1.1 * whole)
g <- voi::evppi(pub, x, pars = list("bureaucratic_barriers"), se = TRUE, B = 100)
stopifnot(is.finite(g$se), g$se > 0)
stopifnot(isTRUE(all.equal(voi::pop_voi(2, 100, 5, 0.05), 2 * 100 / 0.05 * (1 - exp(-0.05 * 5)))))
ev <- data.frame(n = c(10, 50), k = 1, evsi = c(1, 2))
en <- voi::enbs(ev, costs_setup = 2, costs_pp = 0.5, pop = 100, time = 5, dis = 0.05)
stopifnot(all(is.finite(en$enbs)), nrow(attr(en, "enbsmax")) == 1)

# 7. The Pareto optimisation runs end to end on a tiny problem and returns non-dominated solutions.
tiny <- run_pareto_optimization("stem_pub", pop_size = 8L, max_iter = 2L, n_draws = 60L, cores = 2L, out_dir = tempdir())
stopifnot(length(tiny$front) >= 1, ncol(tiny$fitness) == 3, all(tiny$population[, 1] >= 5 - 1e-9))

cat("All tests passed.\n")
