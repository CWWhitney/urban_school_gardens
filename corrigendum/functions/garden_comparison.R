# Comparing the published and the corrected model, and the manuscript with both

# The five model variants of the comparison: published, each correction on its own, all three
garden_variants <- list(
  published        = c(land_block = FALSE, cost_zeroing = FALSE, maint_m2 = FALSE),
  fix_land_block   = c(land_block = TRUE,  cost_zeroing = FALSE, maint_m2 = FALSE),
  fix_cost_zeroing = c(land_block = FALSE, cost_zeroing = TRUE,  maint_m2 = FALSE),
  fix_maint_m2     = c(land_block = FALSE, cost_zeroing = FALSE, maint_m2 = TRUE),
  all_fixed        = c(land_block = TRUE,  cost_zeroing = TRUE,  maint_m2 = TRUE))

# Model outputs compared (names as returned by school_garden_function)
garden_outputs <- c("NPV_garden_inclusive", "NPV_garden_STEM_inclusive",
                    "NPV_garden_public_school_inclusive", "NPV_garden_STEM_public_school_inclusive",
                    "NPV_garden", "NPV_garden_STEM", "NPV_garden_public_school",
                    "NPV_garden_STEM_public_school", "biodiversity", "health", "health_STEM")

# Mean, Monte Carlo s.e., median, 5th and 95th percentile and P(> 0) of one output
summarise_output <- function(v) {
  c(mean = mean(v), mc_se = sd(v) / sqrt(length(v)), median = median(v),
    p5 = unname(quantile(v, .05)), p95 = unname(quantile(v, .95)), prob_positive = mean(v > 0))
}

# Payoff matrices for voi: one column per option, "no garden" = 0 (NPVs are relative to it)
garden_decisions <- function(mc) {
  list(private = cbind(no_garden = 0, garden = mc$y$NPV_garden_inclusive,
                       garden_STEM = mc$y$NPV_garden_STEM_inclusive),
       public  = cbind(no_garden = 0, garden = mc$y$NPV_garden_public_school_inclusive,
                       garden_STEM = mc$y$NPV_garden_STEM_public_school_inclusive))
}

# Every variant of garden_variants (same seed, n runs): summary table, plus whole-model EVPI
compare_variants <- function(variants = garden_variants, outputs = garden_outputs, n = 1e4, seed = 42) {
  res <- lapply(names(variants), function(nm) {
    mc <- run_garden_mc(variants[[nm]], n = n, seed = seed)
    d <- garden_decisions(mc)
    rbind(
      data.frame(variant = nm, output = outputs,
                 t(sapply(outputs, function(o) summarise_output(mc$y[[o]]))), row.names = NULL),
      data.frame(variant = nm, output = c("EVPI_private", "EVPI_public"),
                 mean = c(voi::evpi(d$private), voi::evpi(d$public)), mc_se = NA, median = NA,
                 p5 = NA, p95 = NA, prob_positive = NA))
  })
  do.call(rbind, res)
}

# ---- the manuscript's numbers -------------------------------------------------------------
# Values quoted in Manuscript.pdf (Resubmission folder); `line` is the manuscript's own line number.
manuscript_values <- function() {
  read.csv(text = "item,line,manuscript
npv_median_private_passive,436,1333
npv_p5_private_passive,437,35
npv_p95_private_passive,437,4087
npv_prob_negative_private_passive,438,0.33
npv_median_private_STEM,439,835
npv_p5_private_STEM,440,-738
npv_p95_private_STEM,440,3549
npv_prob_negative_private_STEM,441,0.45
npv_median_public_passive,442,511
npv_p5_public_passive,443,-315
npv_p95_public_passive,443,3495
npv_median_public_STEM,445,-139
npv_p5_public_STEM,446,-738
npv_p95_public_STEM,446,2719
health_median,456,697
health_mean,457,763
health_p95,458,1929
health_STEM_median,460,594
health_STEM_mean,460,605
health_STEM_p95,461,1405
biodiversity_median,462,3.15
biodiversity_mean,463,4.65
cf_private_passive_median_y2,477,143
cf_private_passive_median_y5,477,180
cf_private_passive_mean_y2,478,208
cf_private_passive_mean_y5,478,261
cf_private_STEM_median_y1,480,-136
cf_private_STEM_median_y2,481,75
cf_private_STEM_median_y5,482,154
cf_private_STEM_mean_y1,482,-88
cf_private_STEM_mean_y5,482,208
cf_public_passive_median_y1,484,0
cf_public_passive_mean_y1,485,37
cf_public_passive_mean_y5,485,147")
}

# The quantities the manuscript quotes, from one Monte Carlo run
manuscript_stats <- function(mc) {
  y <- mc$y
  q <- function(v, p) unname(quantile(v, p))
  npv <- c(private_passive = "NPV_garden_inclusive", private_STEM = "NPV_garden_STEM_inclusive",
           public_passive = "NPV_garden_public_school_inclusive", public_STEM = "NPV_garden_STEM_public_school_inclusive")
  o <- c()
  for (s in names(npv)) {
    v <- y[[npv[[s]]]]
    o[paste0("npv_median_", s)] <- median(v)
    o[paste0("npv_p5_", s)] <- q(v, .05)
    o[paste0("npv_p95_", s)] <- q(v, .95)
    o[paste0("npv_prob_negative_", s)] <- mean(v < 0)
  }
  o["health_median"] <- median(y$health); o["health_mean"] <- mean(y$health); o["health_p95"] <- q(y$health, .95)
  o["health_STEM_median"] <- median(y$health_STEM); o["health_STEM_mean"] <- mean(y$health_STEM)
  o["health_STEM_p95"] <- q(y$health_STEM, .95)
  o["biodiversity_median"] <- median(y$biodiversity); o["biodiversity_mean"] <- mean(y$biodiversity)
  cf <- function(pre, yr) y[[paste0(pre, yr)]]
  for (yr in c(2, 5)) {
    o[paste0("cf_private_passive_median_y", yr)] <- median(cf("Cashflow_garden", yr))
    o[paste0("cf_private_passive_mean_y", yr)] <- mean(cf("Cashflow_garden", yr))
  }
  for (yr in c(1, 2, 5)) o[paste0("cf_private_STEM_median_y", yr)] <- median(cf("Cashflow_garden_STEM", yr))
  for (yr in c(1, 5)) o[paste0("cf_private_STEM_mean_y", yr)] <- mean(cf("Cashflow_garden_STEM", yr))
  o["cf_public_passive_median_y1"] <- median(cf("Cashflow_garden_public", 1))
  for (yr in c(1, 5)) o[paste0("cf_public_passive_mean_y", yr)] <- mean(cf("Cashflow_garden_public", yr))
  o
}

# Manuscript numbers against the published code and the corrected model over several Monte Carlo
# seeds, so that Monte Carlo variation can be told apart from real differences
manuscript_seed_check <- function(seeds = 1:20, n = 1e4) {
  original <- load_original_model()
  pub <- t(sapply(seeds, function(s) manuscript_stats(run_garden_mc(FALSE, n = n, seed = s, model = original))))
  cor <- t(sapply(seeds, function(s) manuscript_stats(run_garden_mc(TRUE, n = n, seed = s))))
  ms <- manuscript_values()
  do.call(rbind, lapply(seq_len(nrow(ms)), function(i) {
    it <- ms$item[i]; p <- pub[, it]; c <- cor[, it]
    data.frame(item = it, manuscript_line = ms$line[i], manuscript = ms$manuscript[i],
               published_code_mean = mean(p), published_min = min(p), published_max = max(p),
               manuscript_within_published_range = ms$manuscript[i] >= min(p) & ms$manuscript[i] <= max(p),
               corrected_mean = mean(c), corrected_min = min(c), corrected_max = max(c))
  }))
}

# Number of input variables in the table (the manuscript quotes 104, 71 and 75)
input_counts <- function(file = "inputs/school_garden_estimate.csv") {
  tab <- read.csv(file, check.names = FALSE)
  tab <- tab[tab$variable != "", ]
  c(variables = nrow(tab), constants = sum(tab$distribution == "const"),
    controllable = sum(tab$control_status == "controllable"), csv_lines = length(readLines(file)))
}

# Reuse a saved result unless refresh = TRUE: `expr` is only evaluated when the file is missing (or
# refresh = TRUE), then written to `file`. Lets the Rmd documents re-render quickly.
cached_csv <- function(file, expr, refresh = FALSE) {
  if (!refresh && file.exists(file)) return(read.csv(file))
  res <- expr
  write.csv(res, file, row.names = FALSE)
  res
}
