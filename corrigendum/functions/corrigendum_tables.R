# Table and number helpers for corrigendum_school_garden_model.Rmd. They read the results saved by
# 01_model_comparison.Rmd, 02_value_of_information.Rmd and 03_pareto.Rmd (in outputs/) and return data
# frames for knitr::kable().

# ---- number formatting ------------------------------------------------------------------------
f0 <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
f1 <- function(x) formatC(x, format = "f", digits = 1)
f2 <- function(x) formatC(x, format = "f", digits = 2)
pct_change <- function(a, b) sprintf("%+.0f%%", 100 * (b - a) / abs(a))
arrow <- "→"

# ---- lookups ------------------------------------------------------------------------------------
# `comparison` = outputs/original_vs_corrected.csv; `mchk` = outputs/manuscript_check.csv
cmp_get <- function(comparison, variant, output, col = "mean") comparison[comparison$variant == variant & comparison$output == output, col]
ms_get <- function(mchk, item, col) mchk[mchk$item == item, col]

decision_outputs <- c(NPV_garden_inclusive = "Private school, passive garden",
                      NPV_garden_STEM_inclusive = "Private school, STEM garden",
                      NPV_garden_public_school_inclusive = "Public school, passive garden",
                      NPV_garden_STEM_public_school_inclusive = "Public school, STEM garden")
economic_outputs <- c(NPV_garden = "Private, passive (economic only)", NPV_garden_STEM = "Private, STEM (economic only)",
                      NPV_garden_public_school = "Public, passive (economic only)",
                      NPV_garden_STEM_public_school = "Public, STEM (economic only)")

# Percentage fall of the mean NPV of each decision option, published to all corrections
npv_drop_pct <- function(comparison) {
  sapply(names(decision_outputs), function(o) {
    100 * (cmp_get(comparison, "published", o) - cmp_get(comparison, "all_fixed", o)) / abs(cmp_get(comparison, "published", o))
  })
}
# Fall of the mean NPV from correction 3 alone (million VND)
fix3_drop <- function(comparison) {
  sapply(names(decision_outputs), function(o) cmp_get(comparison, "published", o) - cmp_get(comparison, "fix_maint_m2", o))
}

# ---- tables -------------------------------------------------------------------------------------
# Published against corrected results for the given outputs
npv_table <- function(comparison, outputs) {
  g <- function(v, o, col) cmp_get(comparison, v, o, col)
  do.call(rbind, lapply(names(outputs), function(o) data.frame(
    Output = outputs[[o]],
    `Mean, published (MC s.e.)` = sprintf("%s (%s)", f0(g("published", o, "mean")), f0(g("published", o, "mc_se"))),
    `Mean, corrected` = f0(g("all_fixed", o, "mean")),
    Change = pct_change(g("published", o, "mean"), g("all_fixed", o, "mean")),
    Median = paste(f0(g("published", o, "median")), arrow, f0(g("all_fixed", o, "median"))),
    `5th to 95th percentile` = sprintf("%s to %s %s %s to %s", f0(g("published", o, "p5")), f0(g("published", o, "p95")), arrow,
                                       f0(g("all_fixed", o, "p5")), f0(g("all_fixed", o, "p95"))),
    `P(NPV > 0)` = sprintf("%.2f %s %.2f", g("published", o, "prob_positive"), arrow, g("all_fixed", o, "prob_positive")),
    check.names = FALSE)))
}

# Mean by output for each correction alone
fix_table <- function(comparison, outputs) {
  v <- c(Published = "published", `Fix 1 only` = "fix_land_block", `Fix 2 only` = "fix_cost_zeroing",
         `Fix 3 only` = "fix_maint_m2", `All three` = "all_fixed")
  d <- do.call(rbind, lapply(names(outputs), function(o)
    data.frame(Output = outputs[[o]], t(sapply(v, function(x) f1(cmp_get(comparison, x, o)))), check.names = FALSE)))
  names(d)[-1] <- names(v)
  d
}

# Manuscript values against the published code (20 seeds) and the corrected model
manuscript_table <- function(mchk, items, descriptions) {
  do.call(rbind, lapply(seq_along(items), function(i) {
    x <- mchk[mchk$item == items[i], ]
    dg <- if (abs(x$manuscript) < 10) 2 else 0
    ff <- function(z) formatC(z, format = "f", digits = dg, big.mark = ",")
    data.frame(Quantity = descriptions[i], `MS line` = x$manuscript_line, Manuscript = ff(x$manuscript),
               `Published code: mean [min, max]` = sprintf("%s [%s, %s]", ff(x$published_code_mean), ff(x$published_min), ff(x$published_max)),
               `In range?` = ifelse(x$manuscript_within_published_range, "yes", "NO"),
               `Corrected model` = ff(x$corrected_mean), check.names = FALSE)
  }))
}

# PLS: variance explained, manuscript against published code and corrected model
pls_table <- function(ve) {
  man <- c(private_passive = 81, private_STEM = 75, public_passive = 34, public_STEM = 45)
  nm <- c(private_passive = "Private, passive", private_STEM = "Private, STEM", public_passive = "Public, passive", public_STEM = "Public, STEM")
  data.frame(Scenario = nm[names(man)], Manuscript = paste0(man, "%"),
             `Published code` = sprintf("%.0f%%", sapply(names(man), function(s) ve$var_explained_pct[ve$version == "published" & ve$scenario == s])),
             Corrected = sprintf("%.0f%%", sapply(names(man), function(s) ve$var_explained_pct[ve$version == "corrected" & ve$scenario == s])),
             check.names = FALSE, row.names = NULL)
}
# Predictors that enter or leave the VIP > 1 set between the published and the corrected model
pls_vip_changes <- function(top) {
  nm <- c(private_passive = "Private, passive", private_STEM = "Private, STEM", public_passive = "Public, passive", public_STEM = "Public, STEM")
  out <- unlist(lapply(names(nm), function(s) {
    a <- top$variable[top$version == "published" & top$scenario == s & top$VIP > 1]
    b <- top$variable[top$version == "corrected" & top$scenario == s & top$VIP > 1]
    if (setequal(a, b)) NULL else sprintf("%s: enters VIP > 1: %s; leaves: %s", nm[[s]], paste(setdiff(b, a), collapse = ", "), paste(setdiff(a, b), collapse = ", "))
  }))
  if (length(out)) out else "none"
}

# Per-variable EVPI: the four largest per option, published and corrected
evpi_variable_table <- function(evpi_var) {
  nm <- c(NPV_garden_inclusive = "Private, passive", NPV_garden_STEM_inclusive = "Private, STEM",
          NPV_garden_public_school_inclusive = "Public, passive", NPV_garden_STEM_public_school_inclusive = "Public, STEM")
  do.call(rbind, lapply(names(nm), function(o) do.call(rbind, lapply(c("published", "corrected"), function(v) {
    d <- evpi_var[evpi_var$version == v & evpi_var$output == o, ]
    d <- head(d[order(-d$EVPI), ], 4)
    data.frame(Option = nm[[o]], Model = v,
               `Largest EVPI (variable, million VND)` = if (max(d$EVPI) < 0.05) "all below 0.05" else paste0(d$variable, " ", sprintf("%.1f", d$EVPI), collapse = "; "),
               check.names = FALSE)
  }))))
}

# Pareto: table of fronts for the corrigendum
pareto_table <- function(summary_by_version) {
  p <- summary_by_version
  data.frame(Version = p$version, Scenario = p$scenario, Solutions = p$n_front,
             Economic = sprintf("%s to %s", f0(p$economic_min), f0(p$economic_max)),
             Biodiversity = sprintf("%s to %s", f1(p$biodiversity_min), f1(p$biodiversity_max)),
             Health = sprintf("%s to %s", f0(p$health_min), f0(p$health_max)))
}
