# Corrigendum material: corrected school garden model and re-analysis

This folder is **additive**: it was added on the branch `corrected-model-corrigendum` and nothing outside
`corrigendum/` was changed, so `main` (the version published with the manuscript, Zenodo v1.0.0) is untouched.

It documents three coding defects found in `Garden_Model.R` while re-using the model for a value-of-information
analysis, and how the manuscript's results change when they are corrected:

> Whitney, C., Luu, T. T. G., Kopton, J. & Luedeling, E. (2026) *A decision-analysis framework for school garden
> investments in Vietnam: evaluating trade-offs for nutrition, biodiversity, and economic outcomes.*
> Humanities and Social Sciences Communications 13(1), 580. https://doi.org/10.1057/s41599-026-07204-0

## The three defects (lines of `Garden_Model.R` at v1.0.0)

1. **No-land branch overwritten (lines 483-546).** `land_access` has no effect on any NPV.
2. **Cost-zeroing assignment (lines 532, 537).** `total_cost_public_school <- (total_cost[2:number_of_years] <- 0)`
   zeroes years 2-n of `total_cost` itself, which also feeds the private-school results.
3. **Per-m2 maintenance costs never added (lines 100-105).**

The corrected model is `functions/school_garden_model.R`; each correction is an `options()` switch. The published
model is kept unchanged in `functions/school_garden_model_original.R` (its body is identical to `Garden_Model.R` in
this repository, and `inputs/school_garden_estimate.csv` is identical to `data/inputs_school_garden.csv`).
With all three switches off the corrected model reproduces the published one exactly (`tests.R`).

## Contents

| File | What it does |
|------|--------------|
| `corrigendum_school_garden_model.Rmd` | Builds the corrigendum document (`.docx`) |
| `corrigendum_school_garden_model_CW.docx` | Edited version of the corrigendum draft |
| `01_model_comparison.Rmd` | Published and corrected model, each correction alone, manuscript numbers against both, PLS (VIP), per-variable EVPI |
| `02_value_of_information.Rmd` | Value of information with `voi` (EVPI, EVPPI, EVSI, net benefit of sampling) |
| `03_pareto.Rmd` | Pareto fronts and the 3D Pareto plot, published and corrected |
| `functions/` | All functions (`load_all.R` loads them) |
| `scripts/run_pareto_optimization.R` | Launcher for one NSGA-II optimisation (about an hour per scenario) |
| `inputs/`, `data/`, `outputs/` | Inputs, saved published results, and saved results of the analyses |
| `tests.R` | Sanity checks |

## Running it

Run from this folder (`corrigendum/`). Needs `decisionSupport`, `voi`, `rPref`, `rmoo`, `ecr`, `pls`, `plotly`,
`cowplot`, `fANCOVA`.

```r
source("tests.R")
rmarkdown::render("01_model_comparison.Rmd")   # params = list(refresh = TRUE) to recompute
rmarkdown::render("02_value_of_information.Rmd")
rmarkdown::render("03_pareto.Rmd")
rmarkdown::render("corrigendum_school_garden_model.Rmd")
```

The Pareto optimisation results (`outputs/pareto/result_*.rds`) are saved; to re-run them, see `03_pareto.Rmd`.

## Status

Draft for the co-authors, prepared with Claude Code (Anthropic). The comparison is between the published code and the
corrected code on the same inputs; only coding defects were corrected, not the inputs or model structure. Questions for
the authors are marked **[authors]** in the corrigendum. The Pareto fronts are a single optimisation run per scenario.
