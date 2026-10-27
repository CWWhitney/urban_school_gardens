# Per-variable EVPI with the method of the manuscript (functions/multi_EVPI_test.R, a LOESS-based
# empirical EVPI). Slow: one LOESS fit per input and output.
evpi_per_variable <- function(fixes = TRUE, n = 1e4, seed = 42,
                              outputs = c("NPV_garden_inclusive", "NPV_garden_STEM_inclusive",
                                          "NPV_garden_public_school_inclusive",
                                          "NPV_garden_STEM_public_school_inclusive")) {
  mc <- run_garden_mc(fixes, n = n, seed = seed)
  tab <- data.frame(mc$x, mc$y[outputs])
  ev <- multi_EVPI_test(mc = tab, first_out_var = outputs[1])
  do.call(rbind, lapply(names(ev), function(o) cbind(output = o, ev[[o]], row.names = NULL)))
}
