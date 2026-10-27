# Partial least squares (PLS) regression and Variable Importance in Projection (VIP), as in the
# manuscript's Figure 5 (functions/pls_model.R and plot_pls.R of the original repository)

# VIP scores of a single-response PLS fit made with method = "oscorespls"
vip_scores <- function(object) {
  SS <- c(object$Yloadings)^2 * colSums(object$scores^2)
  W <- colSums(object$loading.weights^2)
  SSW <- sweep(object$loading.weights^2, 2, SS / W, "*")
  sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS))
}

# One-component PLS of each decision NPV on all varying inputs. Returns the variance explained
# and the predictors with VIP > 0.9 (sign of the coefficient included).
pls_vip_check <- function(mc, outputs = c(private_passive = "NPV_garden_inclusive",
                                          private_STEM = "NPV_garden_STEM_inclusive",
                                          public_passive = "NPV_garden_public_school_inclusive",
                                          public_STEM = "NPV_garden_STEM_public_school_inclusive"),
                          ncomp = 1) {
  x <- as.matrix(mc$x)
  x <- x[, apply(x, 2, sd) > 0]
  fits <- lapply(outputs, function(o) {
    y <- mc$y[[o]]
    m <- pls::plsr(y ~ x, method = "oscorespls", scale = TRUE, ncomp = ncomp)
    v <- vip_scores(m)
    d <- data.frame(variable = names(v), VIP = v, coef = m$coefficients[, , 1])
    d <- d[order(-d$VIP), ]
    list(var_explained_pct = 100 * cor(drop(fitted(m)), y)^2, n_vip_gt_1 = sum(v > 1), top = d[d$VIP > 0.9, ])
  })
  list(var_explained = data.frame(scenario = names(outputs),
                                  var_explained_pct = sapply(fits, `[[`, "var_explained_pct"),
                                  n_vip_gt_1 = sapply(fits, `[[`, "n_vip_gt_1"), row.names = NULL),
       top = do.call(rbind, lapply(names(fits), function(s) cbind(scenario = s, head(fits[[s]]$top, 8), row.names = NULL))))
}
