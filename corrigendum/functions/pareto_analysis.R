# Reading, summarising and plotting Pareto fronts (manuscript Figure 6 and the 3D Pareto plot)

pareto_scenarios <- c(nostem_priv = "Private, Passive garden", stem_priv = "Private, STEM garden",
                      nostem_pub = "Public, Passive garden", stem_pub = "Public, STEM garden")
pareto_colours <- c("Private, Passive garden" = "blue", "Private, STEM garden" = "red",
                    "Public, Passive garden" = "green", "Public, STEM garden" = "orange")
pareto_objectives <- c("Economic", "Biodiversity", "Health")

# Published fronts: the saved set1-4.csv of the original repository (objectives only)
read_published_fronts <- function(dir = "data/published_pareto") {
  l <- lapply(1:4, function(i) {
    d <- read.csv(file.path(dir, paste0("set", i, ".csv")), sep = " ")
    names(d)[1:3] <- pareto_objectives
    d
  })
  setNames(l, names(pareto_scenarios))
}

# Corrected fronts: from run_pareto_optimization() (objectives plus the decision variables)
read_corrected_fronts <- function(dir = "outputs/pareto") {
  l <- lapply(names(pareto_scenarios), function(s) {
    r <- readRDS(file.path(dir, paste0("result_", s, ".rds")))
    d <- cbind(as.data.frame(r$fitness)[r$front, ], as.data.frame(r$population)[r$front, ])
    names(d) <- c(pareto_objectives, r$names)
    d
  })
  setNames(l, names(pareto_scenarios))
}

# Published fronts WITH decision variables, read from the saved rmoo result objects
read_published_results <- function(dir = "data/optimization_results") {
  l <- lapply(names(pareto_scenarios), function(s) {
    e <- new.env(); load(file.path(dir, sprintf("result_%s_new.RData", s)), e)
    r <- e[[ls(e)[1]]]
    f <- rmoo::non_dominated_fronts(r)$fit[[1]]
    d <- cbind(as.data.frame(sweep(-r@fitness, 2, c(100, 1, 100), `*`))[f, ], as.data.frame(r@population)[f, ])
    names(d) <- c(pareto_objectives, r@names)
    d
  })
  setNames(l, names(pareto_scenarios))
}

# The paper keeps solutions with positive economic return. Its second filter (biodiversity > 9.6)
# was tied to the old results and is not applied.
keep_positive_economic <- function(fronts) lapply(fronts, function(d) d[d$Economic > 0, ])

pareto_summary_table <- function(sets) {
  do.call(rbind, lapply(names(sets), function(v) do.call(rbind, lapply(names(pareto_scenarios), function(s) {
    d <- sets[[v]][[s]]
    data.frame(version = v, scenario = pareto_scenarios[[s]], n_front = nrow(d),
               economic_min = min(d$Economic), economic_max = max(d$Economic),
               biodiversity_min = min(d$Biodiversity), biodiversity_max = max(d$Biodiversity),
               health_min = min(d$Health), health_max = max(d$Health))
  }))))
}

# ---- plots ---------------------------------------------------------------------------------
combine_fronts <- function(fronts) {
  do.call(rbind, lapply(names(pareto_scenarios), function(s) cbind(fronts[[s]][, pareto_objectives], Scenario = pareto_scenarios[[s]])))
}

# Pairwise 2D fronts in the layout of Figure 6
plot_pareto_pairs <- function(fronts, title) {
  dat <- combine_fronts(fronts)
  pair_plot <- function(x, y) {
    ggplot2::ggplot(dat, ggplot2::aes(.data[[x]], .data[[y]], colour = Scenario)) +
      ggplot2::geom_point(alpha = 0.7, size = 2.5) + ggplot2::scale_colour_manual(values = pareto_colours) +
      ggplot2::theme_gray(base_size = 12) + ggplot2::theme(legend.position = "none") +
      ggplot2::labs(x = paste(x, "(million VND)"), y = paste(y, "(million VND)"))
  }
  legend <- cowplot::get_legend(ggplot2::ggplot(dat, ggplot2::aes(Economic, Biodiversity, colour = Scenario)) +
    ggplot2::geom_point() + ggplot2::scale_colour_manual(values = pareto_colours) + ggplot2::theme_minimal())
  g <- cowplot::plot_grid(
    pair_plot("Economic", "Biodiversity") + ggplot2::theme(axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank()),
    legend, pair_plot("Economic", "Health"),
    pair_plot("Biodiversity", "Health") + ggplot2::theme(axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()), ncol = 2)
  cowplot::plot_grid(cowplot::ggdraw() + cowplot::draw_label(title, fontface = "bold"), g, ncol = 1, rel_heights = c(0.06, 1))
}

# Interactive 3D plot (plotly), as in the paper
plot_pareto_3d <- function(fronts, title) {
  p <- plotly::plot_ly()
  for (s in names(pareto_scenarios)) {
    d <- fronts[[s]]
    p <- plotly::add_trace(p, x = d$Economic, y = d$Biodiversity, z = d$Health, type = "scatter3d", mode = "markers",
                           marker = list(color = pareto_colours[[pareto_scenarios[[s]]]], size = 5), name = pareto_scenarios[[s]])
  }
  plotly::layout(p, title = title, scene = list(xaxis = list(title = "economic"),
                                                 yaxis = list(title = "biodiversity"), zaxis = list(title = "health")))
}

# Static 3D scatter (base graphics, for print and Word): perspective projection of the same points
plot_pareto_3d_static <- function(fronts, title, theta = 40, phi = 25) {
  all <- do.call(rbind, fronts)
  rng <- sapply(all[, pareto_objectives], range)
  pm <- persp(rng[, 1], rng[, 2], matrix(rng[1, 3], 2, 2), zlim = rng[, 3], theta = theta, phi = phi,
              xlab = "Economic", ylab = "Biodiversity", zlab = "Health", ticktype = "detailed",
              col = NA, border = "grey70", main = title, cex.axis = 0.6)
  for (s in names(pareto_scenarios)) {
    d <- fronts[[s]]
    points(trans3d(d$Economic, d$Biodiversity, d$Health, pm), pch = 16, col = pareto_colours[[pareto_scenarios[[s]]]], cex = 0.8)
  }
  legend("bottom", legend = unname(pareto_scenarios), col = unname(pareto_colours[pareto_scenarios]),
         pch = 16, bty = "n", cex = 0.7, ncol = 2)
}

# Published and corrected 3D plots side by side; saved as a png when `file` is given
save_pareto_3d_static <- function(sets, file, width = 2000, height = 900, res = 200) {
  png(file, width = width, height = height, res = res)
  par(mfrow = c(1, 2), mar = c(3, 1, 3, 1))
  plot_pareto_3d_static(sets$published, "Published model")
  plot_pareto_3d_static(sets$corrected, "Corrected model")
  invisible(dev.off())
}

# ---- checking the manuscript's statements (lines 558-573) against the fronts -----------------
# `pub` and `cor`: fronts with decision variables, positive economic return only. NULL for `cor` when not run.
pareto_claims <- function(pub, cor) {
  fmt <- function(x, d = 2) formatC(x, format = "f", digits = d)
  four <- function(S, f) paste(sapply(S, f), collapse = " / ")
  claims <- list(
    "Passive private garden is the most profitable (lines 558-559): highest economic return on the front" = function(S) {
      m <- sapply(S, function(d) max(d$Economic))
      paste0(pareto_scenarios[[names(which.max(m))]], " (max ", fmt(max(m), 0), ")")
    },
    "Gardens without STEM have higher health (559-560): median health, passive vs STEM, private; public" = function(S)
      sprintf("%s vs %s; %s vs %s", fmt(median(S$nostem_priv$Health), 0), fmt(median(S$stem_priv$Health), 0),
              fmt(median(S$nostem_pub$Health), 0), fmt(median(S$stem_pub$Health), 0)),
    "Trade-off between health and economic return in all scenarios (560-561): Spearman correlation" = function(S)
      four(S, function(d) fmt(cor(d$Economic, d$Health, method = "spearman"))),
    "Health and biodiversity synergistic, no trade-off (565-566): Spearman correlation" = function(S)
      four(S, function(d) fmt(cor(d$Biodiversity, d$Health, method = "spearman"))),
    "Animals raise health by 6.5% at 5% lower economic return (563-564): mean difference, animals vs none (our reading of the method)" = function(S) {
      ch <- sapply(S, function(d) c(mean(d$Health[d$if_animals_in_garden > 0.5]) / mean(d$Health[d$if_animals_in_garden <= 0.5]) - 1,
                                    mean(d$Economic[d$if_animals_in_garden > 0.5]) / mean(d$Economic[d$if_animals_in_garden <= 0.5]) - 1))
      sprintf("health %s%%, economic %s%%", fmt(100 * mean(ch[1, ]), 1), fmt(100 * mean(ch[2, ]), 1))
    },
    "Optimal garden size is the largest area (568-569): share of front solutions with size >= 95 m2" = function(S)
      four(S, function(d) fmt(mean(d$size_of_garden >= 95))),
    "School canteen unanimously chosen (569-570): share of front solutions with a canteen" = function(S)
      four(S, function(d) fmt(mean(d$if_school_has_canteen > 0.5))))
  data.frame(claim = names(claims), published = sapply(claims, function(f) f(pub)),
             corrected = sapply(claims, function(f) if (is.null(cor)) "not run" else f(cor)), row.names = NULL)
}

# ---- why does the front change? garden size vs economic return --------------------------------
# Mean economic return (NPV_garden) and biodiversity of a 5 m2 and a 100 m2 garden (canteen, no animals,
# 10 events/yr, no parking; `n_draws` draws of the other inputs), for each combination of corrections.
size_tradeoff <- function(n_draws = 3000, seed = 7, sizes = c(5, 100)) {
  est <- garden_estimate()
  set.seed(seed)
  dr <- suppressWarnings(decisionSupport::random(rho = est, n = n_draws))
  ctrl <- c("size_of_garden", "if_animals_in_garden", "if_school_has_canteen", "school_event_freq", "if_parking")
  rows <- lapply(seq_len(nrow(dr)), function(i) as.list(dr[i, !colnames(dr) %in% ctrl]))
  model <- school_garden_function
  run <- function(size, f) {
    old <- options(garden.fix_land_block = f[[1]], garden.fix_cost_zeroing = f[[2]], garden.fix_maint_m2 = f[[3]])
    on.exit(options(old))
    set.seed(1)
    rowMeans(sapply(rows, function(r) {
      environment(model) <- list2env(c(r, list(size_of_garden = size, if_animals_in_garden = 0, if_school_has_canteen = 1,
                                               school_event_freq = 10, if_parking = 0)), parent = globalenv())
      o <- model(); c(o$NPV_garden, o$biodiversity)
    }))
  }
  cfgs <- list(c(FALSE, FALSE, FALSE), c(TRUE, FALSE, FALSE), c(FALSE, TRUE, FALSE), c(FALSE, FALSE, TRUE), c(TRUE, TRUE, TRUE))
  do.call(rbind, lapply(cfgs, function(cfg) {
    a <- run(sizes[1], cfg); b <- run(sizes[2], cfg)
    data.frame(fix_land_block = cfg[1], fix_cost_zeroing = cfg[2], fix_maint_m2 = cfg[3],
               npv_size5 = a[1], npv_size100 = b[1], biodiversity_size5 = a[2], biodiversity_size100 = b[2])
  }))
}
