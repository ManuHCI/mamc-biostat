# =====================================================================
#  DATA VISUALIZATION STUDIO
#  Publication-quality, colour-labelled charts (ggplot2) +
#  equivalent Python (pandas / seaborn / matplotlib) code for each chart.
# =====================================================================
suppressPackageStartupMessages(library(ggplot2))

PALETTES <- list(
  `MAMC default (colour-blind safe)` = PAL,
  `Okabe-Ito (colour-blind safe)` = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999"),
  `Pastel` = c("#8dd3c7", "#fdb462", "#bebada", "#fb8072", "#80b1d3", "#b3de69", "#fccde5", "#d9d9d9"),
  `Bold` = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d", "#666666"),
  `MAMC maroon shades` = c("#7b1c2e", "#a8394d", "#cf6b7c", "#e9a8b3", "#5a1220", "#3d0b15", "#f4d3d9", "#999999"),
  `Grey (print / journal)` = c("#252525", "#636363", "#969696", "#bdbdbd", "#525252", "#737373", "#d9d9d9", "#000000")
)
pal_get <- function(name, n) rep(PALETTES[[name]] %||% PAL, length.out = max(n, 1))

CHARTS <- list(
  histogram = list(name = "Histogram (distribution)", inputs = c("x_num", "group")),
  density = list(name = "Density curve (smooth distribution)", inputs = c("x_num", "group")),
  boxplot = list(name = "Box plot (+ data points)", inputs = c("y_num", "x_cat")),
  violin = list(name = "Violin plot", inputs = c("y_num", "x_cat")),
  mean_error = list(name = "Mean with error bars (SD / SE / 95% CI)", inputs = c("y_num", "x_cat", "group")),
  bar_count = list(name = "Bar chart of counts / percentages", inputs = c("x_cat", "group")),
  pie = list(name = "Pie / donut chart", inputs = c("x_cat")),
  scatter = list(name = "Scatter plot (+ regression line)", inputs = c("x_num", "y_num", "group")),
  line_time = list(name = "Line chart over time (repeated columns)", inputs = c("vars", "group")),
  heatmap = list(name = "Correlation heat map", inputs = c("vars")),
  qq = list(name = "Normal Q-Q plot", inputs = c("x_num", "group")),
  forest = list(name = "Forest plot of group means (95% CI)", inputs = c("y_num", "x_cat"))
)

theme_mamc <- function(base = 13, legend = "right") {
  theme_minimal(base_size = base) +
    theme(plot.title = element_text(face = "bold", colour = "#0b0b0b"), plot.subtitle = element_text(colour = "#52514e"),
          plot.caption = element_text(colour = "#8a8984", size = rel(0.75)), axis.title = element_text(colour = "#52514e"),
          axis.text = element_text(colour = "#52514e"), panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = "#ecebe8", linewidth = 0.4),
          legend.position = legend, legend.title = element_text(face = "bold", size = rel(0.85)), plot.background = element_rect(fill = "white", colour = NA))
}

summ_ci <- function(y, err) {
  y <- y[!is.na(y)]; n <- length(y); m <- mean(y); s <- sd(y)
  e <- switch(err, SD = s, SE = s / sqrt(n), `95% CI` = qt(.975, max(n - 1, 1)) * s / sqrt(n))
  data.frame(n = n, mean = m, lo = m - e, hi = m + e)
}

# o = list(chart, x, y, group, vars, title, subtitle, xlab, ylab, palette, labels(bool), base, legend, err, bins, style, line, points)
build_chart <- function(d, o) {
  g <- if (!is.null(o$group) && o$group != "(none)") o$group else NULL
  lab_on <- isTRUE(o$labels); cap <- "Made with MAMC BioStat"
  pal <- function(n) pal_get(o$palette, n)
  fac <- function(v) droplevels(as.factor(v))
  ch <- o$chart
  p <- switch(ch,
    histogram = {
      dd <- data.frame(x = d[[o$x]]); if (!is.null(g)) dd$g <- fac(d[[g]]); dd <- na.omit(dd)
      if (is.null(g)) {
        p <- ggplot(dd, aes(x)) + geom_histogram(aes(y = after_stat(density)), bins = o$bins, fill = pal(1), colour = "white", linewidth = 0.5, alpha = 0.9)
        if (isTRUE(o$line)) p <- p + stat_function(fun = dnorm, args = list(mean = mean(dd$x), sd = sd(dd$x)), colour = MAMC_MAROON, linewidth = 1)
        if (lab_on) p <- p + stat_bin(aes(y = after_stat(density), label = ifelse(after_stat(count) > 0, after_stat(count), "")), bins = o$bins, geom = "text", vjust = -0.4, size = 3.2, colour = "#52514e")
        p + labs(y = "Density")
      } else ggplot(dd, aes(x, fill = g)) + geom_histogram(bins = o$bins, colour = "white", linewidth = 0.4, alpha = 0.75, position = "identity") +
          scale_fill_manual(values = pal(nlevels(dd$g)), name = g) + labs(y = "Count")
    },
    density = {
      dd <- data.frame(x = d[[o$x]]); dd$g <- if (!is.null(g)) fac(d[[g]]) else factor("All"); dd <- na.omit(dd)
      p <- ggplot(dd, aes(x, fill = g, colour = g)) + geom_density(alpha = 0.3, linewidth = 1) + scale_fill_manual(values = pal(nlevels(dd$g)), name = g) +
        scale_colour_manual(values = pal(nlevels(dd$g)), name = g) + labs(y = "Density")
      if (is.null(g)) p <- p + theme(legend.position = "none")
      if (lab_on) { md <- aggregate(x ~ g, dd, median); p <- p + geom_vline(data = md, aes(xintercept = x, colour = g), linetype = 2, show.legend = FALSE) +
        geom_label(data = md, aes(x = x, y = 0, label = paste("Median", round(x, 1))), colour = "#0b0b0b", fill = "white", vjust = 0, size = 3.2, show.legend = FALSE) }
      p
    },
    boxplot = , violin = {
      dd <- data.frame(y = d[[o$y]]); dd$x <- if (!is.null(o$xc) && o$xc != "(none)") fac(d[[o$xc]]) else factor(o$y); dd <- na.omit(dd); k <- nlevels(dd$x)
      p <- ggplot(dd, aes(x, y, fill = x))
      p <- if (ch == "boxplot") p + geom_boxplot(width = 0.55, alpha = 0.8, outlier.shape = if (isTRUE(o$points)) NA else 19, colour = "#3a3a38")
           else p + geom_violin(alpha = 0.75, colour = "#3a3a38", trim = FALSE) + geom_boxplot(width = 0.12, fill = "white", outlier.shape = NA)
      if (isTRUE(o$points)) p <- p + geom_jitter(width = 0.12, height = 0, size = 1.8, alpha = 0.45, colour = "#3a3a38")
      p <- p + stat_summary(fun = mean, geom = "point", shape = 23, size = 3.5, fill = "white", colour = "#0b0b0b") + scale_fill_manual(values = pal(k), guide = "none")
      if (lab_on) { s <- aggregate(y ~ x, dd, function(z) c(med = median(z), n = length(z), top = max(z))); s <- data.frame(x = s$x, s$y)
        p <- p + geom_text(data = s, aes(x = x, y = top, label = paste0("n = ", n, "\nmedian ", round(med, 1))), inherit.aes = FALSE, vjust = -0.3, size = 3.2, colour = "#52514e") +
          scale_y_continuous(expand = expansion(mult = c(0.05, 0.18))) }
      p + labs(caption = paste(cap, "| diamond = mean"))
    },
    mean_error = {
      dd <- data.frame(y = d[[o$y]], x = fac(d[[o$xc]])); if (!is.null(g)) dd$g <- fac(d[[g]]) else dd$g <- factor("All"); dd <- na.omit(dd)
      s <- do.call(rbind, lapply(split(dd, list(dd$x, dd$g), drop = TRUE), function(z) cbind(x = z$x[1], g = z$g[1], summ_ci(z$y, o$err))))
      pd <- position_dodge(width = 0.8)
      p <- if (identical(o$style, "Dot + error bar")) ggplot(s, aes(x, mean, colour = g)) + geom_point(size = 4, position = pd) +
               geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2, linewidth = 0.9, position = pd) + scale_colour_manual(values = pal(nlevels(s$g)), name = g)
           else ggplot(s, aes(x, mean, fill = g)) + geom_col(width = 0.7, position = pd, colour = "white", linewidth = 0.6) +
               geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2, linewidth = 0.7, position = pd, colour = "#3a3a38") + scale_fill_manual(values = pal(nlevels(s$g)), name = g)
      if (lab_on) p <- p + geom_text(aes(y = hi, label = round(mean, 1), group = g), position = pd, vjust = -0.6, size = 3.4, colour = "#0b0b0b")
      if (is.null(g)) p <- p + theme(legend.position = "none")
      p + labs(y = paste0("Mean ", o$y, " (+/- ", o$err, ")"), caption = cap) + scale_y_continuous(expand = expansion(mult = c(0, 0.12)))
    },
    bar_count = {
      dd <- data.frame(x = fac(d[[o$xc]])); if (!is.null(g)) dd$g <- fac(d[[g]]); dd <- na.omit(dd)
      if (is.null(g)) {
        s <- as.data.frame(table(dd$x)); names(s) <- c("x", "n"); s$pct <- 100 * s$n / sum(s$n)
        ggplot(s, aes(x, n, fill = x)) + geom_col(width = 0.7, colour = "white") + scale_fill_manual(values = pal(nrow(s)), guide = "none") +
          { if (lab_on) geom_text(aes(label = paste0(n, " (", round(pct, 1), "%)")), vjust = -0.5, size = 3.6, colour = "#0b0b0b") } +
          scale_y_continuous(expand = expansion(mult = c(0, 0.12))) + labs(y = "Number of subjects")
      } else {
        s <- as.data.frame(table(dd$x, dd$g)); names(s) <- c("x", "g", "n"); s$pct <- ave(s$n, s$x, FUN = function(z) 100 * z / sum(z))
        mode <- o$style %||% "Grouped"
        if (mode == "Stacked 100%") ggplot(s, aes(x, pct, fill = g)) + geom_col(width = 0.7, colour = "white", linewidth = 0.6) +
            { if (lab_on) geom_text(aes(label = ifelse(pct >= 4, paste0(round(pct), "%"), "")), position = position_stack(vjust = 0.5), size = 3.4, colour = "white", fontface = "bold") } +
            scale_fill_manual(values = pal(nlevels(s$g)), name = g) + labs(y = paste("% within", o$xc))
        else if (mode == "Stacked") ggplot(s, aes(x, n, fill = g)) + geom_col(width = 0.7, colour = "white", linewidth = 0.6) +
            { if (lab_on) geom_text(aes(label = ifelse(n > 0, n, "")), position = position_stack(vjust = 0.5), size = 3.4, colour = "white", fontface = "bold") } +
            scale_fill_manual(values = pal(nlevels(s$g)), name = g) + labs(y = "Count")
        else ggplot(s, aes(x, n, fill = g)) + geom_col(width = 0.75, position = position_dodge(0.8), colour = "white", linewidth = 0.6) +
            { if (lab_on) geom_text(aes(label = n), position = position_dodge(0.8), vjust = -0.5, size = 3.4, colour = "#0b0b0b") } +
            scale_fill_manual(values = pal(nlevels(s$g)), name = g) + scale_y_continuous(expand = expansion(mult = c(0, 0.12))) + labs(y = "Count")
      }
    },
    pie = {
      s <- as.data.frame(table(fac(na.omit(d[[o$xc]])))); names(s) <- c("x", "n"); s$pct <- 100 * s$n / sum(s$n)
      hole <- if (identical(o$style, "Pie")) 0 else 1.2
      ggplot(s, aes(x = 2, y = n, fill = x)) + geom_col(colour = "white", linewidth = 1, width = 1) + coord_polar(theta = "y", start = 0) + xlim(c(if (hole > 0) 0.6 else 1.5, 2.5)) +
        { if (lab_on) geom_text(aes(label = paste0(round(pct, 1), "%\n(", n, ")")), position = position_stack(vjust = 0.5), size = 3.6, colour = "white", fontface = "bold") } +
        scale_fill_manual(values = pal(nrow(s)), name = o$xc) + theme_mamc(o$base, o$legend) + theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), panel.grid.major = element_blank())
    },
    scatter = {
      dd <- data.frame(x = d[[o$x]], y = d[[o$y]]); if (!is.null(g)) dd$g <- fac(d[[g]]); dd <- na.omit(dd)
      p <- if (is.null(g)) ggplot(dd, aes(x, y)) + geom_point(size = 2.6, alpha = 0.7, colour = pal(1))
           else ggplot(dd, aes(x, y, colour = g)) + geom_point(size = 2.6, alpha = 0.75) + scale_colour_manual(values = pal(nlevels(dd$g)), name = g)
      if (isTRUE(o$line)) p <- p + geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 1, alpha = 0.15, colour = if (is.null(g)) MAMC_MAROON else NULL)
      if (lab_on) { ct <- cor.test(dd$x, dd$y); p <- p + annotate("label", x = -Inf, y = Inf, hjust = -0.05, vjust = 1.2, size = 3.6, fill = "white",
        label = paste0("Pearson r = ", round(ct$estimate, 2), ", ", p_eq(ct$p.value), "\nn = ", nrow(dd))) }
      p
    },
    line_time = {
      need(length(o$vars) >= 2, "Select 2 or more repeated columns in time order.")
      dd <- d[, c(o$vars, g), drop = FALSE]; dd <- dd[complete.cases(dd), , drop = FALSE]; gg <- if (!is.null(g)) fac(dd[[g]]) else factor(rep("All", nrow(dd)))
      s <- do.call(rbind, lapply(levels(gg), function(l) do.call(rbind, lapply(seq_along(o$vars), function(i) cbind(g = l, t = i, summ_ci(dd[[o$vars[i]]][gg == l], o$err))))))
      s$g <- factor(s$g, levels(gg)); pd <- position_dodge(0.15)
      p <- ggplot(s, aes(t, mean, colour = g, group = g)) + geom_line(linewidth = 1.2, position = pd) + geom_point(size = 3.5, position = pd) +
        geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.12, linewidth = 0.8, position = pd) + { if (!lab_on) scale_x_continuous(breaks = seq_along(o$vars), labels = o$vars) } +
        scale_colour_manual(values = pal(nlevels(s$g)), name = g) + labs(y = paste0("Mean (+/- ", o$err, ")"))
      if (lab_on) p <- p + geom_label(data = s[s$t == length(o$vars), ], aes(label = paste0(g, ": ", round(mean, 1))), hjust = -0.15, size = 3.2, fill = "white", label.size = 0, show.legend = FALSE) +
        scale_x_continuous(breaks = seq_along(o$vars), labels = o$vars, expand = expansion(mult = c(0.05, 0.25)))
      if (is.null(g)) p <- p + theme(legend.position = "none")
      p
    },
    heatmap = {
      need(length(o$vars) >= 2, "Select at least 2 numeric variables.")
      R <- cor(d[, o$vars], use = "pairwise.complete.obs", method = o$method %||% "pearson")
      s <- data.frame(a = factor(rep(colnames(R), each = ncol(R)), colnames(R)), b = factor(rep(colnames(R), ncol(R)), rev(colnames(R))), r = as.vector(R))
      ggplot(s, aes(a, b, fill = r)) + geom_tile(colour = "white", linewidth = 1) + { if (lab_on) geom_text(aes(label = sprintf("%.2f", r), colour = abs(r) > 0.6), size = 3.8, show.legend = FALSE) } +
        scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "#0b0b0b")) + scale_fill_gradient2(low = "#2a78d6", mid = "#f2f1ee", high = "#e34948", midpoint = 0, limits = c(-1, 1), name = "r") +
        coord_fixed() + labs(x = NULL, y = NULL) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    },
    qq = {
      dd <- data.frame(x = d[[o$x]]); dd$g <- if (!is.null(g)) fac(d[[g]]) else factor("All"); dd <- na.omit(dd)
      p <- ggplot(dd, aes(sample = x, colour = g)) + stat_qq(size = 2.2, alpha = 0.7) + stat_qq_line(linewidth = 1) + scale_colour_manual(values = pal(nlevels(dd$g)), name = g) +
        labs(x = "Theoretical normal quantiles", y = paste("Sample quantiles of", o$x))
      if (is.null(g)) p <- p + theme(legend.position = "none")
      if (lab_on && is.null(g)) p <- p + annotate("label", x = -Inf, y = Inf, hjust = -0.05, vjust = 1.2, fill = "white", size = 3.6, label = paste("Shapiro-Wilk", p_eq(shapiro_safe(dd$x))))
      p
    },
    forest = {
      dd <- data.frame(y = d[[o$y]], x = fac(d[[o$xc]])); dd <- na.omit(dd)
      s <- do.call(rbind, lapply(levels(dd$x), function(l) cbind(x = l, summ_ci(dd$y[dd$x == l], "95% CI")))); s$x <- factor(s$x, rev(levels(dd$x)))
      ggplot(s, aes(mean, x, colour = x)) + geom_vline(xintercept = mean(dd$y), linetype = 2, colour = "#8a8984") + geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2, linewidth = 1) +
        geom_point(shape = 15, size = 4) + { if (lab_on) geom_text(aes(x = hi, label = sprintf("%.1f (%.1f to %.1f), n=%d", mean, lo, hi, n)), hjust = -0.08, size = 3.4, colour = "#0b0b0b") } +
        scale_colour_manual(values = pal(nrow(s)), guide = "none") + scale_x_continuous(expand = expansion(mult = c(0.05, 0.45))) + labs(x = paste("Mean", o$y, "(95% CI)"), y = NULL, caption = paste(cap, "- dashed line = overall mean"))
    },
    stop("Unknown chart"))
  if (ch != "pie") p <- p + theme_mamc(o$base, o$legend)
  dl <- default_labels(o)
  lb <- list(title = o$title %||% dl$title, subtitle = if (nzchar(o$subtitle %||% "")) o$subtitle else NULL)
  if (!(ch %in% c("heatmap", "pie", "forest", "qq"))) lb$x <- o$xlab %||% dl$x
  if (nzchar(o$ylab %||% "") && !(ch %in% c("heatmap", "pie"))) lb$y <- o$ylab else if (!is.null(dl$y)) lb$y <- dl$y
  if (!(ch %in% c("boxplot", "violin", "mean_error", "forest"))) lb$caption <- cap
  p + do.call(labs, lb)
}

default_labels <- function(o) {
  g <- if (!is.null(o$group) && o$group != "(none)") paste(" by", o$group) else ""
  switch(o$chart,
    histogram = list(title = paste0("Distribution of ", o$x, g), x = o$x), density = list(title = paste0("Density of ", o$x, g), x = o$x),
    boxplot = , violin = list(title = paste0(o$y, if (!is.null(o$xc) && o$xc != "(none)") paste(" by", o$xc) else ""), x = if (!is.null(o$xc) && o$xc != "(none)") o$xc else "", y = o$y),
    mean_error = list(title = paste0("Mean ", o$y, " by ", o$xc, g), x = o$xc), bar_count = list(title = paste0(o$xc, g), x = o$xc),
    pie = list(title = paste("Distribution of", o$xc), x = NULL), scatter = list(title = paste0(o$y, " vs ", o$x, g), x = o$x, y = o$y),
    line_time = list(title = paste0("Change over time", g), x = "Time point"), heatmap = list(title = "Correlation matrix", x = NULL),
    qq = list(title = paste("Normal Q-Q plot of", o$x), x = NULL), forest = list(title = paste("Mean", o$y, "by", o$xc), x = NULL))
}

# ------------------------------------------------------------------
#  Python code generator (pandas + seaborn + matplotlib)
# ------------------------------------------------------------------
py_list <- function(v) paste0("[", paste0("'", gsub("'", "\\\\'", v), "'", collapse = ", "), "]")
py_q <- function(s) paste0("'", gsub("'", "\\\\'", s), "'")

python_code <- function(o, file_name = "your_data.xlsx") {
  g <- if (!is.null(o$group) && o$group != "(none)") o$group else NULL
  pal <- py_list(PALETTES[[o$palette]] %||% PAL); lab <- isTRUE(o$labels)
  dl <- default_labels(o); title <- o$title %||% dl$title
  head <- paste0(
"# Python version of this MAMC BioStat chart
# Requires:  pip install pandas openpyxl matplotlib seaborn scipy
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats

df = pd.read_excel(", py_q(file_name), ")          # or pd.read_csv('file.csv')
palette = ", pal, "
sns.set_theme(style='whitegrid', font_scale=", round(o$base / 12, 2), ")
fig, ax = plt.subplots(figsize=(8, 5.5))
")
  body <- switch(o$chart,
    histogram = if (is.null(g)) paste0("data = df[", py_q(o$x), "].dropna()\n",
      "counts, bins, patches = ax.hist(data, bins=", o$bins, ", density=True, color=palette[0], edgecolor='white', alpha=0.9)\n",
      if (isTRUE(o$line)) "xs = np.linspace(data.min(), data.max(), 200)\nax.plot(xs, stats.norm.pdf(xs, data.mean(), data.std()), color='#7b1c2e', lw=2, label='Normal curve')\nax.legend()\n" else "",
      if (lab) "n_counts, _ = np.histogram(data, bins=bins)\nfor c, h, x0, x1 in zip(n_counts, counts, bins[:-1], bins[1:]):\n    if c > 0: ax.text((x0 + x1) / 2, h, str(c), ha='center', va='bottom', fontsize=9)\n" else "",
      "ax.set_ylabel('Density')\n")
      else paste0("sns.histplot(data=df, x=", py_q(o$x), ", hue=", py_q(g), ", bins=", o$bins, ", palette=palette, alpha=0.6, element='bars', ax=ax)\n"),
    density = paste0("sns.kdeplot(data=df, x=", py_q(o$x), if (!is.null(g)) paste0(", hue=", py_q(g)) else ", color=palette[0]", ", fill=True, alpha=0.3, linewidth=2", if (!is.null(g)) ", palette=palette" else "", ", ax=ax)\n"),
    boxplot = , violin = { xc <- if (!is.null(o$xc) && o$xc != "(none)") o$xc else NULL
      paste0(if (o$chart == "boxplot") "sns.boxplot(" else "sns.violinplot(", "data=df, ", if (!is.null(xc)) paste0("x=", py_q(xc), ", hue=", py_q(xc), ", order=sorted(df[", py_q(xc), "].dropna().unique()), hue_order=sorted(df[", py_q(xc), "].dropna().unique()), legend=False, ") else "", "y=", py_q(o$y), ", palette=palette[:", if (!is.null(xc)) paste0("df[", py_q(xc), "].nunique()") else "1", "], width=0.55, ax=ax)\n",
        if (isTRUE(o$points)) paste0("sns.stripplot(data=df, ", if (!is.null(xc)) paste0("x=", py_q(xc), ", order=sorted(df[", py_q(xc), "].dropna().unique()), ") else "", "y=", py_q(o$y), ", color='#3a3a38', alpha=0.45, size=4, jitter=0.12, ax=ax)\n") else "",
        "sns.pointplot(data=df, ", if (!is.null(xc)) paste0("x=", py_q(xc), ", order=sorted(df[", py_q(xc), "].dropna().unique()), ") else "", "y=", py_q(o$y), ", estimator='mean', errorbar=None, linestyle='none', marker='D', color='white', markeredgecolor='black', markersize=8, ax=ax)  # mean\n",
        if (lab && !is.null(xc)) paste0("for i, (name, grp) in enumerate(df.groupby(", py_q(xc), ")[", py_q(o$y), "]):\n    ax.text(i, grp.max(), f'n = {grp.count()}\\nmedian {grp.median():.1f}', ha='center', va='bottom', fontsize=9)\n") else "") },
    mean_error = paste0("err = ", py_q(o$err), "   # 'SD', 'SE' or '95% CI'\n",
      "def err_fn(x):\n    m, s, n = x.mean(), x.std(), x.count()\n    e = s if err == 'SD' else (s / np.sqrt(n) if err == 'SE' else stats.t.ppf(0.975, n - 1) * s / np.sqrt(n))\n    return (m - e, m + e)\n",
      "sns.barplot(data=df, x=", py_q(o$xc), ", order=sorted(df[", py_q(o$xc), "].dropna().unique()), y=", py_q(o$y), if (!is.null(g)) paste0(", hue=", py_q(g), ", hue_order=sorted(df[", py_q(g), "].dropna().unique())") else paste0(", hue=", py_q(o$xc), ", legend=False"), ", estimator='mean', errorbar=err_fn, capsize=0.15, palette=palette[:", if (!is.null(g)) paste0("df[", py_q(g), "].nunique()") else paste0("df[", py_q(o$xc), "].nunique()"), "], edgecolor='white', ax=ax)\n",
      if (lab) "for c in ax.containers:\n    if hasattr(c, 'datavalues'): ax.bar_label(c, fmt='%.1f', padding=12, fontsize=9)\n" else "",
      "ax.set_ylabel(f'Mean ", o$y, " (± {err})')\n"),
    bar_count = if (is.null(g)) paste0("counts = df[", py_q(o$xc), "].value_counts().sort_index()\n",
        "bars = ax.bar(counts.index.astype(str), counts.values, color=palette[:len(counts)], edgecolor='white')\n",
        if (lab) "pct = 100 * counts / counts.sum()\nax.bar_label(bars, labels=[f'{c} ({p:.1f}%)' for c, p in zip(counts, pct)], padding=3)\n" else "", "ax.set_ylabel('Number of subjects')\n")
      else paste0("tab = pd.crosstab(df[", py_q(o$xc), "], df[", py_q(g), "])\n",
        if (identical(o$style, "Stacked 100%")) "tab = tab.div(tab.sum(axis=1), axis=0) * 100\n" else "",
        "tab.plot(kind='bar', stacked=", if (grepl("Stacked", o$style %||% "")) "True" else "False", ", color=palette[:tab.shape[1]], edgecolor='white', rot=0, ax=ax)\n",
        if (lab) paste0("for c in ax.containers:\n    ax.bar_label(c, fmt='", if (identical(o$style, "Stacked 100%")) "%.0f%%" else "%d", "', label_type='", if (grepl("Stacked", o$style %||% "")) "center" else "edge", "', fontsize=9)\n") else ""),
    pie = paste0("counts = df[", py_q(o$xc), "].value_counts().sort_index()\n",
      "wedges, texts, autotexts = ax.pie(counts, labels=counts.index, colors=palette[:len(counts)], startangle=90, counterclock=False,\n",
      "    autopct=lambda p: f'{p:.1f}%\\n({p * counts.sum() / 100:.0f})', pctdistance=", if (identical(o$style, "Pie")) "0.65" else "0.78", ",\n",
      "    wedgeprops=dict(", if (!identical(o$style, "Pie")) "width=0.45, " else "", "edgecolor='white', linewidth=2), textprops=dict(fontsize=10))\n",
      "plt.setp(autotexts, color='white', fontweight='bold')\nax.axis('equal')\n"),
    scatter = paste0("sns.scatterplot(data=df, x=", py_q(o$x), ", y=", py_q(o$y), if (!is.null(g)) paste0(", hue=", py_q(g), ", palette=palette[:df[", py_q(g), "].nunique()]") else ", color=palette[0]", ", s=45, alpha=0.75, ax=ax)\n",
      if (isTRUE(o$line)) paste0("sns.regplot(data=df, x=", py_q(o$x), ", y=", py_q(o$y), ", scatter=False, color='#7b1c2e', ax=ax)\n") else "",
      if (lab) paste0("d = df[[", py_q(o$x), ", ", py_q(o$y), "]].dropna()\nr, p = stats.pearsonr(d[", py_q(o$x), "], d[", py_q(o$y), "])\n",
                      "ax.text(0.02, 0.97, f'Pearson r = {r:.2f}, p = {p:.3f}\\nn = {len(d)}', transform=ax.transAxes, va='top', bbox=dict(boxstyle='round', fc='white'))\n") else ""),
    line_time = paste0("cols = ", py_list(o$vars), "\n",
      if (!is.null(g)) paste0("long = df.dropna(subset=cols + [", py_q(g), "]).melt(id_vars=", py_q(g), ", value_vars=cols, var_name='Time', value_name='Value')\n")
      else "long = df.dropna(subset=cols).melt(value_vars=cols, var_name='Time', value_name='Value')\n",
      "sns.pointplot(data=long, x='Time', y='Value', order=cols", if (!is.null(g)) paste0(", hue=", py_q(g), ", hue_order=sorted(long[", py_q(g), "].unique()), palette=palette[:long[", py_q(g), "].nunique()], dodge=0.15") else ", color=palette[0]",
      ", errorbar=", switch(o$err, SD = "'sd'", SE = "'se'", "('ci', 95)"), ", capsize=0.1, markers='o', ax=ax)\n", "ax.set_ylabel('Mean (± ", o$err, ")')\n"),
    heatmap = paste0("corr = df[", py_list(o$vars), "].corr(method=", py_q(o$method %||% "pearson"), ")\n",
      "sns.heatmap(corr, annot=", if (lab) "True" else "False", ", fmt='.2f', cmap=sns.diverging_palette(250, 15, as_cmap=True), vmin=-1, vmax=1, center=0, square=True, linewidths=1, ax=ax)\n"),
    qq = paste0("data = df[", py_q(o$x), "].dropna()\nstats.probplot(data, dist='norm', plot=ax)\nax.get_lines()[0].set(color=palette[0], markersize=5, alpha=0.7)\nax.get_lines()[1].set(color='#7b1c2e', linewidth=2)\n",
      if (lab) "w, p = stats.shapiro(data)\nax.text(0.02, 0.97, f'Shapiro-Wilk p = {p:.3f}', transform=ax.transAxes, va='top', bbox=dict(boxstyle='round', fc='white'))\n" else ""),
    forest = paste0("s = df.groupby(", py_q(o$xc), ")[", py_q(o$y), "].agg(['mean', 'std', 'count'])\n",
      "s['e'] = stats.t.ppf(0.975, s['count'] - 1) * s['std'] / np.sqrt(s['count'])\ny = np.arange(len(s))[::-1]\n",
      "for yi, (name, r), col in zip(y, s.iterrows(), palette):\n    ax.errorbar(r['mean'], yi, xerr=r['e'], fmt='s', color=col, markersize=9, capsize=4, lw=2)\n",
      if (lab) "    ax.text(r['mean'] + r['e'], yi, f\"  {r['mean']:.1f} ({r['mean']-r['e']:.1f} to {r['mean']+r['e']:.1f}), n={int(r['count'])}\", va='center', fontsize=9)\n" else "",
      "ax.axvline(df[", py_q(o$y), "].mean(), ls='--', color='grey')\nax.set_yticks(y)\nax.set_yticklabels(s.index)\nax.set_xlabel('Mean ", o$y, " (95% CI)')\n"))
  tail <- paste0("ax.set_title(", py_q(title), ", fontweight='bold', loc='left')\n",
    if (nzchar(o$subtitle %||% "")) paste0("fig.suptitle(", py_q(o$subtitle), ", x=0.02, ha='left', fontsize=10, color='#52514e')\n") else "",
    if (!(o$chart %in% c("pie", "heatmap", "forest", "qq"))) paste0("ax.set_xlabel(", py_q(o$xlab %||% (dl$x %||% "")), ")\n") else "",
    if (nzchar(o$ylab %||% "")) paste0("ax.set_ylabel(", py_q(o$ylab), ")\n") else "",
    "fig.text(0.99, 0.01, 'Made with MAMC BioStat', ha='right', fontsize=7, color='#8a8984')\n",
    "sns.despine()\nplt.tight_layout()\nplt.savefig('mamc_chart.png', dpi=300)\nplt.show()\n")
  paste0(head, body, tail)
}
