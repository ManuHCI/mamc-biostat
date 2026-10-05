# =====================================================================
#  1. DESCRIPTIVE STATISTICS
# =====================================================================

register_test(list(
  id = "desc", name = "Descriptive statistics (mean, SD, median, IQR)", cat = CATEGORIES[1], level = "UG & PG",
  inputs = list(
    list(id = "vars", label = "Numeric variable(s)", type = "num_multi"),
    list(id = "g", label = "Summarise by group (optional)", type = "cat_opt")
  ),
  run = function(d, a) {
    need(length(a$vars) > 0, "Select at least one numeric variable.")
    grp <- !is.null(a$g) && a$g != "(none)"
    rows <- list()
    for (v in a$vars) {
      if (grp) {
        for (lv in sort(unique(na.omit(d[[a$g]])))) {
          r <- desc_row(d[[v]][!is.na(d[[a$g]]) & d[[a$g]] == lv]); rows[[length(rows) + 1]] <- cbind(Variable = v, Group = as.character(lv), r)
        }
      } else rows[[length(rows) + 1]] <- cbind(Variable = v, desc_row(d[[v]]))
    }
    tab <- do.call(rbind, rows)
    txt <- character(0)
    for (v in a$vars) {
      x <- na.omit(d[[v]]); if (length(x) < 3) next
      sk <- skewness(x); sp <- shapiro_safe(x)
      skewed <- (!is.na(sp) && sp < 0.05) || abs(sk) > 1
      txt <- c(txt, sprintf("<li><b>%s</b>: %s. Skewness = %s; Shapiro-Wilk %s &rarr; %s</li>", v,
        if (skewed) paste0("median ", med_iqr(x)) else paste0("mean &plusmn; SD = ", mean_sd(x)), fmt(sk, 2), p_eq(sp),
        if (skewed) "data appear <b>skewed / non-normal</b>: report <b>median (IQR)</b> and use non-parametric tests."
        else "data are approximately normal: report <b>mean &plusmn; SD</b> and parametric tests are appropriate."))
    }
    html <- paste0("<h4>Interpretation</h4><ul>", paste(txt, collapse = ""), "</ul>",
      note_html("Rule of thumb: normally distributed data &rarr; mean &plusmn; SD; skewed data or ordinal scores &rarr; median (IQR). SE describes precision of the mean, SD describes spread of individuals - do not report SE as spread."))
    x1 <- d[[a$vars[1]]]
    steps <- step_list(
      paste0("Mean ", mj("\\bar{x} = \\frac{\\sum x_i}{n} = \\frac{", fmt(sum(x1, na.rm = TRUE), 2), "}{", sum(!is.na(x1)), "} = ", fmt(mean(x1, na.rm = TRUE), 3))),
      paste0("Standard deviation ", mj("SD = \\sqrt{\\frac{\\sum (x_i-\\bar{x})^2}{n-1}} = ", fmt(sd(x1, na.rm = TRUE), 3))),
      paste0("Standard error ", mj("SE = \\frac{SD}{\\sqrt{n}} = \\frac{", fmt(sd(x1, na.rm = TRUE), 3), "}{\\sqrt{", sum(!is.na(x1)), "}} = ", fmt(sd(x1, na.rm = TRUE) / sqrt(sum(!is.na(x1))), 3))),
      paste0("95% CI of mean ", mj("\\bar{x} \\pm t_{0.975,\\,n-1}\\times SE")),
      "Median = middle value after sorting; Q1 and Q3 = 25th and 75th percentiles; IQR = Q3 &minus; Q1.")
    list(tables = list(`Descriptive statistics` = tab), html = html, steps = paste0("<p>Shown for <b>", a$vars[1], "</b>:</p>", steps),
         plot = function() {
           k <- length(a$vars); op <- par(mfrow = c(min(k, 3), 2), mar = c(4, 4, 2.5, 1)); on.exit(par(op))
           for (v in head(a$vars, 3)) {
             x <- na.omit(d[[v]])
             hist(x, col = "#f2d7dc", border = PAL[1], main = paste("Histogram of", v), xlab = v, freq = FALSE)
             if (length(x) > 1) curve(dnorm(x, mean(na.omit(d[[v]])), sd(na.omit(d[[v]]))), add = TRUE, col = PAL[2], lwd = 2)
             if (grp) boxplot(d[[v]] ~ d[[a$g]], col = pal_n(10), main = paste(v, "by", a$g), xlab = a$g, ylab = v)
             else boxplot(x, col = "#f2d7dc", border = PAL[1], main = paste("Box plot of", v), horizontal = TRUE)
           }
         })
  }))

register_test(list(
  id = "freq", name = "Frequency table & proportions (with 95% CI)", cat = CATEGORIES[1], level = "UG & PG",
  inputs = list(list(id = "x", label = "Categorical variable", type = "cat"),
                list(id = "g", label = "Cross-tabulate by (optional)", type = "cat_opt")),
  run = function(d, a) {
    x <- d[[a$x]]; nmiss <- sum(is.na(x)); xv <- x[!is.na(x)]; n <- length(xv)
    tb <- table(xv)
    ci <- t(vapply(as.numeric(tb), function(k) binom.test(k, n)$conf.int * 100, numeric(2)))
    tab <- data.frame(Category = names(tb), Frequency = as.integer(tb), Percent = round(100 * as.numeric(tb) / n, 1),
                      `95% CI low (%)` = round(ci[, 1], 1), `95% CI high (%)` = round(ci[, 2], 1),
                      `Cumulative %` = round(cumsum(100 * as.numeric(tb) / n), 1), check.names = FALSE)
    tab <- rbind(tab, data.frame(Category = "Total (valid)", Frequency = n, Percent = 100, `95% CI low (%)` = NA, `95% CI high (%)` = NA, `Cumulative %` = NA, check.names = FALSE))
    tabs <- list(`Frequency table` = tab)
    if (!is.null(a$g) && a$g != "(none)") {
      ct <- table(d[[a$x]], d[[a$g]])
      m <- as.data.frame.matrix(ct); pr <- round(100 * prop.table(ct, 2), 1)
      cell <- matrix(paste0(as.matrix(m), " (", pr, "%)"), nrow = nrow(m), dimnames = dimnames(ct))
      tabs[[paste0("Cross-table: ", a$x, " x ", a$g, " (column %)")]] <- data.frame(Category = rownames(cell), cell, check.names = FALSE)
    }
    top <- names(tb)[which.max(tb)]
    html <- paste0("<h4>Interpretation</h4><p>Of ", n, " valid observations", if (nmiss) paste0(" (", nmiss, " missing)") else "",
                   ", the most common category was <b>", top, "</b> (", max(tb), ", ", fmt(100 * max(tb) / n, 1), "%).</p>",
                   note_html("95% confidence intervals use the exact Clopper-Pearson method. To test whether two categorical variables are associated, use the Chi-square test."))
    steps <- step_list(paste0("Proportion ", mj("p = \\frac{x}{n} = \\frac{", max(tb), "}{", n, "} = ", fmt(max(tb) / n, 3))),
                       paste0("Approximate (Wald) 95% CI ", mj("p \\pm 1.96\\sqrt{\\frac{p(1-p)}{n}}"), " - the table reports the more accurate exact (Clopper-Pearson) interval."))
    list(tables = tabs, html = html, steps = steps,
         plot = function() { op <- par(mar = c(5, 4, 3, 1)); on.exit(par(op))
           b <- barplot(100 * tb / n, col = pal_n(length(tb)), ylab = "Percent", main = paste("Distribution of", a$x), ylim = c(0, max(100 * tb / n) * 1.2))
           text(b, 100 * tb / n, paste0(tb, " (", round(100 * tb / n, 1), "%)"), pos = 3, cex = 0.85) })
  }))

register_test(list(
  id = "normality", name = "Normality test (Shapiro-Wilk, Q-Q plot)", cat = CATEGORIES[1], level = "UG & PG",
  inputs = list(list(id = "y", label = "Numeric variable", type = "num"),
                list(id = "g", label = "Check within each group (optional)", type = "cat_opt")),
  run = function(d, a) {
    grp <- !is.null(a$g) && a$g != "(none)"
    sets <- if (grp) split(d[[a$y]], d[[a$g]]) else setNames(list(d[[a$y]]), a$y)
    rows <- lapply(names(sets), function(k) { x <- na.omit(sets[[k]])
      sw <- if (length(x) >= 3 && length(x) <= 5000) shapiro.test(x) else list(statistic = NA, p.value = NA)
      se_sk <- sqrt(6 * length(x) * (length(x) - 1) / ((length(x) - 2) * (length(x) + 1) * (length(x) + 3)))
      data.frame(Group = k, n = length(x), `Shapiro-Wilk W` = unname(sw$statistic), p = sw$p.value,
                 Skewness = skewness(x), `Skewness z` = skewness(x) / se_sk, `Excess kurtosis` = kurtosis(x),
                 Conclusion = ifelse(is.na(sw$p.value), "n too small", ifelse(sw$p.value < 0.05, "Not normal", "Approximately normal")),
                 check.names = FALSE) })
    tab <- do.call(rbind, rows)
    html <- paste0("<h4>Interpretation</h4><p>H<sub>0</sub>: the data come from a normal distribution.</p><ul>",
      paste0("<li><b>", tab$Group, "</b> (n = ", tab$n, "): W = ", fmt(tab$`Shapiro-Wilk W`), ", ", p_eq(tab$p), " &rarr; ", tab$Conclusion, "</li>", collapse = ""), "</ul>",
      note_html("With large samples (n &gt; 100) Shapiro-Wilk detects trivial departures - look at the Q-Q plot and histogram too. With small samples (n &lt; 15) it has low power. |Skewness z| &gt; 1.96 also suggests non-normality."))
    list(tables = list(`Normality tests` = tab), html = html,
         steps = step_list("Sort the data: x<sub>(1)</sub> &le; ... &le; x<sub>(n)</sub>.",
                           paste0("Shapiro-Wilk statistic ", mj("W = \\frac{\\left(\\sum a_i x_{(i)}\\right)^2}{\\sum (x_i-\\bar{x})^2}"), " where a<sub>i</sub> are constants from expected normal order statistics."),
                           "W close to 1 &rarr; normal. Small W (small p) &rarr; departure from normality.",
                           "Q-Q plot: points lying on the straight line indicate normality; curves indicate skew, S-shapes indicate heavy/light tails."),
         plot = function() { k <- min(length(sets), 4); op <- par(mfrow = c(k, 2), mar = c(4, 4, 2.5, 1)); on.exit(par(op))
           for (nm in head(names(sets), 4)) { x <- na.omit(sets[[nm]])
             hist(x, freq = FALSE, col = "#f2d7dc", border = PAL[1], main = paste("Histogram -", nm), xlab = a$y)
             curve(dnorm(x, mean(na.omit(sets[[nm]])), sd(na.omit(sets[[nm]]))), add = TRUE, col = PAL[2], lwd = 2)
             qqnorm(x, main = paste("Normal Q-Q -", nm), pch = 19, col = PAL[1]); qqline(x, col = PAL[2], lwd = 2) } })
  }))
