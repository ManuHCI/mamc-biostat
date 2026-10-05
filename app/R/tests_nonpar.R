# =====================================================================
#  3. NON-PARAMETRIC TESTS
# =====================================================================

z_from_p <- function(p) qnorm(p / 2, lower.tail = FALSE)

register_test(list(
  id = "wilcox_one", name = "Wilcoxon signed-rank test (one sample vs median)", cat = CATEGORIES[3], level = "UG & PG",
  inputs = list(list(id = "y", label = "Numeric / ordinal variable", type = "num"),
                list(id = "mu", label = "Hypothesised median", type = "value", default = 0)),
  run = function(d, a) {
    al <- alpha_of(a); cc <- use_cc(d, a$y); x <- cc$d[[1]]; mu <- as.numeric(a$mu)
    w <- suppressWarnings(wilcox.test(x, mu = mu, conf.int = TRUE, exact = length(x) < 50 && !any(duplicated(x - mu)), conf.level = 1 - al))
    nz <- sum(x != mu); r <- z_from_p(w$p.value) / sqrt(nz)
    tab <- data.frame(n = length(x), Median = median(x), `Hypothesised median` = mu, `V (sum of positive ranks)` = w$statistic,
                      `Pseudo-median` = w$estimate, `CI low` = w$conf.int[1], `CI high` = w$conf.int[2], p = w$p.value, `Effect size r` = r, check.names = FALSE)
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>H<sub>0</sub>: median of ", a$y, " = ", mu, ".</p><p>Observed median ", med_iqr(x), "; V = ", w$statistic, ", ", p_eq(w$p.value),
      " &rarr; <b>", sig_word(w$p.value, al), "</b>; effect size r = ", fmt(r, 2), " (", r_word(r), ").</p>", decision_html(w$p.value, al),
      writeup_html(sprintf("The median %s was %s (Wilcoxon signed-rank test vs %s: V = %s, %s).", a$y, med_iqr(x), mu, w$statistic, p_eq(w$p.value))))
    steps <- step_list(paste0("Compute differences d<sub>i</sub> = x<sub>i</sub> &minus; ", mu, "; drop zeros (n = ", nz, ")."),
      "Rank |d<sub>i</sub>| from smallest to largest (average ranks for ties).",
      paste0("V = sum of ranks of positive differences = ", w$statistic, ". Under H<sub>0</sub> ", mj("E(V) = \\frac{n(n+1)}{4},\\quad Var(V) = \\frac{n(n+1)(2n+1)}{24}")),
      paste0(mj("z = \\frac{V - E(V)}{\\sqrt{Var(V)}},\\quad r = \\frac{|z|}{\\sqrt{n}} = ", fmt(r))))
    list(tables = list(`Wilcoxon signed-rank (one sample)` = tab), html = html, steps = steps,
         plot = function() { boxplot(x, horizontal = TRUE, col = "#f2d7dc", border = PAL[1], main = a$y, xlab = a$y); abline(v = mu, lty = 2, lwd = 2, col = PAL[2]) })
  }))

register_test(list(
  id = "mann_whitney", name = "Mann-Whitney U test (Wilcoxon rank-sum)", cat = CATEGORIES[3], level = "UG & PG",
  inputs = list(list(id = "y", label = "Outcome (numeric / ordinal)", type = "num"), list(id = "g", label = "Grouping variable (2 groups)", type = "cat")),
  run = function(d, a) {
    al <- alpha_of(a); cc <- use_cc(d, c(a$y, a$g)); y <- cc$d[[1]]; g <- as_fac(cc$d[[2]]); lv <- two_levels(g, "For 3 or more groups use Kruskal-Wallis.")
    y1 <- y[g == lv[1]]; y2 <- y[g == lv[2]]; n1 <- length(y1); n2 <- length(y2)
    w <- suppressWarnings(wilcox.test(y1, y2, conf.int = TRUE, exact = (n1 + n2) < 50 && !any(duplicated(y)), conf.level = 1 - al))
    rk <- rank(y); R1 <- sum(rk[g == lv[1]]); U1 <- R1 - n1 * (n1 + 1) / 2; U <- min(U1, n1 * n2 - U1)
    z <- z_from_p(w$p.value); r <- z / sqrt(n1 + n2); cl <- U1 / (n1 * n2)
    desc <- data.frame(Group = lv, n = c(n1, n2), Median = c(median(y1), median(y2)), Q1 = c(quantile(y1, .25), quantile(y2, .25)),
                       Q3 = c(quantile(y1, .75), quantile(y2, .75)), `Mean rank` = c(mean(rk[g == lv[1]]), mean(rk[g == lv[2]])), `Sum of ranks` = c(R1, sum(rk[g == lv[2]])), check.names = FALSE)
    res <- data.frame(U = U, `W (R output)` = w$statistic, z = z, p = w$p.value, `Hodges-Lehmann shift` = w$estimate, `CI low` = w$conf.int[1], `CI high` = w$conf.int[2],
                      `Effect size r` = r, `Prob. superiority P(${lv[1]} > ${lv[2]})` = cl, check.names = FALSE)
    names(res)[9] <- paste0("P(", lv[1], " > ", lv[2], ")")
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>H<sub>0</sub>: the distributions of ", a$y, " are the same in both groups (no tendency for one group to have higher values).</p>",
      "<p>", lv[1], ": median ", med_iqr(y1), " vs ", lv[2], ": median ", med_iqr(y2), ". U = ", fmt(U, 1), ", z = ", fmt(z, 2), ", ", p_eq(w$p.value), " &rarr; <b>", sig_word(w$p.value, al),
      "</b>. Effect size r = ", fmt(r, 2), " (", r_word(r), ").</p>", decision_html(w$p.value, al),
      writeup_html(sprintf("%s was %s in %s [median %s] than in %s [median %s] (Mann-Whitney U = %s, %s, r = %s).", a$y,
        if (median(y1) >= median(y2)) "higher" else "lower", lv[1], med_iqr(y1), lv[2], med_iqr(y2), fmt(U, 1), p_eq(w$p.value), fmt(r, 2))))
    steps <- step_list("Combine both groups and rank all values from smallest (rank 1) to largest; ties get the average rank.",
      paste0("Sum of ranks in group 1: R<sub>1</sub> = ", fmt(R1, 1)),
      paste0(mj("U_1 = R_1 - \\frac{n_1(n_1+1)}{2} = ", fmt(R1, 1), " - \\frac{", n1, "(", n1 + 1, ")}{2} = ", fmt(U1, 1), ",\\quad U = \\min(U_1, n_1n_2-U_1) = ", fmt(U, 1))),
      paste0("For large samples: ", mj("z = \\frac{U - n_1n_2/2}{\\sqrt{n_1n_2(n_1+n_2+1)/12}}"), " (with tie correction) = ", fmt(z, 3)),
      paste0("Effect size ", mj("r = \\frac{|z|}{\\sqrt{N}} = ", fmt(r, 3))))
    list(tables = list(`Group summary (median, IQR, ranks)` = desc, `Mann-Whitney U test` = res), html = html, steps = steps,
         plot = function() { boxplot(y ~ g, col = c("#f2d7dc", "#d4e8ef"), border = PAL[1:2], main = paste(a$y, "by", a$g), xlab = a$g, ylab = a$y)
           stripchart(y ~ g, vertical = TRUE, method = "jitter", add = TRUE, pch = 19, col = adjustcolor(PAL[1:2], 0.5)) })
  }))

register_test(list(
  id = "wilcox_paired", name = "Wilcoxon signed-rank test (paired)", cat = CATEGORIES[3], level = "UG & PG",
  inputs = list(list(id = "y1", label = "Measurement 1 (Before)", type = "num"), list(id = "y2", label = "Measurement 2 (After)", type = "num")),
  run = function(d, a) {
    al <- alpha_of(a); need(a$y1 != a$y2, "Choose two different columns.")
    cc <- use_cc(d, c(a$y1, a$y2)); x1 <- cc$d[[1]]; x2 <- cc$d[[2]]; dif <- x2 - x1; nz <- sum(dif != 0)
    w <- suppressWarnings(wilcox.test(x2, x1, paired = TRUE, conf.int = TRUE, exact = length(dif) < 50 && !any(duplicated(abs(dif[dif != 0]))), conf.level = 1 - al))
    z <- z_from_p(w$p.value); r <- z / sqrt(nz)
    desc <- data.frame(Measurement = c(a$y1, a$y2, "Difference"), n = length(dif), Median = c(median(x1), median(x2), median(dif)),
                       Q1 = c(quantile(x1, .25), quantile(x2, .25), quantile(dif, .25)), Q3 = c(quantile(x1, .75), quantile(x2, .75), quantile(dif, .75)))
    res <- data.frame(`Positive differences` = sum(dif > 0), `Negative differences` = sum(dif < 0), Ties = sum(dif == 0), V = w$statistic, z = z, p = w$p.value,
                      `Hodges-Lehmann median change` = w$estimate, `CI low` = w$conf.int[1], `CI high` = w$conf.int[2], `Effect size r` = r, check.names = FALSE)
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>H<sub>0</sub>: the median of paired differences = 0.</p><p>", a$y1, ": ", med_iqr(x1), "; ", a$y2, ": ", med_iqr(x2),
      ". ", sum(dif > 0), " increased, ", sum(dif < 0), " decreased, ", sum(dif == 0), " unchanged. V = ", w$statistic, ", z = ", fmt(z, 2), ", ", p_eq(w$p.value), " &rarr; <b>", sig_word(w$p.value, al),
      "</b>; r = ", fmt(r, 2), " (", r_word(r), ").</p>", decision_html(w$p.value, al),
      writeup_html(sprintf("%s changed from a median of %s to %s (Wilcoxon signed-rank test, z = %s, %s, r = %s).", a$y1, med_iqr(x1), med_iqr(x2), fmt(z, 2), p_eq(w$p.value), fmt(r, 2))))
    steps <- step_list("Compute each subject's difference (After &minus; Before); discard zero differences.",
      "Rank the absolute differences; attach the sign of each difference to its rank.",
      paste0("V = sum of positive ranks = ", w$statistic, ", with ", mj("E(V)=\\frac{n(n+1)}{4} = ", fmt(nz * (nz + 1) / 4, 1))),
      paste0(mj("z = \\frac{V-E(V)}{\\sqrt{n(n+1)(2n+1)/24}},\\quad r = \\frac{|z|}{\\sqrt{n}} = ", fmt(r))))
    list(tables = list(`Descriptives` = desc, `Wilcoxon signed-rank test` = res), html = html, steps = steps,
         plot = function() { matplot(rbind(x1, x2), type = "l", lty = 1, col = adjustcolor(PAL[1], 0.35), xaxt = "n", ylab = "Value", main = "Paired values (median in blue)", xlim = c(0.8, 2.2))
           axis(1, 1:2, c(a$y1, a$y2)); lines(1:2, c(median(x1), median(x2)), lwd = 4, col = PAL[2]) })
  }))

register_test(list(
  id = "kruskal", name = "Kruskal-Wallis H test (+ Dunn post-hoc)", cat = CATEGORIES[3], level = "UG & PG",
  inputs = list(list(id = "y", label = "Outcome (numeric / ordinal)", type = "num"), list(id = "g", label = "Grouping variable (3+ groups)", type = "cat")),
  run = function(d, a) {
    al <- alpha_of(a); cc <- use_cc(d, c(a$y, a$g)); y <- cc$d[[1]]; g <- as_fac(cc$d[[2]]); k <- nlevels(g); N <- length(y)
    need(k >= 2, "Need at least 2 groups.")
    kw <- kruskal.test(y ~ g); H <- unname(kw$statistic); eps2 <- H / (N - 1); rk <- rank(y)
    desc <- data.frame(Group = levels(g), n = as.integer(table(g)), Median = tapply(y, g, median), Q1 = tapply(y, g, quantile, .25), Q3 = tapply(y, g, quantile, .75), `Mean rank` = tapply(rk, g, mean), check.names = FALSE, row.names = NULL)
    dn <- dunn_test(y, g, "holm"); sigp <- dn$comparison[dn$p_adjusted < al]
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>H<sub>0</sub>: all groups come from the same distribution (equal medians).</p><p>H(", k - 1, ") = ", fmt(H, 2), ", ", p_eq(kw$p.value),
      " &rarr; <b>", sig_word(kw$p.value, al), "</b>. Effect size &epsilon;&sup2; = ", fmt(eps2, 3), ".</p>",
      if (kw$p.value < al) paste0("<p><b>Dunn's post-hoc (Holm adjusted):</b> ", if (length(sigp)) paste0("significant differences for <b>", paste(sigp, collapse = "</b>, <b>"), "</b>.") else "no individual pair significant after adjustment.", "</p>") else "",
      decision_html(kw$p.value, al),
      writeup_html(sprintf("%s differed %s between %s groups (Kruskal-Wallis H(%d) = %s, %s).", a$y, if (kw$p.value < al) "significantly" else "non-significantly", a$g, k - 1, fmt(H, 2), p_eq(kw$p.value))))
    ni <- table(g); Ri <- tapply(rk, g, sum)
    steps <- step_list("Rank all N observations together (ties = average rank).",
      paste0("Sum of ranks per group: ", paste0(names(Ri), " = ", fmt(Ri, 1), collapse = "; ")),
      paste0(mj("H = \\frac{12}{N(N+1)}\\sum\\frac{R_j^2}{n_j} - 3(N+1)"), " (corrected for ties) = ", fmt(H, 3)),
      paste0("H follows a &chi;&sup2; distribution with k&minus;1 = ", k - 1, " df; p = ", fmt_p(kw$p.value)),
      paste0("Effect size ", mj("\\varepsilon^2 = \\frac{H}{N-1} = ", fmt(eps2))),
      "Dunn's test compares mean ranks of each pair using a z statistic; p-values are Holm-adjusted for multiple comparisons.")
    list(tables = list(`Group summary` = desc, `Kruskal-Wallis test` = data.frame(H = H, df = k - 1, p = kw$p.value, `Epsilon squared` = eps2, check.names = FALSE),
                       `Dunn post-hoc (Holm)` = dn), html = html, steps = steps,
         plot = function() boxplot(y ~ g, col = adjustcolor(pal_n(k), 0.35), border = pal_n(k), main = paste(a$y, "by", a$g), xlab = a$g, ylab = a$y))
  }))

register_test(list(
  id = "friedman", name = "Friedman test (repeated measures, non-parametric)", cat = CATEGORIES[3], level = "PG",
  inputs = list(list(id = "vars", label = "Repeated columns (3+, wide format)", type = "num_multi")),
  run = function(d, a) {
    al <- alpha_of(a); need(length(a$vars) >= 3, "Select at least 3 repeated columns.")
    cc <- use_cc(d, a$vars); Y <- as.matrix(cc$d); n <- nrow(Y); k <- ncol(Y)
    fr <- friedman.test(Y); Q <- unname(fr$statistic); W <- Q / (n * (k - 1))
    rr <- t(apply(Y, 1, rank))
    desc <- data.frame(Time = colnames(Y), Median = apply(Y, 2, median), Q1 = apply(Y, 2, quantile, .25), Q3 = apply(Y, 2, quantile, .75), `Mean rank` = colMeans(rr), check.names = FALSE, row.names = NULL)
    pw <- combn(k, 2)
    ph <- data.frame(Comparison = paste(colnames(Y)[pw[1, ]], "vs", colnames(Y)[pw[2, ]]),
                     p_unadjusted = apply(pw, 2, function(z) suppressWarnings(wilcox.test(Y[, z[1]], Y[, z[2]], paired = TRUE, exact = FALSE)$p.value)))
    ph$`p (Bonferroni)` <- p.adjust(ph$p_unadjusted, "bonferroni")
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>H<sub>0</sub>: the distribution is the same at all ", k, " time points.</p><p>&chi;&sup2;<sub>F</sub>(", k - 1, ") = ", fmt(Q, 2), ", ", p_eq(fr$p.value),
      " &rarr; <b>", sig_word(fr$p.value, al), "</b>. Kendall's W = ", fmt(W, 3), " (effect size: ", if (W < .1) "very small" else if (W < .3) "small" else if (W < .5) "moderate" else "large", ").</p>",
      decision_html(fr$p.value, al), writeup_html(sprintf("The outcome changed %s across the %d time points (Friedman &chi;&sup2;(%d) = %s, %s, Kendall's W = %s).", if (fr$p.value < al) "significantly" else "non-significantly", k, k - 1, fmt(Q, 2), p_eq(fr$p.value), fmt(W, 2))))
    steps <- step_list("Within each subject (row), rank the k values from 1 to k.",
      paste0("Sum of ranks per time point: ", paste0(colnames(Y), " = ", fmt(colSums(rr), 1), collapse = "; ")),
      paste0(mj("\\chi^2_F = \\frac{12}{nk(k+1)}\\sum R_j^2 - 3n(k+1) = ", fmt(Q, 3)), " (tie-corrected), df = k&minus;1"),
      paste0("Kendall's ", mj("W = \\frac{\\chi^2_F}{n(k-1)} = ", fmt(W))),
      "Post-hoc: pairwise Wilcoxon signed-rank tests with Bonferroni correction.")
    list(tables = list(`Descriptives` = desc, `Friedman test` = data.frame(`Chi-square` = Q, df = k - 1, p = fr$p.value, `Kendall's W` = W, n = n, check.names = FALSE),
                       `Post-hoc pairwise Wilcoxon (Bonferroni)` = ph), html = html, steps = steps,
         plot = function() boxplot(Y, col = adjustcolor(pal_n(k), 0.35), border = pal_n(k), main = "Distribution at each time point", ylab = "Value"))
  }))
