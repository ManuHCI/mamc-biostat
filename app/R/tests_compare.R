# =====================================================================
#  2. COMPARE MEANS (PARAMETRIC)
# =====================================================================

register_test(list(
  id = "t_one", name = "One-sample t-test", cat = CATEGORIES[2], level = "UG & PG",
  inputs = list(list(id = "y", label = "Numeric variable", type = "num"),
                list(id = "mu", label = "Known / reference value (\u03bc\u2080)", type = "value", default = 0)),
  run = function(d, a) {
    al <- alpha_of(a); cc <- use_cc(d, a$y); x <- cc$d[[1]]; n <- length(x); need(n >= 2, "Need at least 2 observations.")
    mu <- as.numeric(a$mu); tt <- t.test(x, mu = mu, conf.level = 1 - al)
    m <- mean(x); s <- sd(x); se <- s / sqrt(n); dd <- (m - mu) / s
    tab <- data.frame(Variable = a$y, n = n, Mean = m, SD = s, SE = se, `Reference (mu0)` = mu, `Mean difference` = m - mu,
                      `CI low` = tt$conf.int[1] - mu, `CI high` = tt$conf.int[2] - mu, t = tt$statistic, df = tt$parameter, p = tt$p.value,
                      `Cohen's d` = dd, check.names = FALSE)
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>H<sub>0</sub>: population mean of <b>", a$y, "</b> = ", mu, ". H<sub>1</sub>: mean &ne; ", mu, ".</p>",
      "<p>The sample mean was ", mean_sd(x), " (n = ", n, "). The mean differed from ", mu, " by ", fmt(m - mu, 2), " ", ci_txt(tt$conf.int[1] - mu, tt$conf.int[2] - mu),
      "; t(", n - 1, ") = ", fmt(tt$statistic, 2), ", ", p_eq(tt$p.value), ". This difference is <b>", sig_word(tt$p.value, al), "</b>. Effect size Cohen's d = ", fmt(dd, 2), " (", d_word(dd), ").</p>",
      decision_html(tt$p.value, al), normality_note(c(setNames(shapiro_safe(x), a$y))),
      writeup_html(sprintf("The mean %s (%s) was %ssignificantly different from the reference value of %s (mean difference %s, %s; t(%d) = %s, %s).",
                           a$y, mean_sd(x), if (tt$p.value < al) "" else "not ", mu, fmt(m - mu, 2), ci_txt(tt$conf.int[1] - mu, tt$conf.int[2] - mu), n - 1, fmt(tt$statistic, 2), p_eq(tt$p.value))))
    steps <- step_list(
      paste0("Sample mean and SD: ", mj("\\bar{x} = ", fmt(m), ",\\quad s = ", fmt(s))),
      paste0("Standard error: ", mj("SE = \\frac{s}{\\sqrt{n}} = \\frac{", fmt(s), "}{\\sqrt{", n, "}} = ", fmt(se))),
      paste0("Test statistic: ", mj("t = \\frac{\\bar{x}-\\mu_0}{SE} = \\frac{", fmt(m), " - ", mu, "}{", fmt(se), "} = ", fmt(tt$statistic))),
      paste0("Degrees of freedom: df = n &minus; 1 = ", n - 1, ". Two-tailed p-value from t distribution = ", fmt_p(tt$p.value)),
      paste0("Critical t (&alpha; = ", al, ") = ", fmt(qt(1 - al / 2, n - 1)), ". |t| ", if (abs(tt$statistic) > qt(1 - al / 2, n - 1)) "&gt;" else "&le;", " critical t."),
      paste0("95% CI for mean: ", mj("\\bar{x} \\pm t_{crit}\\times SE = ", fmt(tt$conf.int[1]), " \\text{ to } ", fmt(tt$conf.int[2]))))
    list(tables = list(`One-sample t-test` = tab), html = html, steps = steps,
         plot = function() { hist(x, col = "#f2d7dc", border = PAL[1], main = paste("Distribution of", a$y), xlab = a$y)
           abline(v = mu, col = PAL[2], lwd = 3, lty = 2); abline(v = m, col = PAL[1], lwd = 3)
           legend("topright", c(paste("Sample mean =", fmt(m, 2)), paste("Reference =", mu)), col = PAL[1:2], lwd = 3, lty = 1:2, bty = "n") })
  }))

register_test(list(
  id = "t_ind", name = "Independent (unpaired) t-test - Student & Welch", cat = CATEGORIES[2], level = "UG & PG",
  inputs = list(list(id = "y", label = "Outcome (numeric)", type = "num"),
                list(id = "g", label = "Grouping variable (2 groups)", type = "cat")),
  run = function(d, a) {
    al <- alpha_of(a); cc <- use_cc(d, c(a$y, a$g)); y <- cc$d[[1]]; g <- as_fac(cc$d[[2]])
    lv <- two_levels(g, "For 3 or more groups use One-way ANOVA.")
    y1 <- y[g == lv[1]]; y2 <- y[g == lv[2]]; n1 <- length(y1); n2 <- length(y2)
    need(n1 >= 2 && n2 >= 2, "Each group needs at least 2 observations.")
    st <- t.test(y1, y2, var.equal = TRUE, conf.level = 1 - al); we <- t.test(y1, y2, var.equal = FALSE, conf.level = 1 - al)
    lev <- levene_test(y, g)
    sp <- sqrt(((n1 - 1) * var(y1) + (n2 - 1) * var(y2)) / (n1 + n2 - 2)); dd <- (mean(y1) - mean(y2)) / sp
    hg <- dd * (1 - 3 / (4 * (n1 + n2) - 9))
    desc <- rbind(cbind(Group = lv[1], desc_row(y1)[, c("n", "mean", "SD", "SE", "median", "IQR")]),
                  cbind(Group = lv[2], desc_row(y2)[, c("n", "mean", "SD", "SE", "median", "IQR")]))
    res <- data.frame(Test = c("Student t-test (equal variances)", "Welch t-test (unequal variances)"),
                      `Mean difference` = mean(y1) - mean(y2), `CI low` = c(st$conf.int[1], we$conf.int[1]), `CI high` = c(st$conf.int[2], we$conf.int[2]),
                      t = c(st$statistic, we$statistic), df = c(st$parameter, we$parameter), p = c(st$p.value, we$p.value), check.names = FALSE)
    use_w <- lev$p < 0.05; pick <- if (use_w) we else st
    levt <- data.frame(Test = "Levene (Brown-Forsythe)", F = lev$F, df1 = lev$df1, df2 = lev$df2, p = lev$p, check.names = FALSE)
    eff <- data.frame(Measure = c("Cohen's d", "Hedges' g"), Value = c(dd, hg), Magnitude = c(d_word(dd), d_word(hg)))
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>H<sub>0</sub>: mean ", a$y, " is equal in <b>", lv[1], "</b> and <b>", lv[2], "</b>.</p>",
      "<p><b>Step 1 - Equal variances?</b> Levene's test ", p_eq(lev$p), " &rarr; ",
      if (use_w) "variances are <b>unequal</b>, so the <b>Welch t-test</b> result is used." else "variances can be assumed <b>equal</b>, so the <b>Student t-test</b> result is used (Welch gives a similar answer).", "</p>",
      "<p><b>Step 2 - Result:</b> ", lv[1], ": ", mean_sd(y1), " (n = ", n1, ") vs ", lv[2], ": ", mean_sd(y2), " (n = ", n2, "). Mean difference = ",
      fmt(mean(y1) - mean(y2), 2), " ", ci_txt(pick$conf.int[1], pick$conf.int[2]), "; t(", fmt(pick$parameter, 1), ") = ", fmt(pick$statistic, 2), ", ", p_eq(pick$p.value),
      ". The difference is <b>", sig_word(pick$p.value, al), "</b>.</p><p><b>Effect size:</b> Cohen's d = ", fmt(dd, 2), " (", d_word(dd), " effect).</p>",
      decision_html(pick$p.value, al), normality_note(setNames(c(shapiro_safe(y1), shapiro_safe(y2)), lv)),
      writeup_html(sprintf("Mean %s was %s in the %s group (%s) compared with the %s group (%s); the difference of %s %s was %s (%s t-test, t(%s) = %s, %s; Cohen's d = %s).",
        a$y, if (mean(y1) > mean(y2)) "higher" else "lower", lv[1], mean_sd(y1), lv[2], mean_sd(y2), fmt(mean(y1) - mean(y2), 2), ci_txt(pick$conf.int[1], pick$conf.int[2]),
        sig_word(pick$p.value, al), if (use_w) "Welch" else "independent", fmt(pick$parameter, 1), fmt(pick$statistic, 2), p_eq(pick$p.value), fmt(dd, 2))))
    steps <- step_list(
      paste0("Group means: ", mj("\\bar{x}_1 = ", fmt(mean(y1)), ",\\; \\bar{x}_2 = ", fmt(mean(y2)), ",\\; s_1 = ", fmt(sd(y1)), ",\\; s_2 = ", fmt(sd(y2)))),
      paste0("Pooled SD: ", mj("s_p = \\sqrt{\\frac{(n_1-1)s_1^2+(n_2-1)s_2^2}{n_1+n_2-2}} = \\sqrt{\\frac{", n1 - 1, "\\times", fmt(var(y1)), "+", n2 - 1, "\\times", fmt(var(y2)), "}{", n1 + n2 - 2, "}} = ", fmt(sp))),
      paste0("Standard error of difference: ", mj("SE = s_p\\sqrt{\\frac{1}{n_1}+\\frac{1}{n_2}} = ", fmt(sp * sqrt(1 / n1 + 1 / n2)))),
      paste0("Student t: ", mj("t = \\frac{\\bar{x}_1-\\bar{x}_2}{SE} = \\frac{", fmt(mean(y1) - mean(y2)), "}{", fmt(sp * sqrt(1 / n1 + 1 / n2)), "} = ", fmt(st$statistic)), " with df = n<sub>1</sub>+n<sub>2</sub>&minus;2 = ", n1 + n2 - 2),
      paste0("Welch t uses separate variances: ", mj("t = \\frac{\\bar{x}_1-\\bar{x}_2}{\\sqrt{s_1^2/n_1+s_2^2/n_2}} = ", fmt(we$statistic)), " with Welch-Satterthwaite df = ", fmt(we$parameter, 2)),
      paste0("Cohen's d: ", mj("d = \\frac{\\bar{x}_1-\\bar{x}_2}{s_p} = ", fmt(dd))),
      "p-value = probability of a t at least this extreme if H<sub>0</sub> were true (two-tailed).")
    list(tables = list(`Group statistics` = desc, `Levene's test for equality of variances` = levt, `t-test results` = res, `Effect size` = eff),
         html = html, steps = steps,
         plot = function() { boxplot(y ~ g, col = c("#f2d7dc", "#d4e8ef"), border = PAL[1:2], main = paste(a$y, "by", a$g), xlab = a$g, ylab = a$y)
           stripchart(y ~ g, vertical = TRUE, method = "jitter", add = TRUE, pch = 19, col = adjustcolor(PAL[1:2], 0.5))
           points(1:2, c(mean(y1), mean(y2)), pch = 18, cex = 2.2, col = "black"); legend("topright", "Mean", pch = 18, pt.cex = 2, bty = "n") })
  }))

register_test(list(
  id = "t_paired", name = "Paired t-test (before-after / matched)", cat = CATEGORIES[2], level = "UG & PG",
  inputs = list(list(id = "y1", label = "Measurement 1 (e.g. Before)", type = "num"),
                list(id = "y2", label = "Measurement 2 (e.g. After)", type = "num")),
  run = function(d, a) {
    al <- alpha_of(a); need(a$y1 != a$y2, "Choose two different columns.")
    cc <- use_cc(d, c(a$y1, a$y2)); x1 <- cc$d[[1]]; x2 <- cc$d[[2]]; n <- length(x1); need(n >= 2, "Need at least 2 complete pairs.")
    dif <- x2 - x1; tt <- t.test(x2, x1, paired = TRUE, conf.level = 1 - al); dz <- mean(dif) / sd(dif)
    desc <- data.frame(Measurement = c(a$y1, a$y2, paste0("Difference (", a$y2, " - ", a$y1, ")")), n = n,
                       Mean = c(mean(x1), mean(x2), mean(dif)), SD = c(sd(x1), sd(x2), sd(dif)), SE = c(sd(x1), sd(x2), sd(dif)) / sqrt(n))
    res <- data.frame(`Mean difference` = mean(dif), `CI low` = tt$conf.int[1], `CI high` = tt$conf.int[2], t = tt$statistic, df = tt$parameter,
                      p = tt$p.value, `Cohen's dz` = dz, `Correlation r (pairs)` = cor(x1, x2), check.names = FALSE)
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>H<sub>0</sub>: the mean of the paired differences (", a$y2, " &minus; ", a$y1, ") = 0.</p>",
      "<p>", a$y1, ": ", mean_sd(x1), "; ", a$y2, ": ", mean_sd(x2), " (n = ", n, " pairs). Mean change = ", fmt(mean(dif), 2), " ", ci_txt(tt$conf.int[1], tt$conf.int[2]),
      "; t(", n - 1, ") = ", fmt(tt$statistic, 2), ", ", p_eq(tt$p.value), ". The change is <b>", sig_word(tt$p.value, al), "</b>. Effect size d<sub>z</sub> = ", fmt(dz, 2), " (", d_word(dz), ").</p>",
      decision_html(tt$p.value, al), normality_note(c(`Differences` = shapiro_safe(dif))),
      note_html("The paired test assumes the <b>differences</b> are approximately normal - not each measurement. If not, use the Wilcoxon signed-rank test."),
      writeup_html(sprintf("%s changed from %s to %s; the mean change of %s %s was %s (paired t-test, t(%d) = %s, %s).",
                           a$y1, mean_sd(x1), mean_sd(x2), fmt(mean(dif), 2), ci_txt(tt$conf.int[1], tt$conf.int[2]), sig_word(tt$p.value, al), n - 1, fmt(tt$statistic, 2), p_eq(tt$p.value))))
    steps <- step_list(
      paste0("For each subject compute the difference d<sub>i</sub> = ", a$y2, " &minus; ", a$y1, "."),
      paste0("Mean and SD of differences: ", mj("\\bar{d} = ", fmt(mean(dif)), ",\\quad s_d = ", fmt(sd(dif)))),
      paste0("Standard error: ", mj("SE = \\frac{s_d}{\\sqrt{n}} = \\frac{", fmt(sd(dif)), "}{\\sqrt{", n, "}} = ", fmt(sd(dif) / sqrt(n)))),
      paste0("Test statistic: ", mj("t = \\frac{\\bar{d}}{SE} = ", fmt(tt$statistic)), " with df = n &minus; 1 = ", n - 1),
      paste0("95% CI of mean difference: ", mj("\\bar{d} \\pm t_{crit} \\times SE = ", fmt(tt$conf.int[1]), " \\text{ to } ", fmt(tt$conf.int[2]))),
      "Pairing removes between-subject variation, so the paired test is more powerful than an unpaired test on the same data.")
    list(tables = list(`Paired statistics` = desc, `Paired t-test` = res), html = html, steps = steps,
         plot = function() { op <- par(mfrow = c(1, 2)); on.exit(par(op))
           matplot(rbind(x1, x2), type = "l", lty = 1, col = adjustcolor(PAL[1], 0.35), xaxt = "n", ylab = "Value", main = "Individual changes", xlim = c(0.8, 2.2))
           axis(1, 1:2, c(a$y1, a$y2)); lines(1:2, c(mean(x1), mean(x2)), lwd = 4, col = PAL[2]); points(1:2, c(mean(x1), mean(x2)), pch = 19, cex = 1.6, col = PAL[2])
           hist(dif, col = "#d4e8ef", border = PAL[2], main = "Distribution of differences", xlab = paste(a$y2, "-", a$y1)); abline(v = 0, lty = 2, lwd = 2) })
  }))

register_test(list(
  id = "anova1", name = "One-way ANOVA (+ Tukey post-hoc)", cat = CATEGORIES[2], level = "UG & PG",
  inputs = list(list(id = "y", label = "Outcome (numeric)", type = "num"),
                list(id = "g", label = "Grouping variable (3 or more groups)", type = "cat")),
  run = function(d, a) {
    al <- alpha_of(a); cc <- use_cc(d, c(a$y, a$g)); y <- cc$d[[1]]; g <- as_fac(cc$d[[2]]); k <- nlevels(g); N <- length(y)
    need(k >= 2, "The grouping variable needs at least 2 groups."); need(all(table(g) >= 2), "Each group needs at least 2 observations.")
    fit <- aov(y ~ g); s <- summary(fit)[[1]]
    ssb <- s$`Sum Sq`[1]; ssw <- s$`Sum Sq`[2]; eta2 <- ssb / (ssb + ssw); omega2 <- (ssb - (k - 1) * s$`Mean Sq`[2]) / (ssb + ssw + s$`Mean Sq`[2])
    at <- data.frame(Source = c("Between groups", "Within groups (error)", "Total"), `Sum of squares` = c(ssb, ssw, ssb + ssw),
                     df = c(k - 1, N - k, N - 1), `Mean square` = c(s$`Mean Sq`, NA), F = c(s$`F value`[1], NA, NA), p = c(s$`Pr(>F)`[1], NA, NA), check.names = FALSE)
    p <- s$`Pr(>F)`[1]; lev <- levene_test(y, g); wel <- oneway.test(y ~ g, var.equal = FALSE)
    desc <- do.call(rbind, lapply(levels(g), function(l) cbind(Group = l, desc_row(y[g == l])[, c("n", "mean", "SD", "SE", "CI95_low", "CI95_high", "median")])))
    tk <- TukeyHSD(fit, conf.level = 1 - al)$g
    tkd <- data.frame(Comparison = rownames(tk), `Mean difference` = tk[, 1], `CI low` = tk[, 2], `CI high` = tk[, 3], `p (Tukey adjusted)` = tk[, 4], check.names = FALSE, row.names = NULL)
    gh <- pairwise.t.test(y, g, pool.sd = FALSE, p.adjust.method = "holm")
    sigp <- tkd$Comparison[tkd$`p (Tukey adjusted)` < al]
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>H<sub>0</sub>: all ", k, " group means of ", a$y, " are equal.</p>",
      "<p><b>Assumption - equal variances:</b> Levene's test ", p_eq(lev$p), if (lev$p < 0.05) paste0(" &rarr; variances differ; rely on <b>Welch's ANOVA</b>: F(", fmt(wel$parameter[1], 0), ", ", fmt(wel$parameter[2], 1), ") = ", fmt(wel$statistic, 2), ", ", p_eq(wel$p.value), ", and Games-Howell type pairwise comparisons (Welch t with Holm correction, shown below).") else " &rarr; equal variances assumed.", "</p>",
      "<p><b>Result:</b> F(", k - 1, ", ", N - k, ") = ", fmt(s$`F value`[1], 2), ", ", p_eq(p), ". The difference among groups is <b>", sig_word(p, al), "</b>. Effect size &eta;&sup2; = ", fmt(eta2, 3), " (", eta_word(eta2), "): ", fmt(100 * eta2, 1), "% of the variation in ", a$y, " is explained by ", a$g, ".</p>",
      if (p < al) paste0("<p><b>Post-hoc (Tukey HSD):</b> ", if (length(sigp)) paste0("significant differences between <b>", paste(sigp, collapse = "</b>, <b>"), "</b>.") else "no individual pair reached significance after adjustment.", "</p>")
      else "<p>Because the overall F test is not significant, post-hoc comparisons are not normally interpreted.</p>",
      decision_html(p, al), normality_note(sapply(split(y, g), shapiro_safe)),
      writeup_html(sprintf("%s differed %s across %s groups (one-way ANOVA, F(%d, %d) = %s, %s, &eta;&sup2; = %s).%s", a$y, if (p < al) "significantly" else "non-significantly",
                           a$g, k - 1, N - k, fmt(s$`F value`[1], 2), p_eq(p), fmt(eta2, 2), if (p < al && length(sigp)) paste0(" Tukey post-hoc tests showed significant differences for ", paste(sigp, collapse = ", "), ".") else "")))
    gm <- mean(y); ni <- table(g); mi <- tapply(y, g, mean)
    steps <- step_list(
      paste0("Grand mean ", mj("\\bar{x} = ", fmt(gm))),
      paste0("Between-group sum of squares: ", mj("SS_B = \\sum n_j(\\bar{x}_j-\\bar{x})^2 = ", paste0(ni, "(", fmt(mi, 2), "-", fmt(gm, 2), ")^2", collapse = "+"), " = ", fmt(ssb))),
      paste0("Within-group sum of squares: ", mj("SS_W = \\sum\\sum (x_{ij}-\\bar{x}_j)^2 = ", fmt(ssw))),
      paste0("Mean squares: ", mj("MS_B = \\frac{SS_B}{k-1} = \\frac{", fmt(ssb), "}{", k - 1, "} = ", fmt(ssb / (k - 1)), ",\\quad MS_W = \\frac{SS_W}{N-k} = \\frac{", fmt(ssw), "}{", N - k, "} = ", fmt(ssw / (N - k)))),
      paste0("F ratio: ", mj("F = \\frac{MS_B}{MS_W} = ", fmt(s$`F value`[1])), " compared with the F distribution (df ", k - 1, ", ", N - k, "); p = ", fmt_p(p)),
      paste0("Effect size: ", mj("\\eta^2 = \\frac{SS_B}{SS_T} = ", fmt(eta2), ",\\quad \\omega^2 = ", fmt(omega2))),
      "If F is significant, Tukey's HSD compares every pair while controlling the family-wise error rate.")
    gtab <- data.frame(Comparison = rownames(gh$p.value)[row(gh$p.value)], vs = colnames(gh$p.value)[col(gh$p.value)], `p (Welch, Holm adjusted)` = as.vector(gh$p.value), check.names = FALSE)
    gtab <- gtab[!is.na(gtab[[3]]), ]
    list(tables = list(`Group descriptives` = desc, `ANOVA table` = at,
                       `Levene's test` = data.frame(F = lev$F, df1 = lev$df1, df2 = lev$df2, p = lev$p),
                       `Welch's ANOVA (unequal variances)` = data.frame(F = wel$statistic, df1 = wel$parameter[1], df2 = wel$parameter[2], p = wel$p.value, row.names = NULL),
                       `Post-hoc: Tukey HSD` = tkd, `Post-hoc: pairwise Welch t (Holm) - use if variances unequal` = gtab),
         html = html, steps = steps,
         plot = function() { op <- par(mar = c(5, 4, 3, 1)); on.exit(par(op))
           boxplot(y ~ g, col = adjustcolor(pal_n(k), 0.35), border = pal_n(k), main = paste(a$y, "by", a$g), xlab = a$g, ylab = a$y)
           se <- tapply(y, g, sd) / sqrt(ni); points(1:k, mi, pch = 18, cex = 2); arrows(1:k, mi - 1.96 * se, 1:k, mi + 1.96 * se, angle = 90, code = 3, length = 0.06, lwd = 2)
           legend("topright", "Mean +/- 95% CI", pch = 18, bty = "n") })
  }))

register_test(list(
  id = "anova2", name = "Two-way ANOVA (factorial, with interaction)", cat = CATEGORIES[2], level = "PG",
  inputs = list(list(id = "y", label = "Outcome (numeric)", type = "num"),
                list(id = "f1", label = "Factor A", type = "cat"), list(id = "f2", label = "Factor B", type = "cat"),
                list(id = "inter", label = "Include A x B interaction?", type = "select", choices = c("Yes", "No"))),
  run = function(d, a) {
    al <- alpha_of(a); need(a$f1 != a$f2, "Factor A and B must be different.")
    cc <- use_cc(d, c(a$y, a$f1, a$f2)); dd <- data.frame(y = cc$d[[1]], A = as_fac(cc$d[[2]]), B = as_fac(cc$d[[3]]))
    old <- options(contrasts = c("contr.sum", "contr.poly")); on.exit(options(old))
    fml <- if (identical(a$inter, "No")) y ~ A + B else y ~ A * B
    fit <- lm(fml, data = dd); dr <- drop1(fit, . ~ ., test = "F")
    rss <- deviance(fit); dfe <- df.residual(fit)
    tab <- data.frame(Source = c(sub("A:B", paste(a$f1, "x", a$f2), sub("^A$", a$f1, sub("^B$", a$f2, rownames(dr)[-1]))), "Residual"),
                      `Sum of squares (Type III)` = c(dr$`Sum of Sq`[-1], rss), df = c(dr$Df[-1], dfe), `Mean square` = c(dr$`Sum of Sq`[-1] / dr$Df[-1], rss / dfe),
                      F = c(dr$`F value`[-1], NA), p = c(dr$`Pr(>F)`[-1], NA), check.names = FALSE)
    tab$`Partial eta squared` <- c(dr$`Sum of Sq`[-1] / (dr$`Sum of Sq`[-1] + rss), NA)
    cm <- aggregate(y ~ A + B, dd, function(z) c(n = length(z), mean = mean(z), SD = sd(z)))
    cm <- data.frame(cm[, 1:2], cm$y); names(cm)[1:2] <- c(a$f1, a$f2)
    li <- paste0("<li><b>", tab$Source[-nrow(tab)], "</b>: F(", tab$df[-nrow(tab)], ", ", dfe, ") = ", fmt(tab$F[-nrow(tab)], 2), ", ", p_eq(tab$p[-nrow(tab)]),
                 " &rarr; ", ifelse(tab$p[-nrow(tab)] < al, "<b>significant</b>", "not significant"), "; partial &eta;&sup2; = ", fmt(tab$`Partial eta squared`[-nrow(tab)], 3), "</li>", collapse = "")
    ip <- if (!identical(a$inter, "No")) tab$p[3] else NA
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><ul>", li, "</ul>",
      if (!is.na(ip) && ip < al) warn_html("The interaction is significant: the effect of ", a$f1, " depends on the level of ", a$f2, ". Interpret main effects with caution - look at the interaction plot and compare simple effects.") else "",
      note_html("Type III sums of squares (sum-to-zero contrasts) are reported, so results are valid for unbalanced designs. Assumptions: normal residuals, equal variances across cells."),
      normality_note(c(Residuals = shapiro_safe(residuals(fit)))))
    steps <- step_list("Total variation is partitioned: SS<sub>Total</sub> = SS<sub>A</sub> + SS<sub>B</sub> + SS<sub>A&times;B</sub> + SS<sub>Error</sub>.",
      paste0("Each effect: ", mj("F = \\frac{MS_{effect}}{MS_{error}},\\quad MS = \\frac{SS}{df}")),
      paste0("MS<sub>error</sub> = ", fmt(rss), " / ", dfe, " = ", fmt(rss / dfe)),
      paste0("Partial effect size: ", mj("\\eta_p^2 = \\frac{SS_{effect}}{SS_{effect}+SS_{error}}")),
      "Type III SS test each effect after adjusting for all other terms in the model.")
    list(tables = list(`Cell means` = cm, `Two-way ANOVA (Type III)` = tab), html = html, steps = steps,
         plot = function() { op <- par(mar = c(5, 4, 3, 1)); on.exit(par(op))
           interaction.plot(dd$A, dd$B, dd$y, col = pal_n(nlevels(dd$B)), lwd = 3, type = "b", pch = 19, xlab = a$f1, ylab = paste("Mean", a$y), trace.label = a$f2, main = "Interaction plot") })
  }))

register_test(list(
  id = "rm_anova", name = "Repeated-measures ANOVA (3+ time points)", cat = CATEGORIES[2], level = "PG",
  inputs = list(list(id = "vars", label = "Select the repeated columns in time order (wide format: one row per subject)", type = "num_multi")),
  run = function(d, a) {
    al <- alpha_of(a); need(length(a$vars) >= 2, "Select at least 2 (preferably 3+) repeated-measure columns.")
    cc <- use_cc(d, a$vars); Y <- as.matrix(cc$d); n <- nrow(Y); k <- ncol(Y); need(n >= 3, "Need at least 3 complete subjects.")
    long <- data.frame(id = factor(rep(seq_len(n), k)), time = factor(rep(colnames(Y), each = n), levels = colnames(Y)), y = as.vector(Y))
    fit <- aov(y ~ time + Error(id / time), data = long); sw <- summary(fit)$`Error: id:time`[[1]]
    F <- sw$`F value`[1]; df1 <- sw$Df[1]; df2 <- sw$Df[2]; p <- sw$`Pr(>F)`[1]
    M <- contr.poly(k); C <- t(M) %*% cov(Y) %*% M; ev <- eigen(C, only.values = TRUE)$values
    gg <- sum(ev)^2 / ((k - 1) * sum(ev^2))
    hf <- min(1, (n * (k - 1) * gg - 2) / ((k - 1) * (n - 1 - (k - 1) * gg)))
    mau <- if (k > 2 && n > k) tryCatch(mauchly.test(lm(Y ~ 1), X = ~1), error = function(e) NULL) else NULL
    mp <- if (is.null(mau)) NA else mau$p.value
    ssb <- summary(fit)$`Error: id`[[1]]$`Sum Sq`[1]; ss_t <- sw$`Sum Sq`[1]; ss_e <- sw$`Sum Sq`[2]
    peta <- ss_t / (ss_t + ss_e)
    tab <- data.frame(Correction = c("Sphericity assumed", "Greenhouse-Geisser", "Huynh-Feldt"), Epsilon = c(1, gg, hf),
                      df1 = df1 * c(1, gg, hf), df2 = df2 * c(1, gg, hf), F = F, p = pf(F, df1 * c(1, gg, hf), df2 * c(1, gg, hf), lower.tail = FALSE), check.names = FALSE)
    use <- if (!is.na(mp) && mp < 0.05) 2 else 1; puse <- tab$p[use]
    desc <- data.frame(Time = colnames(Y), n = n, Mean = colMeans(Y), SD = apply(Y, 2, sd), SE = apply(Y, 2, sd) / sqrt(n), row.names = NULL)
    pw <- combn(k, 2); ph <- data.frame(Comparison = paste(colnames(Y)[pw[1, ]], "vs", colnames(Y)[pw[2, ]]),
      `Mean difference` = colMeans(Y)[pw[1, ]] - colMeans(Y)[pw[2, ]],
      p_unadjusted = apply(pw, 2, function(z) t.test(Y[, z[1]], Y[, z[2]], paired = TRUE)$p.value), check.names = FALSE)
    ph$`p (Bonferroni)` <- p.adjust(ph$p_unadjusted, "bonferroni")
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>H<sub>0</sub>: the mean is the same at all ", k, " time points.</p>",
      "<p><b>Sphericity (Mauchly's test):</b> ", if (is.na(mp)) "not testable (needs &ge; 3 time points and n &gt; k)." else paste0("W = ", fmt(mau$statistic, 3), ", ", p_eq(mp), " &rarr; ",
      if (mp < 0.05) "sphericity is <b>violated</b>; the <b>Greenhouse-Geisser</b> corrected result is used." else "sphericity can be assumed."), "</p>",
      "<p><b>Result:</b> F(", fmt(tab$df1[use], 2), ", ", fmt(tab$df2[use], 2), ") = ", fmt(F, 2), ", ", p_eq(puse), "; partial &eta;&sup2; = ", fmt(peta, 3), ". Change over time is <b>", sig_word(puse, al), "</b>.</p>",
      decision_html(puse, al),
      writeup_html(sprintf("A repeated-measures ANOVA%s showed that %s change in the outcome across the %d time points (F(%s, %s) = %s, %s, partial &eta;&sup2; = %s).",
        if (use == 2) " with Greenhouse-Geisser correction" else "", if (puse < al) "there was a significant" else "there was no significant", k, fmt(tab$df1[use], 2), fmt(tab$df2[use], 2), fmt(F, 2), p_eq(puse), fmt(peta, 2))))
    steps <- step_list("Variation is split into: between-subjects (individual differences, removed from error), time (effect of interest) and residual (time &times; subject).",
      paste0(mj("SS_{time} = n\\sum_j(\\bar{x}_{.j}-\\bar{x})^2 = ", fmt(ss_t), ",\\quad SS_{error} = ", fmt(ss_e))),
      paste0(mj("F = \\frac{SS_{time}/(k-1)}{SS_{error}/((n-1)(k-1))} = \\frac{", fmt(ss_t), "/", df1, "}{", fmt(ss_e), "/", df2, "} = ", fmt(F))),
      paste0("Sphericity = equal variances of all pairwise differences. Greenhouse-Geisser ", mj("\\varepsilon = \\frac{(\\sum\\lambda_i)^2}{(k-1)\\sum\\lambda_i^2} = ", fmt(gg)), " multiplies both df when sphericity is violated."),
      "Post-hoc: paired t-tests between time points with Bonferroni correction.")
    list(tables = list(`Descriptives by time` = desc, `Mauchly's test of sphericity` = data.frame(W = if (is.null(mau)) NA else unname(mau$statistic), p = mp),
                       `Within-subjects effect (time)` = tab, `Post-hoc pairwise (paired t, Bonferroni)` = ph), html = html, steps = steps,
         plot = function() { m <- colMeans(Y); se <- apply(Y, 2, sd) / sqrt(n)
           matplot(t(Y), type = "l", lty = 1, col = adjustcolor("grey50", 0.3), xaxt = "n", ylab = "Value", xlab = "", main = "Individual profiles and mean +/- 95% CI")
           axis(1, 1:k, colnames(Y)); lines(1:k, m, lwd = 4, col = PAL[1]); points(1:k, m, pch = 19, cex = 1.5, col = PAL[1])
           arrows(1:k, m - 1.96 * se, 1:k, m + 1.96 * se, angle = 90, code = 3, length = 0.06, col = PAL[1], lwd = 2) })
  }))

register_test(list(
  id = "ancova", name = "ANCOVA (compare groups adjusting for a covariate)", cat = CATEGORIES[2], level = "PG",
  inputs = list(list(id = "y", label = "Outcome (numeric)", type = "num"), list(id = "g", label = "Group", type = "cat"),
                list(id = "cv", label = "Covariate (numeric, e.g. baseline value / age)", type = "num")),
  run = function(d, a) {
    al <- alpha_of(a); cc <- use_cc(d, c(a$y, a$g, a$cv)); dd <- data.frame(y = cc$d[[1]], g = as_fac(cc$d[[2]]), x = cc$d[[3]])
    old <- options(contrasts = c("contr.sum", "contr.poly")); on.exit(options(old))
    slope <- anova(lm(y ~ x * g, dd)); ps <- slope$`Pr(>F)`[3]
    fit <- lm(y ~ x + g, dd); dr <- drop1(fit, . ~ ., test = "F"); rss <- deviance(fit)
    tab <- data.frame(Source = c(paste("Covariate:", a$cv), paste("Group:", a$g), "Residual"), `Sum of squares (Type III)` = c(dr$`Sum of Sq`[-1], rss),
                      df = c(dr$Df[-1], df.residual(fit)), F = c(dr$`F value`[-1], NA), p = c(dr$`Pr(>F)`[-1], NA), check.names = FALSE)
    tab$`Partial eta squared` <- c(dr$`Sum of Sq`[-1] / (dr$`Sum of Sq`[-1] + rss), NA)
    nd <- data.frame(x = mean(dd$x), g = levels(dd$g)); pr <- predict(fit, nd, se.fit = TRUE)
    adj <- data.frame(Group = levels(dd$g), `Unadjusted mean` = tapply(dd$y, dd$g, mean), `Adjusted mean` = pr$fit, SE = pr$se.fit,
                      `CI low` = pr$fit - qt(.975, df.residual(fit)) * pr$se.fit, `CI high` = pr$fit + qt(.975, df.residual(fit)) * pr$se.fit, check.names = FALSE, row.names = NULL)
    pg <- tab$p[2]
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4>",
      "<p><b>Assumption - homogeneity of regression slopes:</b> group &times; covariate interaction ", p_eq(ps), if (ps < 0.05) warn_html("Slopes differ between groups - standard ANCOVA is not appropriate; consider reporting the interaction model (multiple regression).") else " &rarr; slopes are parallel (assumption met).", "</p>",
      "<p>After adjusting for <b>", a$cv, "</b> (evaluated at its mean ", fmt(mean(dd$x), 2), "), the effect of <b>", a$g, "</b> on ", a$y, " was F(", tab$df[2], ", ", tab$df[3], ") = ", fmt(tab$F[2], 2), ", ", p_eq(pg), " &rarr; <b>", sig_word(pg, al), "</b> (partial &eta;&sup2; = ", fmt(tab$`Partial eta squared`[2], 3), ").</p>",
      decision_html(pg, al), writeup_html(sprintf("After adjusting for %s, %s %s differed between %s groups (ANCOVA, F(%d, %d) = %s, %s, partial &eta;&sup2; = %s).",
        a$cv, a$y, if (pg < al) "significantly" else "did not significantly", a$g, tab$df[2], tab$df[3], fmt(tab$F[2], 2), p_eq(pg), fmt(tab$`Partial eta squared`[2], 2))))
    steps <- step_list("Fit the model: Y = &beta;<sub>0</sub> + &beta;<sub>1</sub>&times;Covariate + Group effects + error.",
      paste0("Adjusted mean for group j: ", mj("\\bar{Y}_j^{adj} = \\bar{Y}_j - b_w(\\bar{X}_j - \\bar{X})"), " where b<sub>w</sub> = common within-group slope = ", fmt(coef(fit)["x"])),
      "F test for group compares the model with and without the group term (Type III).",
      "Checks: linear covariate-outcome relationship, parallel slopes, normal residuals, equal variances.")
    list(tables = list(`ANCOVA table (Type III)` = tab, `Adjusted (estimated marginal) means` = adj,
                       `Homogeneity of slopes` = data.frame(Test = "Group x covariate interaction", F = slope$`F value`[3], p = ps)),
         html = html, steps = steps,
         plot = function() { k <- nlevels(dd$g); plot(dd$x, dd$y, col = pal_n(k)[dd$g], pch = 19, xlab = a$cv, ylab = a$y, main = "Outcome vs covariate by group (parallel ANCOVA lines)")
           cf <- coef(lm(y ~ x + g, dd, contrasts = list(g = "contr.treatment")))
           for (i in seq_len(k)) abline(a = cf[1] + if (i > 1) cf[paste0("g", levels(dd$g)[i])] else 0, b = cf["x"], col = pal_n(k)[i], lwd = 2)
           legend("topleft", levels(dd$g), col = pal_n(k), pch = 19, lwd = 2, bty = "n") })
  }))
