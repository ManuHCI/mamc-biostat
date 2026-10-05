# =====================================================================
#  6. AGREEMENT, DIAGNOSTIC ACCURACY & RELIABILITY
# =====================================================================

kappa_word <- function(k) if (is.na(k)) "" else if (k < 0) "poor (less than chance)" else if (k <= .2) "slight" else if (k <= .4) "fair" else if (k <= .6) "moderate" else if (k <= .8) "substantial" else "almost perfect"

register_test(list(
  id = "kappa", name = "Cohen's kappa (inter-rater agreement, categorical)", cat = CATEGORIES[6], level = "PG",
  inputs = list(list(id = "r1", label = "Rater 1 / Method 1", type = "cat"), list(id = "r2", label = "Rater 2 / Method 2", type = "cat"),
                list(id = "wt", label = "Weighting", type = "select", choices = c("Unweighted (nominal)", "Linear weighted (ordinal)", "Quadratic weighted (ordinal)"))),
  run = function(d, a) {
    al <- alpha_of(a); cc <- use_cc(d, c(a$r1, a$r2)); lv <- sort(unique(c(as.character(cc$d[[1]]), as.character(cc$d[[2]]))))
    tb <- table(factor(cc$d[[1]], lv), factor(cc$d[[2]], lv)); names(dimnames(tb)) <- c(a$r1, a$r2); N <- sum(tb); k <- length(lv); P <- tb / N
    W <- switch(substr(a$wt, 1, 3), Unw = diag(k), Lin = 1 - abs(outer(1:k, 1:k, "-")) / (k - 1), Qua = 1 - (outer(1:k, 1:k, "-") / (k - 1))^2)
    pr <- rowSums(P); pc <- colSums(P); E <- outer(pr, pc); po <- sum(W * P); pe <- sum(W * E); kap <- (po - pe) / (1 - pe)
    wi <- W %*% pc; wj <- t(W) %*% pr  # Fleiss-Cohen-Everitt large-sample SE under H1
    se <- sqrt(sum(P * (W - outer(as.vector(wi), as.vector(wj), "+") * (1 - kap))^2) - (kap - pe * (1 - kap))^2) / ((1 - pe) * sqrt(N))
    se0 <- sqrt(sum(E * (W - outer(as.vector(wi), as.vector(wj), "+"))^2) - pe^2) / ((1 - pe) * sqrt(N)); zz <- kap / se0; p <- 2 * pnorm(-abs(zz)); q <- qnorm(1 - al / 2)
    res <- data.frame(Statistic = c("Observed agreement (Po)", "Expected agreement by chance (Pe)", paste0("Kappa (", a$wt, ")")), Value = c(po, pe, kap),
                      `CI low` = c(NA, NA, kap - q * se), `CI high` = c(NA, NA, kap + q * se), check.names = FALSE)
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>Raw agreement was ", fmt(100 * po, 1), "%, but ", fmt(100 * pe, 1), "% agreement would be expected by chance alone. Kappa corrects for this: &kappa; = ", fmt(kap, 3), " ",
      ci_txt(kap - q * se, kap + q * se, 3), " &rarr; <b>", kappa_word(kap), " agreement</b> (Landis &amp; Koch). Test of &kappa; = 0: z = ", fmt(zz, 2), ", ", p_eq(p), ".</p>",
      note_html("Landis &amp; Koch scale: &lt;0 poor, 0-0.20 slight, 0.21-0.40 fair, 0.41-0.60 moderate, 0.61-0.80 substantial, 0.81-1 almost perfect. Use weighted kappa for ordered categories (e.g. grades I-IV)."),
      writeup_html(sprintf("Agreement between %s and %s was %s (Cohen's &kappa; = %s, %s; observed agreement %s%%).", a$r1, a$r2, kappa_word(kap), fmt(kap, 2), ci_txt(kap - q * se, kap + q * se), fmt(100 * po, 1))))
    steps <- step_list(paste0(mj("P_o = \\frac{\\text{agreements}}{N} = ", fmt(po))), paste0(mj("P_e = \\sum_i \\frac{n_{i\\cdot}}{N}\\times\\frac{n_{\\cdot i}}{N} = ", fmt(pe))),
      paste0(mj("\\kappa = \\frac{P_o-P_e}{1-P_e} = \\frac{", fmt(po), "-", fmt(pe), "}{1-", fmt(pe), "} = ", fmt(kap))), "Weighted kappa gives partial credit for near-disagreements (weights w<sub>ij</sub> in P<sub>o</sub> and P<sub>e</sub>).")
    list(tables = list(`Agreement table` = data.frame(` ` = rownames(tb), as.data.frame.matrix(tb), check.names = FALSE), `Kappa` = res,
                       `Significance` = data.frame(z = zz, p = p)), html = html, steps = steps,
         plot = function() { op <- par(mar = c(5, 5, 3, 1)); on.exit(par(op)); image(1:k, 1:k, t(tb)[, k:1], col = colorRampPalette(c("white", PAL[1]))(30), axes = FALSE, xlab = a$r2, ylab = a$r1, main = "Agreement matrix (diagonal = agreement)")
           axis(1, 1:k, lv); axis(2, 1:k, rev(lv), las = 1); text(rep(1:k, k), rep(k:1, each = k), as.vector(t(tb)), cex = 1.2) })
  }))

register_test(list(
  id = "bland_altman", name = "Bland-Altman analysis (agreement of 2 methods)", cat = CATEGORIES[6], level = "PG",
  inputs = list(list(id = "m1", label = "Method 1 (e.g. reference)", type = "num"), list(id = "m2", label = "Method 2 (e.g. new device)", type = "num")),
  run = function(d, a) {
    need(a$m1 != a$m2, "Choose two different columns."); cc <- use_cc(d, c(a$m1, a$m2)); x1 <- cc$d[[1]]; x2 <- cc$d[[2]]; n <- length(x1)
    df <- x2 - x1; av <- (x1 + x2) / 2; b <- mean(df); s <- sd(df); lo <- b - 1.96 * s; hi <- b + 1.96 * s; tq <- qt(.975, n - 1)
    seb <- s / sqrt(n); sel <- sqrt(3 * s^2 / n); tt <- t.test(df); pr <- cor.test(av, df)
    res <- data.frame(Statistic = c("Mean difference (bias)", "Lower limit of agreement", "Upper limit of agreement"), Value = c(b, lo, hi),
                      `CI low` = c(b - tq * seb, lo - tq * sel, hi - tq * sel), `CI high` = c(b + tq * seb, lo + tq * sel, hi + tq * sel), check.names = FALSE)
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>On average, ", a$m2, " reads <b>", fmt(abs(b), 2), " units ", if (b >= 0) "higher" else "lower", "</b> than ", a$m1, " (bias; one-sample t-test of bias ", p_eq(tt$p.value), ").</p>",
      "<p>95% limits of agreement: <b>", fmt(lo, 2), " to ", fmt(hi, 2), "</b> - for 95% of subjects, the two methods will differ by an amount within this range. Whether this is acceptable is a <b>clinical</b> decision, not a statistical one.</p>",
      if (pr$p.value < 0.05) warn_html("Proportional bias: the difference changes with the magnitude of measurement (r = ", fmt(pr$estimate, 2), ", ", p_eq(pr$p.value), "). Consider log-transforming or regression-based limits.") else note_html("No proportional bias detected (", p_eq(pr$p.value), ")."),
      note_html("A high correlation does NOT mean good agreement - that is why Bland-Altman is used instead of correlation for method comparison."),
      writeup_html(sprintf("The mean bias of %s relative to %s was %s %s with 95%% limits of agreement from %s to %s.", a$m2, a$m1, fmt(b, 2), ci_txt(b - tq * seb, b + tq * seb), fmt(lo, 2), fmt(hi, 2))))
    steps <- step_list("For each subject: difference d = Method 2 &minus; Method 1, and average = (Method 1 + Method 2)/2.", paste0(mj("\\text{Bias} = \\bar d = ", fmt(b), ",\\quad s_d = ", fmt(s))),
      paste0(mj("LoA = \\bar d \\pm 1.96\\,s_d = ", fmt(b), " \\pm 1.96\\times", fmt(s), " = [", fmt(lo), ",\\;", fmt(hi), "]")),
      paste0("SE of bias = s<sub>d</sub>/&radic;n; SE of each limit &asymp; ", mj("\\sqrt{3s_d^2/n}")), "Plot differences against averages; points should scatter randomly around the bias line.")
    list(tables = list(`Bland-Altman` = res), html = html, steps = steps,
         plot = function() { plot(av, df, pch = 19, col = adjustcolor(PAL[1], 0.6), xlab = paste("Average of", a$m1, "and", a$m2), ylab = paste(a$m2, "-", a$m1), main = "Bland-Altman plot", ylim = range(c(df, lo, hi)) * 1.1)
           abline(h = b, col = PAL[2], lwd = 2.5); abline(h = c(lo, hi), col = PAL[6], lty = 2, lwd = 2); abline(h = 0, col = "grey60")
           text(max(av), c(b, lo, hi), c(paste("Bias", fmt(b, 2)), paste("-1.96 SD", fmt(lo, 2)), paste("+1.96 SD", fmt(hi, 2))), pos = 3, cex = 0.8, adj = 1) })
  }))

register_test(list(
  id = "icc", name = "Intraclass correlation (ICC, reliability)", cat = CATEGORIES[6], level = "PG",
  inputs = list(list(id = "vars", label = "Ratings by each rater / repeat (2+ numeric columns)", type = "num_multi")),
  run = function(d, a) {
    need(length(a$vars) >= 2, "Select at least 2 rater columns."); cc <- use_cc(d, a$vars); Y <- as.matrix(cc$d); n <- nrow(Y); k <- ncol(Y)
    gm <- mean(Y); ssr <- k * sum((rowMeans(Y) - gm)^2); ssc <- n * sum((colMeans(Y) - gm)^2); sst <- sum((Y - gm)^2); sse <- sst - ssr - ssc
    msr <- ssr / (n - 1); msc <- ssc / (k - 1); mse <- sse / ((n - 1) * (k - 1)); msw <- (ssc + sse) / (n * (k - 1))
    icc <- c(`ICC(1,1) one-way random, single` = (msr - msw) / (msr + (k - 1) * msw),
             `ICC(2,1) two-way random, absolute agreement, single` = (msr - mse) / (msr + (k - 1) * mse + k * (msc - mse) / n),
             `ICC(3,1) two-way mixed, consistency, single` = (msr - mse) / (msr + (k - 1) * mse),
             `ICC(2,k) two-way random, absolute, average` = (msr - mse) / (msr + (msc - mse) / n),
             `ICC(3,k) two-way mixed, consistency, average` = (msr - mse) / msr)
    F3 <- msr / mse; df1 <- n - 1; df2 <- (n - 1) * (k - 1); FL <- F3 / qf(.975, df1, df2); FU <- F3 * qf(.975, df2, df1)
    ci3 <- c((FL - 1) / (FL + k - 1), (FU - 1) / (FU + k - 1))
    F1 <- msr / msw; d2 <- n * (k - 1); FL1 <- F1 / qf(.975, df1, d2); FU1 <- F1 * qf(.975, d2, df1)
    i2 <- icc[2]; aa <- k * i2 / (n * (1 - i2)); bb <- 1 + k * i2 * (n - 1) / (n * (1 - i2))   # McGraw & Wong CI for ICC(2,1)
    v <- (aa * msc + bb * mse)^2 / ((aa * msc)^2 / (k - 1) + (bb * mse)^2 / ((n - 1) * (k - 1)))
    F2L <- qf(.975, n - 1, v); F2U <- qf(.975, v, n - 1)
    l2 <- n * (msr - F2L * mse) / (F2L * (k * msc + (k * n - k - n) * mse) + n * msr); u2 <- n * (F2U * msr - mse) / (k * msc + (k * n - k - n) * mse + n * F2U * msr)
    res <- data.frame(Type = names(icc), ICC = icc, `CI low` = c((FL1 - 1) / (FL1 + k - 1), l2, ci3[1], l2 * k / (1 + l2 * (k - 1)), 1 - 1 / FL),
                      `CI high` = c((FU1 - 1) / (FU1 + k - 1), u2, ci3[2], u2 * k / (1 + u2 * (k - 1)), 1 - 1 / FU), check.names = FALSE, row.names = NULL)
    res$Reliability <- ifelse(res$ICC < .5, "Poor", ifelse(res$ICC < .75, "Moderate", ifelse(res$ICC < .9, "Good", "Excellent")))
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>", n, " subjects rated by ", k, " raters. The most commonly reported form, ICC(2,1) absolute agreement = <b>", fmt(i2, 3), "</b> &rarr; <b>",
      res$Reliability[2], "</b> reliability (Koo &amp; Li 2016: &lt;0.5 poor, 0.5-0.75 moderate, 0.75-0.9 good, &gt;0.9 excellent). F test (ICC = 0): F(", df1, ", ", df2, ") = ", fmt(F3, 2), ", ", p_eq(pf(F3, df1, df2, lower.tail = FALSE)), ".</p>",
      note_html("Choose the ICC form before analysis: (1) one-way if each subject has different raters; (2) two-way random if raters are a random sample and you want them interchangeable; (3) two-way mixed if these specific raters are the only ones of interest. 'Average' forms apply when the mean of k ratings will be used in practice."))
    steps <- step_list("Two-way ANOVA partitions variance into subjects (rows), raters (columns) and error.",
      paste0(mj("MS_R = ", fmt(msr), ",\\; MS_C = ", fmt(msc), ",\\; MS_E = ", fmt(mse), ",\\; MS_W = ", fmt(msw))),
      paste0(mj("ICC(2,1) = \\frac{MS_R - MS_E}{MS_R + (k-1)MS_E + \\frac{k}{n}(MS_C-MS_E)} = ", fmt(i2))),
      paste0(mj("ICC(3,1) = \\frac{MS_R - MS_E}{MS_R + (k-1)MS_E} = ", fmt(icc[3]))), "ICC = proportion of total variance due to true differences between subjects.")
    list(tables = list(`ICC` = res, `ANOVA` = data.frame(Source = c("Subjects", "Raters", "Error"), SS = c(ssr, ssc, sse), df = c(n - 1, k - 1, df2), MS = c(msr, msc, mse))), html = html, steps = steps,
         plot = function() matplot(t(Y), type = "b", pch = 19, lty = 1, col = adjustcolor(pal_n(n), 0.6), xaxt = "n", xlab = "Rater", ylab = "Rating", main = "Each line = one subject (parallel flat lines = high reliability)") )
  }))

register_test(list(
  id = "diag", name = "Diagnostic test accuracy (sensitivity, specificity, PPV, NPV, LR)", cat = CATEGORIES[6], level = "UG & PG",
  inputs = list(list(id = "t", label = "New test result", type = "cat"), list(id = "tl", label = "'Positive' level of test", type = "level", of = "t"),
                list(id = "g", label = "Gold standard / reference", type = "cat"), list(id = "gl", label = "'Disease present' level", type = "level", of = "g")),
  run = function(d, a) {
    cc <- use_cc(d, c(a$t, a$g)); tp <- as.character(cc$d[[1]]) == a$tl; dz <- as.character(cc$d[[2]]) == a$gl
    TP <- sum(tp & dz); FP <- sum(tp & !dz); FN <- sum(!tp & dz); TN <- sum(!tp & !dz); N <- TP + FP + FN + TN
    bci <- function(x, n) if (n > 0) binom.test(x, n)$conf.int else c(NA, NA)
    mk <- function(nm, x, n) { ci <- bci(x, n); data.frame(Measure = nm, Value = x / n, `CI low` = ci[1], `CI high` = ci[2], Calculation = paste0(x, "/", n), check.names = FALSE) }
    sens <- TP / (TP + FN); spec <- TN / (TN + FP); lrp <- sens / (1 - spec); lrn <- (1 - sens) / spec
    res <- rbind(mk("Sensitivity", TP, TP + FN), mk("Specificity", TN, TN + FP), mk("Positive predictive value (PPV)", TP, TP + FP), mk("Negative predictive value (NPV)", TN, TN + FN),
                 mk("Accuracy", TP + TN, N), mk("Prevalence", TP + FN, N))
    selp <- sqrt(1 / TP - 1 / (TP + FN) + 1 / FP - 1 / (FP + TN)); seln <- sqrt(1 / FN - 1 / (TP + FN) + 1 / TN - 1 / (FP + TN))
    res <- rbind(res, data.frame(Measure = c("Positive likelihood ratio (LR+)", "Negative likelihood ratio (LR-)", "Youden index (J)", "Diagnostic odds ratio"),
      Value = c(lrp, lrn, sens + spec - 1, lrp / lrn), `CI low` = c(exp(log(lrp) - 1.96 * selp), exp(log(lrn) - 1.96 * seln), NA, NA),
      `CI high` = c(exp(log(lrp) + 1.96 * selp), exp(log(lrn) + 1.96 * seln), NA, NA), Calculation = c("Sens / (1 - Spec)", "(1 - Sens) / Spec", "Sens + Spec - 1", "LR+ / LR-"), check.names = FALSE))
    tb <- data.frame(` ` = c(paste("Test +", a$tl), "Test -", "Total"), `Disease present` = c(TP, FN, TP + FN), `Disease absent` = c(FP, TN, FP + TN), Total = c(TP + FP, FN + TN, N), check.names = FALSE)
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><ul><li><b>Sensitivity ", fmt(100 * sens, 1), "%</b>: the test detects ", fmt(100 * sens, 1), "% of people who have the disease (", FN, " false negatives).</li>",
      "<li><b>Specificity ", fmt(100 * spec, 1), "%</b>: the test is negative in ", fmt(100 * spec, 1), "% of people without the disease (", FP, " false positives).</li>",
      "<li><b>PPV ", fmt(100 * TP / (TP + FP), 1), "%</b>: a positive result means disease is present ", fmt(100 * TP / (TP + FP), 1), "% of the time; <b>NPV ", fmt(100 * TN / (TN + FN), 1), "%</b>.</li>",
      "<li><b>LR+ = ", fmt(lrp, 2), "</b>, <b>LR&minus; = ", fmt(lrn, 2), "</b> (LR+ &gt; 10 or LR&minus; &lt; 0.1 = strong evidence to rule in / rule out).</li></ul>",
      note_html("PPV and NPV depend on disease prevalence (", fmt(100 * (TP + FN) / N, 1), "% here). In a population with different prevalence they will change; sensitivity, specificity and LRs do not. Mnemonics: SnNout (high Sensitivity, Negative rules OUT), SpPin (high Specificity, Positive rules IN)."))
    steps <- step_list(paste0("2&times;2 table: TP = ", TP, ", FP = ", FP, ", FN = ", FN, ", TN = ", TN),
      paste0(mj("Sensitivity = \\frac{TP}{TP+FN} = \\frac{", TP, "}{", TP + FN, "} = ", fmt(sens))), paste0(mj("Specificity = \\frac{TN}{TN+FP} = \\frac{", TN, "}{", TN + FP, "} = ", fmt(spec))),
      paste0(mj("PPV = \\frac{TP}{TP+FP},\\quad NPV = \\frac{TN}{TN+FN}")), paste0(mj("LR+ = \\frac{Sens}{1-Spec},\\quad LR- = \\frac{1-Sens}{Spec}")),
      "Confidence intervals for proportions: exact Clopper-Pearson; for LRs: log method.")
    list(tables = list(`2x2 table` = tb, `Diagnostic accuracy` = res), html = html, steps = steps,
         plot = function() { v <- c(sens, spec, TP / (TP + FP), TN / (TN + FN)) * 100; b <- barplot(v, names.arg = c("Sensitivity", "Specificity", "PPV", "NPV"), col = PAL[1:4], ylim = c(0, 110), ylab = "%", main = "Diagnostic accuracy")
           text(b, v, paste0(fmt(v, 1), "%"), pos = 3) })
  }))

register_test(list(
  id = "roc", name = "ROC curve & AUC (best cut-off by Youden)", cat = CATEGORIES[6], level = "PG",
  inputs = list(list(id = "x", label = "Test value / marker (numeric)", type = "num"), list(id = "y", label = "Disease status (gold standard)", type = "cat"),
                list(id = "yl", label = "'Disease present' level", type = "level", of = "y")),
  run = function(d, a) {
    cc <- use_cc(d, c(a$x, a$y)); x <- cc$d[[1]]; yy <- as.integer(as.character(cc$d[[2]]) == a$yl); n1 <- sum(yy); n0 <- sum(1 - yy)
    need(n1 > 0 && n0 > 0, "Both diseased and non-diseased subjects are needed.")
    auc <- auc_mw(x, yy); dir <- "higher values = disease"; if (auc < 0.5) { auc <- 1 - auc; x <- -x; dir <- "lower values = disease" }
    q1 <- auc / (2 - auc); q2 <- 2 * auc^2 / (1 + auc); se <- sqrt((auc * (1 - auc) + (n1 - 1) * (q1 - auc^2) + (n0 - 1) * (q2 - auc^2)) / (n1 * n0))
    zz <- (auc - 0.5) / se; p <- 2 * pnorm(-abs(zz))
    cuts <- sort(unique(x)); se_ <- sapply(cuts, function(c) mean(x[yy == 1] >= c)); sp_ <- sapply(cuts, function(c) mean(x[yy == 0] < c))
    j <- which.max(se_ + sp_ - 1); best <- cuts[j]; shown <- if (dir == "lower values = disease") -best else best
    coords <- data.frame(`Cut-off` = if (dir == "lower values = disease") -cuts else cuts, Sensitivity = se_, Specificity = sp_, Youden = se_ + sp_ - 1, check.names = FALSE)
    res <- data.frame(AUC = auc, SE = se, `CI low` = max(0, auc - 1.96 * se), `CI high` = min(1, auc + 1.96 * se), z = zz, p = p, `Optimal cut-off` = shown,
                      `Sensitivity at cut-off` = se_[j], `Specificity at cut-off` = sp_[j], Direction = dir, check.names = FALSE)
    word <- if (auc >= .9) "excellent" else if (auc >= .8) "good" else if (auc >= .7) "fair/acceptable" else if (auc >= .6) "poor" else "fail (no better than chance)"
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>AUC = <b>", fmt(auc, 3), "</b> ", ci_txt(res$`CI low`, res$`CI high`, 3), ", ", p_eq(p), " (vs 0.5) &rarr; <b>", word, "</b> discrimination. ",
      "There is a ", fmt(100 * auc, 1), "% probability that a randomly chosen diseased person has a ", if (dir == "higher values = disease") "higher" else "lower", " ", a$x, " than a randomly chosen non-diseased person.</p>",
      "<p>Best cut-off (maximum Youden index): <b>", a$x, if (dir == "higher values = disease") " &ge; " else " &le; ", fmt(shown, 3), "</b> with sensitivity ", fmt(100 * se_[j], 1), "% and specificity ", fmt(100 * sp_[j], 1), "%.</p>",
      note_html("AUC 0.5 = chance, 0.7-0.8 acceptable, 0.8-0.9 good, &gt;0.9 excellent. CI by Hanley-McNeil method. The Youden cut-off maximises Sens + Spec; a clinically better cut-off may favour sensitivity (screening) or specificity (confirmation)."),
      writeup_html(sprintf("%s showed %s discrimination (AUC %s, %s). The optimal cut-off of %s gave a sensitivity of %s%% and specificity of %s%%.", a$x, word, fmt(auc, 2), ci_txt(res$`CI low`, res$`CI high`), fmt(shown, 2), fmt(100 * se_[j], 1), fmt(100 * sp_[j], 1))))
    steps <- step_list("For every possible cut-off, compute sensitivity and 1 &minus; specificity; plot them to form the ROC curve.",
      paste0("AUC equals the Mann-Whitney probability: ", mj("AUC = \\frac{U}{n_1 n_0} = P(X_{diseased} > X_{healthy})")),
      paste0("Hanley-McNeil SE: ", mj("SE = \\sqrt{\\frac{A(1-A) + (n_1-1)(Q_1-A^2) + (n_0-1)(Q_2-A^2)}{n_1 n_0}},\\; Q_1=\\frac{A}{2-A},\\; Q_2=\\frac{2A^2}{1+A}")),
      paste0("Youden index ", mj("J = Sens + Spec - 1"), "; best cut-off maximises J = ", fmt(se_[j] + sp_[j] - 1)))
    list(tables = list(`ROC analysis` = res, `Coordinates (selected cut-offs)` = coords[unique(round(seq(1, nrow(coords), length.out = min(25, nrow(coords))))), ]), html = html, steps = steps,
         plot = function() { plot(c(1, 1 - sp_, 0), c(1, se_, 0), type = "s", lwd = 3, col = PAL[1], xlab = "1 - Specificity (false positive rate)", ylab = "Sensitivity", main = paste0("ROC curve: AUC = ", fmt(auc, 3)), xlim = c(0, 1), ylim = c(0, 1))
           abline(0, 1, lty = 2, col = "grey50"); points(1 - sp_[j], se_[j], pch = 19, cex = 1.8, col = PAL[2]); text(1 - sp_[j], se_[j], paste("Cut-off", fmt(shown, 2)), pos = 4) })
  }))

register_test(list(
  id = "cronbach", name = "Cronbach's alpha (questionnaire reliability)", cat = CATEGORIES[6], level = "PG",
  inputs = list(list(id = "vars", label = "Questionnaire items (numeric scores, 2+)", type = "num_multi")),
  run = function(d, a) {
    need(length(a$vars) >= 2, "Select at least 2 items."); cc <- use_cc(d, a$vars); X <- as.matrix(cc$d); k <- ncol(X); n <- nrow(X)
    alpha_f <- function(M) { k <- ncol(M); (k / (k - 1)) * (1 - sum(apply(M, 2, var)) / var(rowSums(M))) }
    al <- alpha_f(X); R <- cor(X); rb <- mean(R[upper.tri(R)]); sa <- k * rb / (1 + (k - 1) * rb)
    it <- data.frame(Item = colnames(X), Mean = colMeans(X), SD = apply(X, 2, sd),
                     `Corrected item-total r` = sapply(1:k, function(j) cor(X[, j], rowSums(X[, -j, drop = FALSE]))),
                     `Alpha if item deleted` = sapply(1:k, function(j) if (k > 2) alpha_f(X[, -j, drop = FALSE]) else NA), check.names = FALSE, row.names = NULL)
    word <- if (al >= .9) "excellent" else if (al >= .8) "good" else if (al >= .7) "acceptable" else if (al >= .6) "questionable" else if (al >= .5) "poor" else "unacceptable"
    weak <- it$Item[it$`Corrected item-total r` < 0.3]
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>Cronbach's &alpha; = <b>", fmt(al, 3), "</b> (", k, " items, n = ", n, ") &rarr; <b>", word, "</b> internal consistency. Standardised &alpha; = ", fmt(sa, 3), ".</p>",
      if (length(weak)) warn_html("Items with corrected item-total correlation &lt; 0.3 (may not measure the same construct): ", paste(weak, collapse = ", "), ". Check 'alpha if item deleted'.") else "",
      note_html("&alpha; &ge; 0.7 is generally acceptable for research; &gt; 0.95 may indicate redundant items. Reverse-scored items must be recoded before analysis."))
    steps <- step_list(paste0(mj("\\alpha = \\frac{k}{k-1}\\left(1 - \\frac{\\sum s_i^2}{s_T^2}\\right) = \\frac{", k, "}{", k - 1, "}\\left(1-\\frac{", fmt(sum(apply(X, 2, var))), "}{", fmt(var(rowSums(X))), "}\\right) = ", fmt(al))),
      "s<sub>i</sub>&sup2; = variance of each item; s<sub>T</sub>&sup2; = variance of total score.", paste0("Standardised: ", mj("\\alpha_{std} = \\frac{k\\bar r}{1+(k-1)\\bar r}"), " where r&#772; = mean inter-item correlation = ", fmt(rb)))
    list(tables = list(`Reliability` = data.frame(`Cronbach's alpha` = al, `Standardised alpha` = sa, Items = k, n = n, check.names = FALSE), `Item statistics` = it), html = html, steps = steps,
         plot = function() { op <- par(mar = c(5, 8, 3, 1)); on.exit(par(op)); barplot(it$`Corrected item-total r`, names.arg = it$Item, horiz = TRUE, las = 1, col = ifelse(it$`Corrected item-total r` < .3, PAL[6], PAL[4]), xlab = "Corrected item-total correlation", main = "Item discrimination"); abline(v = 0.3, lty = 2) })
  }))
