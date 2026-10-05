# =====================================================================
#  5. CORRELATION & REGRESSION
# =====================================================================

scatter_fit <- function(x, y, xl, yl, main) {
  plot(x, y, pch = 19, col = adjustcolor(PAL[1], 0.6), xlab = xl, ylab = yl, main = main)
  abline(lm(y ~ x), col = PAL[2], lwd = 2.5); lines(lowess(x, y), col = PAL[3], lwd = 2, lty = 2)
  legend("topleft", c("Linear fit", "LOWESS (smooth)"), col = PAL[2:3], lwd = 2, lty = 1:2, bty = "n")
}

register_test(list(
  id = "pearson", name = "Pearson correlation (r)", cat = CATEGORIES[5], level = "UG & PG",
  inputs = list(list(id = "x", label = "Variable X (numeric)", type = "num"), list(id = "y", label = "Variable Y (numeric)", type = "num")),
  run = function(d, a) {
    al <- alpha_of(a); need(a$x != a$y, "Choose two different variables."); cc <- use_cc(d, c(a$x, a$y)); x <- cc$d[[1]]; y <- cc$d[[2]]; n <- length(x)
    need(n >= 4, "Need at least 4 complete pairs."); ct <- cor.test(x, y, conf.level = 1 - al); r <- unname(ct$estimate)
    res <- data.frame(n = n, r = r, `r squared` = r^2, `CI low` = ct$conf.int[1], `CI high` = ct$conf.int[2], t = ct$statistic, df = ct$parameter, p = ct$p.value, check.names = FALSE)
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>H<sub>0</sub>: no linear correlation (&rho; = 0).</p><p>r = ", fmt(r, 3), " ", ci_txt(ct$conf.int[1], ct$conf.int[2], 3), ", ", p_eq(ct$p.value),
      " &rarr; a <b>", r_word(r), " ", if (r > 0) "positive" else "negative", "</b> correlation that is <b>", sig_word(ct$p.value, al), "</b>. r&sup2; = ", fmt(r^2, 3), ": ", fmt(100 * r^2, 1), "% of the variation in one variable is shared with the other.</p>",
      decision_html(ct$p.value, al), normality_note(setNames(c(shapiro_safe(x), shapiro_safe(y)), c(a$x, a$y))),
      note_html("Correlation is not causation. Pearson's r measures only <i>linear</i> association and is sensitive to outliers - always look at the scatter plot. For skewed/ordinal data use Spearman."),
      writeup_html(sprintf("There was a %s %s correlation between %s and %s (Pearson r = %s, %s, %s, n = %d).", r_word(r), if (r > 0) "positive" else "negative", a$x, a$y, fmt(r, 2), ci_txt(ct$conf.int[1], ct$conf.int[2]), p_eq(ct$p.value), n)))
    steps <- step_list(paste0(mj("r = \\frac{\\sum (x_i-\\bar x)(y_i-\\bar y)}{\\sqrt{\\sum (x_i-\\bar x)^2\\sum (y_i-\\bar y)^2}} = \\frac{", fmt(sum((x - mean(x)) * (y - mean(y))), 2), "}{\\sqrt{", fmt(sum((x - mean(x))^2), 2), "\\times", fmt(sum((y - mean(y))^2), 2), "}} = ", fmt(r))),
      paste0("Significance: ", mj("t = \\frac{r\\sqrt{n-2}}{\\sqrt{1-r^2}} = ", fmt(ct$statistic)), " with df = n &minus; 2 = ", n - 2),
      paste0("95% CI via Fisher's z-transformation: ", mj("z = \\tfrac12\\ln\\frac{1+r}{1-r},\\; SE = \\frac{1}{\\sqrt{n-3}}")))
    list(tables = list(`Pearson correlation` = res), html = html, steps = steps, plot = function() scatter_fit(x, y, a$x, a$y, paste0("r = ", fmt(r, 2), ", ", p_eq(ct$p.value))))
  }))

register_test(list(
  id = "spearman", name = "Spearman rank correlation (rho) & Kendall tau", cat = CATEGORIES[5], level = "UG & PG",
  inputs = list(list(id = "x", label = "Variable X (numeric / ordinal)", type = "num"), list(id = "y", label = "Variable Y (numeric / ordinal)", type = "num")),
  run = function(d, a) {
    al <- alpha_of(a); need(a$x != a$y, "Choose two different variables."); cc <- use_cc(d, c(a$x, a$y)); x <- cc$d[[1]]; y <- cc$d[[2]]; n <- length(x)
    sp <- suppressWarnings(cor.test(x, y, method = "spearman", exact = FALSE)); kd <- suppressWarnings(cor.test(x, y, method = "kendall", exact = FALSE))
    rho <- unname(sp$estimate); zr <- atanh(rho); se <- 1.06 / sqrt(n - 3); q <- qnorm(1 - al / 2)
    res <- data.frame(Method = c("Spearman rho", "Kendall tau-b"), Coefficient = c(rho, kd$estimate), `CI low (approx.)` = c(tanh(zr - q * se), NA),
                      `CI high (approx.)` = c(tanh(zr + q * se), NA), p = c(sp$p.value, kd$p.value), n = n, check.names = FALSE)
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>H<sub>0</sub>: no monotonic association.</p><p>Spearman &rho; = ", fmt(rho, 3), ", ", p_eq(sp$p.value), " &rarr; a <b>", r_word(rho), " ",
      if (rho > 0) "positive" else "negative", "</b> monotonic association, <b>", sig_word(sp$p.value, al), "</b>. Kendall &tau; = ", fmt(kd$estimate, 3), " (", p_eq(kd$p.value), ").</p>", decision_html(sp$p.value, al),
      note_html("Spearman works on ranks, so it is robust to outliers and skew and suitable for ordinal scores (e.g. pain scales). Kendall's tau is preferred for small samples with many ties."),
      writeup_html(sprintf("%s and %s showed a %s %s correlation (Spearman &rho; = %s, %s, n = %d).", a$x, a$y, r_word(rho), if (rho > 0) "positive" else "negative", fmt(rho, 2), p_eq(sp$p.value), n)))
    rx <- rank(x); ry <- rank(y)
    steps <- step_list("Rank X and Y separately (ties get average ranks).", paste0("Spearman &rho; = Pearson correlation of the ranks. Without ties: ", mj("\\rho = 1 - \\frac{6\\sum d_i^2}{n(n^2-1)},\\; \\sum d_i^2 = ", fmt(sum((rx - ry)^2), 1))),
      paste0("Kendall ", mj("\\tau = \\frac{(\\text{concordant}) - (\\text{discordant})}{\\text{number of pairs}}")))
    list(tables = list(`Rank correlation` = res), html = html, steps = steps,
         plot = function() { op <- par(mfrow = c(1, 2)); on.exit(par(op)); scatter_fit(x, y, a$x, a$y, "Raw data")
           plot(rx, ry, pch = 19, col = adjustcolor(PAL[2], 0.6), xlab = paste("Rank of", a$x), ylab = paste("Rank of", a$y), main = paste("Ranks: rho =", fmt(rho, 2))) })
  }))

register_test(list(
  id = "corr_matrix", name = "Correlation matrix (many variables)", cat = CATEGORIES[5], level = "PG",
  inputs = list(list(id = "vars", label = "Numeric variables (2 or more)", type = "num_multi"), list(id = "method", label = "Method", type = "select", choices = c("pearson", "spearman"))),
  run = function(d, a) {
    need(length(a$vars) >= 2, "Select at least 2 variables."); cc <- use_cc(d, a$vars); X <- as.matrix(cc$d); k <- ncol(X)
    R <- cor(X, method = a$method); P <- matrix(NA, k, k, dimnames = dimnames(R))
    for (i in 1:k) for (j in 1:k) if (i != j) P[i, j] <- suppressWarnings(cor.test(X[, i], X[, j], method = a$method, exact = FALSE)$p.value)
    star <- ifelse(is.na(P), "", ifelse(P < .001, "***", ifelse(P < .01, "**", ifelse(P < .05, "*", ""))))
    disp <- matrix(paste0(fmt(R, 2), star), k, dimnames = dimnames(R))
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>", tools::toTitleCase(a$method), " correlations among ", k, " variables (n = ", nrow(X), "). * p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001 (unadjusted).</p>",
      warn_html("Many correlations are being tested at once; some will be 'significant' by chance. Consider a Bonferroni threshold of 0.05 / ", k * (k - 1) / 2, " = ", fmt(0.05 / (k * (k - 1) / 2), 4), "."))
    list(tables = list(`Correlation matrix` = data.frame(Variable = rownames(disp), disp, check.names = FALSE),
                       `p-value matrix` = data.frame(Variable = rownames(P), matrix(fmt_p(P), k, dimnames = dimnames(P)), check.names = FALSE)), html = html,
         steps = step_list("Each cell is the pairwise correlation coefficient between two variables, computed exactly as in the Pearson / Spearman tests.", "Diagonal = 1 (each variable with itself)."),
         plot = function() { op <- par(mar = c(8, 8, 3, 2)); on.exit(par(op)); cols <- colorRampPalette(c(PAL[2], "white", PAL[1]))(41)
           image(1:k, 1:k, R[, k:1], col = cols, zlim = c(-1, 1), axes = FALSE, xlab = "", ylab = "", main = "Correlation heat map")
           axis(1, 1:k, colnames(R), las = 2); axis(2, 1:k, rev(colnames(R)), las = 1); text(rep(1:k, k), rep(k:1, each = k), fmt(R, 2), cex = 0.8) })
  }))

vif_calc <- function(fit) {
  X <- model.matrix(fit)[, -1, drop = FALSE]; if (ncol(X) < 2) return(NULL)
  v <- sapply(seq_len(ncol(X)), function(j) { r2 <- summary(lm(X[, j] ~ X[, -j]))$r.squared; 1 / (1 - r2) })
  data.frame(Term = colnames(X), VIF = v, Concern = ifelse(v > 10, "Severe multicollinearity", ifelse(v > 5, "Moderate", "OK")))
}

register_test(list(
  id = "lin_reg", name = "Linear regression (simple & multiple)", cat = CATEGORIES[5], level = "UG & PG",
  inputs = list(list(id = "y", label = "Outcome / dependent variable (numeric)", type = "num"), list(id = "xs", label = "Predictor(s) / independent variables", type = "any_multi")),
  run = function(d, a) {
    al <- alpha_of(a); need(length(a$xs) >= 1, "Select at least one predictor."); need(!(a$y %in% a$xs), "Outcome cannot also be a predictor.")
    cc <- use_cc(d, c(a$y, a$xs)); dd <- cc$d; for (v in a$xs) if (!is.numeric(dd[[v]])) dd[[v]] <- as_fac(dd[[v]])
    names(dd) <- make.names(names(dd)); yv <- make.names(a$y); xv <- make.names(a$xs)
    fit <- lm(reformulate(xv, yv), data = dd); s <- summary(fit); ci <- confint(fit, level = 1 - al)
    co <- data.frame(Term = rownames(s$coefficients), B = s$coefficients[, 1], SE = s$coefficients[, 2], `CI low` = ci[, 1], `CI high` = ci[, 2],
                     t = s$coefficients[, 3], p = s$coefficients[, 4], check.names = FALSE, row.names = NULL)
    sdy <- sd(dd[[yv]]); co$`Std. beta` <- NA
    X <- model.matrix(fit); for (i in 2:nrow(co)) co$`Std. beta`[i] <- co$B[i] * sd(X[, i]) / sdy
    fs <- s$fstatistic; pF <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)
    mod <- data.frame(n = nrow(dd), `R squared` = s$r.squared, `Adjusted R squared` = s$adj.r.squared, `Residual SE` = s$sigma, F = fs[1], df1 = fs[2], df2 = fs[3], p = pF, check.names = FALSE, row.names = NULL)
    vf <- vif_calc(fit); sh <- shapiro_safe(residuals(fit))
    sigt <- co$Term[-1][co$p[-1] < al]
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p><b>Model fit:</b> R&sup2; = ", fmt(s$r.squared, 3), " (adjusted ", fmt(s$adj.r.squared, 3), ") &rarr; the predictors explain ", fmt(100 * s$r.squared, 1),
      "% of the variation in ", a$y, ". Overall model F(", fs[2], ", ", fs[3], ") = ", fmt(fs[1], 2), ", ", p_eq(pF), " (<b>", sig_word(pF, al), "</b>).</p><p><b>Coefficients:</b></p><ul>",
      paste0("<li><b>", co$Term[-1], "</b>: B = ", fmt(co$B[-1], 3), " ", ci_txt(co$`CI low`[-1], co$`CI high`[-1], 3), ", ", p_eq(co$p[-1]), " &rarr; for each 1-unit increase (or vs reference category), ", a$y, " changes by ", fmt(co$B[-1], 3), " units",
             if (length(a$xs) > 1) ", holding other variables constant" else "", ".</li>", collapse = ""), "</ul>",
      "<p>Regression equation: <b>", a$y, " = ", fmt(co$B[1], 3), paste0(" ", ifelse(co$B[-1] >= 0, "+", "&minus;"), " ", fmt(abs(co$B[-1]), 3), "&times;", co$Term[-1], collapse = ""), "</b></p>",
      if (!is.na(sh) && sh < 0.05) warn_html("Residuals are not normally distributed (Shapiro-Wilk ", p_eq(sh), "). With large n this matters less; check the residual plots.") else note_html("Residuals: Shapiro-Wilk ", p_eq(sh), " - normality of residuals acceptable."),
      if (!is.null(vf) && any(vf$VIF > 5)) warn_html("Multicollinearity detected (VIF &gt; 5) - predictors are highly correlated with each other.") else "",
      writeup_html(sprintf("In %s linear regression, %s. The model explained %s%% of the variance in %s (R&sup2; = %s, F(%d, %d) = %s, %s).", if (length(a$xs) > 1) "multiple" else "simple",
        if (length(sigt)) paste0(paste(sigt, collapse = ", "), " ", if (length(sigt) > 1) "were significant predictors" else "was a significant predictor", " of ", a$y) else paste0("no predictor was significantly associated with ", a$y),
        fmt(100 * s$r.squared, 1), a$y, fmt(s$r.squared, 2), fs[2], fs[3], fmt(fs[1], 2), p_eq(pF))))
    steps <- step_list(paste0("Model: ", mj("Y = \\beta_0 + \\beta_1X_1 + \\dots + \\beta_kX_k + \\varepsilon")),
      paste0("Least squares chooses &beta; to minimise ", mj("\\sum (y_i-\\hat y_i)^2"), "; in matrix form ", mj("\\hat\\beta = (X^TX)^{-1}X^Ty")),
      if (length(a$xs) == 1 && is.numeric(dd[[xv]])) paste0("Simple regression: ", mj("b_1 = \\frac{\\sum(x-\\bar x)(y-\\bar y)}{\\sum(x-\\bar x)^2} = ", fmt(co$B[2]), ",\\quad b_0 = \\bar y - b_1\\bar x = ", fmt(co$B[1]))) else "Categorical predictors are converted to dummy (0/1) variables; the first category is the reference.",
      paste0("Each coefficient: ", mj("t = \\frac{b}{SE(b)}"), " with df = n &minus; k &minus; 1 = ", fs[3]),
      paste0(mj("R^2 = 1 - \\frac{SS_{res}}{SS_{tot}} = ", fmt(s$r.squared), ",\\quad R^2_{adj} = 1-(1-R^2)\\frac{n-1}{n-k-1}")),
      "Assumptions (LINE): Linearity, Independence, Normal residuals, Equal variance (homoscedasticity) - check the 4 diagnostic plots.")
    tabs <- list(`Model summary` = mod, `Coefficients` = co); if (!is.null(vf)) tabs$`Multicollinearity (VIF)` = vf
    list(tables = tabs, html = html, steps = steps,
         plot = function() { if (length(a$xs) == 1 && is.numeric(dd[[xv]])) { op <- par(mfrow = c(1, 3)); on.exit(par(op)); scatter_fit(dd[[xv]], dd[[yv]], a$xs, a$y, "Regression line")
             plot(fit, which = 1:2, pch = 19, col = adjustcolor(PAL[1], 0.6)) } else { op <- par(mfrow = c(2, 2)); on.exit(par(op)); plot(fit, pch = 19, col = adjustcolor(PAL[1], 0.6)) } })
  }))

auc_mw <- function(score, y) { n1 <- sum(y == 1); n0 <- sum(y == 0); r <- rank(score); (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }

register_test(list(
  id = "log_reg", name = "Binary logistic regression (odds ratios)", cat = CATEGORIES[5], level = "PG",
  inputs = list(list(id = "y", label = "Outcome (binary, e.g. Died / Survived)", type = "cat"), list(id = "yl", label = "Event level (coded 1)", type = "level", of = "y"),
                list(id = "xs", label = "Predictor(s)", type = "any_multi")),
  run = function(d, a) {
    al <- alpha_of(a); need(length(a$xs) >= 1, "Select at least one predictor."); need(!(a$y %in% a$xs), "Outcome cannot also be a predictor.")
    cc <- use_cc(d, c(a$y, a$xs)); dd <- cc$d; need(length(unique(dd[[a$y]])) == 2, "Outcome must have exactly 2 categories.")
    yy <- as.integer(as.character(dd[[a$y]]) == a$yl); for (v in a$xs) if (!is.numeric(dd[[v]])) dd[[v]] <- as_fac(dd[[v]])
    names(dd) <- make.names(names(dd)); xv <- make.names(a$xs); dd$.y <- yy
    fit <- glm(reformulate(xv, ".y"), family = binomial, data = dd); s <- summary(fit); z <- qnorm(1 - al / 2)
    co <- data.frame(Term = rownames(s$coefficients), B = s$coefficients[, 1], SE = s$coefficients[, 2], Wald_z = s$coefficients[, 3], p = s$coefficients[, 4],
                     OR = exp(s$coefficients[, 1]), `OR CI low` = exp(s$coefficients[, 1] - z * s$coefficients[, 2]), `OR CI high` = exp(s$coefficients[, 1] + z * s$coefficients[, 2]), check.names = FALSE, row.names = NULL)
    n <- nrow(dd); lr <- fit$null.deviance - fit$deviance; dfm <- fit$df.null - fit$df.residual; plr <- pchisq(lr, dfm, lower.tail = FALSE)
    cs <- 1 - exp((fit$deviance - fit$null.deviance) / n); nag <- cs / (1 - exp(-fit$null.deviance / n))
    ph <- fitted(fit); grp <- cut(ph, unique(quantile(ph, seq(0, 1, .1))), include.lowest = TRUE)
    obs <- tapply(yy, grp, sum); ex <- tapply(ph, grp, sum); ng <- table(grp)
    hl <- sum((obs - ex)^2 / (ex * (1 - ex / ng))); hdf <- max(1, nlevels(grp) - 2); hp <- pchisq(hl, hdf, lower.tail = FALSE)
    pred <- as.integer(ph >= 0.5); ctab <- table(Observed = factor(yy, 0:1, c("No event", "Event")), Predicted = factor(pred, 0:1, c("No event", "Event")))
    auc <- auc_mw(ph, yy); epv <- min(sum(yy), n - sum(yy)) / length(coef(fit)[-1])
    mod <- data.frame(n = n, Events = sum(yy), `LR chi-square` = lr, df = dfm, p = plr, `Nagelkerke R2` = nag, `Cox-Snell R2` = cs, AIC = AIC(fit),
                      `Hosmer-Lemeshow chi2` = hl, `HL p` = hp, `Accuracy (cut-off 0.5)` = mean(pred == yy), AUC = auc, check.names = FALSE)
    names(mod)[names(mod) == "HL p"] <- "p_HosmerLemeshow"
    sig <- co$Term[-1][co$p[-1] < al]
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>Modelling the probability of <b>", a$y, " = ", a$yl, "</b> (", sum(yy), " events in ", n, ").</p>",
      "<p><b>Model:</b> likelihood-ratio &chi;&sup2;(", dfm, ") = ", fmt(lr, 2), ", ", p_eq(plr), " (", sig_word(plr, al), "); Nagelkerke R&sup2; = ", fmt(nag, 3), "; Hosmer-Lemeshow ", p_eq(hp),
      if (hp < 0.05) " (poor calibration)" else " (good fit)", "; AUC = ", fmt(auc, 3), ".</p><ul>",
      paste0("<li><b>", co$Term[-1], "</b>: OR = ", fmt(co$OR[-1], 2), " ", ci_txt(co$`OR CI low`[-1], co$`OR CI high`[-1]), ", ", p_eq(co$p[-1]), " &rarr; ",
             ifelse(co$OR[-1] > 1, paste0("odds of ", a$yl, " increase ", fmt(co$OR[-1], 2), "-fold"), paste0("odds of ", a$yl, " decrease by ", fmt(100 * (1 - co$OR[-1]), 0), "%")),
             " per unit increase (or vs reference category)", if (length(a$xs) > 1) ", adjusted for other predictors" else "", ".</li>", collapse = ""), "</ul>",
      if (epv < 10) warn_html("Events per variable = ", fmt(epv, 1), " (&lt; 10): too many predictors for the number of events - estimates may be unstable.") else "",
      writeup_html(sprintf("In %s logistic regression, %s (Nagelkerke R&sup2; = %s, AUC = %s).", if (length(a$xs) > 1) "multivariable" else "univariable",
        if (length(sig)) paste0(paste0(sig, " (adjusted OR ", fmt(co$OR[match(sig, co$Term)], 2), ", ", sapply(match(sig, co$Term), function(i) ci_txt(co$`OR CI low`[i], co$`OR CI high`[i])), ")", collapse = "; "), " ", if (length(sig) > 1) "were" else "was", " independently associated with ", a$yl)
        else paste0("no predictor was significantly associated with ", a$yl), fmt(nag, 2), fmt(auc, 2))))
    steps <- step_list(paste0("Model the log-odds: ", mj("\\ln\\left(\\frac{p}{1-p}\\right) = \\beta_0 + \\beta_1X_1 + \\dots + \\beta_kX_k")),
      "Coefficients are estimated by maximum likelihood (iteratively reweighted least squares).",
      paste0("Odds ratio for each predictor: ", mj("OR = e^{\\beta},\\quad 95\\%\\,CI = e^{\\beta \\pm 1.96\\,SE}")),
      paste0("Wald test: ", mj("z = \\beta / SE"), "; model test: ", mj("LR = D_{null} - D_{model} = ", fmt(fit$null.deviance, 2), " - ", fmt(fit$deviance, 2), " = ", fmt(lr, 2))),
      paste0("Predicted probability: ", mj("p = \\frac{1}{1+e^{-(\\beta_0+\\beta_1X_1+\\dots)}}")),
      "Hosmer-Lemeshow groups subjects by deciles of predicted risk and compares observed vs expected events (p &gt; 0.05 = good calibration).")
    list(tables = list(`Model summary` = mod, `Coefficients & odds ratios` = co, `Classification table (cut-off 0.5)` = data.frame(Observed = rownames(ctab), as.data.frame.matrix(ctab), check.names = FALSE)),
         html = html, steps = steps,
         plot = function() { op <- par(mfrow = c(1, 2), mar = c(5, 9, 3, 1)); on.exit(par(op)); k <- nrow(co) - 1
           plot(co$OR[-1], 1:k, log = "x", xlim = range(c(co$`OR CI low`[-1], co$`OR CI high`[-1], 1), finite = TRUE), pch = 15, cex = 1.6, col = PAL[1], yaxt = "n", ylab = "", xlab = "Odds ratio (log scale)", main = "Forest plot")
           axis(2, 1:k, co$Term[-1], las = 1, cex.axis = 0.8); segments(co$`OR CI low`[-1], 1:k, co$`OR CI high`[-1], 1:k, lwd = 2.5, col = PAL[1]); abline(v = 1, lty = 2)
           par(mar = c(5, 4, 3, 1)); o <- order(ph, decreasing = TRUE); tpr <- cumsum(yy[o]) / sum(yy); fpr <- cumsum(1 - yy[o]) / sum(1 - yy)
           plot(c(0, fpr), c(0, tpr), type = "l", lwd = 3, col = PAL[2], xlab = "1 - Specificity", ylab = "Sensitivity", main = paste("ROC, AUC =", fmt(auc, 3))); abline(0, 1, lty = 2) })
  }))
