# =====================================================================
#  R CODE EXPORT
#  For every analysis, write plain R code (base R + survival only) that a
#  student can run in R / RStudio to reproduce the main results and cite the
#  functions used in the Methods section.
# =====================================================================

Q <- function(s) encodeString(as.character(s), quote = '"')
QV <- function(x) paste0("c(", paste(Q(x), collapse = ", "), ")")
col_ <- function(v) paste0("dd[[", Q(v), "]]")
cc_lines <- function(vars) c(
  paste0("dd <- d[complete.cases(d[, ", QV(vars), ", drop = FALSE]), ]   # complete-case analysis"),
  "cat(\"n used =\", nrow(dd), \"\\n\")")
has_g <- function(g) !is.null(g) && length(g) && !identical(g, "(none)") && nzchar(g)
num_ <- function(x, def = 0) { v <- suppressWarnings(as.numeric(x)); if (length(v) == 0 || is.na(v)) def else v }

RCODE <- list()

# ---------------- 1. Descriptive ----------------
RCODE$desc <- function(a) {
  out <- c("describe <- function(x) { x <- x[!is.na(x)]; n <- length(x); se <- sd(x) / sqrt(n)",
           "  c(n = n, mean = mean(x), SD = sd(x), SE = se, CI95_low = mean(x) - qt(0.975, n - 1) * se, CI95_high = mean(x) + qt(0.975, n - 1) * se,",
           "    median = median(x), Q1 = unname(quantile(x, 0.25)), Q3 = unname(quantile(x, 0.75)), IQR = IQR(x), min = min(x), max = max(x),",
           "    skewness = (n / ((n - 1) * (n - 2))) * sum(((x - mean(x)) / sd(x))^3),",
           "    excess_kurtosis = (n * (n + 1) / ((n - 1) * (n - 2) * (n - 3))) * sum(((x - mean(x)) / sd(x))^4) - 3 * (n - 1)^2 / ((n - 2) * (n - 3))) }",
           "n_missing <- function(x) sum(is.na(x))")
  for (v in a$vars) {
    if (has_g(a$g)) out <- c(out, paste0("print(round(sapply(split(d[[", Q(v), "]], d[[", Q(a$g), "]]), describe), 3))   # ", v, " by ", a$g))
    else out <- c(out, paste0("print(round(describe(d[[", Q(v), "]]), 3))   # ", v))
    out <- c(out, paste0("n_missing(d[[", Q(v), "]]); shapiro.test(d[[", Q(v), "]])   # missing values; normality check"))
  }
  out
}

RCODE$freq <- function(a) {
  out <- c(paste0("x <- d[[", Q(a$x), "]]; x <- x[!is.na(x)]"),
           "tb <- table(x); n <- length(x)",
           "ci <- t(sapply(as.numeric(tb), function(k) binom.test(k, n)$conf.int * 100))   # exact (Clopper-Pearson) 95% CI",
           "print(data.frame(Category = names(tb), Frequency = as.integer(tb), Percent = round(100 * as.numeric(tb) / n, 1),",
           "                 CI_low = round(ci[, 1], 1), CI_high = round(ci[, 2], 1), Cumulative = round(cumsum(100 * as.numeric(tb) / n), 1)))")
  if (has_g(a$g)) out <- c(out, paste0("ct <- table(d[[", Q(a$x), "]], d[[", Q(a$g), "]]); print(ct); print(round(100 * prop.table(ct, 2), 1))   # column %"))
  out
}

RCODE$normality <- function(a) {
  if (has_g(a$g)) c(paste0("for (s in split(d[[", Q(a$y), "]], d[[", Q(a$g), "]])) print(shapiro.test(s[!is.na(s)]))"),
                    paste0("qqnorm(d[[", Q(a$y), "]]); qqline(d[[", Q(a$y), "]])"))
  else c(paste0("x <- d[[", Q(a$y), "]]; x <- x[!is.na(x)]"), "shapiro.test(x)", "qqnorm(x); qqline(x)")
}

# ---------------- 2. Parametric ----------------
RCODE$t_one <- function(a) c(cc_lines(a$y), paste0("x <- ", col_(a$y)),
  paste0("t.test(x, mu = ", num_(a$mu), ", conf.level = 1 - alpha)"),
  paste0("cohens_d <- (mean(x) - ", num_(a$mu), ") / sd(x); cohens_d"), "shapiro.test(x)   # normality check")

RCODE$t_ind <- function(a) c(cc_lines(c(a$y, a$g)), paste0("y <- ", col_(a$y), "; g <- droplevels(factor(", col_(a$g), "))"),
  "tapply(y, g, mean); tapply(y, g, sd)",
  "# Levene's test (Brown-Forsythe, median-centred) for equal variances",
  "z <- abs(y - ave(y, g, FUN = median)); anova(lm(z ~ g))",
  "t.test(y ~ g, var.equal = TRUE,  conf.level = 1 - alpha)   # Student t-test",
  "t.test(y ~ g, var.equal = FALSE, conf.level = 1 - alpha)   # Welch t-test (use if variances unequal)",
  "n <- table(g); s <- tapply(y, g, sd); m <- tapply(y, g, mean)",
  "sp <- sqrt(((n[1] - 1) * s[1]^2 + (n[2] - 1) * s[2]^2) / (sum(n) - 2)); cohens_d <- unname((m[1] - m[2]) / sp); cohens_d",
  "tapply(y, g, shapiro.test)   # normality in each group")

RCODE$t_paired <- function(a) c(cc_lines(c(a$y1, a$y2)), paste0("x1 <- ", col_(a$y1), "; x2 <- ", col_(a$y2)),
  "t.test(x2, x1, paired = TRUE, conf.level = 1 - alpha)   # mean of (x2 - x1)",
  "dif <- x2 - x1; cohens_dz <- mean(dif) / sd(dif); cohens_dz", "shapiro.test(dif)   # normality of the differences")

RCODE$anova1 <- function(a) c(cc_lines(c(a$y, a$g)), paste0("y <- ", col_(a$y), "; g <- droplevels(factor(", col_(a$g), "))"),
  "fit <- aov(y ~ g); summary(fit)", "ss <- summary(fit)[[1]][[\"Sum Sq\"]]; eta_squared <- ss[1] / sum(ss); eta_squared",
  "z <- abs(y - ave(y, g, FUN = median)); anova(lm(z ~ g))   # Levene (Brown-Forsythe)",
  "oneway.test(y ~ g, var.equal = FALSE)   # Welch's ANOVA (if variances unequal)",
  "TukeyHSD(fit, conf.level = 1 - alpha)   # post-hoc",
  "pairwise.t.test(y, g, pool.sd = FALSE, p.adjust.method = \"holm\")   # post-hoc if variances unequal",
  "tapply(y, g, shapiro.test)")

RCODE$anova2 <- function(a) c(cc_lines(c(a$y, a$f1, a$f2)),
  paste0("dd2 <- data.frame(y = ", col_(a$y), ", A = droplevels(factor(", col_(a$f1), ")), B = droplevels(factor(", col_(a$f2), ")))"),
  "options(contrasts = c(\"contr.sum\", \"contr.poly\"))   # needed for Type III sums of squares",
  paste0("fit <- lm(", if (identical(a$inter, "No")) "y ~ A + B" else "y ~ A * B", ", data = dd2)"),
  paste0("drop1(fit, . ~ ., test = \"F\")   # Type III ANOVA table: A = ", a$f1, ", B = ", a$f2),
  "shapiro.test(residuals(fit))", "options(contrasts = c(\"contr.treatment\", \"contr.poly\"))   # back to R default")

RCODE$rm_anova <- function(a) c(cc_lines(a$vars), paste0("Y <- as.matrix(dd[, ", QV(a$vars), "]); n <- nrow(Y); k <- ncol(Y)"),
  "long <- data.frame(id = factor(rep(1:n, k)), time = factor(rep(colnames(Y), each = n), levels = colnames(Y)), y = as.vector(Y))",
  "fit <- aov(y ~ time + Error(id / time), data = long); summary(fit)",
  "if (k > 2 && n > k) mauchly.test(lm(Y ~ 1), X = ~1)   # sphericity",
  "M <- contr.poly(k); ev <- eigen(t(M) %*% cov(Y) %*% M, only.values = TRUE)$values",
  "gg_epsilon <- sum(ev)^2 / ((k - 1) * sum(ev^2)); gg_epsilon   # Greenhouse-Geisser epsilon",
  "s <- summary(fit)[[\"Error: id:time\"]][[1]]",
  "p_GG <- pf(s$`F value`[1], s$Df[1] * gg_epsilon, s$Df[2] * gg_epsilon, lower.tail = FALSE); p_GG   # GG-corrected p",
  "pw <- combn(k, 2); p <- apply(pw, 2, function(z) t.test(Y[, z[1]], Y[, z[2]], paired = TRUE)$p.value)",
  "data.frame(comparison = paste(colnames(Y)[pw[1, ]], \"vs\", colnames(Y)[pw[2, ]]), p_bonferroni = p.adjust(p, \"bonferroni\"))")

RCODE$ancova <- function(a) c(cc_lines(c(a$y, a$g, a$cv)),
  paste0("dd2 <- data.frame(y = ", col_(a$y), ", g = droplevels(factor(", col_(a$g), ")), x = ", col_(a$cv), ")"),
  "options(contrasts = c(\"contr.sum\", \"contr.poly\"))",
  "anova(lm(y ~ x * g, data = dd2))   # homogeneity of slopes: look at the x:g row",
  "fit <- lm(y ~ x + g, data = dd2); drop1(fit, . ~ ., test = \"F\")   # ANCOVA (Type III)",
  "predict(fit, data.frame(x = mean(dd2$x), g = levels(dd2$g)), se.fit = TRUE)   # adjusted means",
  "options(contrasts = c(\"contr.treatment\", \"contr.poly\"))")

# ---------------- 3. Non-parametric ----------------
RCODE$wilcox_one <- function(a) c(cc_lines(a$y), paste0("x <- ", col_(a$y)),
  paste0("wilcox.test(x, mu = ", num_(a$mu), ", conf.int = TRUE, conf.level = 1 - alpha,"),
  paste0("            exact = length(x) < 50 && !any(duplicated(x - ", num_(a$mu), ")))"), "median(x)")

RCODE$mann_whitney <- function(a) c(cc_lines(c(a$y, a$g)), paste0("y <- ", col_(a$y), "; g <- droplevels(factor(", col_(a$g), "))"),
  "tapply(y, g, median); tapply(y, g, quantile, c(0.25, 0.75))",
  "y1 <- y[g == levels(g)[1]]; y2 <- y[g == levels(g)[2]]",
  "w <- wilcox.test(y1, y2, conf.int = TRUE, conf.level = 1 - alpha, exact = length(y) < 50 && !any(duplicated(y))); w",
  "U <- min(w$statistic, length(y1) * length(y2) - w$statistic); U   # Mann-Whitney U",
  "z <- qnorm(w$p.value / 2, lower.tail = FALSE); effect_size_r <- z / sqrt(length(y)); effect_size_r")

RCODE$wilcox_paired <- function(a) c(cc_lines(c(a$y1, a$y2)), paste0("x1 <- ", col_(a$y1), "; x2 <- ", col_(a$y2), "; dif <- x2 - x1"),
  "median(x1); median(x2); median(dif)",
  "w <- wilcox.test(x2, x1, paired = TRUE, conf.int = TRUE, conf.level = 1 - alpha,",
  "                 exact = length(dif) < 50 && !any(duplicated(abs(dif[dif != 0])))); w",
  "z <- qnorm(w$p.value / 2, lower.tail = FALSE); effect_size_r <- z / sqrt(sum(dif != 0)); effect_size_r")

RCODE$kruskal <- function(a) c(cc_lines(c(a$y, a$g)), paste0("y <- ", col_(a$y), "; g <- droplevels(factor(", col_(a$g), "))"),
  "tapply(y, g, median)", "kw <- kruskal.test(y ~ g); kw",
  "epsilon_squared <- unname(kw$statistic) / (length(y) - 1); epsilon_squared",
  "# Dunn's post-hoc test (Holm adjusted)",
  "r <- rank(y); n <- length(y); ti <- table(r); tie <- sum(ti^3 - ti) / (12 * (n - 1)); mr <- tapply(r, g, mean); ni <- table(g)",
  "pr <- combn(levels(g), 2)",
  "zz <- apply(pr, 2, function(p) (mr[p[1]] - mr[p[2]]) / sqrt((n * (n + 1) / 12 - tie) * (1 / ni[p[1]] + 1 / ni[p[2]])))",
  "data.frame(comparison = apply(pr, 2, paste, collapse = \" vs \"), z = zz, p_unadjusted = 2 * pnorm(-abs(zz)), p_holm = p.adjust(2 * pnorm(-abs(zz)), \"holm\"))")

RCODE$friedman <- function(a) c(cc_lines(a$vars), paste0("Y <- as.matrix(dd[, ", QV(a$vars), "])"),
  "fr <- friedman.test(Y); fr", "kendalls_W <- unname(fr$statistic) / (nrow(Y) * (ncol(Y) - 1)); kendalls_W",
  "pw <- combn(ncol(Y), 2)",
  "p <- apply(pw, 2, function(z) wilcox.test(Y[, z[1]], Y[, z[2]], paired = TRUE, exact = FALSE)$p.value)",
  "data.frame(comparison = paste(colnames(Y)[pw[1, ]], \"vs\", colnames(Y)[pw[2, ]]), p_bonferroni = p.adjust(p, \"bonferroni\"))")

# ---------------- 4. Categorical ----------------
RCODE$chisq_ind <- function(a) c(cc_lines(c(a$x, a$y)), paste0("tb <- table(", col_(a$x), ", ", col_(a$y), "); tb"),
  "round(100 * prop.table(tb, 1), 1)   # row %",
  "cs <- chisq.test(tb, correct = FALSE); cs", "cs$expected   # check: expected counts should be >= 5",
  "if (all(dim(tb) == 2)) chisq.test(tb, correct = TRUE)   # Yates continuity correction (2x2)",
  "if (mean(cs$expected < 5) > 0.2) fisher.test(tb, simulate.p.value = any(dim(tb) > 2) && sum(tb) > 200, B = 1e5)",
  "cramers_V <- sqrt(unname(cs$statistic) / (sum(tb) * (min(dim(tb)) - 1))); cramers_V")

RCODE$chisq_gof <- function(a) {
  pr <- trimws(a$props %||% "")
  c(cc_lines(a$x), paste0("tb <- table(", col_(a$x), "); tb"),
    if (nzchar(pr)) paste0("p0 <- c(", paste(strsplit(pr, "[,; ]+")[[1]], collapse = ", "), "); p0 <- p0 / sum(p0)") else "p0 <- rep(1 / length(tb), length(tb))   # equal proportions",
    "chisq.test(tb, p = p0)")
}

RCODE$fisher <- function(a) c(cc_lines(c(a$x, a$y)), paste0("tb <- table(", col_(a$x), ", ", col_(a$y), "); tb"),
  "fisher.test(tb, simulate.p.value = any(dim(tb) > 2) && sum(tb) > 300, B = 1e5, conf.level = 1 - alpha)")

RCODE$mcnemar <- function(a) c(cc_lines(c(a$x, a$y)),
  paste0("lv <- sort(unique(c(as.character(", col_(a$x), "), as.character(", col_(a$y), "))))"),
  paste0("tb <- table(factor(", col_(a$x), ", lv), factor(", col_(a$y), ", lv)); tb"),
  "mcnemar.test(tb, correct = TRUE)", "binom.test(tb[1, 2], tb[1, 2] + tb[2, 1], 0.5)   # exact McNemar (use if b + c < 25)")

RCODE$or_rr <- function(a) c(cc_lines(c(a$x, a$y)),
  paste0("exposed <- as.character(", col_(a$x), ") == ", Q(a$xl), "; event <- as.character(", col_(a$y), ") == ", Q(a$yl)),
  "A <- sum(exposed & event); B <- sum(exposed & !event); C <- sum(!exposed & event); D <- sum(!exposed & !event)",
  "matrix(c(A, C, B, D), 2, dimnames = list(c(\"Exposed\", \"Not exposed\"), c(\"Event\", \"No event\")))",
  "h <- if (any(c(A, B, C, D) == 0)) 0.5 else 0   # Haldane correction for zero cells",
  "z <- qnorm(1 - alpha / 2)",
  "OR <- ((A + h) * (D + h)) / ((B + h) * (C + h)); se <- sqrt(sum(1 / (c(A, B, C, D) + h)))",
  "c(OR = OR, low = exp(log(OR) - z * se), high = exp(log(OR) + z * se))",
  "RR <- ((A + h) / (A + B + 2 * h)) / ((C + h) / (C + D + 2 * h)); se <- sqrt(1 / (A + h) - 1 / (A + B + 2 * h) + 1 / (C + h) - 1 / (C + D + 2 * h))",
  "c(RR = RR, low = exp(log(RR) - z * se), high = exp(log(RR) + z * se))",
  "r1 <- A / (A + B); r0 <- C / (C + D); RD <- r1 - r0; NNT <- 1 / abs(RD); c(risk_difference = RD, NNT = NNT)",
  "chisq.test(matrix(c(A, C, B, D), 2)); fisher.test(matrix(c(A, C, B, D), 2))")

RCODE$prop_one <- function(a) c(cc_lines(a$x), paste0("k <- sum(as.character(", col_(a$x), ") == ", Q(a$xl), "); n <- nrow(dd)"),
  paste0("binom.test(k, n, p = ", num_(a$p0, 0.5), ", conf.level = 1 - alpha)   # exact binomial test"),
  paste0("p0 <- ", num_(a$p0, 0.5), "; z <- (k / n - p0) / sqrt(p0 * (1 - p0) / n); c(z = z, p_z_approx = 2 * pnorm(-abs(z)))"))

RCODE$prop_two <- function(a) c(cc_lines(c(a$g, a$y)), paste0("g <- droplevels(factor(", col_(a$g), ")); ev <- as.character(", col_(a$y), ") == ", Q(a$yl)),
  "x <- tapply(ev, g, sum); n <- as.integer(table(g)); x; n",
  "prop.test(x, n, correct = FALSE, conf.level = 1 - alpha)   # two-proportion z-test (z^2 = X-squared)")

# ---------------- 5. Correlation & regression ----------------
RCODE$pearson <- function(a) c(cc_lines(c(a$x, a$y)), paste0("cor.test(", col_(a$x), ", ", col_(a$y), ", method = \"pearson\", conf.level = 1 - alpha)"))

RCODE$spearman <- function(a) c(cc_lines(c(a$x, a$y)),
  paste0("cor.test(", col_(a$x), ", ", col_(a$y), ", method = \"spearman\", exact = FALSE)"),
  paste0("cor.test(", col_(a$x), ", ", col_(a$y), ", method = \"kendall\", exact = FALSE)"))

RCODE$corr_matrix <- function(a) c(cc_lines(a$vars), paste0("X <- as.matrix(dd[, ", QV(a$vars), "])"),
  paste0("round(cor(X, method = ", Q(a$method %||% "pearson"), "), 3)"),
  paste0("P <- outer(1:ncol(X), 1:ncol(X), Vectorize(function(i, j) if (i == j) NA else cor.test(X[, i], X[, j], method = ", Q(a$method %||% "pearson"), ", exact = FALSE)$p.value))"),
  "dimnames(P) <- list(colnames(X), colnames(X)); round(P, 4)")

reg_prep <- function(a, extra = NULL) c(cc_lines(c(extra, a$y, a$xs)),
  vapply(a$xs, function(v) paste0("if (!is.numeric(dd[[", Q(v), "]])) dd[[", Q(v), "]] <- droplevels(factor(dd[[", Q(v), "]]))"), ""),
  "names(dd) <- make.names(names(dd))")

RCODE$lin_reg <- function(a) c(reg_prep(a),
  paste0("fit <- lm(", make.names(a$y), " ~ ", paste(make.names(a$xs), collapse = " + "), ", data = dd)"),
  "summary(fit)", "confint(fit, level = 1 - alpha)", "shapiro.test(residuals(fit))   # normality of residuals",
  "par(mfrow = c(2, 2)); plot(fit); par(mfrow = c(1, 1))   # diagnostic plots")

RCODE$log_reg <- function(a) c(reg_prep(a),
  paste0("dd$event <- as.integer(as.character(dd$", make.names(a$y), ") == ", Q(a$yl), ")   # 1 = ", a$yl),
  paste0("fit <- glm(event ~ ", paste(make.names(a$xs), collapse = " + "), ", family = binomial, data = dd)"),
  "summary(fit)", "z <- qnorm(1 - alpha / 2); b <- coef(summary(fit))",
  "round(cbind(OR = exp(b[, 1]), low = exp(b[, 1] - z * b[, 2]), high = exp(b[, 1] + z * b[, 2]), p = b[, 4]), 4)",
  "lr <- fit$null.deviance - fit$deviance; pchisq(lr, fit$df.null - fit$df.residual, lower.tail = FALSE)   # likelihood-ratio test of the model",
  "n <- nrow(dd); cox_snell <- 1 - exp((fit$deviance - fit$null.deviance) / n); nagelkerke <- cox_snell / (1 - exp(-fit$null.deviance / n)); nagelkerke",
  "ph <- fitted(fit); grp <- cut(ph, unique(quantile(ph, seq(0, 1, 0.1))), include.lowest = TRUE)   # Hosmer-Lemeshow (deciles)",
  "o <- tapply(dd$event, grp, sum); e <- tapply(ph, grp, sum); ng <- table(grp); HL <- sum((o - e)^2 / (e * (1 - e / ng)))",
  "c(HL = HL, p = pchisq(HL, max(1, nlevels(grp) - 2), lower.tail = FALSE))",
  "r <- rank(ph); n1 <- sum(dd$event); AUC <- (sum(r[dd$event == 1]) - n1 * (n1 + 1) / 2) / (n1 * (n - n1)); AUC")

# ---------------- 6. Agreement ----------------
RCODE$kappa <- function(a) {
  w <- switch(substr(a$wt %||% "Unw", 1, 3), Lin = "1 - abs(outer(1:k, 1:k, \"-\")) / (k - 1)   # linear weights",
              Qua = "1 - (outer(1:k, 1:k, \"-\") / (k - 1))^2   # quadratic weights", "diag(k)   # unweighted")
  c(cc_lines(c(a$r1, a$r2)), paste0("lv <- sort(unique(c(as.character(", col_(a$r1), "), as.character(", col_(a$r2), "))))"),
    paste0("tb <- table(factor(", col_(a$r1), ", lv), factor(", col_(a$r2), ", lv)); tb"),
    "N <- sum(tb); k <- length(lv); P <- tb / N", paste0("W <- ", w),
    "pr <- rowSums(P); pc <- colSums(P); E <- outer(pr, pc)",
    "po <- sum(W * P); pe <- sum(W * E); kappa <- (po - pe) / (1 - pe); c(Po = po, Pe = pe, kappa = kappa)",
    "wi <- W %*% pc; wj <- t(W) %*% pr   # Fleiss-Cohen-Everitt standard errors",
    "se <- sqrt(sum(P * (W - outer(as.vector(wi), as.vector(wj), \"+\") * (1 - kappa))^2) - (kappa - pe * (1 - kappa))^2) / ((1 - pe) * sqrt(N))",
    "se0 <- sqrt(sum(E * (W - outer(as.vector(wi), as.vector(wj), \"+\"))^2) - pe^2) / ((1 - pe) * sqrt(N))",
    "z <- qnorm(1 - alpha / 2); c(CI_low = kappa - z * se, CI_high = kappa + z * se, p = 2 * pnorm(-abs(kappa / se0)))")
}

RCODE$bland_altman <- function(a) c(cc_lines(c(a$m1, a$m2)), paste0("x1 <- ", col_(a$m1), "; x2 <- ", col_(a$m2)),
  "dif <- x2 - x1; avg <- (x1 + x2) / 2; bias <- mean(dif); s <- sd(dif)",
  "c(bias = bias, LoA_low = bias - 1.96 * s, LoA_high = bias + 1.96 * s)",
  "n <- length(dif); tq <- qt(0.975, n - 1); se_b <- s / sqrt(n); se_l <- sqrt(3 * s^2 / n)",
  "rbind(bias = bias + c(0, -1, 1) * tq * se_b, LoA_low = bias - 1.96 * s + c(0, -1, 1) * tq * se_l, LoA_high = bias + 1.96 * s + c(0, -1, 1) * tq * se_l)   # value, 95% CI",
  "t.test(dif)   # is the bias different from 0?", "cor.test(avg, dif)   # proportional bias",
  "plot(avg, dif, xlab = \"Mean of two methods\", ylab = \"Difference\"); abline(h = c(bias, bias - 1.96 * s, bias + 1.96 * s), lty = c(1, 2, 2))")

RCODE$icc <- function(a) c(cc_lines(a$vars), paste0("Y <- as.matrix(dd[, ", QV(a$vars), "]); n <- nrow(Y); k <- ncol(Y)"),
  "gm <- mean(Y); ssr <- k * sum((rowMeans(Y) - gm)^2); ssc <- n * sum((colMeans(Y) - gm)^2); sse <- sum((Y - gm)^2) - ssr - ssc",
  "msr <- ssr / (n - 1); msc <- ssc / (k - 1); mse <- sse / ((n - 1) * (k - 1)); msw <- (ssc + sse) / (n * (k - 1))",
  "c(ICC1_1 = (msr - msw) / (msr + (k - 1) * msw),",
  "  ICC2_1 = (msr - mse) / (msr + (k - 1) * mse + k * (msc - mse) / n),   # two-way random, absolute agreement",
  "  ICC3_1 = (msr - mse) / (msr + (k - 1) * mse),",
  "  ICC2_k = (msr - mse) / (msr + (msc - mse) / n), ICC3_k = (msr - mse) / msr)",
  "F <- msr / mse; df1 <- n - 1; df2 <- (n - 1) * (k - 1); c(F = F, p = pf(F, df1, df2, lower.tail = FALSE))   # test of ICC = 0",
  "# 95% confidence intervals (Shrout & Fleiss; McGraw & Wong)",
  "FL <- F / qf(0.975, df1, df2); FU <- F * qf(0.975, df2, df1)",
  "c(ICC3_1_low = (FL - 1) / (FL + k - 1), ICC3_1_high = (FU - 1) / (FU + k - 1), ICC3_k_low = 1 - 1 / FL, ICC3_k_high = 1 - 1 / FU)",
  "F1 <- msr / msw; FL1 <- F1 / qf(0.975, df1, n * (k - 1)); FU1 <- F1 * qf(0.975, n * (k - 1), df1)",
  "c(ICC1_1_low = (FL1 - 1) / (FL1 + k - 1), ICC1_1_high = (FU1 - 1) / (FU1 + k - 1))",
  "i2 <- (msr - mse) / (msr + (k - 1) * mse + k * (msc - mse) / n); aa <- k * i2 / (n * (1 - i2)); bb <- 1 + k * i2 * (n - 1) / (n * (1 - i2))",
  "v <- (aa * msc + bb * mse)^2 / ((aa * msc)^2 / (k - 1) + (bb * mse)^2 / ((n - 1) * (k - 1))); F2L <- qf(0.975, n - 1, v); F2U <- qf(0.975, v, n - 1)",
  "l2 <- n * (msr - F2L * mse) / (F2L * (k * msc + (k * n - k - n) * mse) + n * msr); u2 <- n * (F2U * msr - mse) / (k * msc + (k * n - k - n) * mse + n * F2U * msr)",
  "c(ICC2_1_low = l2, ICC2_1_high = u2, ICC2_k_low = l2 * k / (1 + l2 * (k - 1)), ICC2_k_high = u2 * k / (1 + u2 * (k - 1)))")

RCODE$diag <- function(a) c(cc_lines(c(a$t, a$g)),
  paste0("pos <- as.character(", col_(a$t), ") == ", Q(a$tl), "; dis <- as.character(", col_(a$g), ") == ", Q(a$gl)),
  "TP <- sum(pos & dis); FP <- sum(pos & !dis); FN <- sum(!pos & dis); TN <- sum(!pos & !dis); c(TP = TP, FP = FP, FN = FN, TN = TN)",
  "binom.test(TP, TP + FN)   # sensitivity with exact 95% CI", "binom.test(TN, TN + FP)   # specificity",
  "binom.test(TP, TP + FP)   # PPV", "binom.test(TN, TN + FN)   # NPV",
  "binom.test(TP + TN, TP + FP + FN + TN)   # accuracy", "binom.test(TP + FN, TP + FP + FN + TN)   # prevalence",
  "sens <- TP / (TP + FN); spec <- TN / (TN + FP); lrp <- sens / (1 - spec); lrn <- (1 - sens) / spec",
  "se_p <- sqrt(1 / TP - 1 / (TP + FN) + 1 / FP - 1 / (FP + TN)); se_n <- sqrt(1 / FN - 1 / (TP + FN) + 1 / TN - 1 / (FP + TN))",
  "rbind(LR_pos = exp(log(lrp) + c(0, -1.96, 1.96) * se_p), LR_neg = exp(log(lrn) + c(0, -1.96, 1.96) * se_n))   # value, 95% CI",
  "c(Youden = sens + spec - 1, diagnostic_OR = lrp / lrn)")

RCODE$roc <- function(a) c(cc_lines(c(a$x, a$y)), paste0("x <- ", col_(a$x), "; y <- as.integer(as.character(", col_(a$y), ") == ", Q(a$yl), ")"),
  "n1 <- sum(y); n0 <- sum(1 - y); r <- rank(x)",
  "auc <- (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)   # AUC = Mann-Whitney U / (n1 * n0)",
  "if (auc < 0.5) { auc <- 1 - auc; x <- -x }   # lower values = disease",
  "q1 <- auc / (2 - auc); q2 <- 2 * auc^2 / (1 + auc)",
  "se <- sqrt((auc * (1 - auc) + (n1 - 1) * (q1 - auc^2) + (n0 - 1) * (q2 - auc^2)) / (n1 * n0))   # Hanley-McNeil",
  "c(AUC = auc, CI_low = max(0, auc - 1.96 * se), CI_high = min(1, auc + 1.96 * se), p = 2 * pnorm(-abs((auc - 0.5) / se)))",
  "cuts <- sort(unique(x)); sens <- sapply(cuts, function(c) mean(x[y == 1] >= c)); spec <- sapply(cuts, function(c) mean(x[y == 0] < c))",
  "j <- which.max(sens + spec - 1); c(best_cutoff = abs(cuts[j]), sensitivity = sens[j], specificity = spec[j])   # Youden",
  "plot(1 - spec, sens, type = \"s\", xlab = \"1 - Specificity\", ylab = \"Sensitivity\"); abline(0, 1, lty = 2)")

RCODE$cronbach <- function(a) c(cc_lines(a$vars), paste0("X <- as.matrix(dd[, ", QV(a$vars), "])"),
  "cronbach <- function(M) { k <- ncol(M); (k / (k - 1)) * (1 - sum(apply(M, 2, var)) / var(rowSums(M))) }",
  "cronbach(X)", "R <- cor(X); rb <- mean(R[upper.tri(R)]); ncol(X) * rb / (1 + (ncol(X) - 1) * rb)   # standardised alpha", "colMeans(X); apply(X, 2, sd)   # item mean and SD", "sapply(1:ncol(X), function(j) cronbach(X[, -j, drop = FALSE]))   # alpha if item deleted",
  "sapply(1:ncol(X), function(j) cor(X[, j], rowSums(X[, -j, drop = FALSE])))   # corrected item-total r")

# ---------------- 7. Survival ----------------
ev_line <- function(a) paste0("ev <- if (is.numeric(", col_(a$ev), ") && ", Q(a$evl %||% "1"), " %in% c(\"1\", \"\")) as.integer(", col_(a$ev),
                              ") else as.integer(as.character(", col_(a$ev), ") == ", Q(a$evl %||% "1"), ")   # 1 = event")
RCODE$km <- function(a) {
  g <- has_g(a$g)
  c("library(survival)", cc_lines(c(a$time, a$ev, if (g) a$g)), paste0("tt <- ", col_(a$time)), ev_line(a),
    if (g) c(paste0("g <- droplevels(factor(", col_(a$g), "))"), "fit <- survfit(Surv(tt, ev) ~ g); fit   # median survival",
             "survdiff(Surv(tt, ev) ~ g)   # log-rank test") else "fit <- survfit(Surv(tt, ev) ~ 1); fit",
    "summary(fit, times = unique(round(quantile(tt, c(0.25, 0.5, 0.75)), 1)), extend = TRUE)",
    "plot(fit, col = 1:4, lwd = 2, xlab = \"Time\", ylab = \"Survival probability\")")
}

RCODE$cox <- function(a) c("library(survival)", reg_prep(list(y = a$ev, xs = a$xs), a$time),
  paste0("dd$time <- dd$", make.names(a$time)),
  ev_line(list(ev = make.names(a$ev), evl = a$evl)), "dd$event <- ev",
  paste0("fit <- coxph(Surv(time, event) ~ ", paste(make.names(a$xs), collapse = " + "), ", data = dd)"),
  "summary(fit, conf.int = 1 - alpha)   # hazard ratios = exp(coef)", "cox.zph(fit)   # proportional-hazards assumption")

# ---------------- 8. Sample size ----------------
RCODE$ss_means <- function(a) { pr <- identical(a$design, "Paired (before-after)")
  c(paste0("za <- qnorm(1 - ", num_(a$a), " / 2); zb <- qnorm(", num_(a$pw), ")"),
    paste0("n_formula <- ceiling(", if (pr) "" else "2 * ", "(za + zb)^2 * ", num_(a$sd), "^2 / ", num_(a$delta), "^2); c(n = n_formula, with_dropout = ceiling(n_formula / (1 - ", num_(a$drop), " / 100)))   # normal approximation"),
    paste0("power.t.test(delta = ", num_(a$delta), ", sd = ", num_(a$sd), ", sig.level = ", num_(a$a), ", power = ", num_(a$pw),
           ", type = ", Q(if (pr) "paired" else "two.sample"), ")   # exact (t distribution): n ", if (pr) "pairs" else "per group"),
    paste0("n_exact <- ceiling(power.t.test(delta = ", num_(a$delta), ", sd = ", num_(a$sd), ", sig.level = ", num_(a$a), ", power = ", num_(a$pw),
           ", type = ", Q(if (pr) "paired" else "two.sample"), ")$n); n_exact"),
    paste0("ceiling(ceiling(power.t.test(delta = ", num_(a$delta), ", sd = ", num_(a$sd), ", sig.level = ", num_(a$a), ", power = ", num_(a$pw),
           ", type = ", Q(if (pr) "paired" else "two.sample"), ")$n) / (1 - ", num_(a$drop), " / 100))   # with dropout")) }
RCODE$ss_props <- function(a) c(paste0("p1 <- ", num_(a$p1), "; p2 <- ", num_(a$p2), "; za <- qnorm(1 - ", num_(a$a), " / 2); zb <- qnorm(", num_(a$pw), "); pb <- (p1 + p2) / 2"),
  paste0("n_formula <- ceiling((za * sqrt(2 * pb * (1 - pb)) + zb * sqrt(p1 * (1 - p1) + p2 * (1 - p2)))^2 / (p1 - p2)^2); c(n = n_formula, with_dropout = ceiling(n_formula / (1 - ", num_(a$drop), " / 100)))   # formula"),
  paste0("pp <- power.prop.test(p1 = ", num_(a$p1), ", p2 = ", num_(a$p2), ", sig.level = ", num_(a$a), ", power = ", num_(a$pw), "); pp   # n per group"),
  "ceiling(pp$n)   # n per group (rounded up)",
  paste0("ceiling(ceiling(pp$n) / (1 - ", num_(a$drop), " / 100))   # with dropout"))
RCODE$ss_prev <- function(a) c(paste0("p <- ", num_(a$p), "; d <- ", num_(a$dd), "; z <- qnorm(1 - ", num_(a$a), " / 2); N <- ", num_(a$N), "; deff <- ", num_(a$deff)),
  "n0 <- z^2 * p * (1 - p) / d^2; n1 <- if (N > 0) n0 / (1 + (n0 - 1) / N) else n0   # finite population correction",
  paste0("n <- ceiling(n1 * deff); c(n = n, with_non_response = ceiling(n / (1 - ", num_(a$drop), " / 100)))"))
RCODE$ss_corr <- function(a) c(paste0("r <- ", num_(a$r), "; za <- qnorm(1 - ", num_(a$a), " / 2); zb <- qnorm(", num_(a$pw), ")"),
  "ceiling(((za + zb) / (0.5 * log((1 + r) / (1 - r))))^2 + 3)   # Fisher z method")
RCODE$ss_anova <- function(a) c(paste0("m <- c(", paste(strsplit(a$means %||% "", "[,; ]+")[[1]], collapse = ", "), ")"),
  paste0("power.anova.test(groups = length(m), between.var = var(m), within.var = ", num_(a$sd), "^2, sig.level = ", num_(a$a), ", power = ", num_(a$pw), ")   # n per group"),
  paste0("n <- ceiling(power.anova.test(groups = length(m), between.var = var(m), within.var = ", num_(a$sd), "^2, sig.level = ", num_(a$a), ", power = ", num_(a$pw), ")$n); c(n_per_group = n, total = n * length(m))"),
  paste0("cohens_f <- sqrt(var(m) * (length(m) - 1) / length(m)) / ", num_(a$sd), "; cohens_f"))

# ---------------- assemble ----------------
# entries: list of report entries (name, time, test id, args, data_key); data_sheets: named list sheet -> data frame
rcode_block <- function(e, i) {
  f <- RCODE[[e$id]]
  body <- if (is.null(f)) "# (R code export is not available for this test)" else tryCatch(f(e$args), error = function(err) paste("# could not write code:", conditionMessage(err)))
  needs <- !is.null(e$data_key)
  c("", paste0("# ", strrep("-", 70)), paste0("# ", i, ". ", e$name, "   (run ", e$time, ")"), paste0("# ", strrep("-", 70)),
    if (needs) paste0("d <- ", e$data_key, "   # data used for this analysis"),
    paste0("alpha <- ", num_(e$args$alpha, 0.05)), body)
}

rcode_script <- function(entries, data_sheets, xlsx_name, app_version) {
  surv <- any(vapply(entries, function(e) e$id %in% c("km", "cox"), logical(1)))
  head <- c("# =====================================================================",
            paste0("#  R code for the analyses run in MAMC BioStat v", app_version),
            "#  Maulana Azad Medical College, New Delhi - free & open source (GPL-3)",
            paste0("#  Generated: ", format(Sys.time(), "%d %b %Y %H:%M"), "   |   R ", getRversion(), " was used by MAMC BioStat"),
            "#",
            "#  HOW TO RUN: install R (https://cran.r-project.org) and optionally RStudio.",
            paste0("#  Put this file in the same folder as '", xlsx_name, "'"),
            "#  (downloaded from the same Report page), open it in RStudio and click 'Source',",
            "#  or run each line. The data are read from the 'Data_...' sheets of that Excel file.",
            "#  Only base R is used for the statistics (plus 'survival' for survival analysis).",
            "# =====================================================================", "",
            "if (!requireNamespace(\"readxl\", quietly = TRUE)) install.packages(\"readxl\")",
            if (surv) "if (!requireNamespace(\"survival\", quietly = TRUE)) install.packages(\"survival\")",
            paste0("results_file <- ", Q(xlsx_name)),
            "if (!file.exists(results_file)) results_file <- file.choose()   # pick the Excel file if it is elsewhere")
  dl <- if (length(data_sheets)) c("", "# ---- data (exactly as analysed in MAMC BioStat, after any cleaning) ----",
    vapply(names(data_sheets), function(s) paste0(s, " <- as.data.frame(readxl::read_excel(results_file, sheet = ", Q(s), ", guess_max = 100000))"), "")) else NULL
  blocks <- unlist(lapply(seq_along(entries), function(i) rcode_block(entries[[i]], i)))
  cite <- c("", "# ---- How to cite ----",
            "# citation()                 # R itself",
            if (surv) "# citation(\"survival\"); ",
            paste0("# MAMC BioStat (Version ", app_version, ") [Computer software]. Maulana Azad Medical College, New Delhi; ",
                   format(Sys.Date(), "%Y"), ". Available from: https://github.com/ManuHCI/mamc-biostat"))
  paste(c(head, dl, blocks, cite), collapse = "\n")
}
