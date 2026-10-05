# =====================================================================
#  MAMC BioStat - helper functions
#  Maulana Azad Medical College & Lok Nayak Hospital, New Delhi.  Free & open source (GPL-3).
# =====================================================================

# ---------- test registry --------------------------------------------
TESTS <- new.env()
TEST_ORDER <- character(0)
register_test <- function(t) {
  if (is.null(t$needs_data)) t$needs_data <- TRUE
  assign(t$id, t, envir = TESTS)
  TEST_ORDER <<- c(TEST_ORDER, t$id)
  invisible(t)
}
get_test <- function(id) get(id, envir = TESTS)

CATEGORIES <- c(
  "1. Describe data",
  "2. Compare means (parametric)",
  "3. Compare groups (non-parametric)",
  "4. Categorical data",
  "5. Correlation & regression",
  "6. Agreement, diagnostic & reliability",
  "7. Survival analysis",
  "8. Sample size calculation"
)

# ---------- formatting -----------------------------------------------
fmt <- function(x, d = 3) {
  if (length(x) == 0) return("")
  out <- ifelse(is.na(x), "NA", formatC(round(x, d), format = "f", digits = d))
  out
}
fmt_p <- function(p) {
  if (length(p) == 0) return("")
  ifelse(is.na(p), "NA", ifelse(p < 0.001, "< 0.001", formatC(p, format = "f", digits = 3)))
}
p_eq <- function(p) {           # "p = 0.012" or "p < 0.001"
  ifelse(p < 0.001, "p < 0.001", paste0("p = ", fmt_p(p)))
}
sig_word <- function(p, alpha = 0.05) if (!is.na(p) && p < alpha) "statistically significant" else "not statistically significant"
ci_txt <- function(lo, hi, d = 2) paste0("(95% CI ", fmt(lo, d), " to ", fmt(hi, d), ")")

decision_html <- function(p, alpha = 0.05) {
  if (is.na(p)) return("")
  if (p < alpha)
    sprintf("<div class='decision sig'><b>Decision:</b> p (%s) &lt; &alpha; (%s) &rarr; <b>Reject the null hypothesis (H<sub>0</sub>)</b>. The result is statistically significant.</div>", fmt_p(p), alpha)
  else
    sprintf("<div class='decision ns'><b>Decision:</b> p (%s) &ge; &alpha; (%s) &rarr; <b>Fail to reject H<sub>0</sub></b>. There is not enough evidence of a difference/association.</div>", fmt_p(p), alpha)
}

warn_html <- function(...) paste0("<div class='warnbox'>&#9888; ", paste0(...), "</div>")
note_html <- function(...) paste0("<div class='notebox'>&#9432; ", paste0(...), "</div>")
writeup_html <- function(txt) paste0("<div class='writeup'><b>How to report in your thesis / paper:</b><br><i>", txt, "</i></div>")

# effect size wording
d_word <- function(d) { d <- abs(d); if (is.na(d)) "" else if (d < 0.2) "negligible" else if (d < 0.5) "small" else if (d < 0.8) "medium" else "large" }
r_word <- function(r) { r <- abs(r); if (is.na(r)) "" else if (r < 0.1) "negligible" else if (r < 0.3) "weak" else if (r < 0.5) "moderate" else if (r < 0.7) "strong" else "very strong" }
eta_word <- function(e) { if (is.na(e)) "" else if (e < 0.01) "negligible" else if (e < 0.06) "small" else if (e < 0.14) "medium" else "large" }

# ---------- data helpers ---------------------------------------------
is_num_col <- function(x) is.numeric(x) && !inherits(x, "Date")
cat_candidates <- function(d, max_levels = 15) {
  names(d)[vapply(d, function(x) {
    (!is.numeric(x) && !inherits(x, c("Date", "POSIXt")) && length(unique(na.omit(x))) <= 25) ||
      (is.numeric(x) && length(unique(na.omit(x))) <= max_levels)
  }, logical(1))]
}
num_candidates <- function(d) names(d)[vapply(d, is_num_col, logical(1))]

use_cc <- function(d, vars) {
  vars <- vars[!is.null(vars) & vars != "" & vars != "(none)"]
  x <- d[, vars, drop = FALSE]
  ok <- stats::complete.cases(x)
  list(d = x[ok, , drop = FALSE], n_excl = sum(!ok), n = sum(ok))
}
excl_note <- function(n_excl) if (n_excl > 0) note_html(n_excl, " row(s) with missing values in the selected variables were excluded (complete-case analysis).") else ""

as_fac <- function(x) droplevels(as.factor(x))

need <- function(cond, msg) if (!isTRUE(cond)) stop(msg, call. = FALSE)

# ---------- descriptive helpers --------------------------------------
skewness <- function(x) { x <- na.omit(x); n <- length(x); if (n < 3) return(NA); m <- mean(x); s <- sd(x); (n / ((n - 1) * (n - 2))) * sum(((x - m) / s)^3) }
kurtosis <- function(x) { x <- na.omit(x); n <- length(x); if (n < 4) return(NA); m <- mean(x); s <- sd(x)
  (n * (n + 1) / ((n - 1) * (n - 2) * (n - 3))) * sum(((x - m) / s)^4) - 3 * (n - 1)^2 / ((n - 2) * (n - 3)) }

desc_row <- function(x) {
  n_miss <- sum(is.na(x)); x <- x[!is.na(x)]; n <- length(x)
  if (n == 0) return(data.frame(n = 0, missing = n_miss))
  m <- mean(x); s <- if (n > 1) sd(x) else NA; se <- s / sqrt(n)
  tc <- if (n > 1) qt(0.975, n - 1) else NA
  q <- quantile(x, c(.25, .5, .75), type = 7)
  data.frame(n = n, missing = n_miss, mean = m, SD = s, SE = se,
             CI95_low = m - tc * se, CI95_high = m + tc * se,
             median = q[2], Q1 = q[1], Q3 = q[3], IQR = q[3] - q[1],
             min = min(x), max = max(x), skewness = skewness(x), kurtosis = kurtosis(x),
             row.names = NULL, check.names = FALSE)
}

mean_sd <- function(x, d = 2) paste0(fmt(mean(x), d), " &plusmn; ", fmt(sd(x), d))
med_iqr <- function(x, d = 2) { q <- quantile(x, c(.25, .5, .75)); paste0(fmt(q[2], d), " (IQR ", fmt(q[1], d), "&ndash;", fmt(q[3], d), ")") }

shapiro_safe <- function(x) {
  x <- na.omit(x)
  if (length(x) < 3 || length(x) > 5000 || length(unique(x)) < 3) return(NA)
  tryCatch(shapiro.test(x)$p.value, error = function(e) NA)
}
normality_note <- function(p_list) {
  # p_list: named numeric vector of Shapiro p values
  bad <- names(p_list)[!is.na(p_list) & p_list < 0.05]
  tab <- paste0(names(p_list), ": Shapiro-Wilk ", p_eq(p_list), collapse = "; ")
  if (length(bad))
    warn_html("Normality assumption may be violated (", tab, "). Consider the non-parametric alternative, or rely on the Central Limit Theorem if each group has n &ge; 30.")
  else note_html("Normality check: ", tab, " &rarr; no significant departure from normality.")
}

# ---------- Levene / Brown-Forsythe (median-centred) -----------------
levene_test <- function(y, g) {
  g <- as_fac(g)
  med <- tapply(y, g, median)
  z <- abs(y - med[as.character(g)])
  a <- anova(lm(z ~ g))
  list(F = a$`F value`[1], df1 = a$Df[1], df2 = a$Df[2], p = a$`Pr(>F)`[1])
}

# ---------- Dunn post-hoc test --------------------------------------
dunn_test <- function(y, g, method = "holm") {
  g <- as_fac(g); n <- length(y); r <- rank(y)
  ties <- table(r); tie_corr <- sum(ties^3 - ties) / (12 * (n - 1))
  lv <- levels(g); mr <- tapply(r, g, mean); ni <- table(g)
  pr <- combn(lv, 2)
  out <- data.frame(comparison = apply(pr, 2, paste, collapse = " vs "), stringsAsFactors = FALSE)
  out$mean_rank_diff <- apply(pr, 2, function(p) mr[p[1]] - mr[p[2]])
  out$z <- apply(pr, 2, function(p) {
    se <- sqrt((n * (n + 1) / 12 - tie_corr) * (1 / ni[p[1]] + 1 / ni[p[2]]))
    (mr[p[1]] - mr[p[2]]) / se })
  out$p_unadjusted <- 2 * pnorm(-abs(out$z))
  out$p_adjusted <- p.adjust(out$p_unadjusted, method = method)
  out
}

# ---------- tables -> display ----------------------------------------
round_df <- function(df, d = 3) {
  for (i in seq_along(df)) if (is.numeric(df[[i]])) {
    nm <- names(df)[i]
    df[[i]] <- if (grepl("^(p|P|p-value|p.value)$|^p_|Pr\\(>|p \\(", nm)) fmt_p(df[[i]]) else round(df[[i]], d)
  }
  df
}

# ---------- plotting palette -----------------------------------------
# Validated colour-blind-safe categorical order (adjacent CVD dE >= 9); MAMC maroon is the brand / single-series colour
PAL <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#4a3aa7", "#e34948")
MAMC_MAROON <- "#7b1c2e"
pal_n <- function(n) rep(PAL, length.out = n)

# ---------- MathJax helpers ------------------------------------------
mj <- function(...) paste0("\\[", paste0(...), "\\]")
step_list <- function(...) paste0("<ol class='steps'>", paste0("<li>", c(...), "</li>", collapse = ""), "</ol>")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a
alpha_of <- function(a) as.numeric(a$alpha %||% 0.05)

two_levels <- function(g, test_alt) {
  lv <- levels(as_fac(g))
  need(length(lv) == 2, paste0("The grouping variable must have exactly 2 groups (found ", length(lv), ": ",
                               paste(head(lv, 8), collapse = ", "), "). ", test_alt))
  lv
}
