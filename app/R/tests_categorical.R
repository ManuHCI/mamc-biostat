# =====================================================================
#  4. CATEGORICAL DATA
# =====================================================================

ct_display <- function(tb) {
  m <- as.data.frame.matrix(tb); rp <- round(100 * prop.table(tb, 1), 1)
  out <- matrix(paste0(as.matrix(m), " (", rp, "%)"), nrow = nrow(m), dimnames = dimnames(tb))
  out <- cbind(out, Total = rowSums(tb)); out <- rbind(out, Total = c(colSums(tb), sum(tb)))
  data.frame(` ` = rownames(out), out, check.names = FALSE)
}

register_test(list(
  id = "chisq_ind", name = "Chi-square test of association (R x C table)", cat = CATEGORIES[4], level = "UG & PG",
  inputs = list(list(id = "x", label = "Row variable (e.g. exposure / group)", type = "cat"), list(id = "y", label = "Column variable (e.g. outcome)", type = "cat")),
  run = function(d, a) {
    al <- alpha_of(a); need(a$x != a$y, "Choose two different variables.")
    cc <- use_cc(d, c(a$x, a$y)); tb <- table(as_fac(cc$d[[1]]), as_fac(cc$d[[2]])); names(dimnames(tb)) <- c(a$x, a$y)
    need(all(dim(tb) >= 2), "Each variable needs at least 2 categories.")
    cs <- suppressWarnings(chisq.test(tb, correct = FALSE)); E <- cs$expected; low <- mean(E < 5) * 100
    yat <- if (all(dim(tb) == 2)) suppressWarnings(chisq.test(tb, correct = TRUE)) else NULL
    N <- sum(tb); V <- sqrt(cs$statistic / (N * (min(dim(tb)) - 1)))
    fis <- if (low > 20) tryCatch(fisher.test(tb, simulate.p.value = any(dim(tb) > 2) && N > 200, B = 1e5), error = function(e) NULL) else NULL
    res <- data.frame(Test = c("Pearson chi-square", if (!is.null(yat)) "Continuity-corrected (Yates)", if (!is.null(fis)) "Fisher's exact test"),
                      `Chi-square` = c(cs$statistic, if (!is.null(yat)) yat$statistic, if (!is.null(fis)) NA), df = c(cs$parameter, if (!is.null(yat)) 1, if (!is.null(fis)) NA),
                      p = c(cs$p.value, if (!is.null(yat)) yat$p.value, if (!is.null(fis)) fis$p.value), check.names = FALSE)
    pu <- if (!is.null(fis)) fis$p.value else cs$p.value
    Ed <- data.frame(` ` = rownames(E), round(E, 2), check.names = FALSE)
    sr <- data.frame(` ` = rownames(cs$stdres), round(unclass(cs$stdres), 2), check.names = FALSE)
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>H<sub>0</sub>: ", a$x, " and ", a$y, " are independent (not associated).</p>",
      if (low > 20) warn_html(fmt(low, 0), "% of cells have expected count &lt; 5 &rarr; chi-square approximation is unreliable. <b>Fisher's exact test</b> p-value is used.") else note_html("All expected-count assumptions met (&le;20% cells with expected &lt; 5)."),
      "<p>&chi;&sup2;(", cs$parameter, ", N = ", N, ") = ", fmt(cs$statistic, 2), ", ", p_eq(cs$p.value), ". ", if (!is.null(fis)) paste0("Fisher's exact ", p_eq(fis$p.value), ". ") else "",
      "The association is <b>", sig_word(pu, al), "</b>. Cram&eacute;r's V = ", fmt(V, 3), " (", r_word(V), " association).</p>",
      "<p>Standardised residuals &gt; |1.96| show which cells contribute most (more / fewer cases than expected).</p>", decision_html(pu, al),
      writeup_html(sprintf("There was %s association between %s and %s (&chi;&sup2;(%d) = %s, %s, Cram&eacute;r's V = %s).", if (pu < al) "a significant" else "no significant", a$x, a$y, cs$parameter, fmt(cs$statistic, 2), p_eq(pu), fmt(V, 2))))
    steps <- step_list(paste0("Expected count for each cell: ", mj("E_{ij} = \\frac{\\text{Row total}_i \\times \\text{Column total}_j}{N}"), " e.g. cell (1,1): ", rowSums(tb)[1], " &times; ", colSums(tb)[1], " / ", N, " = ", fmt(E[1, 1], 2)),
      paste0(mj("\\chi^2 = \\sum \\frac{(O_{ij}-E_{ij})^2}{E_{ij}} = ", fmt(cs$statistic))),
      paste0("df = (rows &minus; 1)(columns &minus; 1) = (", nrow(tb) - 1, ")(", ncol(tb) - 1, ") = ", cs$parameter),
      paste0("Cram&eacute;r's ", mj("V = \\sqrt{\\frac{\\chi^2}{N(\\min(r,c)-1)}} = ", fmt(V))),
      "Yates' correction (2&times;2 only) subtracts 0.5 from each |O&minus;E|; it is conservative.")
    list(tables = list(`Observed counts (row %)` = ct_display(tb), `Expected counts` = Ed, `Test results` = res, `Standardised residuals` = sr,
                       `Effect size` = data.frame(`Cramer's V` = V, check.names = FALSE)), html = html, steps = steps,
         plot = function() { op <- par(mar = c(5, 4, 3, 8), xpd = TRUE); on.exit(par(op))
           pr <- 100 * prop.table(tb, 1); barplot(t(pr), col = pal_n(ncol(tb)), ylab = paste("% within", a$x), xlab = a$x, main = paste(a$y, "by", a$x))
           legend("topright", inset = c(-0.22, 0), legend = colnames(tb), fill = pal_n(ncol(tb)), title = a$y, bty = "n") })
  }))

register_test(list(
  id = "chisq_gof", name = "Chi-square goodness-of-fit test", cat = CATEGORIES[4], level = "UG & PG",
  inputs = list(list(id = "x", label = "Categorical variable", type = "cat"),
                list(id = "props", label = "Expected proportions in category order, comma-separated (blank = equal)", type = "text", default = "")),
  run = function(d, a) {
    al <- alpha_of(a); cc <- use_cc(d, a$x); tb <- table(as_fac(cc$d[[1]])); k <- length(tb)
    p0 <- if (nzchar(trimws(a$props %||% ""))) as.numeric(strsplit(a$props, "[,; ]+")[[1]]) else rep(1 / k, k)
    need(length(p0) == k && all(!is.na(p0)), paste0("Provide ", k, " proportions for categories: ", paste(names(tb), collapse = ", ")))
    p0 <- p0 / sum(p0); cs <- suppressWarnings(chisq.test(tb, p = p0))
    tab <- data.frame(Category = names(tb), Observed = as.integer(tb), `Expected proportion` = p0, Expected = cs$expected, `(O-E)^2/E` = (as.integer(tb) - cs$expected)^2 / cs$expected, check.names = FALSE)
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>H<sub>0</sub>: the observed distribution of ", a$x, " matches the expected proportions.</p><p>&chi;&sup2;(", k - 1, ") = ", fmt(cs$statistic, 2), ", ", p_eq(cs$p.value),
      " &rarr; observed distribution <b>", if (cs$p.value < al) "differs significantly from" else "does not differ significantly from", "</b> the expected.</p>",
      if (any(cs$expected < 5)) warn_html("Some expected counts are &lt; 5; consider merging categories or an exact multinomial test.") else "", decision_html(cs$p.value, al))
    steps <- step_list(paste0("Expected count E<sub>i</sub> = N &times; p<sub>i</sub> (N = ", sum(tb), ")"), paste0(mj("\\chi^2 = \\sum\\frac{(O_i-E_i)^2}{E_i} = ", fmt(cs$statistic))), paste0("df = k &minus; 1 = ", k - 1))
    list(tables = list(`Goodness of fit` = tab, `Test` = data.frame(`Chi-square` = cs$statistic, df = k - 1, p = cs$p.value, check.names = FALSE, row.names = NULL)), html = html, steps = steps,
         plot = function() barplot(rbind(as.integer(tb), cs$expected), beside = TRUE, col = c(PAL[1], "grey75"), names.arg = names(tb), legend.text = c("Observed", "Expected"), main = "Observed vs expected counts"))
  }))

register_test(list(
  id = "fisher", name = "Fisher's exact test", cat = CATEGORIES[4], level = "UG & PG",
  inputs = list(list(id = "x", label = "Row variable", type = "cat"), list(id = "y", label = "Column variable", type = "cat")),
  run = function(d, a) {
    al <- alpha_of(a); cc <- use_cc(d, c(a$x, a$y)); tb <- table(as_fac(cc$d[[1]]), as_fac(cc$d[[2]])); names(dimnames(tb)) <- c(a$x, a$y)
    big <- any(dim(tb) > 2) && sum(tb) > 300
    ft <- fisher.test(tb, simulate.p.value = big, B = 1e5, conf.level = 1 - al)
    res <- data.frame(Test = paste0("Fisher's exact test", if (big) " (Monte-Carlo p, 100,000 replicates)" else ""), p = ft$p.value,
                      `Odds ratio (conditional MLE)` = if (!is.null(ft$estimate)) ft$estimate else NA, `CI low` = if (!is.null(ft$conf.int)) ft$conf.int[1] else NA,
                      `CI high` = if (!is.null(ft$conf.int)) ft$conf.int[2] else NA, check.names = FALSE)
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>H<sub>0</sub>: ", a$x, " and ", a$y, " are independent.</p><p>Fisher's exact ", p_eq(ft$p.value), " &rarr; association is <b>", sig_word(ft$p.value, al), "</b>.",
      if (!is.null(ft$estimate)) paste0(" Odds ratio = ", fmt(ft$estimate, 2), " ", ci_txt(ft$conf.int[1], ft$conf.int[2]), ".") else "", "</p>", decision_html(ft$p.value, al),
      note_html("Fisher's test is exact - it does not rely on large-sample approximation, so it is preferred when expected counts are small (&lt; 5)."))
    steps <- step_list("Fix the row and column totals (margins) as observed.",
      paste0("For a 2&times;2 table the probability of a particular table is hypergeometric: ", mj("P = \\frac{(a+b)!\\,(c+d)!\\,(a+c)!\\,(b+d)!}{N!\\,a!\\,b!\\,c!\\,d!}")),
      "Enumerate all possible tables with the same margins; p-value = sum of probabilities of tables as or less likely than the observed one.")
    list(tables = list(`Observed counts (row %)` = ct_display(tb), `Fisher's exact test` = res), html = html, steps = steps,
         plot = function() mosaicplot(tb, col = pal_n(ncol(tb)), main = paste(a$x, "vs", a$y)))
  }))

register_test(list(
  id = "mcnemar", name = "McNemar test (paired proportions, before-after)", cat = CATEGORIES[4], level = "UG & PG",
  inputs = list(list(id = "x", label = "Status at time 1 / test 1 (2 categories)", type = "cat"), list(id = "y", label = "Status at time 2 / test 2 (same 2 categories)", type = "cat")),
  run = function(d, a) {
    al <- alpha_of(a); cc <- use_cc(d, c(a$x, a$y)); lv <- sort(unique(c(as.character(cc$d[[1]]), as.character(cc$d[[2]]))))
    need(length(lv) == 2, paste0("Both variables must share exactly 2 categories (found: ", paste(lv, collapse = ", "), ")."))
    tb <- table(factor(cc$d[[1]], lv), factor(cc$d[[2]], lv)); names(dimnames(tb)) <- c(a$x, a$y)
    b <- tb[1, 2]; c_ <- tb[2, 1]; mc <- mcnemar.test(tb, correct = TRUE); ex <- binom.test(b, b + c_, 0.5)
    res <- data.frame(Test = c("McNemar chi-square (continuity corrected)", "Exact McNemar (binomial)"), `Chi-square` = c(mc$statistic, NA), df = c(1, NA), p = c(mc$p.value, ex$p.value), check.names = FALSE)
    pu <- if (b + c_ < 25) ex$p.value else mc$p.value
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>H<sub>0</sub>: the proportion is the same at both times (discordant pairs b and c are equally likely).</p>",
      "<p>Discordant pairs: b = ", b, " (", lv[1], "&rarr;", lv[2], "), c = ", c_, " (", lv[2], "&rarr;", lv[1], "). ", if (b + c_ < 25) "Because b + c &lt; 25 the exact binomial p-value is used: " else "McNemar &chi;&sup2; ", p_eq(pu),
      " &rarr; change is <b>", sig_word(pu, al), "</b>.</p><p>Proportion '", lv[1], "': time 1 = ", fmt(100 * sum(tb[1, ]) / sum(tb), 1), "%, time 2 = ", fmt(100 * sum(tb[, 1]) / sum(tb), 1), "%.</p>", decision_html(pu, al))
    steps <- step_list("Only discordant pairs (changed category) carry information about change; concordant pairs are ignored.",
      paste0(mj("\\chi^2 = \\frac{(|b-c|-1)^2}{b+c} = \\frac{(|", b, "-", c_, "|-1)^2}{", b + c_, "} = ", fmt(mc$statistic))), "df = 1. For small b + c use the exact binomial test with p = 0.5.")
    list(tables = list(`Paired 2x2 table` = ct_display(tb), `McNemar test` = res), html = html, steps = steps,
         plot = function() barplot(c(`Time 1` = 100 * sum(tb[1, ]) / sum(tb), `Time 2` = 100 * sum(tb[, 1]) / sum(tb)), col = PAL[1:2], ylab = paste("%", lv[1]), main = paste("Proportion", lv[1], "at each time"), ylim = c(0, 100)))
  }))

register_test(list(
  id = "or_rr", name = "Odds ratio, relative risk, NNT (2x2 table)", cat = CATEGORIES[4], level = "UG & PG",
  inputs = list(list(id = "x", label = "Exposure / group variable", type = "cat"), list(id = "xl", label = "Exposed level", type = "level", of = "x"),
                list(id = "y", label = "Outcome variable", type = "cat"), list(id = "yl", label = "Outcome 'event' level (e.g. Yes / Died)", type = "level", of = "y")),
  run = function(d, a) {
    al <- alpha_of(a); z <- qnorm(1 - al / 2); cc <- use_cc(d, c(a$x, a$y))
    ex <- as.character(cc$d[[1]]) == a$xl; ev <- as.character(cc$d[[2]]) == a$yl
    A <- sum(ex & ev); B <- sum(ex & !ev); C <- sum(!ex & ev); D <- sum(!ex & !ev)
    need(all(c(A + B, C + D) > 0), "Both exposed and unexposed groups must have observations.")
    hc <- if (any(c(A, B, C, D) == 0)) 0.5 else 0
    OR <- ((A + hc) * (D + hc)) / ((B + hc) * (C + hc)); seOR <- sqrt(sum(1 / (c(A, B, C, D) + hc)))
    r1 <- A / (A + B); r0 <- C / (C + D); RR <- ((A + hc) / (A + B + 2 * hc)) / ((C + hc) / (C + D + 2 * hc))
    seRR <- sqrt(1 / (A + hc) - 1 / (A + B + 2 * hc) + 1 / (C + hc) - 1 / (C + D + 2 * hc)); RD <- r1 - r0
    seRD <- sqrt(r1 * (1 - r1) / (A + B) + r0 * (1 - r0) / (C + D)); NNT <- 1 / abs(RD)
    tb <- matrix(c(A, C, B, D), 2, dimnames = list(c(paste("Exposed:", a$xl), "Not exposed"), c(paste("Event:", a$yl), "No event")))
    ft <- fisher.test(tb); cs <- suppressWarnings(chisq.test(tb))
    res <- data.frame(Measure = c("Odds ratio (OR)", "Relative risk (RR)", "Risk difference (ARR)", if (RD < 0) "Number needed to treat (NNT)" else "Number needed to harm (NNH)"),
                      Estimate = c(OR, RR, RD, NNT), `CI low` = c(exp(log(OR) - z * seOR), exp(log(RR) - z * seRR), RD - z * seRD, NA),
                      `CI high` = c(exp(log(OR) + z * seOR), exp(log(RR) + z * seRR), RD + z * seRD, NA), check.names = FALSE)
    html <- paste0(excl_note(cc$n_excl), if (hc > 0) note_html("A zero cell was found; 0.5 was added to all cells (Haldane correction) for OR and RR.") else "",
      "<h4>Interpretation</h4><p>Risk of '", a$yl, "' in exposed = ", fmt(100 * r1, 1), "% (", A, "/", A + B, "); in unexposed = ", fmt(100 * r0, 1), "% (", C, "/", C + D, ").</p>",
      "<ul><li><b>Odds ratio</b> = ", fmt(OR, 2), " ", ci_txt(res$`CI low`[1], res$`CI high`[1]), ": the odds of the event are ", fmt(OR, 2), " times in the exposed group.</li>",
      "<li><b>Relative risk</b> = ", fmt(RR, 2), " ", ci_txt(res$`CI low`[2], res$`CI high`[2]), ": the risk is ", fmt(RR, 2), " times in exposed ", if (RR > 1) paste0("(", fmt(100 * (RR - 1), 0), "% higher)") else paste0("(", fmt(100 * (1 - RR), 0), "% lower)"), ".</li>",
      "<li><b>Risk difference</b> = ", fmt(100 * RD, 1), " percentage points; ", if (RD < 0) "NNT" else "NNH", " = ", fmt(NNT, 1), " &rarr; about ", ceiling(NNT), " people need to be exposed/treated for one additional ", if (RD < 0) "event prevented." else "event caused.", "</li></ul>",
      "<p>If the 95% CI of OR or RR includes 1, the association is not statistically significant. Chi-square ", p_eq(cs$p.value), "; Fisher's exact ", p_eq(ft$p.value), ".</p>",
      note_html("Use <b>RR</b> for cohort studies and RCTs; use <b>OR</b> for case-control studies (RR cannot be estimated there). OR approximates RR only when the outcome is rare (&lt;10%)."))
    steps <- step_list(paste0("2&times;2 table: a = ", A, ", b = ", B, ", c = ", C, ", d = ", D),
      paste0(mj("OR = \\frac{a\\times d}{b\\times c} = \\frac{", A, "\\times", D, "}{", B, "\\times", C, "} = ", fmt(OR)), " ; ", mj("SE(\\ln OR) = \\sqrt{\\tfrac1a+\\tfrac1b+\\tfrac1c+\\tfrac1d} = ", fmt(seOR))),
      paste0(mj("RR = \\frac{a/(a+b)}{c/(c+d)} = \\frac{", fmt(r1), "}{", fmt(r0), "} = ", fmt(RR))),
      paste0("95% CI: ", mj("e^{\\ln(OR) \\pm 1.96\\,SE}")),
      paste0(mj("ARR = R_1 - R_0 = ", fmt(RD), ",\\quad NNT = \\frac{1}{|ARR|} = ", fmt(NNT, 1))))
    list(tables = list(`2x2 table` = data.frame(` ` = rownames(tb), tb, Total = rowSums(tb), check.names = FALSE), `Measures of association` = res,
                       `Significance tests` = data.frame(Test = c("Chi-square (Yates)", "Fisher's exact"), p = c(cs$p.value, ft$p.value))), html = html, steps = steps,
         plot = function() { est <- res$Estimate[1:2]; lo <- res$`CI low`[1:2]; hi <- res$`CI high`[1:2]
           plot(est, 1:2, xlim = range(c(lo, hi, 1)), ylim = c(0.5, 2.5), log = "x", pch = 15, cex = 2, col = PAL[1], yaxt = "n", ylab = "", xlab = "Estimate (log scale)", main = "Forest plot: OR and RR with 95% CI")
           axis(2, 1:2, c("OR", "RR"), las = 1); segments(lo, 1:2, hi, 1:2, lwd = 3, col = PAL[1]); abline(v = 1, lty = 2) })
  }))

register_test(list(
  id = "prop_one", name = "One-sample proportion test (binomial)", cat = CATEGORIES[4], level = "UG & PG",
  inputs = list(list(id = "x", label = "Categorical variable", type = "cat"), list(id = "xl", label = "Level counted as 'success'", type = "level", of = "x"),
                list(id = "p0", label = "Hypothesised proportion (0-1)", type = "value", default = 0.5)),
  run = function(d, a) {
    al <- alpha_of(a); cc <- use_cc(d, a$x); k <- sum(as.character(cc$d[[1]]) == a$xl); n <- cc$n; p0 <- as.numeric(a$p0)
    bt <- binom.test(k, n, p0, conf.level = 1 - al); ph <- k / n; zs <- (ph - p0) / sqrt(p0 * (1 - p0) / n)
    res <- data.frame(Successes = k, n = n, `Observed proportion` = ph, `Hypothesised p0` = p0, z = zs, `p (exact binomial)` = bt$p.value,
                      `p (z approx.)` = 2 * pnorm(-abs(zs)), `CI low (exact)` = bt$conf.int[1], `CI high (exact)` = bt$conf.int[2], check.names = FALSE)
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>Observed proportion of '", a$xl, "' = ", fmt(100 * ph, 1), "% ", ci_txt(100 * bt$conf.int[1], 100 * bt$conf.int[2], 1),
      " vs hypothesised ", 100 * p0, "%. Exact binomial ", p_eq(bt$p.value), " &rarr; <b>", sig_word(bt$p.value, al), "</b>.</p>", decision_html(bt$p.value, al))
    steps <- step_list(paste0(mj("\\hat p = \\frac{x}{n} = \\frac{", k, "}{", n, "} = ", fmt(ph))), paste0(mj("z = \\frac{\\hat p - p_0}{\\sqrt{p_0(1-p_0)/n}} = ", fmt(zs))), "The exact test sums binomial probabilities of outcomes as or more extreme than observed.")
    list(tables = list(`Proportion test` = res), html = html, steps = steps,
         plot = function() { plot(1, ph, ylim = c(0, 1), xlim = c(0.5, 1.5), pch = 19, cex = 2, col = PAL[1], xaxt = "n", xlab = "", ylab = "Proportion", main = "Observed proportion with 95% CI")
           arrows(1, bt$conf.int[1], 1, bt$conf.int[2], angle = 90, code = 3, lwd = 2, col = PAL[1]); abline(h = p0, lty = 2, col = PAL[2]) })
  }))

register_test(list(
  id = "prop_two", name = "Two-proportion z-test (compare 2 groups)", cat = CATEGORIES[4], level = "UG & PG",
  inputs = list(list(id = "g", label = "Group variable (2 groups)", type = "cat"), list(id = "y", label = "Outcome variable", type = "cat"),
                list(id = "yl", label = "Outcome 'event' level", type = "level", of = "y")),
  run = function(d, a) {
    al <- alpha_of(a); cc <- use_cc(d, c(a$g, a$y)); g <- as_fac(cc$d[[1]]); lv <- two_levels(g, "For more groups use the Chi-square test.")
    ev <- as.character(cc$d[[2]]) == a$yl; x <- c(sum(ev[g == lv[1]]), sum(ev[g == lv[2]])); n <- as.integer(table(g))
    pt <- prop.test(x, n, correct = FALSE, conf.level = 1 - al); p1 <- x[1] / n[1]; p2 <- x[2] / n[2]; pp <- sum(x) / sum(n)
    zs <- (p1 - p2) / sqrt(pp * (1 - pp) * (1 / n[1] + 1 / n[2]))
    res <- data.frame(Group = lv, Events = x, n = n, Proportion = c(p1, p2))
    t2 <- data.frame(`Difference (p1 - p2)` = p1 - p2, `CI low` = pt$conf.int[1], `CI high` = pt$conf.int[2], z = zs, p = pt$p.value, check.names = FALSE)
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>", lv[1], ": ", fmt(100 * p1, 1), "% vs ", lv[2], ": ", fmt(100 * p2, 1), "% (difference ", fmt(100 * (p1 - p2), 1), " percentage points, ",
      ci_txt(100 * pt$conf.int[1], 100 * pt$conf.int[2], 1), "); z = ", fmt(zs, 2), ", ", p_eq(pt$p.value), " &rarr; <b>", sig_word(pt$p.value, al), "</b>.</p>", decision_html(pt$p.value, al),
      note_html("The two-proportion z-test is equivalent to the 2&times;2 chi-square test (z&sup2; = &chi;&sup2;)."))
    steps <- step_list(paste0(mj("\\hat p_1 = ", fmt(p1), ",\\; \\hat p_2 = ", fmt(p2), ",\\; \\bar p = \\frac{x_1+x_2}{n_1+n_2} = ", fmt(pp))),
      paste0(mj("z = \\frac{\\hat p_1-\\hat p_2}{\\sqrt{\\bar p(1-\\bar p)(1/n_1+1/n_2)}} = ", fmt(zs))), "p = 2 &times; P(Z &gt; |z|)")
    list(tables = list(`Proportions` = res, `Two-proportion test` = t2), html = html, steps = steps,
         plot = function() barplot(100 * c(p1, p2), names.arg = lv, col = PAL[1:2], ylab = paste("%", a$yl), ylim = c(0, 100), main = paste("Proportion", a$yl, "by", a$g)))
  }))
