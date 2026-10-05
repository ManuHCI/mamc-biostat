# =====================================================================
#  7. SURVIVAL ANALYSIS   &   8. SAMPLE SIZE
# =====================================================================

event_int <- function(v, lvl) {
  if (is.numeric(v) && all(na.omit(v) %in% c(0, 1)) && (is.null(lvl) || lvl %in% c("1", ""))) return(as.integer(v))
  as.integer(as.character(v) == lvl)
}

register_test(list(
  id = "km", name = "Kaplan-Meier survival curve & log-rank test", cat = CATEGORIES[7], level = "PG",
  inputs = list(list(id = "time", label = "Follow-up time (numeric)", type = "num"), list(id = "ev", label = "Event / status variable", type = "cat"),
                list(id = "evl", label = "Level meaning 'event occurred' (e.g. 1 / Died)", type = "level", of = "ev"), list(id = "g", label = "Compare groups (optional)", type = "cat_opt")),
  run = function(d, a) {
    al <- alpha_of(a); grp <- !is.null(a$g) && a$g != "(none)"
    cc <- use_cc(d, c(a$time, a$ev, if (grp) a$g)); tt <- cc$d[[1]]; ev <- event_int(cc$d[[2]], a$evl); need(sum(ev) > 0, "No events found - check the event level.")
    g <- if (grp) as_fac(cc$d[[3]]) else factor(rep("All", length(tt)))
    fit <- survival::survfit(survival::Surv(tt, ev) ~ g); st <- summary(fit)$table; if (is.null(dim(st))) st <- t(as.matrix(st))
    med <- data.frame(Group = levels(g), n = st[, "records"], Events = st[, "events"], Censored = st[, "records"] - st[, "events"], `Median survival` = st[, "median"],
                      `Median CI low` = st[, "0.95LCL"], `Median CI high` = st[, "0.95UCL"], check.names = FALSE, row.names = NULL)
    tq <- unique(round(quantile(tt, c(.25, .5, .75)), 1)); ss <- summary(fit, times = tq, extend = TRUE)
    lt <- data.frame(Group = if (is.null(ss$strata)) "All" else sub("^g=", "", ss$strata), Time = ss$time, `At risk` = ss$n.risk, `Survival %` = 100 * ss$surv,
                     `CI low %` = 100 * ss$lower, `CI high %` = 100 * ss$upper, check.names = FALSE)
    tabs <- list(`Summary & median survival` = med, `Survival probability at selected times` = lt)
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>", sum(ev), " events among ", length(tt), " subjects (", length(tt) - sum(ev), " censored). ",
      paste0("Median survival in <b>", med$Group, "</b> = ", ifelse(is.na(med$`Median survival`), "not reached", fmt(med$`Median survival`, 1)), collapse = "; "), ".</p>")
    if (grp && nlevels(g) > 1) {
      lr <- survival::survdiff(survival::Surv(tt, ev) ~ g); df <- length(lr$n) - 1; p <- pchisq(lr$chisq, df, lower.tail = FALSE)
      tabs$`Log-rank test` <- data.frame(Group = levels(g), Observed = lr$obs, Expected = lr$exp, `O/E` = lr$obs / lr$exp, check.names = FALSE)
      tabs$`Log-rank result` <- data.frame(`Chi-square` = lr$chisq, df = df, p = p, check.names = FALSE)
      html <- paste0(html, "<p><b>Log-rank test:</b> &chi;&sup2;(", df, ") = ", fmt(lr$chisq, 2), ", ", p_eq(p), " &rarr; survival curves <b>", if (p < al) "differ significantly" else "do not differ significantly", "</b>.</p>", decision_html(p, al),
        writeup_html(sprintf("Survival differed %s between %s groups (log-rank &chi;&sup2;(%d) = %s, %s).", if (p < al) "significantly" else "non-significantly", a$g, df, fmt(lr$chisq, 2), p_eq(p))))
    }
    html <- paste0(html, note_html("Censored = subjects who did not have the event during follow-up (lost, withdrew, or study ended). Kaplan-Meier uses their information until censoring. Median not reached = survival never fell below 50%."))
    steps <- step_list("Sort subjects by time. At each event time t<sub>i</sub>, count n<sub>i</sub> (at risk) and d<sub>i</sub> (events).",
      paste0("Kaplan-Meier (product-limit) estimator: ", mj("\\hat S(t) = \\prod_{t_i \\le t}\\left(1 - \\frac{d_i}{n_i}\\right)")),
      paste0("Greenwood variance: ", mj("Var[\\hat S(t)] = \\hat S(t)^2\\sum\\frac{d_i}{n_i(n_i-d_i)}")),
      paste0("Log-rank: at each event time, expected events per group ", mj("E_{1i} = d_i\\frac{n_{1i}}{n_i}"), "; then ", mj("\\chi^2 = \\frac{(O_1-E_1)^2}{Var(O_1-E_1)}")))
    list(tables = tabs, html = html, steps = steps,
         plot = function() { k <- nlevels(g); plot(fit, col = pal_n(k), lwd = 2.5, conf.int = k == 1, mark.time = TRUE, xlab = a$time, ylab = "Survival probability", main = "Kaplan-Meier survival curve")
           if (k > 1) legend("bottomleft", levels(g), col = pal_n(k), lwd = 2.5, bty = "n") })
  }))

register_test(list(
  id = "cox", name = "Cox proportional hazards regression (hazard ratios)", cat = CATEGORIES[7], level = "PG",
  inputs = list(list(id = "time", label = "Follow-up time", type = "num"), list(id = "ev", label = "Event / status variable", type = "cat"),
                list(id = "evl", label = "Level meaning 'event occurred'", type = "level", of = "ev"), list(id = "xs", label = "Predictor(s)", type = "any_multi")),
  run = function(d, a) {
    al <- alpha_of(a); need(length(a$xs) >= 1, "Select at least one predictor."); cc <- use_cc(d, c(a$time, a$ev, a$xs)); dd <- cc$d
    dd$.t <- dd[[a$time]]; dd$.e <- event_int(dd[[a$ev]], a$evl); for (v in a$xs) if (!is.numeric(dd[[v]])) dd[[v]] <- as_fac(dd[[v]])
    names(dd) <- make.names(names(dd)); xv <- make.names(a$xs)
    fit <- survival::coxph(as.formula(paste("survival::Surv(.t, .e) ~", paste(xv, collapse = "+"))), data = dd); s <- summary(fit, conf.int = 1 - al)
    co <- data.frame(Term = rownames(s$coefficients), B = s$coefficients[, 1], SE = s$coefficients[, 3], z = s$coefficients[, 4], p = s$coefficients[, 5],
                     HR = s$conf.int[, 1], `HR CI low` = s$conf.int[, 3], `HR CI high` = s$conf.int[, 4], check.names = FALSE, row.names = NULL)
    zph <- tryCatch(survival::cox.zph(fit)$table, error = function(e) NULL)
    tabs <- list(`Model` = data.frame(n = s$n, Events = s$nevent, `LR chi-square` = s$logtest[1], df = s$logtest[2], p = s$logtest[3], Concordance = s$concordance[1], check.names = FALSE, row.names = NULL),
                 `Hazard ratios` = co)
    if (!is.null(zph)) tabs$`Proportional hazards check (Schoenfeld)` <- data.frame(Term = rownames(zph), chisq = zph[, 1], df = zph[, 2], p = zph[, 3], row.names = NULL)
    viol <- if (!is.null(zph)) rownames(zph)[zph[, 3] < 0.05 & rownames(zph) != "GLOBAL"] else character(0)
    html <- paste0(excl_note(cc$n_excl), "<h4>Interpretation</h4><p>", s$nevent, " events in ", s$n, " subjects. Model LR &chi;&sup2;(", s$logtest[2], ") = ", fmt(s$logtest[1], 2), ", ", p_eq(s$logtest[3]), "; concordance (C-index) = ", fmt(s$concordance[1], 3), ".</p><ul>",
      paste0("<li><b>", co$Term, "</b>: HR = ", fmt(co$HR, 2), " ", ci_txt(co$`HR CI low`, co$`HR CI high`), ", ", p_eq(co$p), " &rarr; ", ifelse(co$HR > 1, paste0(fmt(100 * (co$HR - 1), 0), "% higher hazard"), paste0(fmt(100 * (1 - co$HR), 0), "% lower hazard")),
             " per unit increase (or vs reference).</li>", collapse = ""), "</ul>",
      if (length(viol)) warn_html("Proportional hazards assumption may be violated for: ", paste(viol, collapse = ", "), ".") else note_html("Proportional hazards assumption: no violation detected (Schoenfeld residual test)."))
    steps <- step_list(paste0("Model: ", mj("h(t\\mid X) = h_0(t)\\,e^{\\beta_1X_1+\\dots+\\beta_kX_k}")), "h<sub>0</sub>(t) (baseline hazard) is left unspecified - semi-parametric; &beta; estimated by partial likelihood.",
      paste0(mj("HR = e^{\\beta},\\quad 95\\%\\,CI = e^{\\beta\\pm1.96\\,SE}")), "Key assumption: the hazard ratio is constant over time (proportional hazards).")
    list(tables = tabs, html = html, steps = steps,
         plot = function() { op <- par(mar = c(5, 9, 3, 1)); on.exit(par(op)); k <- nrow(co)
           plot(co$HR, 1:k, log = "x", xlim = range(c(co$`HR CI low`, co$`HR CI high`, 1), finite = TRUE), pch = 15, cex = 1.6, col = PAL[1], yaxt = "n", ylab = "", xlab = "Hazard ratio (log scale)", main = "Forest plot of hazard ratios")
           axis(2, 1:k, co$Term, las = 1, cex.axis = 0.8); segments(co$`HR CI low`, 1:k, co$`HR CI high`, 1:k, lwd = 2.5, col = PAL[1]); abline(v = 1, lty = 2) })
  }))

# ---------------------------- SAMPLE SIZE ----------------------------
ss_inflate <- function(n, drop) ceiling(n / (1 - drop / 100))

register_test(list(
  id = "ss_means", name = "Sample size: compare two means", cat = CATEGORIES[8], level = "UG & PG", needs_data = FALSE,
  inputs = list(list(id = "delta", label = "Clinically important difference in means (\u0394)", type = "value", default = 5),
                list(id = "sd", label = "Standard deviation (from pilot / literature)", type = "value", default = 10),
                list(id = "a", label = "Significance level \u03b1 (two-sided)", type = "value", default = 0.05),
                list(id = "pw", label = "Power (1 - \u03b2)", type = "value", default = 0.8),
                list(id = "drop", label = "Expected dropout %", type = "value", default = 10),
                list(id = "design", label = "Design", type = "select", choices = c("Two independent groups", "Paired (before-after)"))),
  run = function(d, a) {
    De <- as.numeric(a$delta); S <- as.numeric(a$sd); A <- as.numeric(a$a); P <- as.numeric(a$pw); pr <- identical(a$design, "Paired (before-after)")
    za <- qnorm(1 - A / 2); zb <- qnorm(P)
    nf <- if (pr) (za + zb)^2 * S^2 / De^2 else 2 * (za + zb)^2 * S^2 / De^2
    pt <- power.t.test(delta = De, sd = S, sig.level = A, power = P, type = if (pr) "paired" else "two.sample"); nt <- ceiling(pt$n)
    res <- data.frame(Method = c("Formula (normal approximation)", "Exact (t distribution, R power.t.test)"), `n per group` = c(ceiling(nf), nt),
                      `n with dropout` = c(ss_inflate(ceiling(nf), as.numeric(a$drop)), ss_inflate(nt, as.numeric(a$drop))), check.names = FALSE)
    if (pr) names(res)[2:3] <- c("n pairs", "n pairs with dropout")
    html <- paste0("<h4>Result</h4><p>To detect a difference of <b>", De, "</b> (SD ", S, ", effect size d = ", fmt(De / S, 2), ") with ", 100 * P, "% power at &alpha; = ", A, ", you need <b>", nt, if (pr) " pairs" else " per group",
      "</b>; allowing ", a$drop, "% dropout: <b>", ss_inflate(nt, as.numeric(a$drop)), if (pr) " pairs" else paste0(" per group (total ", 2 * ss_inflate(nt, as.numeric(a$drop)), ")"), "</b>.</p>",
      writeup_html(sprintf("Assuming a standard deviation of %s, a sample of %d %s will provide %s%% power to detect a difference of %s at a two-sided significance level of %s. Allowing for %s%% dropout, %d %s will be recruited.",
        S, nt, if (pr) "subjects" else "per group", 100 * P, De, A, a$drop, ss_inflate(nt, as.numeric(a$drop)), if (pr) "subjects" else "per group")))
    steps <- step_list(paste0(mj("Z_{1-\\alpha/2} = ", fmt(za), ",\\quad Z_{1-\\beta} = ", fmt(zb))),
      paste0(if (pr) mj("n = \\frac{(Z_{1-\\alpha/2}+Z_{1-\\beta})^2\\,\\sigma_d^2}{\\Delta^2}") else mj("n = \\frac{2(Z_{1-\\alpha/2}+Z_{1-\\beta})^2\\sigma^2}{\\Delta^2}"), " = ", fmt(nf, 2)),
      "The exact method iterates using the t distribution (slightly larger n for small samples).", paste0(mj("n_{final} = \\frac{n}{1 - \\text{dropout}}")))
    list(tables = list(`Sample size` = res), html = html, steps = steps,
         plot = function() { pw <- seq(0.5, 0.99, by = 0.01); nn <- sapply(pw, function(p) power.t.test(delta = De, sd = S, sig.level = A, power = p, type = if (pr) "paired" else "two.sample")$n)
           plot(nn, pw, type = "l", lwd = 3, col = PAL[1], xlab = "n per group", ylab = "Power", main = "Power curve"); abline(h = P, v = pt$n, lty = 2) })
  }))

register_test(list(
  id = "ss_props", name = "Sample size: compare two proportions", cat = CATEGORIES[8], level = "UG & PG", needs_data = FALSE,
  inputs = list(list(id = "p1", label = "Expected proportion in group 1 (0-1)", type = "value", default = 0.3),
                list(id = "p2", label = "Expected proportion in group 2 (0-1)", type = "value", default = 0.15),
                list(id = "a", label = "Significance level \u03b1", type = "value", default = 0.05), list(id = "pw", label = "Power", type = "value", default = 0.8),
                list(id = "drop", label = "Expected dropout %", type = "value", default = 10)),
  run = function(d, a) {
    p1 <- as.numeric(a$p1); p2 <- as.numeric(a$p2); A <- as.numeric(a$a); P <- as.numeric(a$pw); za <- qnorm(1 - A / 2); zb <- qnorm(P); pb <- (p1 + p2) / 2
    nf <- (za * sqrt(2 * pb * (1 - pb)) + zb * sqrt(p1 * (1 - p1) + p2 * (1 - p2)))^2 / (p1 - p2)^2
    pp <- power.prop.test(p1 = p1, p2 = p2, sig.level = A, power = P); nt <- ceiling(pp$n); nd <- ss_inflate(nt, as.numeric(a$drop))
    html <- paste0("<h4>Result</h4><p>To detect ", 100 * p1, "% vs ", 100 * p2, "% with ", 100 * P, "% power (&alpha; = ", A, "): <b>", nt, " per group</b>; with ", a$drop, "% dropout <b>", nd, " per group (total ", 2 * nd, ")</b>.</p>",
      writeup_html(sprintf("Assuming proportions of %s%% and %s%%, %d participants per group provide %s%% power at &alpha; = %s; allowing %s%% dropout, %d per group (%d total) will be enrolled.", 100 * p1, 100 * p2, nt, 100 * P, A, a$drop, nd, 2 * nd)))
    steps <- step_list(paste0(mj("\\bar p = \\frac{p_1+p_2}{2} = ", fmt(pb))), paste0(mj("n = \\frac{\\left[Z_{1-\\alpha/2}\\sqrt{2\\bar p(1-\\bar p)} + Z_{1-\\beta}\\sqrt{p_1(1-p_1)+p_2(1-p_2)}\\right]^2}{(p_1-p_2)^2} = ", fmt(nf, 2))))
    list(tables = list(`Sample size` = data.frame(`n per group` = nt, `n per group with dropout` = nd, Total = 2 * nd, check.names = FALSE)), html = html, steps = steps, plot = NULL)
  }))

register_test(list(
  id = "ss_prev", name = "Sample size: estimate a prevalence / proportion", cat = CATEGORIES[8], level = "UG & PG", needs_data = FALSE,
  inputs = list(list(id = "p", label = "Expected prevalence (0-1)", type = "value", default = 0.2),
                list(id = "dd", label = "Absolute precision / margin of error (e.g. 0.05 = \u00b15%)", type = "value", default = 0.05),
                list(id = "a", label = "Significance level \u03b1", type = "value", default = 0.05), list(id = "N", label = "Population size (0 = infinite)", type = "value", default = 0),
                list(id = "deff", label = "Design effect (1 = simple random; ~2 for cluster)", type = "value", default = 1), list(id = "drop", label = "Non-response %", type = "value", default = 10)),
  run = function(d, a) {
    p <- as.numeric(a$p); D <- as.numeric(a$dd); z <- qnorm(1 - as.numeric(a$a) / 2); N <- as.numeric(a$N); de <- as.numeric(a$deff)
    n0 <- z^2 * p * (1 - p) / D^2; n1 <- if (N > 0) n0 / (1 + (n0 - 1) / N) else n0; n2 <- ceiling(n1 * de); nd <- ss_inflate(n2, as.numeric(a$drop))
    html <- paste0("<h4>Result</h4><p>To estimate a prevalence of ", 100 * p, "% within &plusmn;", 100 * D, "% (", 100 * (1 - as.numeric(a$a)), "% confidence): <b>n = ", n2, "</b>; with ", a$drop, "% non-response <b>", nd, "</b>.</p>",
      note_html("Relative precision is sometimes used instead (e.g. 20% of p: d = 0.2 &times; p). If prevalence is unknown, use p = 0.5 for the maximum sample size."))
    steps <- step_list(paste0(mj("n_0 = \\frac{Z^2\\,p(1-p)}{d^2} = \\frac{", fmt(z, 2), "^2\\times", p, "\\times", 1 - p, "}{", D, "^2} = ", fmt(n0, 1))),
      if (N > 0) paste0("Finite population correction: ", mj("n = \\frac{n_0}{1+(n_0-1)/N} = ", fmt(n1, 1))) else "Population assumed large (no finite population correction).",
      paste0("Multiply by design effect ", de, " and inflate for non-response."))
    list(tables = list(`Sample size` = data.frame(`n (simple)` = ceiling(n0), `n (after FPC & design effect)` = n2, `n with non-response` = nd, check.names = FALSE)), html = html, steps = steps, plot = NULL)
  }))

register_test(list(
  id = "ss_corr", name = "Sample size: detect a correlation", cat = CATEGORIES[8], level = "PG", needs_data = FALSE,
  inputs = list(list(id = "r", label = "Expected correlation r", type = "value", default = 0.3), list(id = "a", label = "Significance level \u03b1", type = "value", default = 0.05),
                list(id = "pw", label = "Power", type = "value", default = 0.8)),
  run = function(d, a) {
    r <- as.numeric(a$r); za <- qnorm(1 - as.numeric(a$a) / 2); zb <- qnorm(as.numeric(a$pw)); C <- 0.5 * log((1 + r) / (1 - r)); n <- ceiling(((za + zb) / C)^2 + 3)
    list(tables = list(`Sample size` = data.frame(`Expected r` = r, n = n, check.names = FALSE)),
         html = paste0("<h4>Result</h4><p>To detect r = ", r, " with ", 100 * as.numeric(a$pw), "% power at &alpha; = ", a$a, ", you need <b>n = ", n, "</b> subjects.</p>"),
         steps = step_list(paste0("Fisher's z: ", mj("C = \\tfrac12\\ln\\frac{1+r}{1-r} = ", fmt(C))), paste0(mj("n = \\left(\\frac{Z_{1-\\alpha/2}+Z_{1-\\beta}}{C}\\right)^2 + 3 = ", n))), plot = NULL)
  }))

register_test(list(
  id = "ss_anova", name = "Sample size: one-way ANOVA (k groups)", cat = CATEGORIES[8], level = "PG", needs_data = FALSE,
  inputs = list(list(id = "k", label = "Number of groups", type = "value", default = 3), list(id = "means", label = "Expected group means, comma-separated", type = "text", default = "10, 12, 15"),
                list(id = "sd", label = "Within-group SD", type = "value", default = 5), list(id = "a", label = "\u03b1", type = "value", default = 0.05), list(id = "pw", label = "Power", type = "value", default = 0.8)),
  run = function(d, a) {
    m <- as.numeric(strsplit(a$means, "[,; ]+")[[1]]); need(length(m) >= 2 && !any(is.na(m)), "Enter at least two numeric means.")
    pa <- power.anova.test(groups = length(m), between.var = var(m), within.var = as.numeric(a$sd)^2, sig.level = as.numeric(a$a), power = as.numeric(a$pw)); n <- ceiling(pa$n)
    f <- sqrt(var(m) * (length(m) - 1) / length(m)) / as.numeric(a$sd)
    list(tables = list(`Sample size` = data.frame(Groups = length(m), `Cohen's f` = f, `n per group` = n, Total = n * length(m), check.names = FALSE)),
         html = paste0("<h4>Result</h4><p>Need <b>", n, " per group</b> (total ", n * length(m), ") for ", 100 * as.numeric(a$pw), "% power; effect size Cohen's f = ", fmt(f, 2), ".</p>"),
         steps = step_list(paste0(mj("f = \\frac{\\sigma_{means}}{\\sigma} = ", fmt(f))), "n is found iteratively from the non-central F distribution with non-centrality &lambda; = k&middot;n&middot;f&sup2;."), plot = NULL)
  }))
