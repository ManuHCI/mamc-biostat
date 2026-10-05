# Automated check: runs every statistical test in MAMC BioStat on the demo data.
# Usage (from the app folder):  Rscript ../tools/run_all_tests.R
for (f in c("helpers.R", list.files("R", pattern = "^(data_check|tests_).*\\.R$"))) source(file.path("R", basename(f)))
d <- read_data("sample_data/MAMC_demo_clinical_trial.xlsx", "x.xlsx")
A <- list(
  desc = list(vars = c("Age", "SBP_baseline"), g = "Group"), freq = list(x = "Group", g = "Sex"), normality = list(y = "CRP", g = "(none)"),
  t_one = list(y = "SBP_baseline", mu = 140), t_ind = list(y = "SBP_week12", g = "Sex"), t_paired = list(y1 = "SBP_baseline", y2 = "SBP_week12"),
  anova1 = list(y = "SBP_week12", g = "Group"), anova2 = list(y = "SBP_week12", f1 = "Group", f2 = "Sex", inter = "Yes"),
  rm_anova = list(vars = c("SBP_baseline", "SBP_week4", "SBP_week8", "SBP_week12")), ancova = list(y = "SBP_week12", g = "Group", cv = "SBP_baseline"),
  wilcox_one = list(y = "Pain_baseline", mu = 5), mann_whitney = list(y = "CRP", g = "Smoker"), wilcox_paired = list(y1 = "Pain_baseline", y2 = "Pain_week12"),
  kruskal = list(y = "CRP", g = "Group"), friedman = list(vars = c("SBP_baseline", "SBP_week4", "SBP_week8")),
  chisq_ind = list(x = "Group", y = "Improved"), chisq_gof = list(x = "Group", props = ""), fisher = list(x = "Smoker", y = "Complication"),
  mcnemar = list(x = "Grade_rater1", y = "Grade_rater2"), or_rr = list(x = "Smoker", xl = "Yes", y = "Complication", yl = "Yes"),
  prop_one = list(x = "Smoker", xl = "Yes", p0 = 0.25), prop_two = list(g = "Sex", y = "Improved", yl = "Yes"),
  pearson = list(x = "SBP_baseline", y = "BP_device"), spearman = list(x = "Age", y = "CRP"), corr_matrix = list(vars = c("Age", "BMI", "SBP_baseline", "HbA1c"), method = "pearson"),
  lin_reg = list(y = "SBP_week12", xs = c("SBP_baseline", "Age", "Group")), log_reg = list(y = "Complication", yl = "Yes", xs = c("Age", "Smoker", "SBP_week12")),
  kappa = list(r1 = "Grade_rater1", r2 = "Grade_rater2", wt = "Unweighted (nominal)"), bland_altman = list(m1 = "SBP_baseline", m2 = "BP_device"),
  icc = list(vars = c("SBP_baseline", "BP_device")), diag = list(t = "Rapid_test", tl = "Positive", g = "Disease", gl = "Present"),
  roc = list(x = "Marker", y = "Disease", yl = "Present"), cronbach = list(vars = paste0("Q", 1:5)),
  km = list(time = "Time_months", ev = "Death", evl = "1", g = "Group"), cox = list(time = "Time_months", ev = "Death", evl = "1", xs = c("Age", "Group")),
  ss_means = list(delta = 5, sd = 10, a = 0.05, pw = 0.8, drop = 10, design = "Two independent groups"), ss_props = list(p1 = .3, p2 = .15, a = .05, pw = .8, drop = 10),
  ss_prev = list(p = .2, dd = .05, a = .05, N = 0, deff = 1, drop = 10), ss_corr = list(r = .3, a = .05, pw = .8), ss_anova = list(k = 3, means = "10,12,15", sd = 5, a = .05, pw = .8))
# mcnemar needs 2 levels -> use derived columns
d$Improved_wk4 <- ifelse(d$SBP_week4 < d$SBP_baseline - 8, "Yes", "No"); A$mcnemar <- list(x = "Improved_wk4", y = "Improved")
pdf(NULL); ok <- 0; bad <- character(0)
for (id in TEST_ORDER) {
  a <- A[[id]]; if (is.null(a)) { bad <- c(bad, paste(id, ": NO ARGS")); next }
  r <- tryCatch({ x <- get_test(id)$run(d, a); if (!is.null(x$plot)) x$plot(); for (t in x$tables) round_df(t); x }, error = function(e) e)
  if (inherits(r, "error")) bad <- c(bad, paste0(id, ": ", conditionMessage(r))) else ok <- ok + 1
}
cat("Tests registered:", length(TEST_ORDER), " passed:", ok, "\n"); if (length(bad)) cat("FAILED:\n", paste(bad, collapse = "\n "), "\n")
# data checker on messy data
m <- read_data("sample_data/MAMC_demo_messy_data.xlsx", "x.xlsx"); ck <- check_data(m); cat("\nIssues found in messy data:", nrow(ck$issues), "\n"); print(ck$issues[, 1:3])
cl <- apply_fixes(m, names(FIX_LABELS)); cat("\nAfter fixes:", attr(cl, "fix_log"), sep = "\n  "); cat("Remaining issues:", nrow(check_data(cl)$issues), "\n")
