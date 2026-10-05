# =====================================================================
#  "Which test should I use?" wizard
# =====================================================================

wizard_ui <- function() {
  rq <- function(id, label, choices, cond = NULL) {
    w <- div(class = "panel-box", radioButtons(id, label, choices = choices, selected = character(0)))
    if (is.null(cond)) w else conditionalPanel(cond, w)
  }
  tagList(
    rq("wz_aim", "1. What do you want to do?", c(
      "Describe / summarise my data" = "describe", "Compare groups or time points" = "compare",
      "Find the relationship / association between two variables" = "relate", "Predict an outcome from several variables (adjusting for confounders)" = "predict",
      "Check agreement / reliability of measurements or raters" = "agree", "Evaluate a diagnostic test" = "diag",
      "Analyse survival / time-to-event data" = "survival", "Calculate sample size for my study" = "ss")),
    rq("wz_desc", "2. What type of variable?", c("Numeric (e.g. age, SBP)" = "num", "Categorical (e.g. sex, group)" = "cat", "I want to check if data are normal" = "norm"), "input.wz_aim == 'describe'"),
    rq("wz_out", "2. What type is your OUTCOME variable?", c("Numeric / continuous (e.g. SBP, Hb)" = "num", "Ordinal score (e.g. pain 0-10, grade)" = "ord", "Categorical (e.g. improved yes/no)" = "cat"), "input.wz_aim == 'compare'"),
    rq("wz_ng", "3. How many groups / measurements are you comparing?", c("One group against a known / reference value" = "1", "Two" = "2", "Three or more" = "3"), "input.wz_aim == 'compare'"),
    rq("wz_paired", "4. Are the measurements independent or paired?", c("Independent - different subjects in each group" = "ind", "Paired / repeated - same subjects measured more than once (before-after, matched)" = "pair"),
       "input.wz_aim == 'compare' && (input.wz_ng == '2' || input.wz_ng == '3')"),
    rq("wz_norm", "5. Is the numeric outcome approximately normally distributed?", c("Yes (or each group n >= 30)" = "yes", "No / skewed / outliers" = "no", "I don't know - check it for me" = "unsure"),
       "input.wz_aim == 'compare' && input.wz_out == 'num'"),
    conditionalPanel("input.wz_aim == 'compare' && input.wz_out == 'num' && input.wz_norm == 'unsure'",
      div(class = "panel-box", uiOutput("wz_norm_vars"), actionButton("wz_check_norm", "Check normality", class = "btn-sm btn-outline-secondary"), htmlOutput("wz_norm_res"))),
    rq("wz_rel", "2. What types are the two variables?", c("Both numeric" = "nn", "Both categorical" = "cc", "One numeric, one categorical (= comparing groups)" = "nc", "Many numeric variables at once" = "many"), "input.wz_aim == 'relate'"),
    rq("wz_rnorm", "3. Are both numeric variables normally distributed with a linear relation?", c("Yes" = "yes", "No / ordinal / outliers" = "no"), "input.wz_aim == 'relate' && input.wz_rel == 'nn'"),
    rq("wz_pred", "2. What type is the outcome to predict?", c("Numeric (e.g. SBP)" = "num", "Binary (e.g. died / survived)" = "bin", "Time-to-event (survival time + status)" = "tte"), "input.wz_aim == 'predict'"),
    rq("wz_ag", "2. What kind of agreement?", c("Two methods measuring the same numeric quantity" = "ba", "Reliability of numeric ratings by 2+ raters / repeats" = "icc",
       "Two raters giving categorical ratings" = "kappa", "Internal consistency of a questionnaire" = "alpha"), "input.wz_aim == 'agree'"),
    rq("wz_dg", "2. What is the test result?", c("Positive / negative (categorical)" = "cat", "A numeric value (need best cut-off)" = "num"), "input.wz_aim == 'diag'"),
    rq("wz_sv", "2. What do you want?", c("Survival curves & compare groups" = "km", "Effect of several factors on survival (hazard ratios)" = "cox"), "input.wz_aim == 'survival'"),
    rq("wz_ss", "2. What is the primary outcome of your study?", c("Compare two means" = "ss_means", "Compare two proportions" = "ss_props", "Estimate a prevalence (cross-sectional)" = "ss_prev",
       "Correlation" = "ss_corr", "Compare 3+ means (ANOVA)" = "ss_anova"), "input.wz_aim == 'ss'")
  )
}

wizard_reco <- function(i) {
  r <- function(id, why, alt = NULL) list(id = id, why = why, alt = alt)
  a <- i$wz_aim; if (is.null(a)) return(NULL)
  switch(a,
    describe = switch(i$wz_desc %||% "", num = r("desc", "Numeric variables are summarised by mean &plusmn; SD (if normal) or median (IQR) (if skewed)."),
                      cat = r("freq", "Categorical variables are summarised as number (%)."), norm = r("normality", "Shapiro-Wilk test with histogram and Q-Q plot."), NULL),
    compare = {
      o <- i$wz_out; n <- i$wz_ng; p <- i$wz_paired %||% "ind"; nm <- i$wz_norm
      if (is.null(o) || is.null(n)) return(NULL)
      if (o == "cat") {
        if (n == "1") r("prop_one", "One categorical outcome compared with a known proportion.", "chisq_gof")
        else if (p == "pair") r("mcnemar", "Paired categorical data (same subjects twice) - only discordant pairs matter.")
        else r("chisq_ind", "Comparing proportions between independent groups. Use Fisher's exact test if expected counts &lt; 5.", c("fisher", "or_rr", "prop_two"))
      } else {
        if (n != "1" && is.null(i$wz_paired)) return(NULL)
        par <- o == "num" && identical(nm, "yes")
        if (o == "num" && is.null(nm)) return(NULL)
        if (o == "num" && identical(nm, "unsure")) return(list(id = NULL, why = "Run the normality check above, then answer Yes or No."))
        key <- paste(n, p, if (par) "P" else "NP")
        switch(key,
          `1 ind P` = r("t_one", "One normally distributed sample vs a reference value.", "wilcox_one"), `1 ind NP` = r("wilcox_one", "Skewed / ordinal data vs a reference median.", "t_one"),
          `1 pair P` = r("t_one", "One sample vs reference value.", "wilcox_one"), `1 pair NP` = r("wilcox_one", "One sample vs reference median.", "t_one"),
          `2 ind P` = r("t_ind", "Two independent groups, normal outcome &rarr; independent t-test (Welch if variances unequal).", "mann_whitney"),
          `2 ind NP` = r("mann_whitney", "Two independent groups, skewed or ordinal outcome &rarr; Mann-Whitney U.", "t_ind"),
          `2 pair P` = r("t_paired", "Same subjects measured twice, differences normal &rarr; paired t-test.", "wilcox_paired"),
          `2 pair NP` = r("wilcox_paired", "Same subjects measured twice, skewed/ordinal &rarr; Wilcoxon signed-rank.", "t_paired"),
          `3 ind P` = r("anova1", "3+ independent groups, normal outcome &rarr; one-way ANOVA with Tukey post-hoc. To adjust for baseline use ANCOVA; for two factors use two-way ANOVA.", c("kruskal", "ancova", "anova2")),
          `3 ind NP` = r("kruskal", "3+ independent groups, skewed/ordinal &rarr; Kruskal-Wallis with Dunn post-hoc.", "anova1"),
          `3 pair P` = r("rm_anova", "Same subjects at 3+ time points, normal &rarr; repeated-measures ANOVA.", "friedman"),
          `3 pair NP` = r("friedman", "Same subjects at 3+ time points, skewed/ordinal &rarr; Friedman test.", "rm_anova"))
      }
    },
    relate = switch(i$wz_rel %||% "", nn = if (is.null(i$wz_rnorm)) NULL else if (i$wz_rnorm == "yes") r("pearson", "Linear relation between two normal numeric variables.", c("spearman", "lin_reg")) else r("spearman", "Monotonic relation; robust to skew, outliers and ordinal data.", "pearson"),
                    cc = r("chisq_ind", "Association between two categorical variables.", c("fisher", "or_rr")), nc = list(id = NULL, why = "This is a group comparison - choose 'Compare groups' in question 1."),
                    many = r("corr_matrix", "Pairwise correlations among many variables."), NULL),
    predict = switch(i$wz_pred %||% "", num = r("lin_reg", "Numeric outcome with one or more predictors."), bin = r("log_reg", "Binary outcome &rarr; adjusted odds ratios."), tte = r("cox", "Time-to-event outcome &rarr; hazard ratios.", "km"), NULL),
    agree = switch(i$wz_ag %||% "", ba = r("bland_altman", "Bias and limits of agreement between two methods.", "icc"), icc = r("icc", "Reliability of numeric ratings."),
                   kappa = r("kappa", "Chance-corrected agreement for categories (weighted for ordered categories)."), alpha = r("cronbach", "Internal consistency of scale items."), NULL),
    diag = switch(i$wz_dg %||% "", cat = r("diag", "Sensitivity, specificity, PPV, NPV and likelihood ratios."), num = r("roc", "ROC curve, AUC and the optimal cut-off.", "diag"), NULL),
    survival = switch(i$wz_sv %||% "", km = r("km", "Kaplan-Meier curves with log-rank test.", "cox"), cox = r("cox", "Cox proportional hazards regression.", "km"), NULL),
    ss = if (is.null(i$wz_ss)) NULL else r(i$wz_ss, "Sample size formula for your primary outcome."))
}
