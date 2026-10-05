# Renders every chart type (R) and writes the matching Python scripts; run from app/
for (f in c("helpers.R", "data_check.R", "visualize.R")) source(file.path("R", f))
d <- read_data("sample_data/MAMC_demo_clinical_trial.xlsx", "x.xlsx")
base <- list(title = "", subtitle = "", xlab = "", ylab = "", palette = names(PALETTES)[1], labels = TRUE, base = 13, legend = "right", err = "95% CI", bins = 15, points = TRUE, line = TRUE, style = "Grouped", method = "pearson")
cases <- list(
  histogram = list(x = "SBP_baseline", group = "(none)"), density = list(x = "CRP", group = "Group"), boxplot = list(y = "SBP_week12", xc = "Group"),
  violin = list(y = "CRP", xc = "Smoker"), mean_error = list(y = "SBP_week12", xc = "Group", group = "Sex"), bar_count = list(xc = "Group", group = "Improved", style = "Stacked 100%"),
  pie = list(xc = "Group", style = "Donut"), scatter = list(x = "SBP_baseline", y = "BP_device", group = "(none)"),
  line_time = list(vars = c("SBP_baseline", "SBP_week4", "SBP_week8", "SBP_week12"), group = "Group"), heatmap = list(vars = c("Age", "BMI", "SBP_baseline", "HbA1c", "CRP")),
  qq = list(x = "HbA1c", group = "(none)"), forest = list(y = "SBP_week12", xc = "Group"))
out <- "../tools/chart_test_out"; dir.create(out, showWarnings = FALSE)
for (ch in names(cases)) {
  o <- modifyList(base, c(list(chart = ch), cases[[ch]]))
  r <- tryCatch({ p <- build_chart(d, o); ggsave(file.path(out, paste0(ch, ".png")), p, width = 8, height = 5.5, dpi = 80)
    py <- sub("your_data.xlsx", "../../app/sample_data/MAMC_demo_clinical_trial.xlsx", python_code(o), fixed = TRUE)
    py <- sub("plt.savefig('mamc_chart.png'", paste0("plt.savefig('py_", ch, ".png'"), py, fixed = TRUE); py <- sub("plt.show()", "", py, fixed = TRUE)
    writeLines(py, file.path(out, paste0(ch, ".py"))); "ok" }, error = function(e) conditionMessage(e))
  cat(sprintf("%-11s %s\n", ch, r))
}
