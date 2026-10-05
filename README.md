# MAMC BioStat

**Free, open-source biostatistics software for medical students and researchers.**
Developed by the faculty of **Maulana Azad Medical College & Lok Nayak Hospital**, University of Delhi, New Delhi.

MAMC BioStat is a point-and-click statistics package built on **R**. Students load an Excel sheet, answer a few questions, and get correct results with plain-English interpretation, the formula worked out with their own numbers, a ready-to-paste thesis sentence, and publication-quality charts. It runs entirely on the user's laptop, with no internet and no data upload.

Licence: **GNU GPL-3.0**. Anyone may use, copy, modify and share it.

---

## What it does

| Module | Features |
|---|---|
| **1. Data** | Import `.xlsx / .xls / .csv`. **Data Health Check** finds blank headers, duplicate names, numbers stored as text, units inside cells, decimal commas, text missing-codes (NA, -, nil), inconsistent spellings (Male / male / MALE), stray spaces, empty rows/columns, duplicate rows, extreme values, 999-type codes, ID columns. It explains each problem and can apply safe automatic fixes. You can download the cleaned file. |
| **2. Choose Test** | A question-by-question wizard (aim → outcome type → number of groups → paired? → normal?) that recommends the right test and alternatives, and can run a normality check for you. |
| **3. Analyse** | 40 methods (list below). Each gives: results tables, automatic **interpretation**, H₀ decision, assumption checks with warnings, effect sizes with 95% CI, **step-by-step calculation with your numbers**, a **"How to report in your thesis"** sentence, plots, and a full **Help** page (purpose, assumptions, formula, background: why and how it works). |
| **4. Visualise** | 12 chart types with colour palettes (colour-blind-safe default), value labels, custom titles and axes, 300-dpi PNG / PDF / TIFF export, **plus matching Python code** (pandas + seaborn + matplotlib) for every chart. |
| **Report** | Every analysis is collected into one downloadable HTML report and an Excel file of all tables. |
| **Learn** | A built-in statistics textbook: basics (p-values, CI, errors, test chooser) plus a page for every test. |

### Statistical methods (40)

- **Describe:** descriptive statistics, frequency tables with exact 95% CI, normality (Shapiro-Wilk, Q-Q)
- **Parametric:** one-sample t, independent t (Student + Welch + Levene), paired t, one-way ANOVA (+ Tukey, Welch ANOVA, Games-Howell type), two-way ANOVA (Type III), repeated-measures ANOVA (Mauchly, Greenhouse-Geisser, Huynh-Feldt), ANCOVA (adjusted means)
- **Non-parametric:** Wilcoxon signed-rank (one sample and paired), Mann-Whitney U, Kruskal-Wallis + Dunn, Friedman + post-hoc
- **Categorical:** chi-square (R×C, Yates, residuals, Cramér's V), goodness-of-fit, Fisher's exact, McNemar, OR / RR / risk difference / NNT, one- and two-proportion tests
- **Correlation & regression:** Pearson, Spearman & Kendall, correlation matrix, simple / multiple linear regression (standardised β, VIF, diagnostics), binary logistic regression (OR, LR test, Nagelkerke R², Hosmer-Lemeshow, AUC)
- **Agreement & diagnostic:** Cohen's kappa (unweighted / linear / quadratic), Bland-Altman, ICC (5 forms), sensitivity / specificity / PPV / NPV / LR, ROC curve + AUC + Youden cut-off, Cronbach's alpha
- **Survival:** Kaplan-Meier + log-rank, Cox regression (+ proportional-hazards test)
- **Sample size:** two means, two proportions, prevalence (with FPC and design effect), correlation, ANOVA

Kappa, ICC, Cronbach's alpha and AUC were checked against the `psych` and `pROC` R packages and give the same values. All other tests use R's own `stats` / `survival` functions.

---

## Installation

### Download (recommended) - no R, no internet, nothing else needed
Go to the **Releases** page and download the file for your computer:

| Computer | File | How to install |
|---|---|---|
| Windows 10 / 11 | `MAMC_BioStat_Setup_x.y.z_Windows10-11.exe` | Double-click → Next → Install. No admin rights needed. |
| Windows 7 / 8.1 | `MAMC_BioStat_Setup_x.y.z_Windows7-8.exe` | Same as above. Needs Windows 7 SP1 with updates, and Chrome or Edge (version 109). |
| Mac with Apple chip (M1-M4) | `MAMC_BioStat_x.y.z_macOS-AppleSilicon.dmg` | Open → drag to Applications → first time: right-click → Open. |
| Older Intel Mac | `MAMC_BioStat_x.y.z_macOS-Intel.dmg` | Same as above. |
| College PC without install rights / USB stick | `..._portable.zip` | Extract → double-click `MAMC BioStat.vbs`. |

The software opens in its own window and closes completely when you close the window.
On Windows, if a blue "Windows protected your PC" box appears, click **More info → Run anyway** (the installer is not digitally signed).
On a Mac, if R is not yet installed, the app installs the bundled R the first time (one click and your Mac password).

### Building the installers
All installers are built and tested automatically by GitHub Actions (`.github/workflows/build-installers.yml`):
open the **Actions** tab → *Build installers* → **Run workflow**, or push a tag such as `v1.1.0` to publish a Release.
The build scripts are plain Python: `build_tools/build_windows.py` (Windows 10/11 and 7/8.1) and `build_tools/build_mac.py` (macOS).

### Run from source (any computer with R ≥ 4.1)
Install R from https://cran.r-project.org, then double-click `launchers/MAMC_BioStat_Windows.bat`
(macOS: `launchers/MAMC_BioStat_Mac.command`, Linux: `launchers/mamc_biostat_linux.sh`). Missing packages are installed once (internet needed).

---

## Preparing your Excel sheet
- Row 1 holds short column names (`Age`, `Sex`, `SBP_baseline`). Avoid merged cells and title rows.
- Use one row per subject and one column per variable.
- Put only numbers in numeric columns. Leave missing cells **blank**.
- Spell each category the same way every time.
- Put repeated measurements in separate columns (`SBP_week0`, `SBP_week4`, ...).
- Remove names, phone numbers and hospital IDs before analysis.

Two demo files are in `app/sample_data/`: a clean synthetic clinical trial (120 patients) and a deliberately messy file for practising the cleaning tools.

---

## Project structure
```
app/
  app.R                  user interface + server (Shiny)
  R/helpers.R            formatting, registry, shared statistics helpers
  R/data_check.R         import, data health check, automatic fixes
  R/tests_*.R            the statistical tests (one register_test() per method)
  R/visualize.R          chart studio (ggplot2) + Python code generator
  R/wizard.R             "which test?" decision logic
  help/*.html            help library: background, formulas (KaTeX), steps
  www/                   CSS, JS, offline KaTeX, logo
  sample_data/           demo datasets
launcher.R               starts the app in its own window (bundled packages, or installs missing ones once)
launchers/               double-click starters for running from source
build_tools/             Python scripts that build the Windows and macOS installers
installer/               application icons
tools/                   automated tests (all methods, all charts, UI smoke test)
```

### Adding a new test
Each method is a self-contained `register_test(list(id, name, cat, level, inputs, run))` block. `inputs` declares what the user must choose (numeric / categorical variables, levels, values). `run(d, a)` returns `tables`, `html` (interpretation), `steps` (worked formula) and `plot`. Add a matching `<!-- TEST:id -->` section to `app/help/`, then run `Rscript ../tools/run_all_tests.R` from `app/`.

---

## Citation
> MAMC BioStat (Version 1.1.0) [Computer software]. Maulana Azad Medical College, New Delhi; 2026. Available from: https://github.com/ManuHCI/mamc-biostat

## Disclaimer
MAMC BioStat is an educational and research aid. The interpretations are generated automatically, so the investigator is still responsible for choosing appropriate methods. For complex designs, consult a biostatistician.
