# Creates the demo datasets shipped with MAMC BioStat (synthetic, no real patients)
set.seed(2026)
n <- 120
grp <- factor(rep(c("Drug A", "Drug B", "Placebo"), each = 40))
sex <- sample(c("Male", "Female"), n, TRUE)
age <- round(rnorm(n, 52, 11))
eff <- c(`Drug A` = -14, `Drug B` = -9, Placebo = -2)[as.character(grp)]
sbp0 <- round(rnorm(n, 150, 12))
sbp4 <- round(sbp0 + eff * 0.5 + rnorm(n, 0, 7))
sbp8 <- round(sbp0 + eff * 0.8 + rnorm(n, 0, 7))
sbp12 <- round(sbp0 + eff + rnorm(n, 0, 8))
bmi <- round(rnorm(n, 26, 4), 1)
smoker <- sample(c("Yes", "No"), n, TRUE, prob = c(.3, .7))
hba1c <- round(rlnorm(n, log(6.5), 0.15), 1)
crp <- round(rlnorm(n, log(4), 0.8), 1)
lp <- -3 + 0.04 * (age - 52) + 0.8 * (smoker == "Yes") + 0.05 * (sbp12 - 140)
complication <- ifelse(runif(n) < plogis(lp + 2), "Yes", "No")
improved <- ifelse(sbp12 < sbp0 - 8, "Yes", "No")
pain0 <- sample(3:9, n, TRUE); pain12 <- pmax(0, pain0 - sample(0:4, n, TRUE) - (grp != "Placebo"))
disease <- ifelse(runif(n) < 0.35, "Present", "Absent")
marker <- round(ifelse(disease == "Present", rnorm(n, 60, 15), rnorm(n, 40, 15)), 1)
rapid_test <- ifelse((disease == "Present" & runif(n) < .85) | (disease == "Absent" & runif(n) < .1), "Positive", "Negative")
grade_r1 <- sample(c("Mild", "Moderate", "Severe"), n, TRUE)
grade_r2 <- ifelse(runif(n) < .75, grade_r1, sample(c("Mild", "Moderate", "Severe"), n, TRUE))
bp_device <- round(sbp0 + rnorm(n, 2, 5))
haz <- 0.02 * exp(0.6 * (grp == "Placebo") + 0.03 * (age - 52))
t_ev <- rexp(n, haz); t_c <- runif(n, 6, 36); time_months <- round(pmin(t_ev, t_c), 1); death <- as.integer(t_ev <= t_c)
lat <- rnorm(n); q <- sapply(1:5, function(i) pmin(5, pmax(1, round(3 + 0.9 * lat + rnorm(n, 0, .8)))))
colnames(q) <- paste0("Q", 1:5)
demo <- data.frame(ID = sprintf("MAMC%03d", 1:n), Age = age, Sex = sex, Group = grp, SBP_baseline = sbp0, SBP_week4 = sbp4, SBP_week8 = sbp8, SBP_week12 = sbp12,
                   BP_device = bp_device, BMI = bmi, Smoker = smoker, HbA1c = hba1c, CRP = crp, Pain_baseline = pain0, Pain_week12 = pain12, Improved = improved,
                   Complication = complication, Disease = disease, Marker = marker, Rapid_test = rapid_test, Grade_rater1 = grade_r1, Grade_rater2 = grade_r2,
                   Time_months = time_months, Death = death, q, stringsAsFactors = FALSE)
dir.create("sample_data", showWarnings = FALSE)
writexl::write_xlsx(demo, "sample_data/MAMC_demo_clinical_trial.xlsx")

# a deliberately messy copy to demonstrate the data checker
dirty <- demo[1:60, c("ID", "Age", "Sex", "Group", "SBP_baseline", "SBP_week12", "HbA1c", "Smoker")]
names(dirty)[5] <- "SBP (baseline) mmHg"
dirty$Sex[c(3, 7, 11)] <- c("male", "FEMALE", "Male ")
dirty$Age <- as.character(dirty$Age); dirty$Age[c(5, 9)] <- c("NA", "45 yrs"); dirty$Age[14] <- "4,5"
dirty$HbA1c[c(2, 20)] <- c(65, 999)
dirty$Smoker[c(4, 8)] <- c("-", "nil")
dirty <- rbind(dirty, dirty[10, ], NA)
dirty$Empty <- NA
writexl::write_xlsx(dirty, "sample_data/MAMC_demo_messy_data.xlsx")
cat("Sample data written\n")
