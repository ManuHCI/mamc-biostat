# =====================================================================
#  Data import, data-quality checking and cleaning
# =====================================================================

NA_CODES <- c("na", "n/a", "n.a.", "nil", "none", "-", "--", "?", ".", "missing", "not done", "nd", "null", "")

read_data <- function(path, name, sheet = 1, skip = 0) {
  ext <- tolower(tools::file_ext(name))
  d <- switch(ext,
    xlsx = , xls = , xlsm = readxl::read_excel(path, sheet = sheet, skip = skip, .name_repair = "minimal",
                                              guess_max = 10000, na = c("", " ")),
    csv = utils::read.csv(path, skip = skip, check.names = FALSE, stringsAsFactors = FALSE, na.strings = c("", " ")),
    tsv = , txt = utils::read.delim(path, skip = skip, check.names = FALSE, stringsAsFactors = FALSE, na.strings = c("", " ")),
    stop("Unsupported file type '.", ext, "'. Please use .xlsx, .xls, .csv or .txt", call. = FALSE))
  d <- as.data.frame(d, stringsAsFactors = FALSE, check.names = FALSE)
  # convert POSIXct dates to Date for readability
  for (i in seq_along(d)) if (inherits(d[[i]], "POSIXt")) d[[i]] <- as.Date(d[[i]])
  d
}

# try to parse text as numbers: handles "12,5", "120 mg", " 45 "
parse_num <- function(x) {
  s <- trimws(as.character(x))
  s[tolower(s) %in% NA_CODES] <- NA
  s <- sub("^([-+]?[0-9]+),([0-9]+)$", "\\1.\\2", s)            # decimal comma
  s <- sub("^([-+]?[0-9]*\\.?[0-9]+)\\s*[A-Za-z%/]+.*$", "\\1", s) # strip units
  suppressWarnings(as.numeric(s))
}

check_data <- function(d) {
  issues <- list()
  add <- function(sev, col, issue, suggestion, fix = NA) {
    issues[[length(issues) + 1]] <<- data.frame(Severity = sev, Column = col, Issue = issue,
                                                Suggestion = suggestion, Fix = fix, stringsAsFactors = FALSE)
  }
  nr <- nrow(d); nc <- ncol(d)
  if (nr == 0 || nc == 0) { add("Critical", "-", "The sheet is empty.", "Check that you selected the correct sheet / header row."); return(list(issues = do.call(rbind, issues), summary = NULL)) }

  # names
  nm <- names(d)
  if (any(is.na(nm) | nm == "" | grepl("^\\.\\.\\.[0-9]+$", nm)))
    add("High", "-", "Some columns have no header (blank column name).",
        "Every column needs a short unique name in the first row. If your header is not in row 1, set 'Rows to skip'.", "clean_names")
  if (any(duplicated(nm[nm != ""])))
    add("High", paste(unique(nm[duplicated(nm)]), collapse = ", "), "Duplicate column names.", "Give each variable a unique name.", "clean_names")
  if (any(grepl("[^A-Za-z0-9_.]", nm)))
    add("Low", paste(head(nm[grepl("[^A-Za-z0-9_.]", nm)], 6), collapse = ", "), "Column names contain spaces or special characters.",
        "Use short names without spaces, e.g. 'SBP_baseline' instead of 'SBP (baseline) mmHg'.", "clean_names")

  empty_r <- which(apply(d, 1, function(r) all(is.na(r) | trimws(as.character(r)) == "")))
  if (length(empty_r)) add("Medium", "-", paste0(length(empty_r), " completely empty row(s)."), "Delete empty rows (often blank lines or totals at the bottom).", "drop_empty_rows")
  empty_c <- which(vapply(d, function(x) all(is.na(x) | trimws(as.character(x)) == ""), logical(1)))
  if (length(empty_c)) add("Medium", paste(nm[empty_c], collapse = ", "), "Completely empty column(s).", "Delete empty columns.", "drop_empty_cols")

  dup <- sum(duplicated(d))
  if (dup > 0) add("Medium", "-", paste0(dup, " duplicate row(s) (identical in every column)."),
                   "Check whether the same patient was entered twice. Remove only if they are true duplicates.", "drop_dup_rows")

  summ <- list()
  for (v in nm) {
    x <- d[[v]]; xn <- x[!is.na(x)]
    miss <- sum(is.na(x)); pm <- 100 * miss / nr
    type <- if (is.numeric(x)) "Numeric" else if (inherits(x, "Date")) "Date" else if (is.logical(x)) "Logical" else "Text / categorical"
    nu <- length(unique(xn))
    if (pm > 20) add(if (pm > 50) "High" else "Medium", v, sprintf("%.1f%% values missing.", pm),
                     "Check source records; high missingness can bias results. Consider excluding the variable or using complete-case analysis carefully.")
    if (nu == 1 && length(xn) > 0) add("Low", v, "Column has the same value in every row (constant).", "A constant variable cannot be analysed; drop it or check entry.")
    if (is.character(x)) {
      xs <- trimws(xn)
      codes <- unique(xn[tolower(xs) %in% NA_CODES])
      if (length(codes)) add("Medium", v, paste0("Text codes used for missing values: ", paste0("'", codes, "'", collapse = ", ")),
                             "Leave missing cells empty. MAMC BioStat can convert these codes to blank (missing).", "na_codes")
      pn <- parse_num(xn); ok <- !is.na(pn); nonmiss <- sum(!tolower(xs) %in% NA_CODES)
      if (nonmiss > 0 && sum(ok) / nonmiss >= 0.8) {
        bad <- unique(xn[!ok & !(tolower(xs) %in% NA_CODES)])
        add("High", v, paste0("Numbers are stored as text.", if (length(bad)) paste0(" Non-numeric entries: ", paste0("'", head(bad, 6), "'", collapse = ", ")) else ""),
            "Convert to numeric. Entries like '<5' or 'trace' must be decided by you (they will become missing). Units ('120 mg') and decimal commas ('12,5') are handled automatically.", "text_to_num")
        type <- "Text (should be numeric)"
      } else {
        if (any(xn != xs)) add("Low", v, "Some entries have leading/trailing spaces (e.g. 'Male ').", "Trim spaces so 'Male' and 'Male ' are not treated as different groups.", "trim")
        low <- tolower(gsub("\\s+", " ", xs))
        grp <- tapply(xs, low, function(z) unique(z))
        var_groups <- grp[vapply(grp, length, integer(1)) > 1]
        if (length(var_groups))
          add("High", v, paste0("Same category spelt differently: ", paste(vapply(head(var_groups, 4), function(z) paste0("{", paste(z, collapse = " | "), "}"), ""), collapse = "; ")),
              "Standardise spelling/case so each category is written one way only.", "case")
        tabx <- table(xs)
        if (nu > 1 && nu <= 15 && any(tabx < 5)) add("Low", v, paste0("Rare categories (<5 cases): ", paste(names(tabx)[tabx < 5], collapse = ", ")),
                                                     "Very small groups give unstable results; consider merging categories.")
        if (nu > 20 && nu > 0.5 * length(xn)) add("Info", v, "Many unique text values - looks like an ID / name / free-text column.",
                                                    "Remove personal identifiers before sharing data. This column will not be used as a group.")
      }
    }
    if (is.numeric(x) && length(xn) >= 5) {
      q <- quantile(xn, c(.25, .75)); iqr <- q[2] - q[1]
      out <- xn[xn < q[1] - 3 * iqr | xn > q[2] + 3 * iqr]
      if (length(out) && iqr > 0) add("Medium", v, paste0("Extreme values (beyond 3 x IQR): ", paste(head(unique(out), 8), collapse = ", ")),
                                      "Verify against case records - may be data-entry errors (e.g. 1200 instead of 120) or codes like 999. Do not delete true values.")
      if (any(xn %in% c(99, 999, 9999, -99, -999)) && max(xn) %in% c(99, 999, 9999))
        add("Medium", v, "Values like 99/999 found - often used as 'missing' codes.", "If these represent missing data, replace them with blank cells.")
      if (nu <= 6 && all(xn == round(xn))) type <- paste0("Numeric code (", nu, " values) - may be categorical")
    }
    summ[[v]] <- data.frame(Variable = v, Type = type, Non_missing = length(xn), Missing = miss,
                            `Missing %` = round(pm, 1), Unique = nu,
                            Example = paste(head(unique(as.character(xn)), 4), collapse = ", "),
                            check.names = FALSE, stringsAsFactors = FALSE)
  }
  iss <- if (length(issues)) do.call(rbind, issues) else
    data.frame(Severity = character(0), Column = character(0), Issue = character(0), Suggestion = character(0), Fix = character(0))
  sev_ord <- c(Critical = 1, High = 2, Medium = 3, Low = 4, Info = 5)
  iss <- iss[order(sev_ord[iss$Severity]), , drop = FALSE]
  list(issues = iss, summary = do.call(rbind, summ))
}

FIX_LABELS <- c(
  clean_names = "Make column names unique & tidy",
  drop_empty_rows = "Remove completely empty rows",
  drop_empty_cols = "Remove completely empty columns",
  drop_dup_rows = "Remove exact duplicate rows",
  na_codes = "Convert text missing-codes (NA, -, nil, ?) to blank",
  trim = "Trim leading/trailing spaces",
  case = "Standardise category spelling (case & spaces)",
  text_to_num = "Convert numbers-stored-as-text to numeric"
)

apply_fixes <- function(d, fixes) {
  log <- character(0)
  if ("drop_empty_rows" %in% fixes) {
    e <- apply(d, 1, function(r) all(is.na(r) | trimws(as.character(r)) == ""))
    d <- d[!e, , drop = FALSE]; log <- c(log, paste(sum(e), "empty rows removed"))
  }
  if ("drop_empty_cols" %in% fixes) {
    e <- vapply(d, function(x) all(is.na(x) | trimws(as.character(x)) == ""), logical(1))
    d <- d[, !e, drop = FALSE]; log <- c(log, paste(sum(e), "empty columns removed"))
  }
  if ("clean_names" %in% fixes) {
    nm <- names(d); nm[is.na(nm) | nm == "" | grepl("^\\.\\.\\.[0-9]+$", nm)] <- paste0("Var", which(is.na(nm) | nm == "" | grepl("^\\.\\.\\.[0-9]+$", nm)))
    nm <- gsub("[^A-Za-z0-9_]+", "_", trimws(nm)); nm <- gsub("^_+|_+$", "", nm)
    nm <- ifelse(grepl("^[0-9]", nm), paste0("X", nm), nm)
    names(d) <- make.unique(nm, sep = "_"); log <- c(log, "column names tidied")
  }
  for (v in names(d)) {
    x <- d[[v]]
    if (!is.character(x)) next
    if ("na_codes" %in% fixes) x[tolower(trimws(x)) %in% NA_CODES] <- NA
    if ("trim" %in% fixes || "case" %in% fixes) x <- trimws(gsub("\\s+", " ", x))
    if ("text_to_num" %in% fixes) {
      pn <- parse_num(x); nonmiss <- sum(!is.na(x) & !(tolower(trimws(x)) %in% NA_CODES))
      if (nonmiss > 0 && sum(!is.na(pn)) / nonmiss >= 0.8) { d[[v]] <- pn; log <- c(log, paste0("'", v, "' converted to numeric")); next }
    }
    if ("case" %in% fixes) {
      low <- tolower(x)
      # most frequent spelling wins
      tb <- table(x); best <- tapply(names(tb), tolower(names(tb)), function(z) z[which.max(tb[z])])
      x <- ifelse(is.na(x), NA, unname(best[low]))
    }
    d[[v]] <- x
  }
  if ("drop_dup_rows" %in% fixes) { du <- duplicated(d); d <- d[!du, , drop = FALSE]; log <- c(log, paste(sum(du), "duplicate rows removed")) }
  rownames(d) <- NULL
  attr(d, "fix_log") <- log
  d
}
