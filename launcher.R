# =====================================================================
#  MAMC BioStat launcher
#  - uses packages bundled with the installer (fully offline), or installs
#    any missing ones once (internet needed only in that case)
#  - opens the software in its own window (Edge / Chrome "app" window) or,
#    if neither is available, in the default web browser
# =====================================================================
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a) || identical(a, "")) b else a
args <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", args[grep("^--file=", args)][1] %||% "launcher.R")))
if (is.na(here) || !dir.exists(file.path(here, "app"))) here <- getwd()
options(repos = c(CRAN = "https://cloud.r-project.org"))

# 1. Libraries: bundled library (read-only is fine) + a writable user library for anything missing
bundled <- file.path(here, "library")
# packages bundled by the installer only work with the R version they were built for
rv_file <- file.path(bundled, "R_VERSION")
if (file.exists(rv_file)) {
  built_for <- trimws(readLines(rv_file, warn = FALSE)[1])
  if (!identical(built_for, paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1], sep = "."))) {
    message("  Bundled packages are for R ", built_for, "; this computer has R ", getRversion(), " - using a separate library.")
    bundled <- file.path(here, "__none__")
  }
}
user_lib <- file.path(tools::R_user_dir("MAMCBioStat", "data"), paste0("library-", getRversion()$major, ".", getRversion()$minor))
dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(unique(c(if (dir.exists(bundled)) bundled, user_lib, .libPaths())))

pkgs <- c("shiny", "readxl", "writexl", "DT", "ggplot2", "bslib", "jsonlite", "survival")
loads <- function(p) suppressWarnings(suppressMessages(requireNamespace(p, quietly = TRUE)))

# Whole dependency tree: a computer may already have some packages with a missing dependency (e.g. httpuv without Rcpp)
needed_tree <- function() {
  w <- c("Depends", "Imports", "LinkingTo")
  idb <- utils::installed.packages()
  local <- unlist(tools::package_dependencies(intersect(pkgs, rownames(idb)), db = idb, which = w, recursive = TRUE))
  cdb <- tryCatch(suppressWarnings(utils::available.packages()), error = function(e) NULL)
  remote <- if (!is.null(cdb) && nrow(cdb)) unlist(tools::package_dependencies(pkgs, db = cdb, which = w, recursive = TRUE)) else character(0)
  base <- rownames(utils::installed.packages(priority = "base"))
  setdiff(unique(c(pkgs, local, remote)), c(base, "R"))
}

if (!all(vapply(pkgs, loads, logical(1)))) {
  message("\n  First run: setting up MAMC BioStat (internet needed once, about 2-5 minutes) ...\n")
  for (attempt in 1:2) {
    tree <- needed_tree()
    inst <- rownames(utils::installed.packages())
    todo <- union(tree[!(tree %in% inst)], pkgs[!vapply(pkgs, loads, logical(1))])
    if (!length(todo)) break
    message("  Installing: ", paste(todo, collapse = ", "))
    utils::install.packages(todo, lib = user_lib, dependencies = c("Depends", "Imports", "LinkingTo"),
                            type = if (.Platform$OS.type == "windows" || Sys.info()[["sysname"]] == "Darwin") "binary" else getOption("pkgType"))
  }
  bad <- pkgs[!vapply(pkgs, loads, logical(1))]
  if (length(bad)) {
    utils::install.packages(unique(c(bad, unlist(tools::package_dependencies(bad, recursive = TRUE)))), lib = user_lib)
    bad <- pkgs[!vapply(pkgs, loads, logical(1))]
  }
  if (length(bad)) stop("Could not set up: ", paste(bad, collapse = ", "),
                        ". Check the internet connection (or college proxy/firewall) and run the launcher again.", call. = FALSE)
  message("\n  Setup complete.\n")
}

# 2. Open in its own window if possible
find_app_browser <- function() {
  os <- Sys.info()[["sysname"]]
  cand <- if (os == "Windows") {
    pf <- c(Sys.getenv("ProgramFiles(x86)"), Sys.getenv("ProgramFiles"), Sys.getenv("LOCALAPPDATA"))
    c(file.path(pf, "Microsoft", "Edge", "Application", "msedge.exe"), file.path(pf, "Google", "Chrome", "Application", "chrome.exe"))
  } else if (os == "Darwin") {
    c("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome", "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge")
  } else c(Sys.which("google-chrome"), Sys.which("chromium"), Sys.which("chromium-browser"), Sys.which("microsoft-edge"))
  cand <- cand[nzchar(cand) & file.exists(cand)]
  if (length(cand)) cand[1] else NA
}
open_window <- function(url) {
  if (identical(Sys.getenv("MAMC_NO_BROWSER"), "1")) return(invisible())
  b <- find_app_browser()
  prof <- file.path(tools::R_user_dir("MAMCBioStat", "cache"), "window")
  dir.create(prof, recursive = TRUE, showWarnings = FALSE)
  if (!is.na(b)) {
    ok <- tryCatch({ system2(b, c(paste0("--app=", url), paste0("--user-data-dir=", shQuote(prof)), "--window-size=1400,900", "--no-first-run"), wait = FALSE); TRUE },
                   error = function(e) FALSE)
    if (ok) return(invisible())
  }
  utils::browseURL(url)
}

port <- as.integer(Sys.getenv("MAMC_PORT", httpuv::randomPort()))
url <- paste0("http://127.0.0.1:", port)
Sys.setenv(MAMC_DESKTOP = Sys.getenv("MAMC_DESKTOP", "1"))
message("\n  MAMC BioStat is running at ", url, "\n  (It closes automatically when you close its window.)\n")
shiny::runApp(file.path(here, "app"), port = port, host = "127.0.0.1", launch.browser = function(u) open_window(url))
