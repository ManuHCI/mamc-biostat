# =====================================================================
#  MAMC BioStat
#  Free & open-source biostatistics software for UG and PG students
#  Developed by the faculty of Maulana Azad Medical College & Lok Nayak Hospital, New Delhi
#  Licence: GNU GPL-3.0   |   Engine: R
# =====================================================================
APP_VERSION <- "1.1.4"
suppressPackageStartupMessages({ library(shiny); library(readxl); library(DT); library(ggplot2) })
options(shiny.maxRequestSize = 100 * 1024^2, warn = 1)

for (f in c("helpers.R", "data_check.R", "tests_descriptive.R", "tests_compare.R", "tests_nonpar.R", "tests_categorical.R",
            "tests_correlation.R", "tests_agreement.R", "tests_survival_ss.R", "wizard.R", "visualize.R"))
  source(file.path("R", f), local = FALSE)

# ---- help library ----------------------------------------------------
load_help <- function() {
  txt <- paste(unlist(lapply(sort(list.files("help", pattern = "\\.html$", full.names = TRUE)), function(f) readLines(f, warn = FALSE, encoding = "UTF-8"))), collapse = "\n")
  parts <- strsplit(txt, "<!-- TEST:")[[1]][-1]
  setNames(lapply(parts, function(p) sub("^[a-z0-9_]+ -->", "", p)), sub(" -->.*$", "", substr(parts, 1, 40)))
}
HELP <- load_help()
help_for <- function(id) HELP[[id]] %||% "<p>Help for this test is being prepared.</p>"

df_html <- function(df, caption = NULL) {
  df <- round_df(as.data.frame(df, check.names = FALSE), 3)
  hd <- paste0("<tr>", paste0("<th>", names(df), "</th>", collapse = ""), "</tr>")
  rows <- apply(df, 1, function(r) paste0("<tr>", paste0("<td>", ifelse(is.na(r) | r == "NA", "", r), "</td>", collapse = ""), "</tr>"))
  paste0(if (!is.null(caption)) paste0("<div class='tab-caption'>", caption, "</div>") else "", "<div style='overflow-x:auto'><table class='rtab'>", hd, paste(rows, collapse = ""), "</table></div>")
}

test_choices <- function(cat) { ids <- TEST_ORDER[vapply(TEST_ORDER, function(i) get_test(i)$cat == cat, logical(1))]
  setNames(ids, vapply(ids, function(i) get_test(i)$name, "")) }

brand <- tags$span(class = "brand-mark", tags$img(src = "mamc_logo.png", height = "38px", alt = ""), "MAMC BioStat")

# =====================================================================
ui <- tagList(
  tags$head(tags$link(rel = "stylesheet", href = "katex/katex.min.css"), tags$link(rel = "stylesheet", href = "style.css"),
            tags$script(src = "katex/katex.min.js"), tags$script(src = "katex/contrib/auto-render.min.js"), tags$script(src = "app.js"),
            tags$title("MAMC BioStat"), tags$link(rel = "icon", type = "image/png", href = "mamc_logo.png")),
  div(class = "corner-badge", title = "MAMC BioStat - Maulana Azad Medical College, New Delhi - Free & Open Source", tags$img(src = "mamc_logo.png", alt = "MAMC"), span(class = "cb-text", HTML("&copy; Maulana Azad Medical College, New Delhi &middot; Free &amp; Open Source"))),
  navbarPage(title = brand, id = "nav", windowTitle = "MAMC BioStat", collapsible = TRUE,
    theme = bslib::bs_theme(version = 5, primary = "#7b1c2e", base_font = bslib::font_collection("Segoe UI", "Helvetica Neue", "Arial", "sans-serif")),

    # ---------------- HOME ----------------
    tabPanel("Home", icon = icon("house"),
      div(class = "container-fluid", style = "max-width:1200px",
        div(class = "hero2",
          tags$img(class = "hero-logo", src = "mamc_logo.png", alt = "Maulana Azad Medical College logo"),
          div(class = "hero-text",
            div(class = "hero-kicker", "Biostatistics for medical students & researchers"),
            h1(HTML("MAMC <span>BioStat</span>")),
            div(class = "hero-inst", "Maulana Azad Medical College & Lok Nayak Hospital, New Delhi"),
            p(class = "hero-tag", "Analyse your thesis data correctly, understand every step, and report it with confidence."),
            div(class = "pills", span(class = "pill", paste(length(TEST_ORDER), "statistical tests")), span(class = "pill", paste(length(CHARTS), "chart types")),
                span(class = "pill", "Step-by-step formulas"), span(class = "pill", "Works offline"), span(class = "pill", "Free & open source"))),
          div(class = "hero-cta",
            actionButton("go_data", HTML("&#9654;&nbsp; Start: Load my Excel data"), class = "btn-primary btn-cta"),
            actionButton("demo_home", "Try with demo data", class = "btn-outline-primary btn-cta"),
            actionButton("go_wizard", "Which test should I use?", class = "btn-outline-primary btn-cta"))),
        div(class = "steps4",
          div(class = "card4", div(class = "ic", style = "background:#e8f0fb", icon("file-import")), div(class = "num", "STEP 1"), h4("Load & clean data"), p("Import Excel/CSV. The Data Health Check finds errors and shows how to fix them.")),
          div(class = "card4", div(class = "ic", style = "background:#fdeee6", icon("compass")), div(class = "num", "STEP 2"), h4("Choose the right test"), p("Answer 4-5 simple questions; the wizard recommends the correct test.")),
          div(class = "card4", div(class = "ic", style = "background:#e6f6ef", icon("calculator")), div(class = "num", "STEP 3"), h4("Analyse & learn"), p("Results, plain-English interpretation, worked formula with your numbers, thesis sentence.")),
          div(class = "card4", div(class = "ic", style = "background:#fdf4df", icon("chart-column")), div(class = "num", "STEP 4"), h4("Visualise & report"), p("Publication-quality charts (300 dpi) and a complete downloadable report."))),
        div(class = "row2",
          div(class = "box2", h3("Before you start - prepare your Excel sheet"), tags$ul(
            tags$li("Row 1 = short column names (Age, Sex, SBP_baseline); no merged cells"),
            tags$li("One row per patient, one column per variable; leave missing cells blank"),
            tags$li("Write each category the same way (Male / Female)"),
            tags$li("Repeated measurements in separate columns (SBP_week0, SBP_week4 ...)")),
            actionLink("demo_messy", "See an example of messy data and how it is cleaned \u2192")),
          div(class = "box2", h3("Learn statistics"), p(class = "quote", "Every test explained - when to use it, its assumptions, the formula, and why it works."),
            tags$ul(tags$li("Statistics basics: p-value, CI, errors, power"), tags$li(paste(length(TEST_ORDER), "illustrated help pages"))),
            actionLink("go_learn", "Open the Learn section \u2192"))),
        p(class = "home-foot", paste0("Version ", APP_VERSION, " \u00b7 Powered by R \u00b7 Your data never leave your computer")))),

    # ---------------- DATA ----------------
    tabPanel("1. Data", icon = icon("table"),
      sidebarLayout(
        sidebarPanel(width = 3, class = "sidebar-box",
          fileInput("file", "Import Excel / CSV file", accept = c(".xlsx", ".xls", ".csv", ".txt")),
          uiOutput("sheet_ui"), numericInput("skip", "Rows to skip above header", 0, min = 0, step = 1),
          actionButton("demo", "Load demo data", class = "btn-outline-secondary btn-sm"), hr(),
          h5("Variable type corrections"),
          helpText("Numeric codes (0/1, 1/2/3) that represent categories should be treated as categorical."),
          uiOutput("type_ui"), actionButton("apply_types", "Apply type changes", class = "btn-sm btn-outline-primary"), hr(),
          downloadButton("dl_clean", "Download current (cleaned) data (.xlsx)", class = "btn-sm")),
        mainPanel(width = 9,
          tabsetPanel(id = "data_tabs",
            tabPanel("Data Health Check", br(), uiOutput("verdict"), br(), uiOutput("issues_ui"), uiOutput("fix_ui"), h4("Variable summary"), DTOutput("var_summary")),
            tabPanel("View data", br(), DTOutput("data_view")))))),

    # ---------------- WIZARD ----------------
    tabPanel("2. Choose Test", icon = icon("signs-post"),
      div(class = "container-fluid", style = "max-width:1150px", h3("Which statistical test should I use?"),
        p("Answer the questions - the recommendation appears on the right."),
        fluidRow(column(7, wizard_ui()), column(5, div(style = "position:sticky;top:70px", uiOutput("wz_result")))))),

    # ---------------- ANALYSE ----------------
    tabPanel("3. Analyse", icon = icon("calculator"),
      sidebarLayout(
        sidebarPanel(width = 4, class = "sidebar-box",
          selectInput("cat", "Category", choices = CATEGORIES),
          selectInput("test", "Statistical test", choices = test_choices(CATEGORIES[1])),
          uiOutput("test_need"), hr(), h5("Select what is required"), uiOutput("inputs_ui"),
          numericInput("alpha", "Significance level (α)", 0.05, min = 0.001, max = 0.2, step = 0.01),
          actionButton("run", "Run analysis", class = "btn-primary btn-lg w-100", icon = icon("play"))),
        mainPanel(width = 8,
          uiOutput("test_header"),
          tabsetPanel(id = "res_tabs",
            tabPanel("Results", br(), uiOutput("results_ui")),
            tabPanel("Plot", br(), plotOutput("res_plot", height = "540px"), downloadButton("dl_plot", "Download plot (PNG, 300 dpi)", class = "btn-sm")),
            tabPanel("Step-by-step calculation", br(), uiOutput("steps_ui")),
            tabPanel("Help: background & formula", br(), div(class = "help-body", uiOutput("help_ui"))))))),

    # ---------------- VISUALISE ----------------
    tabPanel("4. Visualise", icon = icon("chart-column"),
      sidebarLayout(
        sidebarPanel(width = 3, class = "sidebar-box",
          selectInput("v_chart", "Chart type", choices = setNames(names(CHARTS), vapply(CHARTS, `[[`, "", "name"))),
          uiOutput("v_inputs"), hr(), h5("Labels & style"),
          textInput("v_title", "Title (blank = automatic)"), textInput("v_sub", "Subtitle"),
          textInput("v_xlab", "X-axis label"), textInput("v_ylab", "Y-axis label"),
          selectInput("v_pal", "Colour palette", names(PALETTES)),
          checkboxInput("v_labels", "Show value labels on chart", TRUE),
          sliderInput("v_base", "Font size", 9, 22, 13), selectInput("v_legend", "Legend position", c("right", "bottom", "top", "left", "none")),
          numericInput("v_w", "Export width (inches)", 8, 3, 20), numericInput("v_h", "Export height (inches)", 5.5, 3, 20)),
        mainPanel(width = 9,
          tabsetPanel(
            tabPanel("Chart", br(), plotOutput("v_plot", height = "580px"),
              downloadButton("v_png", "PNG (300 dpi)", class = "btn-sm"), downloadButton("v_pdf", "PDF (vector)", class = "btn-sm"), downloadButton("v_tiff", "TIFF (journals)", class = "btn-sm")),
            tabPanel("Python code for this chart", br(),
              p("The same chart in Python (pandas + seaborn + matplotlib). Change the file name in the pd.read_excel(...) line to your own file, then run it in Python / Jupyter / Spyder."),
              downloadButton("v_py", "Download .py file", class = "btn-sm"), br(), br(), uiOutput("v_code")))))),

    # ---------------- REPORT ----------------
    tabPanel("Report", icon = icon("file-lines"),
      div(class = "container-fluid", style = "max-width:1150px",
        h3("Analysis report"), p("Every analysis you run is added here automatically."),
        downloadButton("dl_report", "Download full report (HTML - open in any browser, copy into Word)", class = "btn-primary"), " ",
        downloadButton("dl_tables", "Download all result tables (Excel)"), " ", actionButton("clear_report", "Clear report", class = "btn-outline-danger"),
        hr(), uiOutput("report_ui"))),

    # ---------------- LEARN ----------------
    tabPanel("Learn", icon = icon("graduation-cap"),
      sidebarLayout(sidebarPanel(width = 3, class = "sidebar-box",
          selectInput("learn_topic", "Topic", choices = c(`Statistics basics (start here)` = "basics", setNames(TEST_ORDER, vapply(TEST_ORDER, function(i) get_test(i)$name, ""))), size = 22, selectize = FALSE)),
        mainPanel(width = 9, div(class = "panel-box help-body", uiOutput("learn_ui"))))),

    # ---------------- ABOUT ----------------
    tabPanel("About", icon = icon("circle-info"),
      div(class = "container-fluid", style = "max-width:1200px",
        div(class = "about-head",
          tags$img(class = "l", src = "mamc_logo.png", alt = "MAMC logo"), tags$img(class = "r", src = "mic_logo.png", alt = "MAMC Medical Innovation Centre logo"),
          h1(HTML("MAMC <span>BioStat</span>")),
          div(class = "hero-inst", "Maulana Azad Medical College & Lok Nayak Hospital, University of Delhi, New Delhi"),
          p(class = "about-sub", paste0("Free, open-source biostatistics software for medical students, residents and researchers \u00b7 Version ", APP_VERSION))),
        div(class = "grid2",
          div(class = "box2", h3("Free & open source"), p(HTML("Released under the <b>GNU General Public License v3.0</b>. Anyone may use, study, share and improve it. It runs entirely on your computer &ndash; no data ever leaves your laptop."))),
          div(class = "box2", h3("Why R?"), p("All calculations use R, the open-source language used by statisticians worldwide and accepted by journals and regulators. Results match SPSS / Stata / GraphPad for the same methods."))),
        div(class = "grid2",
          div(class = "box2", h3("How to cite"), tags$code(class = "cite", paste0("MAMC BioStat (Version ", APP_VERSION, ") [Computer software]. Maulana Azad Medical College, New Delhi; ", format(Sys.Date(), "%Y"), ". Available from: https://github.com/ManuHCI/mamc-biostat"))),
          div(class = "box2", h3("Disclaimer"), p("MAMC BioStat is an educational and research aid. Interpretations are generated automatically - the investigator remains responsible for choosing appropriate methods. Consult a biostatistician for complex designs."))),
        p(class = "home-foot", paste0("Built with R ", getRversion(), ", shiny, readxl, writexl, DT, ggplot2, survival, bslib \u00b7 KaTeX for formula display")),
        div(class = "contrib-box",
          div(class = "lbl2", "Contributed by"),
          div(class = "names",
            div(tags$b("Dr. Aashima Dabas"), "Professor, Department of Paediatrics"),
            div(tags$b("Dr. Manu Kumar Shetty"), "Professor, Department of Pharmacology")),
          div(class = "mic-line", "Medical Innovation Centre, Maulana Azad Medical College, New Delhi"))))
  )
)

# =====================================================================
DESKTOP <- new.env(); DESKTOP$n <- 0
server <- function(input, output, session) {
  # Desktop mode: quit R automatically a few seconds after the last window is closed
  DESKTOP$n <- DESKTOP$n + 1
  Sys.setenv(MAMC_CONNECTED = "1")
  session$onSessionEnded(function() {
    DESKTOP$n <- DESKTOP$n - 1
    if (identical(Sys.getenv("MAMC_DESKTOP"), "1"))
      later::later(function() if (DESKTOP$n <= 0) stopApp(), 8)
  })
  rv <- reactiveValues(data = NULL, name = NULL, res = NULL, report = list())

  load_into <- function(d, nm) { rv$data <- d; rv$name <- nm; updateTabsetPanel(session, "data_tabs", "Data Health Check")
    showNotification(paste0("Loaded '", nm, "': ", nrow(d), " rows x ", ncol(d), " columns"), type = "message") }
  demo_load <- function(messy = FALSE) {
    f <- if (messy) "sample_data/MAMC_demo_messy_data.xlsx" else "sample_data/MAMC_demo_clinical_trial.xlsx"
    load_into(read_data(f, f), basename(f)); updateNavbarPage(session, "nav", "1. Data") }
  observeEvent(input$demo, demo_load()); observeEvent(input$demo_home, demo_load()); observeEvent(input$demo_messy, demo_load(TRUE))
  observeEvent(input$go_data, updateNavbarPage(session, "nav", "1. Data"))
  observeEvent(input$go_wizard, updateNavbarPage(session, "nav", "2. Choose Test"))
  observeEvent(input$go_learn, updateNavbarPage(session, "nav", "Learn"))

  output$sheet_ui <- renderUI({ req(input$file); ext <- tolower(tools::file_ext(input$file$name))
    if (ext %in% c("xlsx", "xls", "xlsm")) selectInput("sheet", "Sheet", choices = readxl::excel_sheets(input$file$datapath)) })
  observe({
    req(input$file); ext <- tolower(tools::file_ext(input$file$name)); sh <- if (ext %in% c("xlsx", "xls", "xlsm")) input$sheet else 1
    if (ext %in% c("xlsx", "xls", "xlsm")) req(sh)
    d <- tryCatch(read_data(input$file$datapath, input$file$name, sheet = sh, skip = input$skip %||% 0), error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 10); NULL })
    if (!is.null(d)) isolate(load_into(d, input$file$name))
  })

  check <- reactive({ req(rv$data); check_data(rv$data) })
  output$verdict <- renderUI({
    if (is.null(rv$data)) return(div(class = "notebox", "No data loaded yet. Import an Excel/CSV file on the left, or click 'Load demo data'."))
    iss <- check()$issues; serious <- sum(iss$Severity %in% c("Critical", "High")); med <- sum(iss$Severity == "Medium")
    if (serious == 0 && med == 0) div(class = "verdict-ok", HTML(paste0("&#10004; <b>Data look clean and ready for analysis</b> - ", nrow(rv$data), " rows, ", ncol(rv$data), " variables.", if (nrow(iss)) paste0(" (", nrow(iss), " minor note(s) below.)") else "")))
    else if (serious == 0) div(class = "verdict-ok", style = "border-left-color:#e0a000;background:#fff8e6", HTML(paste0("&#10004; <b>Data are usable</b> - ", nrow(rv$data), " rows, ", ncol(rv$data), " variables. Please verify the ", med, " item(s) marked Medium below (e.g. extreme values) against your records.")))
    else div(class = "verdict-bad", HTML(paste0("&#9888; <b>", serious, " issue(s) need attention before analysis.</b> Read the suggestions below. You can fix them in Excel, or let MAMC BioStat apply the safe automatic fixes.")))
  })
  output$issues_ui <- renderUI({ req(rv$data); iss <- check()$issues; if (!nrow(iss)) return(NULL)
    rows <- paste0("<tr><td class='sev-", iss$Severity, "'>", iss$Severity, "</td><td>", htmltools::htmlEscape(iss$Column), "</td><td>", htmltools::htmlEscape(iss$Issue), "</td><td>", htmltools::htmlEscape(iss$Suggestion), "</td><td>",
                   ifelse(is.na(iss$Fix), "<i>manual</i>", "&#10003; auto-fix available"), "</td></tr>", collapse = "")
    HTML(paste0("<h4>Issues found & how to clean</h4><div style='overflow-x:auto'><table class='rtab'><tr><th>Severity</th><th>Column</th><th>Problem</th><th>What to do</th><th>Fix</th></tr>", rows, "</table></div>")) })
  output$fix_ui <- renderUI({ req(rv$data); fx <- unique(na.omit(check()$issues$Fix)); if (!length(fx)) return(NULL)
    div(class = "panel-box", checkboxGroupInput("fixes", "Apply automatic fixes (your original file is never changed):", choices = setNames(fx, FIX_LABELS[fx]), selected = setdiff(fx, "drop_dup_rows")),
        helpText("Duplicate-row removal is not pre-selected: confirm they are true duplicates first."), actionButton("apply_fix", "Apply selected fixes", class = "btn-primary", icon = icon("wand-magic-sparkles"))) })
  observeEvent(input$apply_fix, { req(rv$data, input$fixes); d <- apply_fixes(rv$data, input$fixes); rv$data <- d
    showNotification(HTML(paste0("Done: ", paste(attr(d, "fix_log"), collapse = "; "))), type = "message", duration = 8) })
  output$var_summary <- renderDT({ req(rv$data); datatable(check()$summary, rownames = FALSE, options = list(pageLength = 50, dom = "ft", scrollX = TRUE)) })
  output$data_view <- renderDT({ req(rv$data); datatable(rv$data, options = list(pageLength = 15, scrollX = TRUE), filter = "top") })
  output$type_ui <- renderUI({ req(rv$data)
    tagList(selectizeInput("to_cat", "Treat as categorical:", choices = num_candidates(rv$data), multiple = TRUE),
            selectizeInput("to_num", "Treat as numeric:", choices = setdiff(names(rv$data), num_candidates(rv$data)), multiple = TRUE)) })
  observeEvent(input$apply_types, { req(rv$data); d <- rv$data
    for (v in input$to_cat) d[[v]] <- as.character(d[[v]]); for (v in input$to_num) d[[v]] <- parse_num(d[[v]]); rv$data <- d
    showNotification("Variable types updated", type = "message") })
  output$dl_clean <- downloadHandler(filename = function() paste0("cleaned_", sub("\\.[^.]+$", "", rv$name %||% "data"), ".xlsx"),
                                     content = function(file) writexl::write_xlsx(rv$data, file))

  # ---------------- wizard ----------------
  output$wz_norm_vars <- renderUI({ if (is.null(rv$data)) return(helpText("Load data first, then pick the outcome to check."))
    tagList(selectInput("wz_nv", "Outcome variable", num_candidates(rv$data)), selectInput("wz_ng_var", "Group variable (optional)", c("(none)", cat_candidates(rv$data)))) })
  observeEvent(input$wz_check_norm, { req(rv$data, input$wz_nv)
    sets <- if (input$wz_ng_var != "(none)") split(rv$data[[input$wz_nv]], rv$data[[input$wz_ng_var]]) else list(All = rv$data[[input$wz_nv]])
    ps <- sapply(sets, shapiro_safe); ns <- sapply(sets, function(z) sum(!is.na(z)))
    verdict <- if (any(ps < 0.05, na.rm = TRUE) && any(ns < 30)) "<b>Not normal</b> &rarr; answer <b>No</b>" else if (any(ps < 0.05, na.rm = TRUE)) "Shapiro-Wilk is significant, but every group has n &ge; 30, so parametric tests are usually acceptable (answer <b>Yes</b>), or choose No to be conservative." else "<b>Approximately normal</b> &rarr; answer <b>Yes</b>"
    output$wz_norm_res <- renderUI(HTML(paste0("<br>", paste0(names(ps), ": n = ", ns, ", Shapiro-Wilk ", p_eq(ps), collapse = "<br>"), "<br>", verdict))) })
  wz <- reactive({ i <- reactiveValuesToList(input); wizard_reco(i[grep("^wz_", names(i))]) })
  output$wz_result <- renderUI({ r <- wz()
    if (is.null(r)) return(div(class = "panel-box", h4("Recommendation"), p("Answer the questions on the left...")))
    if (is.null(r$id)) return(div(class = "panel-box", HTML(r$why)))
    t <- get_test(r$id)
    div(class = "reco", p("Recommended test:"), h3(t$name), HTML(paste0("<p>", r$why, "</p>")),
        if (length(r$alt)) HTML(paste0("<p><b>Also consider:</b> ", paste(vapply(r$alt, function(a) get_test(a)$name, ""), collapse = "; "), "</p>")),
        actionButton("wz_go", "Open this test", class = "btn-primary", icon = icon("arrow-right")), " ",
        actionButton("wz_help", "Read about it", class = "btn-outline-secondary")) })
  goto_test <- function(id) { t <- get_test(id); updateSelectInput(session, "cat", selected = t$cat)
    session$userData$pending_test <- id; updateSelectInput(session, "test", choices = test_choices(t$cat), selected = id); updateNavbarPage(session, "nav", "3. Analyse") }
  observeEvent(input$wz_go, goto_test(wz()$id))
  observeEvent(input$wz_help, { updateSelectInput(session, "learn_topic", selected = wz()$id); updateNavbarPage(session, "nav", "Learn") })

  # ---------------- analyse ----------------
  observeEvent(input$cat, { pend <- session$userData$pending_test
    ch <- test_choices(input$cat); sel <- if (!is.null(pend) && pend %in% ch) pend else ch[1]; session$userData$pending_test <- NULL
    updateSelectInput(session, "test", choices = ch, selected = sel) }, ignoreInit = TRUE)
  cur_test <- reactive({ req(input$test); get_test(input$test) })

  output$test_need <- renderUI({ t <- cur_test(); if (t$needs_data && is.null(rv$data)) div(class = "warnbox", "Load data first (tab 1. Data).") })
  output$inputs_ui <- renderUI({
    t <- cur_test(); d <- rv$data
    if (t$needs_data && is.null(d)) return(NULL)
    nums <- if (!is.null(d)) num_candidates(d) else character(0); cats <- if (!is.null(d)) cat_candidates(d) else character(0); alln <- if (!is.null(d)) names(d) else character(0)
    used <- list(num = 0, cat = 0)
    pick <- function(pool, k) { if (!length(pool)) return(NULL); pool[min(k, length(pool))] }
    lapply(t$inputs, function(sp) {
      id <- paste0("in_", sp$id)
      switch(sp$type,
        num = { used$num <<- used$num + 1; selectInput(id, sp$label, nums, selected = pick(nums, used$num)) },
        cat = { used$cat <<- used$cat + 1; nl <- vapply(cats, function(v) length(unique(na.omit(d[[v]]))), 1)
          pref <- if (grepl("2 groups|2 categories", sp$label)) cats[nl == 2] else if (grepl("3", sp$label)) cats[nl >= 3] else cats
          pool <- c(pref, setdiff(cats, pref)); selectInput(id, sp$label, cats, selected = pick(pool, used$cat)) },
        cat_opt = selectInput(id, sp$label, c("(none)", cats)),
        num_multi = selectizeInput(id, sp$label, nums, multiple = TRUE, options = list(plugins = list("remove_button"))),
        any_multi = selectizeInput(id, sp$label, alln, multiple = TRUE, options = list(plugins = list("remove_button"))),
        value = numericInput(id, sp$label, sp$default),
        text = textInput(id, sp$label, sp$default %||% ""),
        select = selectInput(id, sp$label, sp$choices),
        level = uiOutput(paste0("lvl_", sp$id)))
    })
  })
  observe({ t <- cur_test(); d <- rv$data
    for (sp in t$inputs) if (sp$type == "level") local({ s <- sp
      output[[paste0("lvl_", s$id)]] <- renderUI({ v <- input[[paste0("in_", s$of)]]; req(v, d, v %in% names(d))
        lv <- sort(unique(as.character(na.omit(rv$data[[v]]))))
        guess <- lv[tolower(lv) %in% c("yes", "1", "present", "positive", "died", "dead", "event", "case", "true")]
        selectInput(paste0("in_", s$id), s$label, lv, selected = if (length(guess)) guess[1] else lv[length(lv)]) }) }) })

  output$test_header <- renderUI({ t <- cur_test()
    div(h3(class = "test-title", t$name, span(class = "badge-level", t$level)), p(style = "color:#5d5c58", t$cat)) })
  output$help_ui <- renderUI(HTML(help_for(cur_test()$id)))

  observeEvent(input$test, { rv$res <- NULL })
  observeEvent(input$run, {
    t <- cur_test(); if (t$needs_data && is.null(rv$data)) { showNotification("Please load data first.", type = "error"); return() }
    a <- lapply(t$inputs, function(sp) input[[paste0("in_", sp$id)]]); names(a) <- vapply(t$inputs, `[[`, "", "id"); a$alpha <- input$alpha
    miss <- vapply(t$inputs, function(sp) is.null(a[[sp$id]]) || (length(a[[sp$id]]) == 1 && is.na(a[[sp$id]])), logical(1))
    if (any(miss)) { showNotification(paste("Please fill in:", paste(vapply(t$inputs[miss], `[[`, "", "label"), collapse = "; ")), type = "error", duration = 8); return() }
    res <- withCallingHandlers(tryCatch(t$run(rv$data, a), error = function(e) list(error = conditionMessage(e))), warning = function(w) invokeRestart("muffleWarning"))
    res$test <- t; res$args <- a; res$time <- format(Sys.time(), "%d %b %Y %H:%M")
    rv$res <- res; updateTabsetPanel(session, "res_tabs", "Results")
    if (is.null(res$error)) {
      img <- NULL
      if (!is.null(res$plot)) { tf <- tempfile(fileext = ".png"); png(tf, 1800, 1150, res = 170); try(res$plot(), silent = TRUE); dev.off(); img <- base64enc_file(tf) }
      rv$report[[length(rv$report) + 1]] <- list(name = t$name, time = res$time, args = a, html = res$html, tables = res$tables, steps = res$steps, img = img)
    }
  })
  output$results_ui <- renderUI({
    r <- rv$res
    if (is.null(r)) return(div(class = "notebox", "Select variables on the left and click ", tags$b("Run analysis"), ". New to this test? Read the 'Help' tab first."))
    if (!is.null(r$error)) return(div(class = "warnbox", HTML(paste0("<b>Could not run the test:</b> ", htmltools::htmlEscape(r$error)))))
    vars <- paste0(names(r$args)[names(r$args) != "alpha"], " = ", vapply(r$args[names(r$args) != "alpha"], function(v) paste(v, collapse = ", "), ""), collapse = "; ")
    HTML(paste0("<p style='color:#5d5c58;font-size:13px'>Inputs: ", htmltools::htmlEscape(vars), " | &alpha; = ", r$args$alpha, "</p>", r$html,
                paste(mapply(df_html, r$tables, names(r$tables)), collapse = "")))
  })
  output$steps_ui <- renderUI({ r <- rv$res; if (is.null(r) || !is.null(r$error)) return(p("Run an analysis to see the worked calculation with your numbers."))
    HTML(paste0("<div class='panel-box'><h4>How the result was calculated (with your data)</h4>", r$steps, "</div>")) })
  output$res_plot <- renderPlot({ r <- rv$res; req(r, is.null(r$error), r$plot); r$plot() }, res = 96)
  output$dl_plot <- downloadHandler(filename = function() paste0("MAMC_", rv$res$test$id, "_plot.png"),
    content = function(file) { png(file, width = 3000, height = 2000, res = 300); rv$res$plot(); dev.off() })

  # ---------------- visualise ----------------
  output$v_inputs <- renderUI({
    d <- rv$data; if (is.null(d)) return(div(class = "warnbox", "Load data first (tab 1. Data)."))
    nums <- num_candidates(d); cats <- cat_candidates(d); ins <- CHARTS[[input$v_chart]]$inputs; ch <- input$v_chart
    tagList(
      if ("x_num" %in% ins) selectInput("v_x", if (ch == "scatter") "X variable (numeric)" else "Variable (numeric)", nums),
      if ("y_num" %in% ins) selectInput("v_y", if (ch == "scatter") "Y variable (numeric)" else "Numeric variable (Y axis)", nums, selected = nums[min(2, length(nums))]),
      if ("x_cat" %in% ins) selectInput("v_xc", "Group on X axis (categorical)", if (ch %in% c("boxplot", "violin")) c("(none)", cats) else cats, selected = if (length(cats)) cats[1]),
      if (ch %in% c("bar_count", "pie")) selectInput("v_xc", "Categorical variable", cats),
      if ("vars" %in% ins) selectizeInput("v_vars", if (ch == "line_time") "Repeated columns (in time order)" else "Numeric variables", nums, multiple = TRUE, selected = head(nums, 4), options = list(plugins = list("remove_button"))),
      if ("group" %in% ins) selectInput("v_group", if (ch %in% c("bar_count", "mean_error")) "Split / colour by (optional)" else "Colour by group (optional)", c("(none)", cats)),
      if (ch %in% c("mean_error", "line_time")) selectInput("v_err", "Error bars", c("95% CI", "SE", "SD")),
      if (ch == "mean_error") selectInput("v_style", "Style", c("Bar + error bar", "Dot + error bar")),
      if (ch == "bar_count") selectInput("v_style", "Bar layout (with split)", c("Grouped", "Stacked", "Stacked 100%")),
      if (ch == "pie") selectInput("v_style", "Style", c("Donut", "Pie")),
      if (ch == "histogram") sliderInput("v_bins", "Number of bins", 5, 60, 15),
      if (ch %in% c("boxplot", "violin")) checkboxInput("v_points", "Show individual data points", TRUE),
      if (ch %in% c("histogram", "scatter")) checkboxInput("v_line", if (ch == "scatter") "Add regression line with 95% CI" else "Overlay normal curve", TRUE),
      if (ch == "heatmap") selectInput("v_method", "Correlation", c("pearson", "spearman")))
  })
  v_opts <- reactive({ req(rv$data, input$v_chart); ins <- CHARTS[[input$v_chart]]$inputs
    if ("x_num" %in% ins) req(input$v_x); if ("y_num" %in% ins) req(input$v_y); if ("vars" %in% ins) req(length(input$v_vars) >= 2)
    if ("x_cat" %in% ins || input$v_chart %in% c("bar_count", "pie")) req(input$v_xc)
    list(chart = input$v_chart, x = input$v_x, y = input$v_y, xc = input$v_xc, group = input$v_group %||% "(none)", vars = input$v_vars,
         title = input$v_title, subtitle = input$v_sub, xlab = input$v_xlab, ylab = input$v_ylab, palette = input$v_pal, labels = input$v_labels,
         base = input$v_base, legend = input$v_legend, err = input$v_err %||% "95% CI", bins = input$v_bins %||% 15, points = input$v_points %||% TRUE,
         line = input$v_line %||% TRUE, style = input$v_style %||% "Grouped", method = input$v_method %||% "pearson") })
  v_plot_obj <- reactive({ o <- v_opts(); tryCatch(build_chart(rv$data, o), error = function(e) { validate(need(FALSE, paste("Cannot draw this chart:", conditionMessage(e)))) }) })
  output$v_plot <- renderPlot(print(v_plot_obj()), res = 96)
  vsave <- function(ext, dev) downloadHandler(filename = function() paste0("MAMC_chart_", input$v_chart, ".", ext),
    content = function(file) ggsave(file, v_plot_obj(), width = input$v_w, height = input$v_h, dpi = 300, device = dev, bg = "white"))
  output$v_png <- vsave("png", "png"); output$v_pdf <- vsave("pdf", "pdf"); output$v_tiff <- vsave("tiff", "tiff")
  v_py <- reactive(python_code(v_opts(), rv$name %||% "your_data.xlsx"))
  output$v_code <- renderUI(tags$pre(class = "pycode", v_py()))
  output$v_py <- downloadHandler(filename = function() paste0("mamc_chart_", input$v_chart, ".py"), content = function(file) writeLines(v_py(), file))

  # ---------------- report ----------------
  report_html <- function(for_file = FALSE) {
    if (!length(rv$report)) return("<p>No analyses yet.</p>")
    paste(vapply(rev(seq_along(rv$report)), function(i) { e <- rv$report[[i]]
      args <- e$args[names(e$args) != "alpha"]
      paste0("<div class='report-entry'><h3>", i, ". ", e$name, "</h3><p style='color:#666'>", e$time, " &middot; ", htmltools::htmlEscape(paste0(names(args), " = ", vapply(args, function(v) paste(v, collapse = ", "), ""), collapse = "; ")), "</p>",
             e$html, paste(mapply(df_html, e$tables, names(e$tables)), collapse = ""),
             if (!is.null(e$img)) paste0("<img src='data:image/png;base64,", e$img, "' style='max-width:100%;border:1px solid #eee'>") else "",
             if (for_file) paste0("<details><summary>Step-by-step calculation</summary>", e$steps, "</details>") else "", "</div>") }, ""), collapse = "")
  }
  output$report_ui <- renderUI(HTML(report_html()))
  observeEvent(input$clear_report, rv$report <- list())
  output$dl_report <- downloadHandler(filename = function() paste0("MAMC_BioStat_report_", format(Sys.Date()), ".html"), content = function(file) {
    css <- paste(readLines("www/style.css", warn = FALSE), collapse = "\n")
    writeLines(paste0("<!DOCTYPE html><html><head><meta charset='utf-8'><title>MAMC BioStat report</title><style>", css, " body{max-width:1000px;margin:30px auto;padding:0 20px;font-family:Segoe UI,Arial,sans-serif}</style>",
      "<link rel='stylesheet' href='https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.css'><script src='https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js'></script>",
      "<script src='https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/contrib/auto-render.min.js' onload=\"renderMathInElement(document.body,{delimiters:[{left:'\\\\[',right:'\\\\]',display:true}]})\"></script></head><body>",
      "<div class='hero'><h1>MAMC BioStat - Analysis Report</h1><p>Data: ", htmltools::htmlEscape(rv$name %||% "-"), " &middot; generated ", format(Sys.time(), "%d %b %Y %H:%M"), "</p></div>",
      report_html(TRUE), "<hr><p style='color:#777;font-size:12px'>Generated by MAMC BioStat v", APP_VERSION, " (R ", getRversion(), ") - Maulana Azad Medical College &amp; Lok Nayak Hospital, New Delhi. Free &amp; open source (GPL-3).</p></body></html>"), file, useBytes = TRUE) })
  output$dl_tables <- downloadHandler(filename = function() paste0("MAMC_BioStat_tables_", format(Sys.Date()), ".xlsx"), content = function(file) {
    sh <- list(); for (i in seq_along(rv$report)) for (nm in names(rv$report[[i]]$tables)) {
      key <- substr(gsub("[^A-Za-z0-9 ]", "", paste0(i, " ", nm)), 1, 31); while (key %in% names(sh)) key <- paste0(substr(key, 1, 29), "_", length(sh))
      sh[[key]] <- as.data.frame(rv$report[[i]]$tables[[nm]], check.names = FALSE) }
    if (!length(sh)) sh <- list(Info = data.frame(Note = "No analyses run yet")); writexl::write_xlsx(sh, file) })

  # ---------------- learn ----------------
  output$learn_ui <- renderUI(HTML(help_for(input$learn_topic)))
}

base64enc_file <- function(f) jsonlite::base64_enc(readBin(f, "raw", file.info(f)$size))

shinyApp(ui, server)
