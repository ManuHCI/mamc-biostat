# Check that the exported R code reproduces the app's results (run from the app folder)
for (f in c("helpers.R", list.files("R", pattern = "^(data_check|tests_|rcode).*\\.R$"))) source(file.path("R", basename(f)))
src <- readLines("../tools/run_all_tests.R"); eval(parse(text = src[grep("^d <- read_data", src):grep("A\\$mcnemar <-", src)]))
tmp <- tempfile(fileext = ".xlsx"); writexl::write_xlsx(list(Data_1 = d), tmp)
nums_in <- function(txt) { m <- regmatches(txt, gregexpr("-?[0-9]*\\.?[0-9]+(e-?[0-9]+)?", txt)); as.numeric(unlist(m)) }
pdf(NULL); options(warn = -1); bad <- 0
for (id in TEST_ORDER) {
  a <- A[[id]]; a$alpha <- 0.05; t <- get_test(id); r <- t$run(d, a)
  pv <- unlist(lapply(r$tables, function(tb) unlist(tb[grepl("^p$|^p |^p_|^p\\(|Pr\\(|^p \\(", names(tb))])))
  key <- if (length(pv)) pv else unlist(lapply(r$tables, function(tb) unlist(Filter(is.numeric, tb))))
  key <- suppressWarnings(as.numeric(key)); key <- key[is.finite(key)]
  e <- list(id = id, name = t$name, time = "now", args = a, data_key = if (t$needs_data) "Data_1" else NULL)
  code <- rcode_script(list(e), if (t$needs_data) list(Data_1 = d) else list(), tmp, "test")
  code <- sub(paste0("results_file <- ", Q(tmp)), paste0("results_file <- ", Q(tmp)), code, fixed = TRUE)
  env <- new.env(); old <- options(digits = 15)
  out <- tryCatch(capture.output(withVisible <- source(textConnection(code), local = env, echo = FALSE, print.eval = TRUE)), error = function(e) paste("ERROR:", conditionMessage(e)))
  options(old); options(contrasts = c("contr.treatment", "contr.poly"))
  got <- c(nums_in(out), unlist(Filter(is.numeric, as.list(env))))
  hit <- vapply(key, function(k) any(abs(got - k) <= 5e-4 * abs(k) + 5e-5), logical(1))
  flag <- if (any(grepl("^ERROR", out))) "ERROR" else if (length(key) && mean(hit) < 1) "CHECK" else "ok"
  if (flag != "ok") bad <- bad + 1
  if (flag == "CHECK") { kk <- if (length(pv)) "p" else "all"; cat("   missing:", paste(signif(key[!hit], 6)[1:min(8, sum(!hit))], collapse = ", "), "\n") }
  cat(sprintf("%-14s %-6s %d/%d key values reproduced %s\n", id, flag, sum(hit), length(key), if (flag == "ERROR") out[grep("^ERROR", out)][1] else ""))
}
cat("\nTests with differences:", bad, "\n")
