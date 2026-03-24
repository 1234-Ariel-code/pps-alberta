###############################################################################
# SECTION 3: STRUCTURAL RELATIONSHIP QUESTIONS (SEM)
#
# Goal:
# For each TQS domain k = 1,...,6, fit:
#   Need_k ~ Comp_k
# where Comp_k and Need_k are latent variables measured by Year 4 items.
#
# Primary analysis:
#   - lavaan SEM
#   - ordered indicators
#   - WLSMV estimator
#
# Sensitivity analysis:
#   - overlap-removed WLSMV
#   - centered numeric indicators + robust MLR estimator with FIML
#
# IMPORTANT:
# Table 4 in the appendix contains repeated / potentially inconsistent question
# IDs across some competency and need blocks. This script is robust to missing
# or renamed columns in the actual Year 4 file.
###############################################################################

############################
# 0. PACKAGES
############################
required_pkgs <- c(
  "readxl", "dplyr", "tidyr", "stringr", "purrr", "tibble",
  "lavaan", "semPlot", "ggplot2", "readr"
)

installed <- rownames(installed.packages())
for (p in required_pkgs) {
  if (!(p %in% installed)) install.packages(p)
}

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(tibble)
library(lavaan)
library(semPlot)
library(ggplot2)
library(readr)

############################
# 1. FILE PATHS
############################
setwd("~/Documents/BIST601-Project")

file_y4 <- "data/Year 4-Optimum Learning for All Students- Implementing Professional Practice Standards-Teacher.xlsx"

out_dir <- "outputs_section3_sem"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

summary_fig_dir <- file.path(out_dir, "summary_figures")
if (!dir.exists(summary_fig_dir)) dir.create(summary_fig_dir, recursive = TRUE)

############################
# 2. YEAR 4 DOMAIN MAPPINGS
# (from appendix Table 4, preserved as documented)
############################

comp_map_y4 <- list(
  Comp1 = c("Q5", "Q4", "Q81", "Q82", "Q83"),
  Comp2 = c("Q84", "Q85", "Q86", "Q87", "Q88"),
  Comp3 = c("Q89", "Q96", "Q97", "Q90"),
  Comp4 = c("Q91", "Q92", "Q93", "Q94", "Q95"),
  Comp5 = c("Q98", "Q100", "Q101", "Q99"),
  Comp6 = c("Q104", "Q102", "Q103")
)

need_map_y4 <- list(
  Need1 = c("Q44", "Q105", "Q106", "Q107", "Q108"),
  Need2 = c("Q109", "Q110", "Q113", "Q114", "Q116"),
  Need3 = c("Q94", "Q111", "Q112", "Q115"),
  Need4 = c("Q117", "Q118", "Q95", "Q121", "Q119"),
  Need5 = c("Q120", "Q98", "Q123", "Q124"),
  Need6 = c("Q122", "Q97", "Q96")
)

domain_pairs <- tibble(
  Domain = paste0("Domain", 1:6),
  Comp = paste0("Comp", 1:6),
  Need = paste0("Need", 1:6)
)

############################
# 3. HELPER FUNCTIONS
############################

# --- 3.1 Convert selected columns to numeric
convert_questions_to_numeric <- function(df, question_ids) {
  existing_qs <- intersect(question_ids, names(df))
  
  if (length(existing_qs) == 0) {
    warning("No mapped question columns found in the dataset.")
    return(df)
  }
  
  df %>%
    mutate(across(all_of(existing_qs), ~ suppressWarnings(as.numeric(.x))))
}

# --- 3.2 Prepare ordered factor version for lavaan WLSMV
make_ordered_indicator <- function(x) {
  vals <- sort(unique(na.omit(x)))
  if (length(vals) <= 1) return(x)
  ordered(x, levels = vals)
}

# --- 3.3 Inspect a set of items
inspect_items <- function(df, items, label = NULL, out_dir = ".") {
  out <- tibble(
    Item = items,
    Present = items %in% names(df),
    N_nonmissing = map_int(items, ~ if (.x %in% names(df)) sum(!is.na(df[[.x]])) else NA_integer_),
    N_missing = map_int(items, ~ if (.x %in% names(df)) sum(is.na(df[[.x]])) else NA_integer_),
    Unique_values = map_chr(items, ~ if (.x %in% names(df)) paste(sort(unique(na.omit(df[[.x]]))), collapse = ", ") else NA_character_)
  )
  
  if (!is.null(label)) {
    write_csv(out, file.path(out_dir, paste0("inspect_", label, ".csv")))
  }
  
  out
}

# --- 3.4 Build lavaan syntax for one domain pair
build_domain_model <- function(comp_name, need_name, comp_items, need_items) {
  comp_part <- paste0(comp_name, " =~ ", paste(comp_items, collapse = " + "))
  need_part <- paste0(need_name, " =~ ", paste(need_items, collapse = " + "))
  reg_part  <- paste0(need_name, " ~ ", comp_name)
  
  paste(comp_part, need_part, reg_part, sep = "\n")
}

# --- 3.5 Extract main SEM results
extract_sem_results <- function(fit, comp_name, need_name, domain_label, model_label) {
  
  pe <- parameterEstimates(fit, standardized = TRUE) %>%
    as_tibble()
  
  ss <- standardizedSolution(fit) %>%
    as_tibble()
  
  path_row <- pe %>%
    filter(lhs == need_name, op == "~", rhs == comp_name) %>%
    mutate(
      Domain = domain_label,
      Model = model_label
    )
  
  std_path_row <- ss %>%
    filter(lhs == need_name, op == "~", rhs == comp_name) %>%
    select(lhs, op, rhs, est.std) %>%
    rename(std_estimate = est.std)
  
  path_row <- left_join(path_row, std_path_row, by = c("lhs", "op", "rhs"))
  
  loadings <- pe %>%
    filter(op == "=~") %>%
    mutate(
      Domain = domain_label,
      Model = model_label
    )
  
  fit_tab <- tibble(
    Domain = domain_label,
    Model = model_label,
    chisq = suppressWarnings(fitMeasures(fit, "chisq")),
    df = suppressWarnings(fitMeasures(fit, "df")),
    pvalue = suppressWarnings(fitMeasures(fit, "pvalue")),
    cfi = suppressWarnings(fitMeasures(fit, "cfi")),
    tli = suppressWarnings(fitMeasures(fit, "tli")),
    rmsea = suppressWarnings(fitMeasures(fit, "rmsea")),
    srmr = suppressWarnings(fitMeasures(fit, "srmr"))
  )
  
  list(
    path = path_row,
    loadings = loadings,
    fit = fit_tab
  )
}

# --- 3.6 Plot gamma coefficients across domains
plot_gamma_summary <- function(path_df, title_text, file_name, out_dir) {
  if (nrow(path_df) == 0) return(NULL)
  
  g <- path_df %>%
    ggplot(aes(x = Domain, y = std_estimate)) +
    geom_col(fill = "#2C7FB8", width = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = title_text,
      x = "Domain",
      y = "Standardized path coefficient (Need ~ Comp)"
    ) +
    theme_minimal(base_size = 12)
  
  ggsave(file.path(out_dir, file_name), g, width = 10, height = 6, dpi = 300)
  g
}

# --- 3.7 Safe SEM fitting wrapper
fit_sem_safe <- function(model_syntax, data, ordered_items = NULL,
                         estimator = "WLSMV",
                         parameterization = "theta",
                         std.lv = TRUE,
                         meanstructure = FALSE,
                         missing = NULL) {
  tryCatch(
    {
      if (is.null(ordered_items)) {
        sem(
          model = model_syntax,
          data = data,
          estimator = estimator,
          std.lv = std.lv,
          meanstructure = meanstructure,
          missing = missing
        )
      } else {
        sem(
          model = model_syntax,
          data = data,
          ordered = ordered_items,
          estimator = estimator,
          parameterization = parameterization,
          std.lv = std.lv,
          meanstructure = meanstructure
        )
      }
    },
    error = function(e) {
      message("SEM failed: ", e$message)
      return(NULL)
    }
  )
}

############################
# 4. LOAD YEAR 4 DATA
############################
y4 <- read_excel(file_y4)

# Save actual column names for inspection
writeLines(names(y4), file.path(out_dir, "year4_actual_column_names.txt"))

# Quick search files for suspicious variables
suspect_cols <- names(y4)[grepl("Q94|Q95|Q96|Q97|Q98|Q99|Q100|Q101|Q102|Q103|Q104", names(y4))]
writeLines(suspect_cols, file.path(out_dir, "year4_suspicious_q9_q10_columns.txt"))
print(suspect_cols)

all_y4_items <- unique(c(unlist(comp_map_y4), unlist(need_map_y4)))
y4 <- convert_questions_to_numeric(y4, all_y4_items)

############################
# 5. PRELIMINARY ITEM INSPECTION
############################
for (k in 1:6) {
  comp_name <- paste0("Comp", k)
  need_name <- paste0("Need", k)
  inspect_items(y4, comp_map_y4[[comp_name]], label = comp_name, out_dir = out_dir)
  inspect_items(y4, need_map_y4[[need_name]], label = need_name, out_dir = out_dir)
}

# Identify overlaps from appendix mapping
overlap_table <- tibble(
  Domain = paste0("Domain", 1:6),
  Comp = paste0("Comp", 1:6),
  Need = paste0("Need", 1:6)
) %>%
  rowwise() %>%
  mutate(
    OverlapItems = paste(intersect(comp_map_y4[[Comp]], need_map_y4[[Need]]), collapse = ", "),
    NOverlap = length(intersect(comp_map_y4[[Comp]], need_map_y4[[Need]]))
  ) %>%
  ungroup()

write_csv(overlap_table, file.path(out_dir, "domain_overlap_table.csv"))
print(overlap_table)

############################
# 6. PRIMARY ANALYSIS: ORDERED SEM (WLSMV)
############################
primary_fit_objects <- list()
primary_path_rows <- list()
primary_loading_rows <- list()
primary_fit_rows <- list()
primary_domain_log <- list()

for (k in 1:6) {
  
  comp_name <- paste0("Comp", k)
  need_name <- paste0("Need", k)
  domain_label <- paste0("Domain", k)
  
  comp_items <- comp_map_y4[[comp_name]]
  need_items <- need_map_y4[[need_name]]
  
  comp_items_exist <- intersect(comp_items, names(y4))
  need_items_exist <- intersect(need_items, names(y4))
  
  missing_comp <- setdiff(comp_items, names(y4))
  missing_need <- setdiff(need_items, names(y4))
  
  cat("\n====================================================\n")
  cat("PRIMARY ORDERED SEM:", domain_label, "\n")
  
  if (length(missing_comp) > 0) {
    cat("Missing competency items:", paste(missing_comp, collapse = ", "), "\n")
  }
  if (length(missing_need) > 0) {
    cat("Missing need items:", paste(missing_need, collapse = ", "), "\n")
  }
  
  primary_domain_log[[domain_label]] <- tibble(
    Domain = domain_label,
    CompItemsRequested = paste(comp_items, collapse = ", "),
    NeedItemsRequested = paste(need_items, collapse = ", "),
    CompItemsUsed = paste(comp_items_exist, collapse = ", "),
    NeedItemsUsed = paste(need_items_exist, collapse = ", "),
    MissingComp = paste(missing_comp, collapse = ", "),
    MissingNeed = paste(missing_need, collapse = ", "),
    NCompUsed = length(comp_items_exist),
    NNeedUsed = length(need_items_exist)
  )
  
  if (length(comp_items_exist) < 3 || length(need_items_exist) < 3) {
    cat("Skipping", domain_label, "- fewer than 3 indicators remain for one latent factor.\n")
    next
  }
  
  all_items_exist <- unique(c(comp_items_exist, need_items_exist))
  
  dat <- y4 %>%
    select(all_of(all_items_exist)) %>%
    mutate(across(everything(), ~ as.numeric(.x)))
  
  # Remove rows with no information at all
  dat <- dat[rowSums(!is.na(dat)) > 0, , drop = FALSE]
  
  dat_ord <- dat %>%
    mutate(across(everything(), make_ordered_indicator))
  
  model_syntax <- build_domain_model(
    comp_name = comp_name,
    need_name = need_name,
    comp_items = comp_items_exist,
    need_items = need_items_exist
  )
  
  cat(model_syntax, "\n")
  cat("====================================================\n")
  
  fit <- fit_sem_safe(
    model_syntax = model_syntax,
    data = dat_ord,
    ordered_items = all_items_exist,
    estimator = "WLSMV",
    parameterization = "theta",
    std.lv = TRUE,
    meanstructure = FALSE
  )
  
  if (is.null(fit)) next
  
  primary_fit_objects[[domain_label]] <- fit
  
  capture.output(
    summary(fit, standardized = TRUE, fit.measures = TRUE),
    file = file.path(out_dir, paste0(domain_label, "_primary_WLSMV_summary.txt"))
  )
  
  sem_out <- extract_sem_results(
    fit = fit,
    comp_name = comp_name,
    need_name = need_name,
    domain_label = domain_label,
    model_label = "Primary_WLSMV"
  )
  
  primary_path_rows[[domain_label]] <- sem_out$path
  primary_loading_rows[[domain_label]] <- sem_out$loadings
  primary_fit_rows[[domain_label]] <- sem_out$fit
  
  try({
    png(file.path(out_dir, paste0(domain_label, "_primary_WLSMV_path.png")),
        width = 1200, height = 900, res = 150)
    semPaths(
      fit,
      what = "std",
      layout = "tree",
      style = "lisrel",
      residuals = FALSE,
      intercepts = FALSE,
      edge.label.cex = 0.9,
      sizeMan = 7,
      sizeLat = 9,
      mar = c(6, 6, 6, 6)
    )
    dev.off()
  }, silent = TRUE)
}

primary_domain_log_df <- bind_rows(primary_domain_log)
write_csv(primary_domain_log_df, file.path(out_dir, "primary_domain_item_usage_log.csv"))

primary_paths_df <- bind_rows(primary_path_rows)
primary_loadings_df <- bind_rows(primary_loading_rows)
primary_fit_df <- bind_rows(primary_fit_rows)

if (nrow(primary_paths_df) > 0) {
  write_csv(primary_paths_df, file.path(out_dir, "primary_WLSMV_structural_paths.csv"))
  print(primary_paths_df)
}

if (nrow(primary_loadings_df) > 0) {
  write_csv(primary_loadings_df, file.path(out_dir, "primary_WLSMV_loadings.csv"))
}

if (nrow(primary_fit_df) > 0) {
  write_csv(primary_fit_df, file.path(out_dir, "primary_WLSMV_fit_indices.csv"))
  print(primary_fit_df)
}

if (nrow(primary_paths_df) > 0) {
  plot_gamma_summary(
    primary_paths_df,
    "Primary Ordered SEM: Standardized Need ~ Competency Paths by Domain",
    "primary_WLSMV_gamma_summary.png",
    out_dir = out_dir
  )
}

############################
# 7. SENSITIVITY ANALYSIS A:
# REMOVE OVERLAPPING INDICATORS WITHIN DOMAIN PAIRS
############################
sensitivity_overlap_fit_objects <- list()
sens_overlap_path_rows <- list()
sens_overlap_loading_rows <- list()
sens_overlap_fit_rows <- list()
sens_overlap_log <- list()

for (k in 1:6) {
  
  comp_name <- paste0("Comp", k)
  need_name <- paste0("Need", k)
  domain_label <- paste0("Domain", k)
  
  comp_items <- comp_map_y4[[comp_name]]
  need_items <- need_map_y4[[need_name]]
  
  overlap_items <- intersect(comp_items, need_items)
  
  # remove overlap from need side only
  if (length(overlap_items) > 0) {
    need_items_sens <- setdiff(need_items, overlap_items)
  } else {
    need_items_sens <- need_items
  }
  
  comp_items_exist <- intersect(comp_items, names(y4))
  need_items_exist <- intersect(need_items_sens, names(y4))
  
  sens_overlap_log[[domain_label]] <- tibble(
    Domain = domain_label,
    OverlapRemoved = paste(overlap_items, collapse = ", "),
    CompItemsUsed = paste(comp_items_exist, collapse = ", "),
    NeedItemsUsed = paste(need_items_exist, collapse = ", "),
    NCompUsed = length(comp_items_exist),
    NNeedUsed = length(need_items_exist)
  )
  
  if (length(comp_items_exist) < 3 || length(need_items_exist) < 3) {
    message("Skipping overlap-removed model for ", domain_label,
            " because fewer than 3 indicators remain on one side.")
    next
  }
  
  all_items_exist <- unique(c(comp_items_exist, need_items_exist))
  
  dat <- y4 %>%
    select(all_of(all_items_exist)) %>%
    mutate(across(everything(), ~ as.numeric(.x)))
  
  dat <- dat[rowSums(!is.na(dat)) > 0, , drop = FALSE]
  
  dat_ord <- dat %>%
    mutate(across(everything(), make_ordered_indicator))
  
  model_syntax <- build_domain_model(
    comp_name = comp_name,
    need_name = need_name,
    comp_items = comp_items_exist,
    need_items = need_items_exist
  )
  
  fit <- fit_sem_safe(
    model_syntax = model_syntax,
    data = dat_ord,
    ordered_items = all_items_exist,
    estimator = "WLSMV",
    parameterization = "theta",
    std.lv = TRUE,
    meanstructure = FALSE
  )
  
  if (is.null(fit)) next
  
  sensitivity_overlap_fit_objects[[domain_label]] <- fit
  
  capture.output(
    summary(fit, standardized = TRUE, fit.measures = TRUE),
    file = file.path(out_dir, paste0(domain_label, "_sensitivity_overlap_removed_summary.txt"))
  )
  
  sem_out <- extract_sem_results(
    fit = fit,
    comp_name = comp_name,
    need_name = need_name,
    domain_label = domain_label,
    model_label = "Sensitivity_OverlapRemoved"
  )
  
  sens_overlap_path_rows[[domain_label]] <- sem_out$path
  sens_overlap_loading_rows[[domain_label]] <- sem_out$loadings
  sens_overlap_fit_rows[[domain_label]] <- sem_out$fit
}

sens_overlap_log_df <- bind_rows(sens_overlap_log)
write_csv(sens_overlap_log_df, file.path(out_dir, "sensitivity_overlap_removed_log.csv"))

sens_overlap_paths_df <- bind_rows(sens_overlap_path_rows)
sens_overlap_loadings_df <- bind_rows(sens_overlap_loading_rows)
sens_overlap_fit_df <- bind_rows(sens_overlap_fit_rows)

if (nrow(sens_overlap_paths_df) > 0) {
  write_csv(sens_overlap_paths_df, file.path(out_dir, "sensitivity_overlap_removed_paths.csv"))
}
if (nrow(sens_overlap_loadings_df) > 0) {
  write_csv(sens_overlap_loadings_df, file.path(out_dir, "sensitivity_overlap_removed_loadings.csv"))
}
if (nrow(sens_overlap_fit_df) > 0) {
  write_csv(sens_overlap_fit_df, file.path(out_dir, "sensitivity_overlap_removed_fit_indices.csv"))
}

############################
# 8. SENSITIVITY ANALYSIS B:
# CENTERED NUMERIC INDICATORS + ROBUST MLR
############################
center_numeric <- function(x) {
  x - mean(x, na.rm = TRUE)
}

sensitivity_mlr_fit_objects <- list()
sens_mlr_path_rows <- list()
sens_mlr_loading_rows <- list()
sens_mlr_fit_rows <- list()
sens_mlr_log <- list()

for (k in 1:6) {
  
  comp_name <- paste0("Comp", k)
  need_name <- paste0("Need", k)
  domain_label <- paste0("Domain", k)
  
  comp_items <- comp_map_y4[[comp_name]]
  need_items <- need_map_y4[[need_name]]
  
  comp_items_exist <- intersect(comp_items, names(y4))
  need_items_exist <- intersect(need_items, names(y4))
  
  sens_mlr_log[[domain_label]] <- tibble(
    Domain = domain_label,
    CompItemsUsed = paste(comp_items_exist, collapse = ", "),
    NeedItemsUsed = paste(need_items_exist, collapse = ", "),
    NCompUsed = length(comp_items_exist),
    NNeedUsed = length(need_items_exist)
  )
  
  if (length(comp_items_exist) < 3 || length(need_items_exist) < 3) {
    message("Skipping centered MLR model for ", domain_label,
            " because fewer than 3 indicators remain on one side.")
    next
  }
  
  all_items_exist <- unique(c(comp_items_exist, need_items_exist))
  
  dat <- y4 %>%
    select(all_of(all_items_exist)) %>%
    mutate(across(everything(), ~ as.numeric(.x))) %>%
    mutate(across(everything(), center_numeric))
  
  dat <- dat[rowSums(!is.na(dat)) > 0, , drop = FALSE]
  
  model_syntax <- build_domain_model(
    comp_name = comp_name,
    need_name = need_name,
    comp_items = comp_items_exist,
    need_items = need_items_exist
  )
  
  fit <- fit_sem_safe(
    model_syntax = model_syntax,
    data = dat,
    ordered_items = NULL,
    estimator = "MLR",
    std.lv = TRUE,
    meanstructure = FALSE,
    missing = "fiml"
  )
  
  if (is.null(fit)) next
  
  sensitivity_mlr_fit_objects[[domain_label]] <- fit
  
  capture.output(
    summary(fit, standardized = TRUE, fit.measures = TRUE),
    file = file.path(out_dir, paste0(domain_label, "_sensitivity_MLR_centered_summary.txt"))
  )
  
  sem_out <- extract_sem_results(
    fit = fit,
    comp_name = comp_name,
    need_name = need_name,
    domain_label = domain_label,
    model_label = "Sensitivity_MLR_Centered"
  )
  
  sens_mlr_path_rows[[domain_label]] <- sem_out$path
  sens_mlr_loading_rows[[domain_label]] <- sem_out$loadings
  sens_mlr_fit_rows[[domain_label]] <- sem_out$fit
}

sens_mlr_log_df <- bind_rows(sens_mlr_log)
write_csv(sens_mlr_log_df, file.path(out_dir, "sensitivity_mlr_log.csv"))

sens_mlr_paths_df <- bind_rows(sens_mlr_path_rows)
sens_mlr_loadings_df <- bind_rows(sens_mlr_loading_rows)
sens_mlr_fit_df <- bind_rows(sens_mlr_fit_rows)

if (nrow(sens_mlr_paths_df) > 0) {
  write_csv(sens_mlr_paths_df, file.path(out_dir, "sensitivity_MLR_centered_paths.csv"))
}
if (nrow(sens_mlr_loadings_df) > 0) {
  write_csv(sens_mlr_loadings_df, file.path(out_dir, "sensitivity_MLR_centered_loadings.csv"))
}
if (nrow(sens_mlr_fit_df) > 0) {
  write_csv(sens_mlr_fit_df, file.path(out_dir, "sensitivity_MLR_centered_fit_indices.csv"))
}

############################
# 9. COMBINED COMPARISON TABLES
############################
all_paths_df <- bind_rows(
  primary_paths_df,
  sens_overlap_paths_df,
  sens_mlr_paths_df
)

all_fit_df <- bind_rows(
  primary_fit_df,
  sens_overlap_fit_df,
  sens_mlr_fit_df
)

if (nrow(all_paths_df) > 0) {
  write_csv(all_paths_df, file.path(out_dir, "all_sem_paths_combined.csv"))
}
if (nrow(all_fit_df) > 0) {
  write_csv(all_fit_df, file.path(out_dir, "all_sem_fit_indices_combined.csv"))
}

if (nrow(all_paths_df) > 0) {
  gamma_plot <- all_paths_df %>%
    ggplot(aes(x = Domain, y = std_estimate, fill = Model)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Comparison of Standardized Need ~ Competency Paths Across SEM Specifications",
      x = "Domain",
      y = "Standardized path coefficient"
    ) +
    theme_minimal(base_size = 12)
  
  ggsave(file.path(out_dir, "all_sem_gamma_comparison.png"),
         gamma_plot, width = 11, height = 6, dpi = 300)
}

############################
# 10. GLOBAL REPORT TABLE
############################
if (nrow(all_paths_df) > 0) {
  report_table <- all_paths_df %>%
    select(Domain, Model, est, se, z, pvalue, std_estimate) %>%
    arrange(Domain, Model)
  
  write_csv(report_table, file.path(out_dir, "report_table_sem_paths.csv"))
  print(report_table)
}

############################
# 11. SESSION INFO
############################
writeLines(
  capture.output(sessionInfo()),
  file.path(out_dir, "sessionInfo_section3_sem.txt")
)

###############################################################################
# SECTION 3A: NON-TECHNICAL SUMMARY FIGURES
###############################################################################

############################
# FIGURE 1: Conceptual SEM diagram
############################
fig1_sem_concept <- ggplot() +
  xlim(0, 10) + ylim(0, 10) +
  annotate("rect", xmin = 0.5, xmax = 2.8, ymin = 6.5, ymax = 8.5,
           fill = "#D9EAF7", color = "black") +
  annotate("text", x = 1.65, y = 7.5,
           label = "Implementation\nitems\n(e.g., Q5, Q4, Q81...)", size = 4) +
  annotate("label", x = 4.2, y = 7.5,
           label = "Competency\nimplementation", size = 4, fill = "#BFD7EA") +
  annotate("label", x = 6.8, y = 7.5,
           label = "Professional\nlearning need", size = 4, fill = "#FAD4C0") +
  annotate("rect", xmin = 8.0, xmax = 9.7, ymin = 6.5, ymax = 8.5,
           fill = "#FCE5D6", color = "black") +
  annotate("text", x = 8.85, y = 7.5,
           label = "Need\nitems\n(e.g., Q44, Q105...)", size = 4) +
  annotate("segment", x = 2.8, xend = 3.45, y = 7.5, yend = 7.5,
           arrow = arrow(length = unit(0.15, "inches"))) +
  annotate("segment", x = 5.0, xend = 5.95, y = 7.5, yend = 7.5,
           arrow = arrow(length = unit(0.15, "inches"))) +
  annotate("segment", x = 7.65, xend = 8.0, y = 7.5, yend = 7.5,
           arrow = arrow(length = unit(0.15, "inches"))) +
  labs(
    title = "Conceptual Structure of the Domain-Specific SEM",
    subtitle = "Each model links competency implementation to professional learning need"
  ) +
  theme_void(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  file.path(summary_fig_dir, "Figure1_SEM_Conceptual_Diagram.png"),
  fig1_sem_concept, width = 11, height = 5, dpi = 300
)

############################
# FIGURE 2: Primary standardized path coefficients
############################
if (nrow(primary_paths_df) > 0) {
  fig2_gamma_primary <- primary_paths_df %>%
    ggplot(aes(x = Domain, y = std_estimate)) +
    geom_col(fill = "#2C7FB8", width = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Implementation-to-Need Relationship by Domain",
      subtitle = "Primary ordered SEM (WLSMV)",
      x = "Domain",
      y = "Standardized effect of implementation on need"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 20, hjust = 1)
    )
  
  ggsave(
    file.path(summary_fig_dir, "Figure2_Primary_Gamma_By_Domain.png"),
    fig2_gamma_primary, width = 10, height = 6, dpi = 300
  )
}

############################
# FIGURE 3: Compare coefficients across model specifications
############################
if (nrow(all_paths_df) > 0) {
  fig3_gamma_compare <- all_paths_df %>%
    ggplot(aes(x = Domain, y = std_estimate, fill = Model)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "How Stable Are the SEM Results Across Model Versions?",
      subtitle = "Comparison of primary and sensitivity analyses",
      x = "Domain",
      y = "Standardized effect of implementation on need"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 20, hjust = 1),
      legend.position = "top"
    )
  
  ggsave(
    file.path(summary_fig_dir, "Figure3_Gamma_Comparison_Across_Models.png"),
    fig3_gamma_compare, width = 11, height = 6, dpi = 300
  )
}

############################
# FIGURE 4: Model fit summary
############################
if (nrow(primary_fit_df) > 0) {
  fit_plot_data <- primary_fit_df %>%
    select(Domain, cfi, rmsea, srmr) %>%
    pivot_longer(cols = c(cfi, rmsea, srmr), names_to = "Metric", values_to = "Value")
  
  fig4_fit_summary <- ggplot(fit_plot_data, aes(x = Domain, y = Value, color = Metric, group = Metric)) +
    geom_point(size = 3) +
    geom_line() +
    labs(
      title = "Selected SEM Fit Indices by Domain",
      subtitle = "Primary ordered SEM (WLSMV)",
      x = "Domain",
      y = "Fit index value",
      color = "Metric"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 20, hjust = 1),
      legend.position = "top"
    )
  
  ggsave(
    file.path(summary_fig_dir, "Figure4_SEM_Fit_Summary.png"),
    fig4_fit_summary, width = 10, height = 6, dpi = 300
  )
}

###############################################################################
# END OF SECTION 3 CODE
###############################################################################

