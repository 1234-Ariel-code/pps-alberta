###############################################################################
# SECTION 2: GROUP COMPARISON QUESTIONS (MANOVA / ANOVA)
#
# Questions:
# 4. Do teacher competency profiles differ by grade level taught (Year 4)?
# 5. Do teacher competency profiles differ by subject specialization (Year 4)?
# 6. Do teacher competency profiles differ by teaching experience (Year 4)?
# 7. Do teachers’ implementation levels across the six TQS competencies differ
#    jointly across the four survey years?
###############################################################################

############################
# 0. PACKAGES
############################
required_pkgs <- c(
  "dplyr", "tidyr", "ggplot2", "readr", "readxl", "purrr", "stringr",
  "car", "effectsize", "emmeans", "broom", "broom.mixed", "psych"
)

installed <- rownames(installed.packages())
for (p in required_pkgs) {
  if (!(p %in% installed)) install.packages(p)
}

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(readxl)
library(purrr)
library(stringr)
library(car)
library(effectsize)
library(emmeans)
library(broom)
library(psych)

############################
# 1. FILE PATHS
############################
setwd("~/Documents/BIST601-Project")
file_y1 <- "data/Year1teachers.csv"
file_y2 <- "data/Year2Teacher.csv"
file_y3 <- "data/Year3Teacher.csv"
file_y4 <- "data/Year 4-Optimum Learning for All Students- Implementing Professional Practice Standards-Teacher.xlsx"

out_dir <- "outputs_section2"
if (!dir.exists(out_dir)) dir.create(out_dir)

############################
# 2. QUESTION-TO-COMPETENCY MAPPINGS
# (from appendix / Section 1)
############################

comp_map_y1 <- list(
  Comp1 = c("Q4", "Q5", "Q6"),
  Comp2 = c("Q7", "Q8", "Q9", "Q11"),
  Comp3 = c("Q14", "Q15"),
  Comp4 = c("Q10", "Q13"),
  Comp5 = c("Q16", "Q17", "Q18", "Q19"),
  Comp6 = c("Q12")
)

comp_map_y23 <- list(
  Comp1 = c("Q5", "Q4", "Q81", "Q82", "Q83"),
  Comp2 = c("Q84", "Q85", "Q86", "Q87", "Q88"),
  Comp3 = c("Q89", "Q96", "Q97", "Q90"),
  Comp4 = c("Q91", "Q92", "Q93", "Q94", "Q95"),
  Comp5 = c("Q98", "Q100", "Q101", "Q99"),
  Comp6 = c("Q104", "Q102", "Q103")
)

comp_map_y4 <- list(
  Comp1 = c("Q5", "Q4", "Q81", "Q82", "Q83"),
  Comp2 = c("Q84", "Q85", "Q86", "Q87", "Q88"),
  Comp3 = c("Q89", "Q96", "Q97", "Q90"),
  Comp4 = c("Q91", "Q92", "Q93", "Q94", "Q95"),
  Comp5 = c("Q98", "Q100", "Q101", "Q99"),
  Comp6 = c("Q104", "Q102", "Q103")
)

comp_vars <- names(comp_map_y4)  # Comp1...Comp6

############################
# 3. HELPER FUNCTIONS
############################

# --- 3.1 Read Qualtrics CSVs for Years 1–3
read_teacher_csv <- function(path) {
  df <- read_csv(path, show_col_types = FALSE)
  if (nrow(df) >= 2) {
    df <- df[-c(1, 2), ]
  }
  df
}

# --- 3.2 Convert selected columns to numeric
convert_questions_to_numeric <- function(df, question_ids) {
  existing_qs <- intersect(question_ids, names(df))
  df %>%
    mutate(across(all_of(existing_qs), ~ suppressWarnings(as.numeric(.x))))
}

# --- 3.3 Score row-mean domains
score_domains <- function(df, domain_map, min_prop = 0.5) {
  out <- df
  for (dom in names(domain_map)) {
    qs <- intersect(domain_map[[dom]], names(df))
    if (length(qs) == 0) {
      out[[dom]] <- NA_real_
      next
    }
    nonmiss_required <- ceiling(length(qs) * min_prop)
    out[[dom]] <- apply(df[, qs, drop = FALSE], 1, function(x) {
      n_nonmiss <- sum(!is.na(x))
      if (n_nonmiss >= nonmiss_required) mean(x, na.rm = TRUE) else NA_real_
    })
  }
  out
}

# --- 3.4 Helper for complete cases on DV set + one grouping variable
make_manova_data <- function(df, dvs, group_var) {
  df %>%
    select(all_of(c(group_var, dvs))) %>%
    filter(!is.na(.data[[group_var]])) %>%
    mutate(across(all_of(dvs), as.numeric)) %>%
    filter(if_all(all_of(dvs), ~ !is.na(.x))) %>%
    mutate(
      !!group_var := as.factor(.data[[group_var]])
    ) %>%
    droplevels()
}

# --- 3.5 Run MANOVA + follow-up ANOVAs + posthoc
run_manova_followups <- function(df, dvs, group_var, output_prefix) {
  
  # Prepare complete-case data for MANOVA
  dat <- make_manova_data(df, dvs, group_var)
  
  # Drop groups with too few observations if desired
  group_counts <- table(dat[[group_var]])
  keep_levels <- names(group_counts[group_counts >= 5])  # configurable threshold
  dat <- dat %>%
    filter(.data[[group_var]] %in% keep_levels) %>%
    droplevels()
  
  if (nlevels(dat[[group_var]]) < 2) {
    warning(paste("Not enough groups for", group_var))
    return(NULL)
  }
  
  # MANOVA model
  manova_formula <- as.formula(
    paste0("cbind(", paste(dvs, collapse = ", "), ") ~ ", group_var)
  )
  
  manova_fit <- manova(manova_formula, data = dat)
  
  # Multivariate tests
  manova_pillai <- summary(manova_fit, test = "Pillai")
  manova_wilks  <- summary(manova_fit, test = "Wilks")
  
  # Save multivariate summaries
  capture.output(manova_pillai,
                 file = file.path(out_dir, paste0(output_prefix, "_MANOVA_Pillai.txt")))
  capture.output(manova_wilks,
                 file = file.path(out_dir, paste0(output_prefix, "_MANOVA_Wilks.txt")))
  
  # Follow-up ANOVAs
  anova_results <- list()
  eta_results <- list()
  posthoc_results <- list()
  
  for (dv in dvs) {
    fit_aov <- aov(as.formula(paste0(dv, " ~ ", group_var)), data = dat)
    fit_car <- car::Anova(lm(as.formula(paste0(dv, " ~ ", group_var)), data = dat), type = 3)
    
    # Save ANOVA table
    aov_tab <- broom::tidy(fit_aov)
    aov_tab$DV <- dv
    anova_results[[dv]] <- aov_tab
    
    # Effect size
    eta_tab <- effectsize::eta_squared(fit_aov, partial = FALSE)
    eta_tab$DV <- dv
    eta_results[[dv]] <- eta_tab
    
    # Post hoc if >2 groups
    if (nlevels(dat[[group_var]]) > 2) {
      em <- emmeans(fit_aov, specs = as.formula(paste0("~", group_var)))
      post <- pairs(em, adjust = "tukey") %>%
        as.data.frame()
      post$DV <- dv
      posthoc_results[[dv]] <- post
    }
  }
  
  anova_results_df <- bind_rows(anova_results)
  eta_results_df   <- bind_rows(eta_results)
  posthoc_results_df <- bind_rows(posthoc_results)
  
  # Multiple testing correction across the six follow-up ANOVAs
  # Extract p-values for the grouping term only
  anova_group <- anova_results_df %>%
    filter(term == group_var) %>%
    mutate(
      p_adj_bh = p.adjust(p.value, method = "BH"),
      p_adj_holm = p.adjust(p.value, method = "holm")
    )
  
  # Save outputs
  write_csv(anova_results_df,
            file.path(out_dir, paste0(output_prefix, "_ANOVA_raw.csv")))
  write_csv(anova_group,
            file.path(out_dir, paste0(output_prefix, "_ANOVA_group_term_adjusted.csv")))
  write_csv(eta_results_df,
            file.path(out_dir, paste0(output_prefix, "_eta_squared.csv")))
  
  if (nrow(posthoc_results_df) > 0) {
    write_csv(posthoc_results_df,
              file.path(out_dir, paste0(output_prefix, "_posthoc_tukey.csv")))
  }
  
  # Return everything
  list(
    data = dat,
    manova_fit = manova_fit,
    manova_pillai = manova_pillai,
    manova_wilks = manova_wilks,
    anova_all = anova_results_df,
    anova_group = anova_group,
    eta = eta_results_df,
    posthoc = posthoc_results_df
  )
}

############################
# 4. LOAD AND SCORE DATA
############################
y1 <- read_teacher_csv(file_y1)
y2 <- read_teacher_csv(file_y2)
y3 <- read_teacher_csv(file_y3)
y4 <- read_excel(file_y4)

# Convert relevant implementation items to numeric
y1 <- convert_questions_to_numeric(y1, unique(unlist(comp_map_y1)))
y2 <- convert_questions_to_numeric(y2, unique(unlist(comp_map_y23)))
y3 <- convert_questions_to_numeric(y3, unique(unlist(comp_map_y23)))
y4 <- convert_questions_to_numeric(y4, unique(unlist(comp_map_y4)))

# Score composites
y1_scored <- score_domains(y1, comp_map_y1)
y2_scored <- score_domains(y2, comp_map_y23)
y3_scored <- score_domains(y3, comp_map_y23)
y4_scored <- score_domains(y4, comp_map_y4)

############################
# 5. IDENTIFY YEAR 4 GROUPING VARIABLES
############################
# IMPORTANT:
# The appendix specifies the questions conceptually (grade level taught,
# subject specialization, teaching experience), but not the exact Year 4
# variable names in the screenshot.
#
# So first we inspect all column names.
############################

writeLines(names(y4_scored), file.path(out_dir, "year4_column_names.txt"))

# OPTIONAL: print likely candidates to console
candidate_vars <- names(y4_scored)[str_detect(names(y4_scored), regex("grade|subject|experience|year|teach", ignore_case = TRUE))]
print(candidate_vars)

# -------------------------------------------------------------------------
# >>> YOU MUST CONFIRM / EDIT THESE THREE LINES AFTER CHECKING THE DATA <<<
# -------------------------------------------------------------------------
grade_var      <- "Q58"   # <-- replace if different
subject_var    <- "Q59"   # <-- replace if different
experience_var <- "Q65"   # <-- replace if different

# If the subgroup variables are numeric codes, convert to factor.
# If you later get the codebook, replace with factor labels explicitly.

y4_scored <- y4_scored %>%
  mutate(
    across(all_of(c(grade_var, subject_var, experience_var)), as.factor)
  )

############################
# 6. QUESTION 4
# Do competency profiles differ by grade level taught (Year 4)?
############################

res_q4_grade <- run_manova_followups(
  df = y4_scored,
  dvs = comp_vars,
  group_var = grade_var,
  output_prefix = "Q4_grade_level"
)

# Optional visualization: boxplots by grade level for each competency
if (!is.null(res_q4_grade)) {
  plot_q4 <- res_q4_grade$data %>%
    pivot_longer(cols = all_of(comp_vars), names_to = "Competency", values_to = "Score") %>%
    ggplot(aes(x = .data[[grade_var]], y = Score, fill = .data[[grade_var]])) +
    geom_boxplot(outlier.alpha = 0.3) +
    facet_wrap(~ Competency, scales = "free_y") +
    labs(
      title = "Year 4 Teacher Competency Profiles by Grade Level",
      x = "Grade Level Taught",
      y = "Competency Composite Score"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position = "none")
  
  ggsave(file.path(out_dir, "Q4_grade_level_boxplots.png"),
         plot_q4, width = 12, height = 8, dpi = 300)
}

############################
# 7. QUESTION 5
# Do competency profiles differ by subject specialization (Year 4)?
############################

res_q5_subject <- run_manova_followups(
  df = y4_scored,
  dvs = comp_vars,
  group_var = subject_var,
  output_prefix = "Q5_subject_specialization"
)

if (!is.null(res_q5_subject)) {
  plot_q5 <- res_q5_subject$data %>%
    pivot_longer(cols = all_of(comp_vars), names_to = "Competency", values_to = "Score") %>%
    ggplot(aes(x = .data[[subject_var]], y = Score, fill = .data[[subject_var]])) +
    geom_boxplot(outlier.alpha = 0.3) +
    facet_wrap(~ Competency, scales = "free_y") +
    labs(
      title = "Year 4 Teacher Competency Profiles by Subject Specialization",
      x = "Subject Specialization",
      y = "Competency Composite Score"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position = "none")
  
  ggsave(file.path(out_dir, "Q5_subject_boxplots.png"),
         plot_q5, width = 12, height = 8, dpi = 300)
}

############################
# 8. QUESTION 6
# Do competency profiles differ by teaching experience (Year 4)?
############################

res_q6_experience <- run_manova_followups(
  df = y4_scored,
  dvs = comp_vars,
  group_var = experience_var,
  output_prefix = "Q6_teaching_experience"
)

if (!is.null(res_q6_experience)) {
  plot_q6 <- res_q6_experience$data %>%
    pivot_longer(cols = all_of(comp_vars), names_to = "Competency", values_to = "Score") %>%
    ggplot(aes(x = .data[[experience_var]], y = Score, fill = .data[[experience_var]])) +
    geom_boxplot(outlier.alpha = 0.3) +
    facet_wrap(~ Competency, scales = "free_y") +
    labs(
      title = "Year 4 Teacher Competency Profiles by Teaching Experience",
      x = "Teaching Experience",
      y = "Competency Composite Score"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position = "none")
  
  ggsave(file.path(out_dir, "Q6_experience_boxplots.png"),
         plot_q6, width = 12, height = 8, dpi = 300)
}

############################
# 9. QUESTION 7
# Do teachers’ implementation levels across the six TQS competencies differ
# jointly across the four survey years?
############################

# Add year identifiers and keep common composite names
y1_comp <- y1_scored %>%
  select(all_of(names(comp_map_y1))) %>%
  rename_with(~ paste0("tmp_", .x))  # avoid mismatch in case names differ

y2_comp <- y2_scored %>%
  select(all_of(names(comp_map_y23))) %>%
  rename_with(~ paste0("tmp_", .x))

y3_comp <- y3_scored %>%
  select(all_of(names(comp_map_y23))) %>%
  rename_with(~ paste0("tmp_", .x))

y4_comp <- y4_scored %>%
  select(all_of(names(comp_map_y4))) %>%
  rename_with(~ paste0("tmp_", .x))

# Standardize column names
common_names <- paste0("Comp", 1:6)
names(y1_comp) <- common_names
names(y2_comp) <- common_names
names(y3_comp) <- common_names
names(y4_comp) <- common_names

# Add year
y1_comp$Year <- "Year 1"
y2_comp$Year <- "Year 2"
y3_comp$Year <- "Year 3"
y4_comp$Year <- "Year 4"

# Combine
all_years_comp <- bind_rows(y1_comp, y2_comp, y3_comp, y4_comp) %>%
  mutate(Year = factor(Year, levels = c("Year 1", "Year 2", "Year 3", "Year 4")))

# MANOVA across years
res_q7_year <- run_manova_followups(
  df = all_years_comp,
  dvs = common_names,
  group_var = "Year",
  output_prefix = "Q7_across_years"
)

if (!is.null(res_q7_year)) {
  plot_q7 <- res_q7_year$data %>%
    pivot_longer(cols = all_of(common_names), names_to = "Competency", values_to = "Score") %>%
    ggplot(aes(x = Year, y = Score, fill = Year)) +
    geom_boxplot(outlier.alpha = 0.25) +
    facet_wrap(~ Competency, scales = "free_y") +
    labs(
      title = "Teacher Competency Profiles Across the Four Survey Years",
      x = "Survey Year",
      y = "Competency Composite Score"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
  
  ggsave(file.path(out_dir, "Q7_across_years_boxplots.png"),
         plot_q7, width = 12, height = 8, dpi = 300)
}

############################
# 10. ADDITIONAL DESCRIPTIVE TABLES FOR SECTION 2
############################

# Means by grade / subject / experience / year
if (!is.null(res_q4_grade)) {
  q4_means <- res_q4_grade$data %>%
    group_by(.data[[grade_var]]) %>%
    summarise(across(all_of(comp_vars), list(mean = ~ mean(.x, na.rm = TRUE),
                                             sd   = ~ sd(.x, na.rm = TRUE),
                                             n    = ~ sum(!is.na(.x))),
                     .names = "{.col}_{.fn}"))
  write_csv(q4_means, file.path(out_dir, "Q4_grade_descriptives.csv"))
}

if (!is.null(res_q5_subject)) {
  q5_means <- res_q5_subject$data %>%
    group_by(.data[[subject_var]]) %>%
    summarise(across(all_of(comp_vars), list(mean = ~ mean(.x, na.rm = TRUE),
                                             sd   = ~ sd(.x, na.rm = TRUE),
                                             n    = ~ sum(!is.na(.x))),
                     .names = "{.col}_{.fn}"))
  write_csv(q5_means, file.path(out_dir, "Q5_subject_descriptives.csv"))
}

if (!is.null(res_q6_experience)) {
  q6_means <- res_q6_experience$data %>%
    group_by(.data[[experience_var]]) %>%
    summarise(across(all_of(comp_vars), list(mean = ~ mean(.x, na.rm = TRUE),
                                             sd   = ~ sd(.x, na.rm = TRUE),
                                             n    = ~ sum(!is.na(.x))),
                     .names = "{.col}_{.fn}"))
  write_csv(q6_means, file.path(out_dir, "Q6_experience_descriptives.csv"))
}

if (!is.null(res_q7_year)) {
  q7_means <- res_q7_year$data %>%
    group_by(Year) %>%
    summarise(across(all_of(common_names), list(mean = ~ mean(.x, na.rm = TRUE),
                                                sd   = ~ sd(.x, na.rm = TRUE),
                                                n    = ~ sum(!is.na(.x))),
                     .names = "{.col}_{.fn}"))
  write_csv(q7_means, file.path(out_dir, "Q7_year_descriptives.csv"))
}

############################
# 11. SESSION INFO
############################
writeLines(capture.output(sessionInfo()),
           file.path(out_dir, "sessionInfo_section2.txt"))


###############################################################################
# SECTION 2A: NON-TECHNICAL SUMMARY FIGURES
###############################################################################

summary_fig_dir <- file.path(out_dir, "summary_figures")
if (!dir.exists(summary_fig_dir)) dir.create(summary_fig_dir)

############################
# FIGURE 1: Year 4 profiles by grade level
############################
if (!is.null(res_q4_grade)) {
  
  fig1_grade <- res_q4_grade$data %>%
    pivot_longer(cols = all_of(comp_vars), names_to = "Competency", values_to = "Score") %>%
    ggplot(aes(x = .data[[grade_var]], y = Score, fill = .data[[grade_var]])) +
    geom_boxplot(outlier.alpha = 0.25) +
    facet_wrap(~ Competency, scales = "free_y") +
    labs(
      title = "Year 4 Teacher Competency Profiles by Grade Level",
      subtitle = "Each panel shows one competency domain",
      x = "Grade level taught",
      y = "Competency score"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 25, hjust = 1),
      plot.title = element_text(face = "bold"),
      legend.position = "none"
    )
  
  ggsave(
    file.path(summary_fig_dir, "Figure1_GradeLevel_Profiles.png"),
    fig1_grade, width = 12, height = 8, dpi = 300
  )
}

############################
# FIGURE 2: Year 4 profiles by teaching experience
############################
if (!is.null(res_q6_experience)) {
  
  fig2_experience <- res_q6_experience$data %>%
    pivot_longer(cols = all_of(comp_vars), names_to = "Competency", values_to = "Score") %>%
    ggplot(aes(x = .data[[experience_var]], y = Score, fill = .data[[experience_var]])) +
    geom_boxplot(outlier.alpha = 0.25) +
    facet_wrap(~ Competency, scales = "free_y") +
    labs(
      title = "Year 4 Teacher Competency Profiles by Teaching Experience",
      subtitle = "Each panel shows one competency domain",
      x = "Teaching experience",
      y = "Competency score"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 25, hjust = 1),
      plot.title = element_text(face = "bold"),
      legend.position = "none"
    )
  
  ggsave(
    file.path(summary_fig_dir, "Figure2_Experience_Profiles.png"),
    fig2_experience, width = 12, height = 8, dpi = 300
  )
}

############################
# FIGURE 3: Cross-year mean profile
############################
if (!is.null(res_q7_year)) {
  
  fig3_year <- res_q7_year$data %>%
    pivot_longer(cols = all_of(common_names), names_to = "Competency", values_to = "Score") %>%
    group_by(Year, Competency) %>%
    summarise(
      mean_score = mean(Score, na.rm = TRUE),
      sd_score = sd(Score, na.rm = TRUE),
      n = sum(!is.na(Score)),
      se = sd_score / sqrt(n),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = Year, y = mean_score, group = Competency, color = Competency)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = mean_score - se, ymax = mean_score + se), width = 0.1) +
    labs(
      title = "Teacher Competency Profiles Across the Four Survey Years",
      subtitle = "Average score by competency domain and survey year",
      x = "Survey year",
      y = "Average competency score",
      color = "Competency"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "right"
    )
  
  ggsave(
    file.path(summary_fig_dir, "Figure3_CrossYear_Profile_Trends.png"),
    fig3_year, width = 12, height = 7, dpi = 300
  )
}

############################
# OPTIONAL FIGURE 4: Domain-level effect size summary
# Uses Year term / group term effect sizes from ANOVA outputs
############################

# Example: if grade-level eta-squared results exist
if (!is.null(res_q4_grade)) {
  
  fig4_eta_grade <- res_q4_grade$eta %>%
    filter(Parameter != "Residuals") %>%
    ggplot(aes(x = DV, y = Eta2)) +
    geom_col(fill = "#2C7FB8", width = 0.7) +
    labs(
      title = "Magnitude of Grade-Level Differences Across Competency Domains",
      subtitle = "Eta-squared from follow-up ANOVAs",
      x = "Competency domain",
      y = expression(eta^2)
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 25, hjust = 1),
      plot.title = element_text(face = "bold")
    )
  
  ggsave(
    file.path(summary_fig_dir, "Figure4_GradeLevel_EffectSizes.png"),
    fig4_eta_grade, width = 10, height = 6, dpi = 300
  )
}

###############################################################################
# END OF SECTION 2 CODE
###############################################################################

