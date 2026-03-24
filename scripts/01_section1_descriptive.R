###############################################################################
# SECTION 1: DESCRIPTIVE AND EXPLORATORY QUESTIONS
# Project: Teacher data, Years 1–4
# Questions:
# 1) Overall levels and distributions of teacher competency implementation
#    across the 6 TQS domains for each year
# 2) Year 4 comparison of implementation and professional learning needs
#    across the 6 domains
# 3) Correlations among the 6 TQS competency domains
###############################################################################

############################
# 0. PACKAGES
############################
required_pkgs <- c(
  "readr", "readxl", "dplyr", "tidyr", "stringr", "purrr",
  "ggplot2", "forcats", "tibble", "scales", "corrplot", "psych"
)

installed <- rownames(installed.packages())
for (p in required_pkgs) {
  if (!(p %in% installed)) install.packages(p)
}

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(forcats)
library(tibble)
library(scales)
library(corrplot)
library(psych)

############################
# 1. FILE PATHS
############################
setwd("~/Documents/BIST601-Project")
file_y1 <- "data/Year1teachers.csv"
file_y2 <- "data/Year2Teacher.csv"
file_y3 <- "data/Year3Teacher.csv"
file_y4 <- "data/Year 4-Optimum Learning for All Students- Implementing Professional Practice Standards-Teacher.xlsx"

# OPTIONAL: set output directory
out_dir <- "outputs_section1"
if (!dir.exists(out_dir)) dir.create(out_dir)

############################
# 2. QUESTION-TO-DOMAIN MAPPINGS
############################

# ---- Year 1 implementation mapping (from Table 3)
comp_map_y1 <- list(
  Comp1 = c("Q4", "Q5", "Q6"),
  Comp2 = c("Q7", "Q8", "Q9", "Q11"),
  Comp3 = c("Q14", "Q15"),
  Comp4 = c("Q10", "Q13"),
  Comp5 = c("Q16", "Q17", "Q18", "Q19"),
  Comp6 = c("Q12")
)

# ---- Year 2 / Year 3 implementation mapping (from Table 3)
comp_map_y23 <- list(
  Comp1 = c("Q5", "Q4", "Q81", "Q82", "Q83"),
  Comp2 = c("Q84", "Q85", "Q86", "Q87", "Q88"),
  Comp3 = c("Q89", "Q96", "Q97", "Q90"),
  Comp4 = c("Q91", "Q92", "Q93", "Q94", "Q95"),
  Comp5 = c("Q98", "Q100", "Q101", "Q99"),
  Comp6 = c("Q104", "Q102", "Q103")
)

# ---- Year 4 implementation mapping (from Table 4)
comp_map_y4 <- list(
  Comp1 = c("Q5", "Q4", "Q81", "Q82", "Q83"),
  Comp2 = c("Q84", "Q85", "Q86", "Q87", "Q88"),
  Comp3 = c("Q89", "Q96", "Q97", "Q90"),
  Comp4 = c("Q91", "Q92", "Q93", "Q94", "Q95"),
  Comp5 = c("Q98", "Q100", "Q101", "Q99"),
  Comp6 = c("Q104", "Q102", "Q103")
)

# ---- Year 4 learning need mapping (from Table 4)
# NOTE:
# The appendix explicitly warns about replicated question numbers.
# We preserve the mapping exactly as written here and recommend verifying
# these IDs with the client before final reporting.
need_map_y4 <- list(
  Need1 = c("Q44", "Q105", "Q106", "Q107", "Q108"),
  Need2 = c("Q109", "Q110", "Q113", "Q114", "Q116"),
  Need3 = c("Q94", "Q111", "Q112", "Q115"),
  Need4 = c("Q117", "Q118", "Q95", "Q121", "Q119"),
  Need5 = c("Q120", "Q98", "Q123", "Q124"),
  Need6 = c("Q122", "Q97", "Q96")
)

############################
# 3. HELPER FUNCTIONS
############################

# --- 3.1 Read Qualtrics CSVs for Years 1-3
# Based on your earlier files, Years 1-3 contain metadata rows after the header.
# We remove the first two rows of data (question text / import ID rows).
read_teacher_csv <- function(path) {
  df <- read_csv(path, show_col_types = FALSE)
  
  # Drop first two rows if present
  if (nrow(df) >= 2) {
    df <- df[-c(1, 2), ]
  }
  
  df
}

# --- 3.2 Convert selected question columns to numeric safely
convert_questions_to_numeric <- function(df, question_ids) {
  existing_qs <- intersect(question_ids, names(df))
  
  df %>%
    mutate(across(all_of(existing_qs), ~ suppressWarnings(as.numeric(.x))))
}

# --- 3.3 Score domains by row mean
# min_prop = minimum proportion of non-missing items required to score
score_domains <- function(df, domain_map, min_prop = 0.5) {
  out <- df
  
  for (dom in names(domain_map)) {
    qs <- intersect(domain_map[[dom]], names(df))
    
    if (length(qs) == 0) {
      warning(paste("No items found for", dom))
      out[[dom]] <- NA_real_
      next
    }
    
    nonmiss_required <- ceiling(length(qs) * min_prop)
    
    out[[dom]] <- apply(df[, qs, drop = FALSE], 1, function(x) {
      n_nonmiss <- sum(!is.na(x))
      if (n_nonmiss >= nonmiss_required) {
        mean(x, na.rm = TRUE)
      } else {
        NA_real_
      }
    })
  }
  
  out
}

# --- 3.4 Summary table for domains
domain_summary <- function(df, domain_vars, year_label, construct_label = "Implementation") {
  df %>%
    summarise(across(all_of(domain_vars),
                     list(
                       n = ~ sum(!is.na(.)),
                       mean = ~ mean(., na.rm = TRUE),
                       sd = ~ sd(., na.rm = TRUE),
                       median = ~ median(., na.rm = TRUE),
                       min = ~ min(., na.rm = TRUE),
                       max = ~ max(., na.rm = TRUE)
                     ),
                     .names = "{.col}__{.fn}")) %>%
    pivot_longer(cols = everything(),
                 names_to = c("Domain", ".value"),
                 names_sep = "__") %>%
    mutate(
      Year = year_label,
      Construct = construct_label
    ) %>%
    select(Year, Construct, Domain, everything())
}

# --- 3.5 Long format for plotting
to_long_domains <- function(df, domain_vars, year_label, construct_label = "Implementation") {
  df %>%
    select(all_of(domain_vars)) %>%
    mutate(row_id = row_number()) %>%
    pivot_longer(cols = all_of(domain_vars),
                 names_to = "Domain",
                 values_to = "Score") %>%
    mutate(
      Year = year_label,
      Construct = construct_label
    )
}

# --- 3.6 Nice domain labels
domain_labels_comp <- c(
  Comp1 = "Comp1: Fostering Effective Relationships",
  Comp2 = "Comp2: Engaging in Career-Long Learning",
  Comp3 = "Comp3: Professional Body of Knowledge",
  Comp4 = "Comp4: Inclusive Environments",
  Comp5 = "Comp5: FNMI Knowledge",
  Comp6 = "Comp6: Legal Frameworks & Policies"
)

domain_labels_need <- c(
  Need1 = "Need1: Fostering Effective Relationships",
  Need2 = "Need2: Engaging in Career-Long Learning",
  Need3 = "Need3: Professional Body of Knowledge",
  Need4 = "Need4: Inclusive Environments",
  Need5 = "Need5: FNMI Knowledge",
  Need6 = "Need6: Legal Frameworks & Policies"
)

############################
# 4. LOAD DATA
############################
y1 <- read_teacher_csv(file_y1)
y2 <- read_teacher_csv(file_y2)
y3 <- read_teacher_csv(file_y3)
y4 <- read_excel(file_y4)

############################
# 5. CONVERT RELEVANT QUESTION COLUMNS TO NUMERIC
############################
all_qs_y1 <- unique(unlist(comp_map_y1))
all_qs_y23 <- unique(unlist(comp_map_y23))
all_qs_y4 <- unique(c(unlist(comp_map_y4), unlist(need_map_y4)))

y1 <- convert_questions_to_numeric(y1, all_qs_y1)
y2 <- convert_questions_to_numeric(y2, all_qs_y23)
y3 <- convert_questions_to_numeric(y3, all_qs_y23)
y4 <- convert_questions_to_numeric(y4, all_qs_y4)

############################
# 6. SCORE IMPLEMENTATION DOMAINS
############################
y1_scored <- score_domains(y1, comp_map_y1)
y2_scored <- score_domains(y2, comp_map_y23)
y3_scored <- score_domains(y3, comp_map_y23)
y4_scored <- score_domains(y4, comp_map_y4)

# Score Year 4 needs
y4_scored <- score_domains(y4_scored, need_map_y4)

############################
# 7. QUESTION 1
# Overall levels and distributions of teacher competency implementation
# across the six TQS domains for each year
############################

comp_vars <- names(comp_map_y4)  # Comp1...Comp6

# Summaries by year
sum_y1 <- domain_summary(y1_scored, names(comp_map_y1), "Year 1")
sum_y2 <- domain_summary(y2_scored, names(comp_map_y23), "Year 2")
sum_y3 <- domain_summary(y3_scored, names(comp_map_y23), "Year 3")
sum_y4 <- domain_summary(y4_scored, names(comp_map_y4), "Year 4")

# Combine
summary_q1 <- bind_rows(sum_y1, sum_y2, sum_y3, sum_y4)

# Save table
write_csv(summary_q1, file.path(out_dir, "Q1_domain_summary_by_year.csv"))

# Print to console
print(summary_q1)

# Long data for plotting
long_y1 <- to_long_domains(y1_scored, names(comp_map_y1), "Year 1")
long_y2 <- to_long_domains(y2_scored, names(comp_map_y23), "Year 2")
long_y3 <- to_long_domains(y3_scored, names(comp_map_y23), "Year 3")
long_y4 <- to_long_domains(y4_scored, names(comp_map_y4), "Year 4")

long_comp_all <- bind_rows(long_y1, long_y2, long_y3, long_y4) %>%
  mutate(
    Domain = factor(Domain, levels = names(domain_labels_comp), labels = domain_labels_comp)
  )

# ---- Plot 1A: Mean implementation by year and domain
plot_q1_bar <- long_comp_all %>%
  group_by(Year, Domain) %>%
  summarise(
    mean_score = mean(Score, na.rm = TRUE),
    sd_score = sd(Score, na.rm = TRUE),
    n = sum(!is.na(Score)),
    se = sd_score / sqrt(n),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = Domain, y = mean_score, fill = Year)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_score - se, ymax = mean_score + se),
                position = position_dodge(width = 0.8), width = 0.2) +
  scale_y_continuous(limits = c(0, 5)) +
  labs(
    title = "Teacher Competency Implementation Across TQS Domains by Year",
    x = "TQS Domain",
    y = "Mean Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(out_dir, "Q1_mean_barplot_by_year.png"),
       plot_q1_bar, width = 12, height = 7, dpi = 300)

# ---- Plot 1B: Distributions by year and domain (boxplots)
plot_q1_box <- ggplot(long_comp_all, aes(x = Domain, y = Score, fill = Year)) +
  geom_boxplot(outlier.alpha = 0.3) +
  facet_wrap(~ Year, ncol = 2) +
  scale_y_continuous(limits = c(0, 5)) +
  labs(
    title = "Distribution of Teacher Competency Scores Across TQS Domains by Year",
    x = "TQS Domain",
    y = "Teacher-Level Composite Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none")

ggsave(file.path(out_dir, "Q1_boxplots_by_year.png"),
       plot_q1_box, width = 12, height = 8, dpi = 300)

############################
# 8. QUESTION 2
# Year 4 comparison:
# - implementation across 6 domains
# - professional learning needs across 6 domains
############################

need_vars <- names(need_map_y4)

# Summaries
summary_y4_comp <- domain_summary(y4_scored, comp_vars, "Year 4", "Implementation")
summary_y4_need <- domain_summary(y4_scored, need_vars, "Year 4", "PL Need")

summary_q2 <- bind_rows(summary_y4_comp, summary_y4_need)

write_csv(summary_q2, file.path(out_dir, "Q2_year4_impl_need_summary.csv"))
print(summary_q2)

# Long format
long_y4_comp <- to_long_domains(y4_scored, comp_vars, "Year 4", "Implementation") %>%
  mutate(
    Domain = factor(Domain, levels = names(domain_labels_comp), labels = domain_labels_comp)
  )

long_y4_need <- to_long_domains(y4_scored, need_vars, "Year 4", "PL Need") %>%
  mutate(
    Domain = factor(Domain, levels = names(domain_labels_need), labels = domain_labels_need)
  )

# ---- Plot 2A: Bar plot of means (Year 4 implementation)
plot_q2_impl_bar <- long_y4_comp %>%
  group_by(Domain) %>%
  summarise(
    mean_score = mean(Score, na.rm = TRUE),
    sd_score = sd(Score, na.rm = TRUE),
    n = sum(!is.na(Score)),
    se = sd_score / sqrt(n),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = Domain, y = mean_score)) +
  geom_col(fill = "#2C7FB8", width = 0.7) +
  geom_errorbar(aes(ymin = mean_score - se, ymax = mean_score + se), width = 0.2) +
  scale_y_continuous(limits = c(0, 5)) +
  labs(
    title = "Year 4: Mean Teacher Implementation Scores by TQS Domain",
    x = "TQS Domain",
    y = "Mean Implementation Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(out_dir, "Q2_year4_implementation_barplot.png"),
       plot_q2_impl_bar, width = 11, height = 6, dpi = 300)

# ---- Plot 2B: Boxplot of implementation (Year 4)
plot_q2_impl_box <- ggplot(long_y4_comp, aes(x = Domain, y = Score, fill = Domain)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_y_continuous(limits = c(0, 5)) +
  labs(
    title = "Year 4: Distribution of Teacher Implementation Scores by TQS Domain",
    x = "TQS Domain",
    y = "Implementation Composite Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none")

ggsave(file.path(out_dir, "Q2_year4_implementation_boxplot.png"),
       plot_q2_impl_box, width = 11, height = 6, dpi = 300)

# ---- Plot 2C: Bar plot of means (Year 4 professional learning need)
# IMPORTANT:
# If these need scales are not all on the same raw scale, verify with client before
# comparing raw means directly. If necessary, standardize or rescale.
plot_q2_need_bar <- long_y4_need %>%
  group_by(Domain) %>%
  summarise(
    mean_score = mean(Score, na.rm = TRUE),
    sd_score = sd(Score, na.rm = TRUE),
    n = sum(!is.na(Score)),
    se = sd_score / sqrt(n),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = Domain, y = mean_score)) +
  geom_col(fill = "#F03B20", width = 0.7) +
  geom_errorbar(aes(ymin = mean_score - se, ymax = mean_score + se), width = 0.2) +
  labs(
    title = "Year 4: Mean Professional Learning Need Scores by Domain",
    x = "PL Need Domain",
    y = "Mean Need Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(out_dir, "Q2_year4_need_barplot.png"),
       plot_q2_need_bar, width = 11, height = 6, dpi = 300)

# ---- Plot 2D: Boxplot of need scores (Year 4)
plot_q2_need_box <- ggplot(long_y4_need, aes(x = Domain, y = Score, fill = Domain)) +
  geom_boxplot(outlier.alpha = 0.3) +
  labs(
    title = "Year 4: Distribution of Professional Learning Need Scores by Domain",
    x = "PL Need Domain",
    y = "Need Composite Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none")

ggsave(file.path(out_dir, "Q2_year4_need_boxplot.png"),
       plot_q2_need_box, width = 11, height = 6, dpi = 300)

############################
# 9. QUESTION 3
# Relationships among the six competency domains
# Correlation matrix of teacher-level composite competency scores
############################

# We will do this for each year, and especially Year 4

# ---- helper for correlation extraction
cor_table <- function(df, domain_vars, year_label) {
  cor_mat <- cor(df[, domain_vars], use = "pairwise.complete.obs")
  cor_df <- as.data.frame(as.table(cor_mat)) %>%
    rename(Domain1 = Var1, Domain2 = Var2, Correlation = Freq) %>%
    mutate(Year = year_label)
  cor_df
}

# Correlation matrices
cor_y1 <- cor(y1_scored[, names(comp_map_y1)], use = "pairwise.complete.obs")
cor_y2 <- cor(y2_scored[, names(comp_map_y23)], use = "pairwise.complete.obs")
cor_y3 <- cor(y3_scored[, names(comp_map_y23)], use = "pairwise.complete.obs")
cor_y4 <- cor(y4_scored[, names(comp_map_y4)], use = "pairwise.complete.obs")

# Save numeric matrices
write.csv(cor_y1, file.path(out_dir, "Q3_correlation_matrix_year1.csv"), row.names = TRUE)
write.csv(cor_y2, file.path(out_dir, "Q3_correlation_matrix_year2.csv"), row.names = TRUE)
write.csv(cor_y3, file.path(out_dir, "Q3_correlation_matrix_year3.csv"), row.names = TRUE)
write.csv(cor_y4, file.path(out_dir, "Q3_correlation_matrix_year4.csv"), row.names = TRUE)

# Print Year 4 matrix
print(cor_y4)

# ---- Correlation plot Year 4
png(file.path(out_dir, "Q3_correlation_plot_year4.png"), width = 1000, height = 900, res = 150)
corrplot(
  cor_y4,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 30,
  number.cex = 0.8,
  col = colorRampPalette(c("#B2182B", "#F7F7F7", "#2166AC"))(200),
  title = "Year 4: Correlation Matrix of TQS Competency Domain Scores",
  mar = c(0, 0, 2, 0)
)
dev.off()

# ---- Optional: rank domains by average absolute correlation (Year 4)
avg_abs_cor_y4 <- tibble(
  Domain = colnames(cor_y4),
  AvgAbsCorrelation = sapply(seq_len(ncol(cor_y4)), function(j) {
    mean(abs(cor_y4[j, -j]), na.rm = TRUE)
  })
) %>%
  arrange(desc(AvgAbsCorrelation))

write_csv(avg_abs_cor_y4, file.path(out_dir, "Q3_year4_avg_absolute_correlations.csv"))
print(avg_abs_cor_y4)

############################
# 10. OPTIONAL RELIABILITY CHECKS (HIGHLY RECOMMENDED)
############################
# Cronbach alpha for each implementation domain
# (Useful for report appendix)
alpha_results <- map_df(names(comp_map_y4), function(dom) {
  qs <- comp_map_y4[[dom]]
  qs <- intersect(qs, names(y4_scored))
  
  a <- psych::alpha(y4_scored[, qs], warnings = FALSE, check.keys = FALSE)
  
  tibble(
    Domain = dom,
    Alpha = a$total$raw_alpha
  )
})

write_csv(alpha_results, file.path(out_dir, "Q1Q3_year4_alpha_by_domain.csv"))
print(alpha_results)

############################
# 11. SESSION INFO
############################
writeLines(capture.output(sessionInfo()),
           file.path(out_dir, "sessionInfo_section1.txt"))

###############################################################################
# SECTION 1A: NON-TECHNICAL SUMMARY FIGURES
# These are simplified, presentation-friendly visuals for the report summary.
###############################################################################

# Create a separate folder for summary figures
summary_fig_dir <- file.path(out_dir, "summary_figures")
if (!dir.exists(summary_fig_dir)) dir.create(summary_fig_dir)

############################
# FIGURE 1: Average implementation by domain and year
############################

fig1_summary <- long_comp_all %>%
  group_by(Year, Domain) %>%
  summarise(
    mean_score = mean(Score, na.rm = TRUE),
    sd_score = sd(Score, na.rm = TRUE),
    n = sum(!is.na(Score)),
    se = sd_score / sqrt(n),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = Domain, y = mean_score, fill = Year)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_score - se, ymax = mean_score + se),
    position = position_dodge(width = 0.8),
    width = 0.18
  ) +
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, 1)) +
  labs(
    title = "Average Teacher Implementation by TQS Domain and Year",
    subtitle = "Higher scores indicate stronger self-reported implementation",
    x = NULL,
    y = "Average score"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

ggsave(
  file.path(summary_fig_dir, "Figure1_Implementation_By_Year.png"),
  fig1_summary, width = 12, height = 7, dpi = 300
)

############################
# FIGURE 2: Year 4 implementation vs need by domain
############################

# Build a side-by-side comparison dataset
fig2_impl <- long_y4_comp %>%
  group_by(Domain) %>%
  summarise(
    mean_score = mean(Score, na.rm = TRUE),
    sd_score = sd(Score, na.rm = TRUE),
    n = sum(!is.na(Score)),
    se = sd_score / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(Type = "Implementation")

fig2_need <- long_y4_need %>%
  mutate(
    Domain = factor(
      Domain,
      levels = levels(long_y4_need$Domain),
      labels = levels(long_y4_comp$Domain)
    )
  ) %>%
  group_by(Domain) %>%
  summarise(
    mean_score = mean(Score, na.rm = TRUE),
    sd_score = sd(Score, na.rm = TRUE),
    n = sum(!is.na(Score)),
    se = sd_score / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(Type = "Professional Learning Need")

fig2_data <- bind_rows(fig2_impl, fig2_need)

fig2_summary <- ggplot(fig2_data, aes(x = Domain, y = mean_score, fill = Type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_score - se, ymax = mean_score + se),
    position = position_dodge(width = 0.8),
    width = 0.18
  ) +
  labs(
    title = "Year 4: Implementation and Professional Learning Need by Domain",
    subtitle = "Use caution if need domains are on different raw response scales",
    x = NULL,
    y = "Average score"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

ggsave(
  file.path(summary_fig_dir, "Figure2_Year4_Impl_vs_Need.png"),
  fig2_summary, width = 12, height = 7, dpi = 300
)

############################
# FIGURE 3: Year 4 correlation heatmap
############################

png(file.path(summary_fig_dir, "Figure3_Year4_Correlation_Heatmap.png"),
    width = 1200, height = 1000, res = 180)

corrplot(
  cor_y4,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 25,
  number.cex = 0.8,
  col = colorRampPalette(c("#B2182B", "#F7F7F7", "#2166AC"))(200),
  title = "Year 4 Correlations Among TQS Competency Domains",
  mar = c(0, 0, 2, 0)
)

dev.off()

############################
# OPTIONAL FIGURE 4: Year 4 implementation spread by domain
############################

fig4_summary <- ggplot(long_y4_comp, aes(x = Domain, y = Score, fill = Domain)) +
  geom_boxplot(outlier.alpha = 0.25) +
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, 1)) +
  labs(
    title = "Year 4: Spread of Implementation Scores by Domain",
    subtitle = "Boxplots show how much teacher responses vary within each domain",
    x = NULL,
    y = "Implementation score"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

ggsave(
  file.path(summary_fig_dir, "Figure4_Year4_Implementation_Boxplots.png"),
  fig4_summary, width = 11, height = 6, dpi = 300
)

###############################################################################
# END OF SECTION 1 CODE
###############################################################################
