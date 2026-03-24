###############################################################################
# SECTION 4: LONGITUDINAL CONTEXT QUESTIONS
#
# Q9. How do professional learning needs change over time across project years?
# Q10. Are observed patterns in Year 4 consistent with trends identified in
#      earlier years of the project?
#
# IMPORTANT:
# Based on the appendix provided:
# - Years 1–3 mappings are available for implementation competencies
# - Year 4 mappings are available for implementation + PL need
#
# Therefore:
# - longitudinal implementation analysis across Years 1–4 is fully supported
# - longitudinal PL-need analysis across Years 1–4 requires additional mapping
###############################################################################

############################
# 0. PACKAGES
############################
required_pkgs <- c(
  "readr", "readxl", "dplyr", "tidyr", "stringr", "purrr",
  "ggplot2", "tibble", "scales", "forcats", "broom"
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
library(tibble)
library(scales)
library(forcats)
library(broom)

############################
# 1. FILE PATHS
############################
setwd("~/Documents/BIST601-Project")
file_y1 <- "data/Year1teachers.csv"
file_y2 <- "data/Year2Teacher.csv"
file_y3 <- "data/Year3Teacher.csv"
file_y4 <- "data/Year 4-Optimum Learning for All Students- Implementing Professional Practice Standards-Teacher.xlsx"


out_dir <- "outputs_section4_longitudinal"
if (!dir.exists(out_dir)) dir.create(out_dir)

############################
# 2. APPENDIX-BASED MAPPINGS
############################

# Years 1-3 implementation mappings (Table 3)
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

# Year 4 implementation mappings (Table 4)
comp_map_y4 <- list(
  Comp1 = c("Q5", "Q4", "Q81", "Q82", "Q83"),
  Comp2 = c("Q84", "Q85", "Q86", "Q87", "Q88"),
  Comp3 = c("Q89", "Q96", "Q97", "Q90"),
  Comp4 = c("Q91", "Q92", "Q93", "Q94", "Q95"),
  Comp5 = c("Q98", "Q100", "Q101", "Q99"),
  Comp6 = c("Q104", "Q102", "Q103")
)

# Year 4 PL-need mappings (Table 4)
need_map_y4 <- list(
  Need1 = c("Q44", "Q105", "Q106", "Q107", "Q108"),
  Need2 = c("Q109", "Q110", "Q113", "Q114", "Q116"),
  Need3 = c("Q94", "Q111", "Q112", "Q115"),
  Need4 = c("Q117", "Q118", "Q95", "Q121", "Q119"),
  Need5 = c("Q120", "Q98", "Q123", "Q124"),
  Need6 = c("Q122", "Q97", "Q96")
)

domain_labels <- c(
  Comp1 = "Comp1: Fostering Effective Relationships",
  Comp2 = "Comp2: Engaging in Career-Long Learning",
  Comp3 = "Comp3: Professional Body of Knowledge",
  Comp4 = "Comp4: Inclusive Environments",
  Comp5 = "Comp5: FNMI Knowledge",
  Comp6 = "Comp6: Legal Frameworks & Policies"
)

############################
# 3. HELPER FUNCTIONS
############################

read_teacher_csv <- function(path) {
  df <- read_csv(path, show_col_types = FALSE)
  if (nrow(df) >= 2) {
    df <- df[-c(1, 2), ]
  }
  df
}

convert_questions_to_numeric <- function(df, question_ids) {
  existing_qs <- intersect(question_ids, names(df))
  df %>%
    mutate(across(all_of(existing_qs), ~ suppressWarnings(as.numeric(.x))))
}

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
      if (n_nonmiss >= nonmiss_required) {
        mean(x, na.rm = TRUE)
      } else {
        NA_real_
      }
    })
  }
  out
}

to_long_domains <- function(df, domain_vars, year_label) {
  df %>%
    select(all_of(domain_vars)) %>%
    mutate(row_id = row_number()) %>%
    pivot_longer(cols = all_of(domain_vars),
                 names_to = "Domain",
                 values_to = "Score") %>%
    mutate(Year = year_label)
}

############################
# 4. LOAD DATA
############################
y1 <- read_teacher_csv(file_y1)
y2 <- read_teacher_csv(file_y2)
y3 <- read_teacher_csv(file_y3)
y4 <- read_excel(file_y4)

# Convert implementation items
y1 <- convert_questions_to_numeric(y1, unique(unlist(comp_map_y1)))
y2 <- convert_questions_to_numeric(y2, unique(unlist(comp_map_y23)))
y3 <- convert_questions_to_numeric(y3, unique(unlist(comp_map_y23)))
y4 <- convert_questions_to_numeric(y4, unique(c(unlist(comp_map_y4), unlist(need_map_y4))))

# Score implementation composites
y1_scored <- score_domains(y1, comp_map_y1)
y2_scored <- score_domains(y2, comp_map_y23)
y3_scored <- score_domains(y3, comp_map_y23)
y4_scored <- score_domains(y4, comp_map_y4)

# Score Year 4 need composites
y4_scored <- score_domains(y4_scored, need_map_y4)

############################
# 5. LONGITUDINAL IMPLEMENTATION ANALYSIS (YEARS 1–4)
# This addresses Q10 directly and supports trend context for Q9.
############################

# Standardize domain names across years
common_names <- paste0("Comp", 1:6)

y1_comp <- y1_scored %>%
  select(all_of(names(comp_map_y1)))
names(y1_comp) <- common_names
y1_comp$Year <- "Year 1"

y2_comp <- y2_scored %>%
  select(all_of(names(comp_map_y23)))
names(y2_comp) <- common_names
y2_comp$Year <- "Year 2"

y3_comp <- y3_scored %>%
  select(all_of(names(comp_map_y23)))
names(y3_comp) <- common_names
y3_comp$Year <- "Year 3"

y4_comp <- y4_scored %>%
  select(all_of(names(comp_map_y4)))
names(y4_comp) <- common_names
y4_comp$Year <- "Year 4"

all_impl <- bind_rows(y1_comp, y2_comp, y3_comp, y4_comp) %>%
  mutate(Year = factor(Year, levels = c("Year 1", "Year 2", "Year 3", "Year 4")))

# Long format
all_impl_long <- all_impl %>%
  pivot_longer(cols = all_of(common_names),
               names_to = "Domain",
               values_to = "Score") %>%
  mutate(
    Domain = factor(Domain, levels = names(domain_labels), labels = domain_labels)
  )

############################
# 6. DESCRIPTIVE TREND TABLES
############################

impl_trend_summary <- all_impl_long %>%
  group_by(Year, Domain) %>%
  summarise(
    n = sum(!is.na(Score)),
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE),
    median = median(Score, na.rm = TRUE),
    q1 = quantile(Score, 0.25, na.rm = TRUE),
    q3 = quantile(Score, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(impl_trend_summary,
          file.path(out_dir, "longitudinal_implementation_trend_summary.csv"))

print(impl_trend_summary)

############################
# 7. VISUALIZE IMPLEMENTATION TRENDS
############################

# 7A. Mean trend plot with SE bars
plot_impl_trend <- all_impl_long %>%
  group_by(Year, Domain) %>%
  summarise(
    mean_score = mean(Score, na.rm = TRUE),
    sd_score = sd(Score, na.rm = TRUE),
    n = sum(!is.na(Score)),
    se = sd_score / sqrt(n),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = Year, y = mean_score, group = Domain, color = Domain)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_score - se, ymax = mean_score + se), width = 0.1) +
  labs(
    title = "Implementation Trends Across Project Years by TQS Domain",
    x = "Survey Year",
    y = "Mean Implementation Score",
    color = "TQS Domain"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(out_dir, "implementation_trends_lineplot.png"),
       plot_impl_trend, width = 12, height = 7, dpi = 300)

# 7B. Boxplots by year within domain
plot_impl_box <- ggplot(all_impl_long, aes(x = Year, y = Score, fill = Year)) +
  geom_boxplot(outlier.alpha = 0.25) +
  facet_wrap(~ Domain, scales = "free_y") +
  labs(
    title = "Distribution of Implementation Scores Across Years",
    x = "Survey Year",
    y = "Implementation Composite Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

ggsave(file.path(out_dir, "implementation_trends_boxplots.png"),
       plot_impl_box, width = 12, height = 8, dpi = 300)

############################
# 8. FORMAL TESTS OF IMPLEMENTATION DIFFERENCES ACROSS YEARS
############################

# One-way ANOVA by domain across years
impl_year_anova <- all_impl_long %>%
  group_by(Domain) %>%
  group_modify(~ {
    fit <- aov(Score ~ Year, data = .x)
    broom::tidy(fit)
  }) %>%
  ungroup()

# Keep Year term only
impl_year_anova_year_term <- impl_year_anova %>%
  filter(term == "Year") %>%
  mutate(
    p_adj_bh = p.adjust(p.value, method = "BH"),
    p_adj_holm = p.adjust(p.value, method = "holm")
  )

write_csv(impl_year_anova,
          file.path(out_dir, "implementation_year_anova_raw.csv"))
write_csv(impl_year_anova_year_term,
          file.path(out_dir, "implementation_year_anova_adjusted.csv"))

print(impl_year_anova_year_term)

############################
# 9. YEAR 4 CONSISTENCY WITH EARLIER YEARS
# This is the main direct answer to Q10
############################

# Compare Year 4 rank ordering of domains to earlier years
domain_rank_table <- impl_trend_summary %>%
  group_by(Year) %>%
  arrange(desc(mean), .by_group = TRUE) %>%
  mutate(RankWithinYear = row_number()) %>%
  ungroup()

write_csv(domain_rank_table,
          file.path(out_dir, "domain_rankings_by_year.csv"))

print(domain_rank_table)

# Correlation of domain-level mean profile between Year 4 and earlier years
mean_profile_wide <- impl_trend_summary %>%
  select(Year, Domain, mean) %>%
  pivot_wider(names_from = Year, values_from = mean)

profile_correlations <- tibble(
  Comparison = c("Year4 vs Year1", "Year4 vs Year2", "Year4 vs Year3"),
  Correlation = c(
    cor(mean_profile_wide$`Year 4`, mean_profile_wide$`Year 1`, use = "complete.obs"),
    cor(mean_profile_wide$`Year 4`, mean_profile_wide$`Year 2`, use = "complete.obs"),
    cor(mean_profile_wide$`Year 4`, mean_profile_wide$`Year 3`, use = "complete.obs")
  )
)

write_csv(profile_correlations,
          file.path(out_dir, "year4_profile_consistency_correlations.csv"))

print(profile_correlations)

############################
# 10. PROFESSIONAL LEARNING NEEDS OVER TIME (QUESTION 9)
############################

# IMPORTANT:
# Based on the appendix provided, we only have explicit PL-need mappings for Year 4.
# Therefore, a full 4-year longitudinal analysis of PL needs is NOT yet defensible.
#
# What we can do now:
# - describe Year 4 PL-need profile in detail
# - compare Year 4 need profile to Year 4 implementation profile
# - state that Years 1–3 PL-need mappings are required for true longitudinal analysis
############################

need_vars <- paste0("Need", 1:6)

y4_need_long <- y4_scored %>%
  select(all_of(need_vars)) %>%
  pivot_longer(cols = everything(), names_to = "NeedDomain", values_to = "Score")

y4_need_summary <- y4_need_long %>%
  group_by(NeedDomain) %>%
  summarise(
    n = sum(!is.na(Score)),
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE),
    median = median(Score, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(y4_need_summary,
          file.path(out_dir, "year4_need_summary_only.csv"))

print(y4_need_summary)

# Plot Year 4 need profile
plot_y4_need <- ggplot(y4_need_long, aes(x = NeedDomain, y = Score, fill = NeedDomain)) +
  geom_boxplot(outlier.alpha = 0.25) +
  labs(
    title = "Year 4 Professional Learning Need Profile by Domain",
    x = "PL Need Domain",
    y = "Need Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

ggsave(file.path(out_dir, "year4_need_boxplots.png"),
       plot_y4_need, width = 10, height = 6, dpi = 300)

############################
# 11. CONDITIONAL FRAMEWORK FOR TRUE LONGITUDINAL PL-NEED ANALYSIS
############################

# This section creates a placeholder note for the report and workflow.
need_longitudinal_note <- c(
  "QUESTION 9 NOTE:",
  "The appendix provided explicit professional learning need mappings only for Year 4.",
  "Therefore, a longitudinal analysis of PL needs across Years 1–4 cannot be conducted",
  "defensibly until the client provides the corresponding PL-need item mappings for Years 1–3.",
  "At present, only the Year 4 PL-need profile can be summarized directly."
)

writeLines(need_longitudinal_note,
           file.path(out_dir, "QUESTION9_limitation_note.txt"))

############################
# 12. OPTIONAL: IF CLIENT LATER PROVIDES YEARS 1–3 NEED MAPPINGS
# TEMPLATE FUNCTION READY TO USE
############################

score_need_if_mapping_exists <- function(df, need_map) {
  score_domains(df, need_map)
}

# Example usage later:
# need_map_y1 <- list(...)
# need_map_y23 <- list(...)
# y1_need_scored <- score_need_if_mapping_exists(y1, need_map_y1)
# y2_need_scored <- score_need_if_mapping_exists(y2, need_map_y23)
# y3_need_scored <- score_need_if_mapping_exists(y3, need_map_y23)
# then bind_rows the same way as implementation

############################
# 13. SESSION INFO
############################
writeLines(capture.output(sessionInfo()),
           file.path(out_dir, "sessionInfo_section4_longitudinal.txt"))

###############################################################################
# SECTION 4A: NON-TECHNICAL SUMMARY FIGURES
###############################################################################

summary_fig_dir <- file.path(out_dir, "summary_figures")
if (!dir.exists(summary_fig_dir)) dir.create(summary_fig_dir)

############################
# FIGURE 1: Implementation trends across years by domain
############################

fig1_trends <- all_impl_long %>%
  group_by(Year, Domain) %>%
  summarise(
    mean_score = mean(Score, na.rm = TRUE),
    sd_score = sd(Score, na.rm = TRUE),
    n = sum(!is.na(Score)),
    se = sd_score / sqrt(n),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = Year, y = mean_score, group = Domain, color = Domain)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_score - se, ymax = mean_score + se), width = 0.1) +
  labs(
    title = "Implementation Trends Across the Four Survey Years",
    subtitle = "Average implementation score by TQS domain",
    x = "Survey year",
    y = "Average implementation score",
    color = "TQS domain"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

ggsave(
  file.path(summary_fig_dir, "Figure1_Implementation_Trends.png"),
  fig1_trends, width = 12, height = 7, dpi = 300
)

############################
# FIGURE 2: Year-by-year distributions within domain
############################

fig2_distributions <- ggplot(all_impl_long, aes(x = Year, y = Score, fill = Year)) +
  geom_boxplot(outlier.alpha = 0.25) +
  facet_wrap(~ Domain, scales = "free_y") +
  labs(
    title = "Implementation Score Distributions Across Years",
    subtitle = "Each panel shows one competency domain",
    x = "Survey year",
    y = "Implementation score"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

ggsave(
  file.path(summary_fig_dir, "Figure2_Yearly_Domain_Distributions.png"),
  fig2_distributions, width = 12, height = 8, dpi = 300
)

############################
# FIGURE 3: Year 4 professional learning need profile
############################

# A cleaner summary bar plot for Year 4 need
fig3_need <- y4_need_summary %>%
  ggplot(aes(x = NeedDomain, y = mean)) +
  geom_col(fill = "#F03B20", width = 0.7) +
  geom_errorbar(
    aes(ymin = mean - sd / sqrt(n), ymax = mean + sd / sqrt(n)),
    width = 0.18
  ) +
  labs(
    title = "Year 4 Professional Learning Need Profile",
    subtitle = "Average need score by domain",
    x = "Need domain",
    y = "Average need score"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

ggsave(
  file.path(summary_fig_dir, "Figure3_Year4_Need_Profile.png"),
  fig3_need, width = 10, height = 6, dpi = 300
)

############################
# FIGURE 4: What is currently supported longitudinally?
############################

support_df <- tibble(
  Analysis = c(
    "Implementation trends across Years 1–4",
    "Professional learning needs across Years 1–4"
  ),
  Status = c("Supported by current appendix", "Not yet supported by current appendix")
)

fig4_support <- ggplot(support_df, aes(x = Analysis, y = 1, fill = Status)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = Status), color = "black", size = 4, vjust = -0.2) +
  scale_y_continuous(limits = c(0, 1.2), breaks = NULL) +
  labs(
    title = "What the Current Documentation Supports",
    subtitle = "Longitudinal implementation is currently analysable; need trends require additional mappings",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 10, hjust = 1),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(summary_fig_dir, "Figure4_Longitudinal_Support_Status.png"),
  fig4_support, width = 10, height = 5, dpi = 300
)

###############################################################################
# END OF SECTION 4 CODE
###############################################################################

