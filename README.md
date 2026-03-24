# BIST601 Biostatistical Consulting Project

This repository contains the analysis workflow, organized outputs, data files, and report materials for a biostatistical consulting project based on the **Optimal Learning Project**, a four-year Alberta education initiative examining implementation of the Professional Practice Standards.

## Project scope

The repository is organized around four analytical sections:

1. **Descriptive and exploratory analyses**
2. **Group comparison analyses (MANOVA/ANOVA)**
3. **Structural equation modeling (SEM)**
4. **Longitudinal context and trend interpretation**

These sections follow the staged analysis workflow described in the project documents.

## Research questions covered

### Section 1: Descriptive and exploratory questions
- Overall levels and distributions of teacher competency implementation across Teacher Quality Standard domains by year
- Year 4 comparison of implementation and professional learning need across competency domains
- Correlation structure among Year 4 teacher competency domains

### Section 2: Group comparison questions
- Whether competency profiles differ by grade level taught in Year 4
- Whether competency profiles differ by subject specialization in Year 4
- Whether competency profiles differ by teaching experience in Year 4
- Whether implementation advancement differs jointly across the four survey years

### Section 3: Structural relationship questions
- Relationship between perceived competency implementation and corresponding professional learning needs within each TQS domain
- SEM analyses using latent constructs and domain-specific indicators

### Section 4: Longitudinal context questions
- Change in professional learning needs over time
- Whether Year 4 patterns are consistent with trends from earlier years

## Repository structure

```text
BIST601-biostat-consulting-project/
├── README.md
├── LICENSE
├── .gitignore
├── BIST601-biostat-consulting-project.Rproj
├── data/
│   ├── raw/
│   ├── processed/
│   └── metadata/
├── docs/
├── scripts/
├── outputs/
│   ├── section1/
│   ├── section2/
│   ├── section3/
│   └── section4/
├── report/
└── archive/
```

## Contents

The contents of the repo is at follow:

- Original data files in `data/raw/`
- Original project instructions in `docs/`
- Original R scripts are structured for clearer execution order
- Original outputs in section-specific `figures`, `tables`, `text_results`, `logs`, `summary_figures`, and `report_drafts`
- The main report in `report/final_report.docx`

## Script map

| Section script | Repository script |
|---|---|
| `section1.R` | `01_section1_descriptive.R` |
| `section2.R` | `02_section2_group_comparisons.R` |
| `section3.R` | `03_section3_sem.R` |
| `section4.R` | `04_section4_longitudinal.R` |

## Outputs map

Each section follows an internal structure:

- `figures/` for PNG figures
- `tables/` for CSV result tables
- `text_results/` for TXT statistical summaries and notes
- `logs/` for session information and log-like artifacts
- `summary_figures/` for section-level presentation-ready summary figures
- `report_drafts/` for section-level draft report documents

## Notes

- This repository package was organized to make it user-friendly and easier for reproducibity of the analysis.
- Temporary system files are `.DS_Store`, `__MACOSX`, `.Rhistory`, `.RData`, and `.Rapp.history`.

## Source project documents

The client project context is included in:

- `docs/project_questions.docx`
- `docs/scientific_background.docx`

## Main report

The final integrated report is located at:

- `report/final_report.docx`
