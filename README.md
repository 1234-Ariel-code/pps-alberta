# Biostatistical Consulting Project

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

## Reproducibility

This repository is organized so that the analysis can be reproduced from the project root using relative paths. The workflow is divided into four main sections: descriptive exploration, group comparisons, structural equation modeling, and longitudinal interpretation.

### Requirements

This project was developed in **R** and is intended to be run in **RStudio** using the included `.Rproj` file.

Install the required R packages before running the scripts.

```r
install.packages(c(
  "tidyverse",
  "readxl",
  "psych",
  "lavaan",
  "semPlot",
  "ggplot2",
  "dplyr",
  "tidyr",
  "stringr",
  "purrr",
  "tibble"
))
```

Depending on your local setup and the exact script content, you may also need:

```r
install.packages(c(
  "here",
  "janitor",
  "scales",
  "patchwork"
))
```

### Project setup

1. Clone the repository:

```bash
git clone https://github.com/YOUR_USERNAME/pps-alberta.git
cd pps-alberta
```

2. Open the project in RStudio by clicking the `.Rproj` file.

3. Make sure the required data files are available in the expected folders under `data/`.

4. Run the scripts from the repository root so that all relative paths resolve correctly.

### Recommended execution order

Run the scripts in the following order:

```r
source("scripts/00_setup.R")
source("scripts/00_helpers.R")
source("scripts/01_section1_descriptive.R")
source("scripts/02_section2_group_comparisons.R")
source("scripts/03_section3_sem.R")
source("scripts/04_section4_longitudinal.R")
```

You may also run them individually from the terminal:

```bash
Rscript scripts/01_section1_descriptive.R
Rscript scripts/02_section2_group_comparisons.R
Rscript scripts/03_section3_sem.R
Rscript scripts/04_section4_longitudinal.R
```

### Data availability

If the repository is shared publicly, some raw or processed data files may not be included because of project restrictions or confidentiality requirements. In that case:

- place the authorized raw files in `data/raw/`
- place any required cleaned files in `data/processed/`
- use the metadata files in `data/metadata/` to match filenames, variables, and scoring structure

### Expected outputs

Running the scripts should generate section-specific outputs under:

- `outputs/section1/`
- `outputs/section2/`
- `outputs/section3/`
- `outputs/section4/`

These outputs may include figures, summary tables, model results, and logs.

### Notes

- All scripts should be run from the repository root.
- Relative paths are preferred throughout the project for portability.
- If a script fails because of a missing package, install the package and rerun the script.
- If a script fails because of a missing file, verify that the required input data are located in the correct `data/` subfolder.
