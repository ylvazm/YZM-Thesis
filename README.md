# TITLE

> Master's Thesis | Universitetet i Oslo | 2026  
> Author: Ylva Zoe Matejko

---

## Overview

This repository contains the R code and raw data used for the empirical analyses in my master's thesis on **TITLE**, using and examplary **TIMSS 2023** dataset.

The analyses examine ... [maybe one or two sentences from my abstract]

---

## Repository Structure

```
├── Scripts/             # All R analysis scripts (run in order, see below)
│    └── Supplemental/   # Supplemental analysis scripts containing explorations not reported in the thesis
├── Data/                # Relevant data files 
├── Objects/             # Relevant objects files
├── Models/              # This is where the models are stored
└── Output/              # This is where the figures and tables for reporting are stored
```

---

## Requirements and Setup

- **R** version [e.g. 4.3.0 or higher]
- All required packages are listed at the top of each script and can be installed via `install.packages()`
- write here what needs to be done for seamless analysis.
- Download the repository or set up something to access the repository from your pc idk
- The scripts are wirtten for local access so I recommend that

---

## Data

The analyses are based on **TIMSS 2023** data. The scripts numbered 01 to 03 contain all code related to data wrangling, starting with the raw TIMSS 2023 data files including all achievement items and participants in grades 4 and 8, and ending with the fully ready-to-analyse data files for grade 8 science. 

> The next section will walk you through the steps I took in data wrangling.
> If you wish to skip the data wrangling, go straight to "How to Run the Analysis" below. The fully cleaned ready-to-analyse data file is in the "Data" folder, too.

---

## How to Run the Analysis

The scripts are designed to be executed **in the following order**:

> Skip to step 4 if you wish to skip the data wrangling and start with the analysis right away.

---

### Step 1 — Run `01_Subset_Choice.R`

- **Purpose:** The purpose of this script is to determine the choice of subset of the TIMSS 2023 data to be used in the present study.
- This script corresponds to chapter 2.2.1 in the thesis.

---

### Step 2 — Run `02_Scoring_v0.R`, `02_Scoring_v1.R`, `02_Scoring_v2.R`, `02_Scoring_v3.R`

- **Purpose:** These four scripts score the raw TIMSS 2023 Grade 8 achievement data using the scoring algorithm provided in the TIMSS 2023 International Database User Guide (Fishbein et al., 2025), each implementing a different treatment of omitted items. 
- `02_Scoring_v0.R` creates a response dataset where the original codes for omitted and not-reached items are kept as-is, while valid item responses are scored. 
- `02_Scoring_v1.R` treats omitted items as not administered. 
- `02_Scoring_v2.R` scores omitted items as incorrect. 
- `02_Scoring_v3.R` treats omitted items as not administered but additionally computes the completeness indicator variables needed for Modified Laplace Smoothing. 
- Each script saves its output as a separate .Rdata file (GRADE8DATA_COMPLETE_v0_SCR.Rdata through GRADE8DATA_COMPLETE_v3_SCR.Rdata), which are used in the subsequent analyses.

---

### Step 3 — Run `03_Data_Prep.R`

- **Purpose:** Prepare the three scored datasets for analysis. 
- Essentially, for each version of the dataset, variables irrelevant to further analyses are removed. 
- The output of these scripts are three versions of the dataset fully prepared for the analyses, g8s_v0 through g8s_v3.
- Optional: To examine the preparation of the three dataset versions by making some logical checks, run `03_Data_Prep_Check.R` from the supplemental scripts folder.

---

### Step 4 — Run `04_Descriptives.R`

> Start here if you wish to skip the data wrangling and start with the analysis right away.

- **Purpose:** This script produces the descriptive statistics for the full sample in the analyses.
- The key output of this script is the descriptive table included in Appendix A.1 and the omission statistics graph reported in chapter 2.2.2
- Optional: For additional graphs, run `04_Descriptives_Supplemental.R` from the supplemental scripts folder.

---

### Step 5 — Run `05_Subsets.R`

- **Purpose:** In this script, the subsample for item parameter estimation is drawn and compared with the full sample to check representativity.
- The output of this script are three versions of the sampled subset fully prepare for the analyses, corresponding to the three versions of the full dataset.

---

### Step 6 — Run `06_Model_Estimation.R` and `06_Model_Documentation`

- **Purpose:** These scripts contain the code to estimate all three models for the present analysis and extract relevant objects from them. 
- For each model, first randomly drawn subsample is used to estimate item parameters. Then, the resulting parameters are applied to the full sample models to estimate country means and variances.
- The central output of `06_Model_Estimation.R` are model1, model2, and model3, which will be stored for further inspection and extraction of results.
- The central output of `06_Model_Documentation.R` are the item parameter tables, as well as model and item fit statistics.

---

### Step 7 — Run `07_Results_Means.R`, `07_Results_Variances.R`, `07_Results_Pseudo-Items.R`

- **Purpose:** These scripts produce the results reported in my thesis.
- In `07_Results_Means.R`, all reported results regarding country means and achievement ranks are produced. This includes (1) the extraction of country means and the computation of achievement rankings for all models, (2) the investigation of the agreement between the produced rankings and ranking differences, (3) the central graph depicting both 1 and 2, and (4) the investigation of the relationship between item omissions and rank differences between the models.
- In `07_Results_Means.R`, all reported results regarding country variances and variance ranks are produced. This includes (1) the extraction of country varaincess and the computation of variance rankings for all models, (2) the investigation of the agreement between the produced rankings and ranking differences, (3) the central graph depicting both 1 and 2, and (4) the investigation of the relationship between item omissions and rank differences between the models.
- Run `07_Results_Pseudo-Items.R` to investigate the Pseudo-Items estimated in Modified Laplace Smoothing and obtain the graph from Chapter...
- Optionally, for further explorations of the means and variances, run `07_Results_Supplemental.R` from the supplemental scripts folder.

---

## Output

The `Output/` folder will contain:

- **Figures:** [e.g. "Plots comparing ability estimate distributions across omission treatments"]
- **Tables:** [e.g. "Summary statistics, model fit indices"]
- **Other objects:** [e.g. "Saved model objects (.rds files)"]

---

## Citation

If you use this code, please cite the thesis as:

[Your Name] ([Year]). *Item Omission Treatments in ILSAs: A TIMSS Case Study*. Master's Thesis, [University Name].

---

## License

[Choose one, e.g. MIT / CC BY 4.0 / or leave as "All rights reserved"]

---

## References

- Fishbein, B., Taneva, M., & Kowolik, K. (2025). TIMSS 2023 User Guide for the International Database. Boston College, TIMSS & PIRLS International Study Center. https://timss2023.org/data

