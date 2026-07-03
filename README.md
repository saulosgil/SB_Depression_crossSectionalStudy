# 🧠📉 SB_Depression_crossSectionalStudy

Repository with **R** scripts for analyzing the association between **sedentary behavior** and **depressive symptoms** in a cross-sectional study.

The project includes routines for:

- 📦 data preparation;
- 🧹 handling of missing data;
- 📊 logistic regression analyses;
- 📈 linear trend analyses;
- 📉 modeling with **restricted cubic splines (RCS)**;
- 🧾 generation of intermediate analytical datasets.

The repository currently contains multiple analysis folders, main R scripts, `.rds` files, an `.xlsx` spreadsheet, a `.jpeg` image, and an RStudio project file.

---

## 📖 About the project

This repository organizes the analytical workflow of a cross-sectional study investigating the relationship between:

- **total sedentary time**;
- **mentally passive sedentary behavior**;
- **mentally active sedentary behavior**;

and **depression-related outcomes**.

Based on the available scripts, the depression variable is handled both as a **continuous score** and as a **categorical outcome**, including a dichotomization of the depression score into `< 20` versus `≥ 20`. The sedentary exposures also appear both continuously and categorized as `< 4 h/day` versus `≥ 4 h/day`.

---

## 🎯 Analytical objectives

Based on the repository's files, this project allows:

- assessing the association between **sedentary behavior (SB)** and depression;
- separating analyses by **mentally passive sedentary behavior (MPSB)** and **mentally active sedentary behavior (MASB)**;
- testing linear and non-linear associations;
- adjusting models for sociodemographic and clinical variables;
- exploring additional analyses with subgroups and alternative model versions.

---

## 🗂️ Repository structure

```
SB_Depression_crossSectionalStudy/
├── Hallgren_analises_AdjustedMvpa_continuos/
├── Stubbs_analises_AdjustedMvpa_continuos_older54/
├── analises_AdjustedMvpa_categorical/
├── analises_AdjustedMvpa_continuos/
├── .gitignore
├── SB_Depression_crossSectionalStudy.Rproj
├── calculando_vars.R
├── data_prep.R
├── database_17122025.rds
├── df_para_CalcularVars.rds
├── df_para_analise.rds
├── df_para_analise_hallgren.rds
├── linear_trend_masb.R
├── linear_trend_mpsb.R
├── linear_trend_sb.R
├── logistic_reg.R
├── rcs_analysis.R
└── tratamento_missing.R
```

The repository contains four main analysis directories, in addition to scripts for data preparation, modeling, and exploratory analysis.

---

## 📁 Main files

### `data_prep.R`
Data preparation script. The file name indicates it centralizes the initial data organization workflow for analysis.

### `tratamento_missing.R`
Script dedicated to handling missing data, an important step in building the final analytical dataset.

### `calculando_vars.R`
Script for calculating and deriving analytical variables from the intermediate datasets.

### `logistic_reg.R`
Implements **Firth's penalized logistic regression**, described in the script itself as a strategy to reduce bias, handle complete or quasi-complete separation, and ensure finite estimates of odds ratios and 95% CIs. The script fits models for total SB, MPSB, and MASB, adjusting for age, gender, race, income, hypertension, type 2 diabetes, and obesity.

### `rcs_analysis.R`
Implements models with **restricted cubic splines (3 knots)** to examine non-linear associations between sedentary behavior and depression score. The script explicitly uses knots at the **10th, 50th, and 90th percentiles**, adjusting for age, sex, race/ethnicity, income, hypertension, type 2 diabetes, obesity, and MVPA.

### `linear_trend_sb.R`, `linear_trend_mpsb.R`, `linear_trend_masb.R`
Scripts for assessing the linear trend for total, mentally passive, and mentally active sedentary behavior.

### `splines_curves.jpeg`
Image file associated with the spline curves generated in the analyses.

---

## 🧪 Core analytical variables

Based on the public scripts, the project works with the following main variables:

### Exposures
- `total_sb_hday` → total sedentary behavior in hours/day;
- `total_sb_mp_hday` → mentally passive sedentary behavior in hours/day;
- `total_sb_ma_hday` → mentally active sedentary behavior in hours/day.

### Outcome
- `depressao_score` → depression score;
- `dep_cat` → categorical variable derived from the depression score (`< 20` vs `≥ 20`).

### Covariates
Models are adjusted for:
- age/age category;
- gender;
- race;
- income;
- systemic arterial hypertension (`has`);
- type 2 diabetes (`dm2`);
- obesity;
- MVPA, in some analyses.

---

## 📈 Statistical strategy

Based on the available scripts, the analytical workflow includes:

- **Firth's** penalized logistic regression for binary outcomes;
- calculation of **odds ratios** and **95% confidence intervals**;
- analysis of non-linear associations using **restricted cubic splines**;
- specific analyses for **total SB**, **MPSB**, and **MASB**;
- categorization of sedentary exposures into `< 4 h/day` and `≥ 4 h/day`;
- inclusion of sociodemographic and metabolic/clinical covariates.

---

## 💻 Technologies used

- **R**
- RStudio project with a `.Rproj` file
- Packages explicitly used in the scripts:
  - `tidyverse`
  - `patchwork`
  - `rms`
  - `aod`
  - `readr`
  - `logistf`

---

## ▶️ How to use

### 1. Clone the repository

```bash
git clone https://github.com/saulosgil/SB_Depression_crossSectionalStudy.git
```

### 2. Open the project in RStudio

```r
SB_Depression_crossSectionalStudy.Rproj
```

### 3. Run the analytical workflow

A suggested order is:

```r
source("data_prep.R")
source("tratamento_missing.R")
source("calculando_vars.R")
source("logistic_reg.R")
source("rcs_analysis.R")
```

Additional analyses can then be run depending on the objective:

```r
source("linear_trend_sb.R")
source("linear_trend_mpsb.R")
source("linear_trend_masb.R")
```

---

## 🔁 Reproducibility

To maintain the project's reproducibility:

- ✅ preserve the current folder and file structure;
- ✅ keep the `.rds` and `.xlsx` files in the expected directory;
- ✅ run the scripts from the project root;
- ✅ use the `.Rproj` file to avoid relative path issues.

The repository contains multiple intermediate `.rds` objects, suggesting a staged workflow, with preparation and analysis kept separate.

---

## 👨‍💻 Author

**Saulo Gil**
GitHub: [@saulosgil](https://github.com/saulosgil)
