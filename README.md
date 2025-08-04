# projected_agronomy_impacts

# Climate Adaptation Meta-Analysis: Yield Impact Bootstrapping

This repository contains code and cleaned data used to analyze the projected yield impacts of climate change on maize, rice, and wheat under different adaptation strategies. The analysis uses bootstrapped polynomial regression models to assess non-linear responses and quantify uncertainty.

---

## 📂 Contents

- `bootstrapping+plotting+.R` — Main R script for data cleaning, outlier removal, bootstrapping, and plotting.
- `data/adaptation_data_subset.xlsx` — Cleaned dataset used in the analysis (replace with link if hosted externally).
- `results/` — Folder for plots.
- `LICENSE` — Open access license (MIT recommended).

---

## 📊 Methods Summary

- Focus crops: **Maize, Rice, Wheat**
- Yield changes (`dp`) are regressed against temperature changes (`dt`) using second-order polynomial models.
- Bootstrap (R = 500) is used to generate confidence intervals, stratified by `ref_no` to control for study-level clustering.
- Adaptation groups are harmonized into six categories: `No adaptation`, `Variety`, `Fertilizer`, `Bundled`, `Irrigation`, `Planting date`.
- For **Maize**, `Bundled` practices are excluded to reduce inconsistency.
- Outliers are removed using 1.5×IQR rule within each crop × adaptation group.

---

## 📈 Output

The main output is a multi-panel plot showing:
- Bootstrapped regression lines for each adaptation group.
- 95% confidence ribbons.
- Jittered raw data points with transparency to indicate data density.

---

## 📦 Requirements

Run the script using R (≥ 4.1) with the following packages:

```r
install.packages(c("readxl", "dplyr", "readr", "stringr", "boot",
                   "ggplot2", "janitor", "tidyr", "forcats", "purrr", "patchwork"))
