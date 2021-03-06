# Introduction

This repository holds all of the code necessary to recreate the analysis done in [Cancer and Cardiovascular Risk in Women With Hypertensive Disorders of Pregnancy Carrying a Common IGF1R Variant](https://doi.org/10.1016/j.mayocp.2020.03.037), published in Mayo Clinic Proceedings (2020).

# Details

All analyses were carried out using R version 3.6.1 Action of the  Toes (The R Project
for Statistical Computing). The `mice` package was used for multiple imputation. Regressions relied on the `survival` and `stats` packages. Likelihood ratio tests made use of the `lmtest` package. Plots were made with `ggplot2`.

# Project Layout

This repository adopted/modified the formatting used by the `ProjectTemplate` package. For more details about ProjectTemplate, see http://projecttemplate.net

## Overview of the contents of this repository

+ **munge** code used for cleaning data and preparing it for analysis

+ **docs** contains pdf/html versions of the reports generated in **reports**.
    + `2020-02-05_inclusion-exclusion-sample-size.html` contains sample size information and Table 1.

+ **data** contact the [UKBiobank](https://www.ukbiobank.ac.uk) with any data requests