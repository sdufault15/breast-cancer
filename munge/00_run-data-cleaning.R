###############################
# Suzanne Dufault
# August 15, 2018
# Munge
###############################

library(here)

source(here("munge", "01_lib.R")) # libraries
source(here("munge", "02_initial-data.R")) # covariate information
source(here("munge", "03_exposure-data.R")) # exposure information
source(here("munge", "04_outcome-data.R")) # outcome information
source(here("munge", "05_other-cancers.R")) # outcome information for other cancers
source(here("munge", "06_cv-outcomes.R")) # outcome information for CV related mortality and morbidity
source(here("munge", "07_genomic-data.R")) # genomic data
source(here("munge", "08_genomic-cleaning.R")) # classifying the genotype probabilities
source(here("munge", "09_bc-self-reported.R")) # obtaining self-reported bc as well
source(here("munge", "10_remove-withdrawals.R")) # removing women who have withdrawn from study
source(here("munge", "11_diabetes-hypothyroidism.R")) # additional outcomes
source(here("munge", "12_hypertension.R")) # additional outcomes
rm(ukb_dta)

# Blood data
source(here("munge", "13_blood-data.R")) # estrogen, IGF1, etc. 