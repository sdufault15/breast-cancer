# Updated qwraps2 is not yet on CRAN
# if (!("devtools" %in% rownames(installed.packages()))) { 
#   warning("installing devtools from https://cran.rstudio.com")
#   install.packages("devtools", repo = "https://cran.rstudio.com")
# }
# 
# devtools::install_github("dewittpe/qwraps2", build_vignettes = TRUE)


# library
library(tidyverse)
library(hexbin)
#library(qwraps2)
library(splitstackshape)
# options(qwraps2_markup = "latex")
library(here)
library(xtable)
load(here("data", "ukb_working_data_sub_n207797_exp_out_cleanSNPS.RData"))
dx_dta <- work_dta_subset %>%
  select(eid, hypertension_dx) # pulling out the hypertension (ICD) variable
load(here("data", "ukb_working_data_sub_n207797_exp_out_cleanSNPS_blood.RData")) # _blood doesn't contain the hypertension_dx variable
work_dta_subset <- full_join(work_dta_subset, dx_dta, by = "eid") 
dta <- as.data.frame(work_dta_subset)
rm(work_dta_subset)

load(here("data", "imputations.RData"))

# Plotting 
library(RColorBrewer)
myColors <- brewer.pal(8, 'Set2')
library(gridExtra)
library(viridis)
library(ggthemr)

# Analysis
library(survival)
library(mice)
library(survminer)
library(nnet)

# Cleaning the imputed data
dta.i <- mice::complete(dta.imp, "long", include = TRUE)
#dta.i <- left_join(dta.i, dta[,c("eid")], by = "eid")
otheroutcomes <- dta %>% select(eid, date_death, smoking, bc.registry, respiratory.first.diag.date:bc.selfreport)
dta.i <- left_join(dta.i, otheroutcomes, by = "eid")

dta.i <- dta.i %>% mutate(year_of_birth_cat = cut(year_of_birth_f34_0_0, breaks = seq(1935,1970,by = 5)),
                          familyhx = ifelse(hxmother == 1 & hxsib == 0, "mother", 
                                            ifelse(hxsib == 1 & hxmother == 0, "sib",
                                                   ifelse(hxmother == 1 & hxsib == 1, "both", "neither"))))
dta.i$familyhx <- as.factor(dta.i$familyhx)
dta.i$familyhx <- relevel(dta.i$familyhx, "neither")
dta.i$agefirstbirth5yr <- dta.i$agefirstbirth/5
dta.i$bmi5pt <- dta.i$bmi/5
dta.i$smoking <- factor(dta.i$smoking, ordered = FALSE)
dta.i$smoking <- relevel(dta.i$smoking, "Never")

dta.i <- dta.i %>% filter(agefirstbirth < T.eof)

dta.i <- dta.i %>% mutate(agelastbirth = ifelse(parity == 1, agefirstbirth, agelastbirth))

icd.only <- dta.i %>% filter(pih.icd == 1 & pih.selfreport == 0 | pih.icd.unspec == 1 & pih.selfreport == 0) %>% select(eid) %>% distinct()
dta.is <- dta.i %>% filter(!eid %in% icd.only$eid)

dta1 <- dta.is %>% filter(.imp == 1)
dta2 <- dta.is %>% filter(.imp == 2)
dta3 <- dta.is %>% filter(.imp == 3)
dta4 <- dta.is %>% filter(.imp == 4)
dta5 <- dta.is %>% filter(.imp == 5)
