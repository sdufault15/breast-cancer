###############################
# Suzanne Dufault
# March 7, 2019
# Opening Data Files from Mark
# BLOOD PRESSURE AND HYPERTENSION
###############################

load(here("data", "ukb_working_data_sub_n207797_exp_out_cleanSNPs.RData"))

#. The blood pressure effect of the genotype is impressive, but would be more meaningful if put in terms that are clinically relevant. 
# So lets look at the percentage of women on blood pressure medications as one outcome, field 6153: code 2, 
# and the number with a diagnosis of hypertension as another, field 20002: code 1065 or 1072 to see if this is different for 
# PIH+ women carrying a T allele (or two). Use same covariates of age, BMI, and smoking. (Expect results of both to be very similar)

# the bpmed variable already contains the women on blood pressure medications

# may need to be lower case for hypertension, etc
hypertension <- ukb_dta %>%
  filter_at(vars(eid, 
                 starts_with("noncancer_illness_code_selfreporteduses_datacoding_6_f20002")), any_vars(. %in% c("1065", "1072"))) %>%
  select(eid) %>%
  mutate(hypertension = 1)

work_dta_subset <- left_join(work_dta_subset, hypertension, by = "eid") %>%
  group_by(eid) %>%
  mutate(hypertension = ifelse(is.na(hypertension), 0, hypertension)) %>%
  ungroup

hypertension_dx <- ukb_dta %>%
  filter_at(vars(c(eid,
                 starts_with("diagnoses_secondary_icd10uses_datacoding_19_f41204"),
                 starts_with("diagnoses_main_icd10uses_datacoding_19_f41202"))), any_vars(. == "I10")) %>%
  select(eid) %>%
  mutate(hypertension_dx = 1)

work_dta_subset <- left_join(work_dta_subset, hypertension_dx, by = "eid") %>%
  group_by(eid) %>%
  mutate(hypertension_dx = ifelse(is.na(hypertension_dx), 0, hypertension_dx)) %>%
  ungroup

save(work_dta_subset, file = here("data", "ukb_working_data_sub_n207797_exp_out_cleanSNPs.RData"))
rm(hypertension, hypertension_dx)
