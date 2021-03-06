###############################
# Suzanne Dufault
# March 7, 2019
# Opening Data Files from Mark
# DIABETES and HYPOTHYROIDISM
###############################

load(here("data", "ukb_working_data_sub_n207797_exp_out_cleanSNPs.RData")) # Subset with fewer than 6 children + exposure information + breast cancer information


# Diabetes Diagnosis
diabetes <- ukb_dta %>% filter_at(vars(eid, starts_with("noncancer_illness_code_selfreporteduses_datacoding_6_f20002")), 
                                  any_vars(. %in% c(1220,1223)))
diabetes$diabetes_ind <- 1
diabetes <- diabetes %>% select(eid, diabetes_ind)

# Hypothyroidism Diagnosis
hypoth <- ukb_dta %>% filter_at(vars(eid, starts_with("noncancer_illness_code_selfreporteduses_datacoding_6_f20002")), 
                                any_vars(. %in% c(1226)))
hypoth$hypothyroid_ind <- 1
hypoth <- hypoth %>% select(eid, hypothyroid_ind)

work_dta_subset <- left_join(left_join(
  work_dta_subset,
  diabetes, by = "eid"),
  hypoth, by = "eid")

save(work_dta_subset, file = here("data", "ukb_working_data_sub_n207797_exp_out_cleanSNPs.RData"))

rm(hypoth, diabetes)
