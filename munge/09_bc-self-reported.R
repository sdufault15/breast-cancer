###############################
# Suzanne Dufault
# January 28, 2019
# Opening Data Files from Mark
# Self-reported Cancer
###############################

load(here("data", "ukb_working_data_sub_n207797_exp_out_cleanSNPs.RData")) # Subset with fewer than 6 children + exposure information

bc_dta <- ukb_dta %>% select_at(vars(c(eid, starts_with("cancer_code_selfreporteduses_datacoding_3_f20001"))))

bc_year <- ukb_dta %>% select_at(vars(c(eid, starts_with("interpolated_age_of_participant_when_cancer_first_diagnoseduses_datacoding_13_f20007"))))

colnames(bc_dta) <- colnames(bc_year) <- c("eid", paste0("m", 0:17))

bc_dta <- bc_dta %>% gather("measurement", "bc.selfreport", 2:ncol(bc_dta))
bc_year <- bc_year %>% gather("measurement", "date", 2:ncol(bc_year)) 

date.dta <- full_join(bc_dta, bc_year, by = c("eid", "measurement")) %>% 
  filter(!is.na(date), bc.selfreport == "1002") %>% arrange(eid)

date.dta <- date.dta %>% group_by(eid) %>% 
  mutate(bc.selfreport.age = min(date)) %>% 
  select(eid, bc.selfreport.age) %>% 
  distinct() %>%
  mutate(bc.selfreport.age = as.numeric(bc.selfreport.age)) %>%
  mutate(bc.selfreport.age = ifelse(bc.selfreport.age < 0, NA, bc.selfreport.age))

work_dta_subset <- left_join(work_dta_subset, date.dta, by = "eid")
work_dta_subset <- work_dta_subset %>% mutate(bc.selfreport = ifelse(is.na(bc.selfreport.age), 0, 1))

save(work_dta_subset, file = here("data", "ukb_working_data_sub_n207797_exp_out_cleanSNPs.RData"))

rm(bc_dta, bc_year, date.dta)