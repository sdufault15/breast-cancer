###############################
# Suzanne Dufault
# January 29, 2019
# Opening Data Files from Mark
# Removing any women who have withdrawn from the study
###############################

withdraw <- read_csv(file = here("data", "w37368_20181016.csv"), 
                     col_names = "eid", 
                     col_types = 'c')

work_dta_subset <- work_dta_subset %>% 
  filter(!eid %in% withdraw$eid)

save(work_dta_subset, 
     file = here("data", "ukb_working_data_sub_n207797_exp_out_cleanSNPs.RData"))

rm(withdraw)