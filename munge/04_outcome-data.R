###############################
# Suzanne Dufault
# August 14, 2018
# Opening Data Files from Mark
# OUTCOME ONLY
###############################
load(here("data", "ukb_working_data_sub_n220116_exp.RData")) # Subset with fewer than 6 children + exposure information

cancer_dta <- ukb_dta %>% select_at(vars(c(eid, starts_with("type_of_cancer_icd10uses_datacoding_19_f40006"),
                                           starts_with("date_of_cancer_diagnosis_f40005"),
                                           starts_with("diagnoses_main_icd10uses_datacoding_19_f41202"),
                                           starts_with("diagnoses_secondary_icd10uses_datacoding_19_f41204"))))
################
# BREAST CANCER
################

# REGISTRY: It appears that the registry can report 32 different instances of cancer, each with a corresponding date of diagnosis.
# Invasive
bc.registry <- ukb_dta %>% filter_at(vars(starts_with("type_of_cancer_icd10uses_datacoding_19_f40006")), 
                                        any_vars(. %in% c("C50", paste0("C50", 0:9))))

bc.reg.recs <- bc.registry %>% select_at(vars(eid, starts_with("type_of_cancer_icd10uses_datacoding_19_f40006")))
reg.date <- bc.registry %>% select_at(vars(eid, starts_with("date_of_cancer_diagnosis_f40005")))
colnames(reg.date) <- colnames(bc.reg.recs) <- c("eid", paste0("m", 0:31))

bc.reg.recs <- bc.reg.recs %>% gather("measurement", "cancertype.registry", 2:ncol(bc.reg.recs))
reg.date <- reg.date %>% gather("measurement", "date", 2:ncol(reg.date)) 

date.dta <- full_join(bc.reg.recs, reg.date, by = c("eid", "measurement")) %>% 
  filter(!is.na(date), cancertype.registry %in% c("C50", paste0("C50", 0:9))) %>% arrange(eid)

date.dta <- date.dta %>% group_by(eid) %>% mutate(first.diag.registry = min(date)) %>% select(eid, first.diag.registry) %>% distinct()


# In situ
bc.insitu <- ukb_dta %>% filter_at(vars(starts_with("type_of_cancer_icd10uses_datacoding_19_f40006")), 
                                   any_vars(. %in% c("D05", paste0("D05", 0:9))))
bc.insitu.recs <- bc.insitu %>% select_at(vars(eid, starts_with("type_of_cancer_icd10uses_datacoding_19_f40006")))
reg.date.is <- bc.insitu %>% select_at(vars(eid, starts_with("date_of_cancer_diagnosis_f40005")))
colnames(reg.date.is) <- colnames(bc.insitu.recs) <- c("eid", paste0("m", 0:31))

bc.insitu.recs <- bc.insitu.recs %>% gather("measurement", "cancertype.registry", 2:ncol(bc.insitu.recs))
reg.date.is <- reg.date.is %>% gather("measurement", "date", 2:ncol(reg.date.is)) 

date.dta.is <- full_join(bc.insitu.recs, reg.date.is, by = c("eid", "measurement")) %>% 
  filter(!is.na(date), cancertype.registry %in% c("D05", paste0("D05", 0:9))) %>% arrange(eid)

date.dta.is <- date.dta.is %>% group_by(eid) %>% mutate(first.diag.registry.is = min(date)) %>% select(eid, first.diag.registry.is) %>% distinct()

work_dta_subset <- left_join(work_dta_subset, date.dta, by = "eid")
work_dta_subset <- left_join(work_dta_subset, date.dta.is, by = "eid")
work_dta_subset <- work_dta_subset %>% mutate(bc.registry = ifelse(!is.na(first.diag.registry), 1, 0), 
                                              bc.registry.is = ifelse(!is.na(first.diag.registry.is), 1, 0))

# ICD:
bc.icd <- ukb_dta %>% filter_at(vars(starts_with("diagnoses_main_icd10uses_datacoding_19_f41202"), starts_with("diagnoses_secondary_icd10uses_datacoding_19_f41204")), 
                                any_vars(. %in% c("C50", paste0("C50", 0:9)))) 

work_dta_subset <- work_dta_subset %>% mutate(bc.icd = ifelse(work_dta_subset$eid %in% bc.icd$eid, 1, 0))

mean(unique(bc.icd$eid) %in% unique(bc.registry$eid)) # 92% of the ICD recorded breast cancer cases are also recorded in the registry

work_dta_subset <- work_dta_subset %>% mutate(agebc = as.numeric(format(first.diag.registry, "%Y")) - year_of_birth_f34_0_0,
                                              agebc.is = as.numeric(format(first.diag.registry.is, "%Y")) - year_of_birth_f34_0_0)

work_dta_subset <- work_dta_subset %>% group_by(eid) %>% 
  mutate(min.bc.date = min(first.diag.registry, first.diag.registry.is, na.rm = TRUE),
         min.bc.age = min(agebc, agebc.is, na.rm = TRUE),
         bc.OUTCOME = ifelse(bc.registry.is == 1 | bc.registry == 1, 1, 0))

save(work_dta_subset, file = "data/ukb_working_data_sub_n220116_exp_out.RData")

rm(bc.icd, bc.reg.recs, bc.registry, cancer_dta, date.dta, reg.date)
