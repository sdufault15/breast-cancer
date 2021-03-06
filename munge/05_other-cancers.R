###############################
# Suzanne Dufault
# October 30, 2018
# Opening Data Files from Mark
# NON-BREAST CANCERS
###############################
load(here("data", "ukb_working_data_sub_n220116_exp_out.RData")) # Subset with fewer than 6 children + exposure information + breast cancer information

cancer_dta <- ukb_dta %>% select_at(vars(c(eid, starts_with("type_of_cancer_icd10uses_datacoding_19_f40006"),
                                           starts_with("date_of_cancer_diagnosis_f40005"),
                                           starts_with("diagnoses_main_icd10uses_datacoding_19_f41202"),
                                           starts_with("diagnoses_secondary_icd10uses_datacoding_19_f41204"))))

################
# OTHER CANCERS
################

# REGISTRY: It appears that the registry can report 32 different instances of cancer, each with a corresponding date of diagnosis.

# Respiratory
respiratory.registry <- ukb_dta %>% filter_at(vars(starts_with("type_of_cancer_icd10uses_datacoding_19_f40006")), 
                                     any_vars(. %in% paste0(rep(paste0("C", 30:39), each = 10), 0:9)))

resp <- respiratory.registry %>% select_at(vars(eid, starts_with("type_of_cancer_icd10uses_datacoding_19_f40006")))
reg.date <- respiratory.registry %>% select_at(vars(eid, starts_with("date_of_cancer_diagnosis_f40005")))
colnames(resp) <- colnames(reg.date) <- c("eid", paste0("m", 0:31))

resp <- resp %>% gather("measurement", "cancertype.registry", 2:ncol(resp))
reg.date <- reg.date %>% gather("measurement", "date", 2:ncol(reg.date)) 

date.dta <- full_join(resp, reg.date, by = c("eid", "measurement")) %>% 
  filter(!is.na(date), cancertype.registry %in% paste0(rep(paste0("C", 30:39), each = 10), 0:9)) %>% 
  arrange(eid)

date.dta <- date.dta %>% group_by(eid) %>% mutate(respiratory.first.diag.date = min(date)) %>% select(eid, respiratory.first.diag.date) %>% distinct()

work_dta_subset <- left_join(work_dta_subset, date.dta, by = "eid")
work_dta_subset <- work_dta_subset %>% mutate(respiratory.diag = ifelse(is.na(respiratory.first.diag.date), 0, 1))
rm(resp, reg.date, date.dta)

# Gastrointestinal
gastrointestinal.registry <- ukb_dta %>% filter_at(vars(starts_with("type_of_cancer_icd10uses_datacoding_19_f40006")), 
                                                   any_vars(. %in% paste0(rep(paste0("C", 15:26), each = 10), 0:9)))
gast <- gastrointestinal.registry %>% select_at(vars(eid, starts_with("type_of_cancer_icd10uses_datacoding_19_f40006")))
reg.date <- gastrointestinal.registry %>% select_at(vars(eid, starts_with("date_of_cancer_diagnosis_f40005")))
colnames(gast) <- colnames(reg.date) <- c("eid", paste0("m", 0:31))

gast <- gast %>% gather("measurement", "cancertype.registry", 2:ncol(gast))
reg.date <- reg.date %>% gather("measurement", "date", 2:ncol(reg.date)) 

date.dta <- full_join(gast, reg.date, by = c("eid", "measurement")) %>% 
  filter(!is.na(date), cancertype.registry %in% paste0(rep(paste0("C", 15:26), each = 10), 0:9)) %>% arrange(eid)

date.dta <- date.dta %>% group_by(eid) %>% mutate(gastrointestinal.first.diag.date = min(date)) %>% select(eid, gastrointestinal.first.diag.date) %>% distinct()

work_dta_subset <- left_join(work_dta_subset, date.dta, by = "eid")
work_dta_subset <- work_dta_subset %>% mutate(gastrointestinal.diag = ifelse(is.na(gastrointestinal.first.diag.date), 0, 1))
rm(gast, reg.date, date.dta)


# Melanoma
melanoma.registry <- ukb_dta %>% filter_at(vars(starts_with("type_of_cancer_icd10uses_datacoding_19_f40006")), 
                                                   any_vars(. %in% c("C50", paste0("C43", 0:9))))
mel <- melanoma.registry %>% select_at(vars(eid, starts_with("type_of_cancer_icd10uses_datacoding_19_f40006")))
reg.date <- melanoma.registry %>% select_at(vars(eid, starts_with("date_of_cancer_diagnosis_f40005")))
colnames(mel) <- colnames(reg.date) <- c("eid", paste0("m", 0:31))

mel <- mel %>% gather("measurement", "cancertype.registry", 2:ncol(mel))
reg.date <- reg.date %>% gather("measurement", "date", 2:ncol(reg.date)) 

date.dta <- full_join(mel, reg.date, by = c("eid", "measurement")) %>% 
  filter(!is.na(date), cancertype.registry %in% c("C50", paste0("C43", 0:9))) %>% arrange(eid)

date.dta <- date.dta %>% group_by(eid) %>% mutate(melanoma.first.diag.date = min(date)) %>% select(eid, melanoma.first.diag.date) %>% distinct()

work_dta_subset <- left_join(work_dta_subset, date.dta, by = "eid")
work_dta_subset <- work_dta_subset %>% mutate(melanoma.diag = ifelse(is.na(melanoma.first.diag.date), 0, 1))
rm(mel, date.dta, reg.date)

# Female genital organs
female.genital.organs.registry <- ukb_dta %>% filter_at(vars(starts_with("type_of_cancer_icd10uses_datacoding_19_f40006")), 
                                           any_vars(. %in% paste0(rep(paste0("C", 51:58), each = 10), 0:9)))
fgo <- female.genital.organs.registry %>% select_at(vars(eid, starts_with("type_of_cancer_icd10uses_datacoding_19_f40006")))
reg.date <- female.genital.organs.registry %>% select_at(vars(eid, starts_with("date_of_cancer_diagnosis_f40005")))
colnames(fgo) <- colnames(reg.date) <- c("eid", paste0("m", 0:31))

fgo <- fgo %>% gather("measurement", "cancertype.registry", 2:ncol(fgo))
reg.date <- reg.date %>% gather("measurement", "date", 2:ncol(reg.date)) 

date.dta <- full_join(fgo, reg.date, by = c("eid", "measurement")) %>% 
  filter(!is.na(date), cancertype.registry %in% paste0(rep(paste0("C", 51:58), each = 10), 0:9)) %>% arrange(eid)

date.dta <- date.dta %>% group_by(eid) %>% mutate(female.genital.organs.first.diag.date = min(date)) %>% select(eid, female.genital.organs.first.diag.date) %>% distinct()

work_dta_subset <- left_join(work_dta_subset, date.dta, by = "eid")
work_dta_subset <- work_dta_subset %>% mutate(female.genital.organs.diag = ifelse(is.na(female.genital.organs.first.diag.date), 0, 1))
rm(fgo, date.dta, reg.date)

# Blood/lymph
blood.lymph.registry <- ukb_dta %>% filter_at(vars(starts_with("type_of_cancer_icd10uses_datacoding_19_f40006")), 
                                                        any_vars(. %in% paste0(rep(paste0("C", 81:96), each = 10), 0:9)))
blood <- blood.lymph.registry %>% select_at(vars(eid, starts_with("type_of_cancer_icd10uses_datacoding_19_f40006")))
reg.date <- blood.lymph.registry %>% select_at(vars(eid, starts_with("date_of_cancer_diagnosis_f40005")))
colnames(blood) <- colnames(reg.date) <- c("eid", paste0("m", 0:31))

blood <- blood %>% gather("measurement", "cancertype.registry", 2:ncol(blood))
reg.date <- reg.date %>% gather("measurement", "date", 2:ncol(reg.date)) 

date.dta <- full_join(blood, reg.date, by = c("eid", "measurement")) %>% 
  filter(!is.na(date), cancertype.registry %in% paste0(rep(paste0("C", 81:96), each = 10), 0:9)) %>% arrange(eid)

date.dta <- date.dta %>% group_by(eid) %>% mutate(blood.lymph.first.diag.date = min(date)) %>% select(eid, blood.lymph.first.diag.date) %>% distinct()

work_dta_subset <- left_join(work_dta_subset, date.dta, by = "eid")
work_dta_subset <- work_dta_subset %>% mutate(blood.lymph.diag = ifelse(is.na(blood.lymph.first.diag.date), 0, 1))
rm(blood, date.dta, reg.date)

# All non-breast and non-skin
all.non.breast.non.skin <- ukb_dta %>% filter_at(vars(starts_with("type_of_cancer_icd10uses_datacoding_19_f40006")), 
                                                 any_vars(. %in% paste0(rep(paste0("C", c("00", 1:43, 45:49, 51:96)), each = 10), 0:9))) 
all <- all.non.breast.non.skin %>% select_at(vars(eid, starts_with("type_of_cancer_icd10uses_datacoding_19_f40006")))
reg.date <- all.non.breast.non.skin %>% select_at(vars(eid, starts_with("date_of_cancer_diagnosis_f40005")))
colnames(all) <- colnames(reg.date) <- c("eid", paste0("m", 0:31))

all <- all %>% gather("measurement", "cancertype.registry", 2:ncol(all))
reg.date <- reg.date %>% gather("measurement", "date", 2:ncol(reg.date)) 

date.dta <- full_join(all, reg.date, by = c("eid", "measurement")) %>% 
  filter(!is.na(date), cancertype.registry %in% paste0(rep(paste0("C", c("00", 1:43, 45:49, 51:96)), each = 10), 0:9)) %>% arrange(eid)

date.dta <- date.dta %>% group_by(eid) %>% mutate(all.non.breast.non.skin.diag.date = min(date)) %>% select(eid, all.non.breast.non.skin.diag.date) %>% distinct()

work_dta_subset <- left_join(work_dta_subset, date.dta, by = "eid")
work_dta_subset <- work_dta_subset %>% mutate(all.non.breast.non.skin.diag = ifelse(is.na(all.non.breast.non.skin.diag.date), 0, 1))
rm(all, date.dta, reg.date)

save(work_dta_subset, file = here("data", "ukb_working_data_sub_n220116_exp_out.RData"))

rm(all.non.breast.non.skin, blood.lymph.registry, female.genital.organs.registry, gastrointestinal.registry, melanoma.registry, respiratory.registry, cancer_dta)
