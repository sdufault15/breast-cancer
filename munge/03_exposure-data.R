###############################
# Suzanne Dufault
# August 14, 2018
# Opening Data Files from Mark
# EXPOSURE ONLY
###############################

#####
# Exposure
#####

#load("/Volumes/Seagate Backup Plus Drive/BiobankData/my_ukb_data.rda")
#load(here("data", "ukb_working_data_full_n272974.RData")) # Full Working Dataset
load(here("data", "ukb_working_data_sub_n220116.RData")) # Subset with fewer than 6 children

# Identifying self-reported PIH (field 20002, code 1073)
self_report <- ukb_dta %>% filter_at(vars(starts_with("noncancer_illness_code_selfreporteduses_datacoding_6_f20002")), any_vars(. == 1073))

self_report <- self_report %>% select_at(vars(eid,
                                              starts_with("noncancer_illness_code_selfreporteduses_datacoding_6_f20002"), 
                                              starts_with("interpolated_year_when_noncancer_illness_first_diagnoseduses_datacoding_13_f20008")))

recs <- self_report %>% select_at(vars(eid, starts_with("noncancer_illness_code_selfreporteduses_datacoding_6_f20002"))) 
dates <- self_report %>% select_at(vars(eid, starts_with("interpolated_year_when_noncancer_illness_first_diagnoseduses_datacoding_13_f20008")))
names(dates) <- names(recs) <- c("eid", paste0("m", 1:(ncol(recs)-1)))

recs <- recs %>% gather("measurement", "selfreport", 2:ncol(recs)) %>% arrange(eid)
dates <- dates %>% gather("measurement", "date", 2:ncol(dates)) %>% arrange(eid)

sr.recs <- full_join(recs, dates, by = c("eid", "measurement"))
sr.recs <- sr.recs %>% filter(selfreport == 1073) %>% group_by(eid) %>% mutate(date2 = min(date)) %>%  select(eid, self.report.date = date2) %>% distinct()
sr.recs$self.report.date <- as.numeric(sr.recs$self.report.date)
sr.recs$self.report.date[sr.recs$self.report.date < 0] <- NA

# Identifying ICD-reported PIH (field 41202 or 41204, codes O13, O140, O141, O142, O149, O150, O151, O152, O159, (and O14 or O15 if there are any))
icd.main <- ukb_dta %>% 
  filter_at(vars(starts_with("diagnoses_main_icd10uses_datacoding_19_f41202")), 
            any_vars(. %in% c("O13", "O140", "O141", "O142", "O149", "O150", "O151", "O152", "O159", "O14", "O15")))
icd.sec <- ukb_dta %>% 
  filter_at(vars(starts_with("diagnoses_secondary_icd10uses_datacoding_19_f41204")), 
            any_vars(. %in% c("O13", "O140", "O141", "O142", "O149", "O150", "O151", "O152", "O159", "O14", "O15")))
icd.unspec <- ukb_dta %>%
  filter_at(vars(c(starts_with("diagnoses_main_icd10uses_datacoding_19_f41202"), starts_with("diagnoses_secondary_icd10uses_datacoding_19_f41204"))), 
            any_vars(. == "O16"))

####
# Merging working data with indicators for self-report, ICD-10, and ICD-10 unspecified
####
work_dta_subset <- work_dta_subset %>% mutate(pih.selfreport = ifelse(work_dta_subset$eid %in% self_report$eid, 1, 0))
work_dta_subset <- work_dta_subset %>% mutate(pih.icd = ifelse(work_dta_subset$eid %in% c(icd.main$eid, icd.sec$eid), 1, 0))
work_dta_subset <- work_dta_subset %>% mutate(pih.icd.unspec = ifelse(work_dta_subset$eid %in% icd.unspec$eid, 1, 0))

work_dta_subset <- left_join(work_dta_subset, sr.recs, by = "eid")

sum(work_dta_subset$pih.selfreport) 
sum(work_dta_subset$pih.icd) 
sum(work_dta_subset$pih.icd.unspec) 

# Creating one summary variable of age at PIH diagnosis
# work_dta_subset %>% filter(pih.selfreport == 1 | pih.icd == 1 | pih.icd.unspec == 1) %>% group_by(pih.selfreport, pih.icd, pih.icd.unspec, !is.na(self.report.date)) %>% summarize(n = n_distinct(eid))
# work_dta_subset %>% filter(pih.selfreport == 1 | pih.icd == 1 | pih.icd.unspec == 1, is.na(self.report.date), parity == 1) %>% group_by(is.na(agefirstbirth) | agefirstbirth < 0, is.na(agelastbirth) | agelastbirth < 0) %>% summarize(n = n_distinct(eid))
# work_dta_subset %>% filter(pih.selfreport == 1 | pih.icd == 1 | pih.icd.unspec == 1, is.na(self.report.date)) %>% group_by(parity) %>% summarize(n = n_distinct(eid))
# work_dta_subset %>% filter(pih.selfreport == 1 | pih.icd == 1 | pih.icd.unspec == 1, parity == 1, is.na(self.report.date)) %>% group_by(is.na(agelastbirth)) %>% summarize(n = n_distinct(eid))
# work_dta_subset %>% filter(pih.selfreport == 1 | pih.icd == 1 | pih.icd.unspec == 1, is.na(self.report.date)) %>% group_by(is.na(agefirstbirth), is.na(agelastbirth), agefirstbirth < 0 & agelastbirth < 0, parity) %>% summarize(n = n_distinct(eid))
# work_dta_subset %>% filter(pih.selfreport == 1 | pih.icd == 1 | pih.icd.unspec == 1, is.na(self.report.date), !is.na(agefirstbirth) & !is.na(agelastbirth), agefirstbirth > 0 & agelastbirth > 0) %>% 
#   group_by(parity, cut(agelastbirth - agefirstbirth, breaks = c(0,1,2,5,10,15,20))) %>% summarize(n = n_distinct(eid)) 

save(work_dta_subset, file = "data/ukb_working_data_sub_n220116_exp.RData")

rm(dates, recs, self_report, icd.main, icd.sec, icd.unspec, sr.recs)
