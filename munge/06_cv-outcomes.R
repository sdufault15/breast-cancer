###############################
# Suzanne Dufault
# November 14, 2018
# Opening Data Files from Mark
# CARDIOVASCULAR OUTCOMES
###############################

load(here("data", "ukb_working_data_sub_n220116_exp_out.RData")) # Subset with fewer than 6 children + exposure information + breast cancer information

# cv_dta <- ukb_dta %>% select_at(vars(c(eid, starts_with("pulse_wave_arterial_stiffness_index_f21021"),
#                                        starts_with("vascularheart_problems_diagnosed_by_doctoruses_datacoding_100605_f6150"),
#                                        starts_with("diastolic_blood_pressure_automated_reading_f4079"),
#                                        starts_with("systolic_blood_pressure_automated_reading_f4080"),
#                                        starts_with("medication_for_cholesterol_blood_pressure_diabetes_or_take_exogenous_hormonesuses_datacoding_100626_f6153"))))
# 
# cv_icd <- ukb_dta %>% select_at(vars(c(eid, 
#                                        starts_with("diagnoses_main_icd10uses_datacoding_19_f41202"),
#                                        starts_with("diagnoses_secondary_icd10uses_datacoding_19_f41204"))))

# Arterial Stiffness Index
art_stiff_ind <- ukb_dta %>% select_at(vars(c(eid, starts_with("pulse_wave_arterial_stiffness_index_f21021")))) # Three possible observations, we will take the mean
art_stiff_ind$art.stiff.ind <- apply(art_stiff_ind[,2:4],1,function(x){mean(as.numeric(x), na.rm = TRUE)})
art_stiff_ind$art.stiff.ind[is.nan(art_stiff_ind$art.stiff.ind)] <- NA # replacing unobserved with NA
art_stiff_ind <- art_stiff_ind %>% select(eid, art.stiff.ind)

# Vascular/Heart Problems Diagnosed by Doctor
# Some women have multiple diagnoses, take most extreme for analyses?
heart_prob_diag <- ukb_dta %>% 
  filter_at(vars(eid, starts_with("vascularheart_problems_diagnosed_by_doctoruses_datacoding_100605_f6150")), any_vars(. %in% c("Heart attack", "Angina", "Stroke"))) %>%
  mutate(heart.attack.angina.stroke = 1) %>%
  select(eid, heart.attack.angina.stroke)

heart_prob_diag_2 <- ukb_dta %>% 
  filter_at(vars(eid, starts_with("vascularheart_problems_diagnosed_by_doctoruses_datacoding_100605_f6150")), any_vars(. %in% c("High blood pressure"))) %>%
  mutate(high.blood.pressure = 1) %>%
  select(eid, high.blood.pressure)


# Blood Pressure Readings 
bp <- ukb_dta %>% select_at(vars(c(eid, starts_with("diastolic_blood_pressure_automated_reading_f4079"), starts_with("systolic_blood_pressure_automated_reading_f4080"))))
bp$diastolic.mean <- apply(bp[,2:7],1,function(x){mean(as.numeric(x), na.rm = TRUE)})
bp$systolic.mean <- apply(bp[,8:13],1,function(x){mean(as.numeric(x), na.rm = TRUE)})
bp <- bp %>% select(eid, diastolic.mean, systolic.mean)

bp.med <- ukb_dta %>% 
  filter_at(vars(eid, starts_with("medication_for_cholesterol_blood_pressure_diabetes_or_take_exogenous_hormonesuses_datacoding_100626_f6153")), any_vars(. %in% c("Blood pressure medication"))) 
# 49,253 total women have been on a blood pressure lowering medication (full cohort)
bp.med$bp.med <- 1
bp.med <- bp.med %>% select(eid, bp.med)

# Hospital Diagnosis Codes for IH or Cerebrovascular Disease
cv_icd <- ukb_dta %>% filter_at(vars(eid, starts_with("diagnoses_main_icd10uses_datacoding_19_f41202"), starts_with("diagnoses_secondary_icd10uses_datacoding_19_f41204")),
                               any_vars(. %in% c(paste0("I", 200:259), paste0("I", 20:25), paste0("I", 630:669), paste0("I", 63:66))))
cv_icd$cv.icd <- 1
cv_icd <- cv_icd %>% select(eid, cv.icd)

## Merging with other data

work_dta_subset <- left_join(left_join(left_join(left_join(left_join(left_join(
  work_dta_subset, 
  art_stiff_ind, by = "eid"), 
  heart_prob_diag, by = "eid"), 
  heart_prob_diag_2, by = "eid"),
  bp, by = "eid"), 
  bp.med, by = "eid"),
  cv_icd, by = "eid")


work_dta_subset <- work_dta_subset %>% 
  mutate(heart.attack.angina.stroke = ifelse(is.na(heart.attack.angina.stroke), 0, heart.attack.angina.stroke),
         high.blood.pressure = ifelse(is.na(high.blood.pressure), 0, high.blood.pressure),
         bp.med = ifelse(is.na(bp.med), 0, bp.med),
         cv.icd = ifelse(is.na(cv.icd), 0, cv.icd))

save(work_dta_subset, file = here("data", "ukb_working_data_sub_n220116_exp_out.RData"))

rm(bp, bp.med, cv_icd, art_stiff_ind, heart_prob_diag, heart_prob_diag_2)
