###############################
# Suzanne Dufault
# June 27, 2018
# Opening Data Files from Mark
###############################


##################
# COVARIATE HISTORIES
##################
# install.packages("ukbtools")
# devtools::install_github("kenhanscombe/ukbtools", build_vignettes = TRUE, dependencies = TRUE, force= TRUE)

# This part takes a moment, which is why it is commented out. 
#ukb_dta <- ukb_df("ukb22626", path = "/Volumes/Seagate Backup Plus Drive/BiobankData", n_threads = "dt") 
ukb_key <- ukb_df_field("ukb22626", path = "/Volumes/Seagate Backup Plus Drive/BiobankData")
#save(ukb_dta, file = "/Volumes/Seagate Backup Plus Drive/BiobankData/my_ukb_data.rda")


load("/Volumes/Seagate Backup Plus Drive/BiobankData/my_ukb_data.rda")
######
# Starting working dataset
######

work_dta <- ukb_dta %>% select_at(vars(c(eid, year_of_birth_f34_0_0, month_of_birthuses_datacoding_8_f52_0_0, 
                                         starts_with("body_mass_index_bmi_f21001"),
                                         starts_with("age_at_first_live_birthuses_datacoding_100586_f2754"),
                                         starts_with("age_at_last_live_birthuses_datacoding_100586_f2764"),
                                         starts_with("age_when_periods_started_menarcheuses_datacoding_100291_f2714"),
                                         starts_with("number_of_live_birthsuses_datacoding_100584_f2734"),
                                         starts_with("qualificationsuses_datacoding_100305_f6138"),
                                         starts_with("illnesses_of_motheruses_datacoding_1010_f20110"),
                                         starts_with("illnesses_of_siblingsuses_datacoding_1010_f20111"),
                                         starts_with("alcohol_intake_frequencyuses_datacoding_100402_f1558"),
                                         starts_with("alcohol_intake_versus_10_years_previouslyuses_datacoding_100417_f1628"),
                                         starts_with("number_of_daysweek_of_moderate_physical_activity_10_minutesuses_datacoding_100291_f884"), 
                                         starts_with("number_of_daysweek_of_vigorous_physical_activity_10_minutesuses_datacoding_100291_f904"),
                                         starts_with("pack_years_of_smoking_f20161"),
                                         starts_with("smoking_statususes_datacoding_90_f20116"),
                                         starts_with("ethnic_backgrounduses_datacoding_1001_f21000"),
                                         starts_with("date_of_death_f40000"))))

####
# COVARIATES
####

bmi <- work_dta %>% select_at(vars(c(eid, starts_with("body_mass_index_bmi_f21001"))))
bmi$bmi <- apply(bmi[,2:4],1,function(x){mean(as.numeric(x), na.rm = TRUE)})

agefirstbirth <- work_dta %>% select_at(vars(c(eid, starts_with("age_at_first_live_birthuses_datacoding_100586_f2754")))) 
agefirstbirth$agefirstbirth <- apply(agefirstbirth[,2:4], 1, function(x){min(x, na.rm = TRUE)})

agelastbirth <- work_dta %>% select_at(vars(c(eid, starts_with("age_at_last_live_birthuses_datacoding_100586_f2764")))) 
agelastbirth$agelastbirth <- apply(agelastbirth[,2:4], 1, function(x){max(x, na.rm = TRUE)})

agemenarche <- work_dta %>% select_at(vars(c(eid, starts_with("age_when_periods_started_menarcheuses_datacoding_100291_f2714"))))
agemenarche$agemenarche <- apply(agemenarche[,2:4], 1, function(x){min(x, na.rm = TRUE)})
agemenarche$agemenarche[agemenarche$agemenarche == -1] <- "Do not know"
agemenarche$agemenarche[agemenarche$agemenarche == -3] <- "Prefer not to answer"

numberlivebirths <- work_dta %>% select_at(vars(c(eid, starts_with("number_of_live_birthsuses_datacoding_100584_f2734"))))
numberlivebirths$parity <- apply(numberlivebirths[,2:4], 1, function(x){max(x, na.rm = TRUE)})

college <- work_dta %>% filter_at(vars(starts_with("qualificationsuses_datacoding_100305_f6138")), any_vars(. %in% c("College or University degree", "Other professional qualifications eg: nursing, teaching")))
lowerlevel <- work_dta %>% filter_at(vars(starts_with("qualificationsuses_datacoding_100305_f6138")), any_vars(. %in% c("A levels/AS levels or equivalent", "O levels/GSCEs or equivalent", "CSEs or equivalent", "NVQ or HND or HNC or equivalent")))
mean(unique(college$eid) %in% unique(lowerlevel$eid)) # 49% of women report both degrees
# Coding in this order results in women with a college degree and a lowerlevel degree being recorded as "College or University".
work_dta <- work_dta %>% mutate(education = ifelse(work_dta$eid %in% lowerlevel$eid, "Lower level", 
                                                   ifelse(work_dta$eid %in% college$eid, "College or University", "Other"))) # other includes "none of the above" and "prefer not to answer"    

familyhistory <- work_dta %>% select_at(vars(c(eid, starts_with("illnesses_of_motheruses_datacoding_1010_f20110"), starts_with("illnesses_of_siblingsuses_datacoding_1010_f20111"))))
hxmother <- familyhistory %>% filter_at(vars(starts_with('illnesses_of_motheruses')), any_vars(. %in% c("Breast cancer")))
hxsib <- familyhistory %>% filter_at(vars(starts_with('illnesses_of_siblingsuses')), any_vars(. %in% c("Breast cancer")))
familyhistory <- familyhistory %>% mutate(hxmother = ifelse(eid %in% hxmother$eid, 1, 0), hxsib = ifelse(eid %in% hxsib$eid, 1, 0))

alcohol <- work_dta %>% select_at(vars(c(eid, starts_with("alcohol_intake_frequencyuses_datacoding_100402")))) 
alcohol %>% filter(eid %in% eid[which(is.na(alcohol$alcohol_intake_frequencyuses_datacoding_100402_f1558_0_0))])
alcohol$alcohol <- alcohol$alcohol_intake_frequencyuses_datacoding_100402_f1558_0_0
alcohol$alcohol[alcohol$eid == 1524068] <- alcohol$alcohol_intake_frequencyuses_datacoding_100402_f1558_1_0[alcohol$eid== 1524068]
alcohol$alcohol[is.na(alcohol$alcohol)] <- alcohol$alcohol_intake_frequencyuses_datacoding_100402_f1558_2_0[is.na(alcohol$alcohol)]

exercise <- work_dta %>% select_at(vars(c(eid, starts_with("number_of_daysweek_of_moderate_physical_activity_10_minutesuses_datacoding_100291_f884"), 
                                          starts_with("number_of_daysweek_of_vigorous_physical_activity_10_minutesuses_datacoding_100291_f904"))))
exercise$exercise <- apply(exercise[,2:7], 1, function(x){max(x, na.rm = TRUE)})

smoking <- work_dta %>% select_at(vars(c(eid, starts_with("smoking_statususes_datacoding_90_f20116"))))
smoking %>% filter(eid %in% eid[which(is.na(smoking$smoking_statususes_datacoding_90_f20116_0_0))])
smoking$smoking <- smoking$smoking_statususes_datacoding_90_f20116_0_0
smoking$smoking[smoking$eid == 1524068] <- smoking$smoking_statususes_datacoding_90_f20116_1_0[smoking$eid== 1524068]
smoking$smoking[is.na(alcohol$alcohol)] <- smoking$smoking_statususes_datacoding_90_f20116_2_0[is.na(smoking$smoking)]


datedeath <- work_dta %>% select_at(vars(c(eid, starts_with("date_of_death_f40000"))))
datedeath$date_death <- datedeath$date_of_death_f40000_0_0 # there are no additional dates that are solely recorded in the 1_0 or 2_0 columns


work_dta <- work_dta %>% mutate(ethnicity = 
                                  ifelse(ethnic_backgrounduses_datacoding_1001_f21000_0_0 %in% c("White", "British", "Irish", "Any other white background"), "White",
                                         ifelse(ethnic_backgrounduses_datacoding_1001_f21000_0_0 %in% c("White and Black Caribbean", "White and Black African", "African", "Any other Black background", "Black or Black British"), "Black",
                                                ifelse(ethnic_backgrounduses_datacoding_1001_f21000_0_0 %in% c("Indian", "Pakistani", "Bangladeshi"), "Indian/Pakistani",
                                                       ifelse(ethnic_backgrounduses_datacoding_1001_f21000_0_0 %in% c("Mixed", "Any other mixed background", "Caribbean"), "Mixed",
                                                              ifelse(ethnic_backgrounduses_datacoding_1001_f21000_0_0 %in% c("Chinese", "White and Asian", "Any other Asian background", "Asian or Asian British"), "Asian/Chinese",
                                                                     ifelse(is.na(ethnic_backgrounduses_datacoding_1001_f21000_0_0) & ethnic_backgrounduses_datacoding_1001_f21000_2_0 %in% c("White", "British", "Irish", "Any other white background"), "White", 
                                                                            ifelse(ethnic_backgrounduses_datacoding_1001_f21000_0_0 %in% c("Other ethnic group"), "Other", 
                                                                                   ifelse(ethnic_backgrounduses_datacoding_1001_f21000_0_0 %in% c("Prefer not to answer", "Do not know"), "Not reported", NA))))))))) 

work_dta_full <- work_dta %>% select(eid, year_of_birth_f34_0_0, month_of_birthuses_datacoding_8_f52_0_0, education, ethnicity)
work_dta_full <- full_join(full_join(full_join(full_join(full_join(full_join(full_join(full_join(full_join(full_join(work_dta_full, agefirstbirth[c("eid", "agefirstbirth")], "eid"), 
                                                                                                 bmi[c("eid", "bmi")], "eid"),
                                                                                       agelastbirth[c("eid", "agelastbirth")], "eid"), 
                                                                             agemenarche[c("eid", "agemenarche")], "eid"), 
                                                                   numberlivebirths[c("eid","parity")], "eid"),
                                                         familyhistory[c("eid", "hxmother", "hxsib")], "eid"),
                                               alcohol[c("eid", "alcohol")], "eid"),
                                     exercise[c("eid", "exercise")], "eid"),
                          smoking[c("eid", "smoking")], "eid"),
                          datedeath[c("eid","date_death")], "eid")
names(work_dta_full)

work_dta_full <- work_dta_full %>% select(eid, year_of_birth_f34_0_0, month_of_birthuses_datacoding_8_f52_0_0, 
                                          bmi, agefirstbirth, agelastbirth, agemenarche, parity, education, 
                                          hxmother, hxsib, alcohol, exercise, smoking, ethnicity,
                                          date_death)
dim(work_dta_full)


# For those who prefer not to answer regarding parity, there is also 100% missiningness on age of first birth. It seems likely that these individuals did not have a live pregnancy.
# We are also removing women with > 6 children
ppna <- work_dta_full %>% filter(parity == -3) 
all(is.na(ppna$agefirstbirth))

# Recoding
work_dta_full$parity <- as.numeric(work_dta_full$parity)
work_dta_full$year_of_birth_f34_0_0 <- as.numeric(work_dta_full$year_of_birth_f34_0_0)
work_dta_full$agefirstbirth <- as.numeric(work_dta_full$agefirstbirth)
work_dta_full$agelastbirth <- as.numeric(work_dta_full$agelastbirth)
work_dta_full$agemenarche <- as.numeric(work_dta_full$agemenarche)

work_dta_subset <- work_dta_full %>% filter(parity > 0 & parity < 6)

# Year of Last Birth
work_dta_subset <- work_dta_subset %>% mutate(year_first_birth = ifelse(agefirstbirth > 0, agefirstbirth + year_of_birth_f34_0_0, NA), 
                                                                                         year_last_birth = ifelse(agelastbirth > 0, agelastbirth + year_of_birth_f34_0_0, NA))


save(work_dta_full, file = "data/ukb_working_data_full_n272974.RData") # all women
save(work_dta_subset, file = "data/ukb_working_data_sub_n220116.RData") # women with 1 - 5 children

rm(agefirstbirth, agelastbirth, agemenarche, alcohol, bmi, college, exercise, familyhistory, hxmother, hxsib, lowerlevel, numberlivebirths, ppna, smoking, datedeath)
