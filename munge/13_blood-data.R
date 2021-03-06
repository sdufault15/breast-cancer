
library(tidyverse)
#ukb_dta <- ukb_df("ukb22626", path = "/Volumes/Seagate Backup Plus Drive/BiobankData", n_threads = "dt") 
# UK Biobank spreadsheet containing blood levels with blood draw dates for
# IGF-1 (30770), Estrogen (30800), SHBG (30830), Testosterone (30850), and Vit. D (30890)

ukb_new_dta <- read.csv("/Volumes/My Passport/ukb29950.csv")
head(ukb_new_dta)

# Names don't match yet, fixing them now:

ukb_new_dta <- ukb_new_dta %>%
  transmute(eid, 
            sex = X31.0.0,
            parity_1 = X2734.0.0,
            parity_2 = X2734.1.0,
            parity_3 = X2734.2.0,
            igf1_1 = X30770.0.0,
            igf1_2 = X30770.1.0,
            estrogen_1 = X30800.0.0,
            estrogen_2 = X30800.1.0,
            shbg_1 = X30830.0.0,
            shbg_2 = X30830.1.0,
            testosterone_1 = X30850.0.0,
            testosterone_2 = X30850.1.0,
            vitD_1 = X30890.0.0,
            vitD_2 = X30890.1.0
  )

# Cleaning
parity <- ukb_new_dta %>% select_at(vars(c(eid, starts_with("parity"))))
parity$parity <- apply(parity[,2:4],1,function(x){max(as.numeric(x), na.rm = TRUE)})

igf1 <- ukb_new_dta %>% select_at(vars(c(eid, starts_with("igf1"))))
igf1$igf1 <- apply(igf1[,2:3],1,function(x){mean(as.numeric(x), na.rm = TRUE)})

estrogen <- ukb_new_dta %>% select_at(vars(c(eid, starts_with("estrogen"))))
estrogen$estrogen <- apply(estrogen[,2:3],1,function(x){mean(as.numeric(x), na.rm = TRUE)})

shbg <- ukb_new_dta %>% select_at(vars(c(eid, starts_with("shbg"))))
shbg$shbg <- apply(shbg[,2:3],1,function(x){mean(as.numeric(x), na.rm = TRUE)})

testosterone <- ukb_new_dta %>% select_at(vars(c(eid, starts_with("testosterone"))))
testosterone$testosterone <- apply(testosterone[,2:3],1,function(x){mean(as.numeric(x), na.rm = TRUE)})

vitD <- ukb_new_dta %>% select_at(vars(c(eid, starts_with("vitD"))))
vitD$vitD <- apply(vitD[,2:3],1,function(x){mean(as.numeric(x), na.rm = TRUE)})

ukb_blood_dta <- 
  full_join(vitD[,c("eid", "vitD")],
            full_join(shbg[,c("eid", "shbg")],
                      full_join(testosterone[, c("eid", "testosterone")],
                                full_join(estrogen[,c("eid", "estrogen")], 
                                          full_join(parity[,c("eid", "parity")], igf1[,c("eid", "igf1")], by = "eid"),
                                          by = "eid"),
                                by = "eid"),
                      by = "eid"),
            by = "eid")

ukb_blood_dta <- ukb_blood_dta %>%
  mutate(eid = as.character(eid))

# Merging

load(here("data", "ukb_working_data_sub_n207797_exp_out_cleanSNPs.RData"))
work_dta_subset <- left_join(work_dta_subset,
                             ukb_blood_dta, by = c("eid", "parity"))
save(work_dta_subset, file = here("data", "ukb_working_data_sub_n207797_exp_out_cleanSNPS_blood.RData"))
