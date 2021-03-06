# This file can be modified to meet the criteria used for classifying genotype probabilities.
# The current cutoff is 0.95 for both SNPs.

load("data/ukb_working_data_sub_n220116_exp_out_rs2016347.RData")
load("data/ukb_working_data_sub_n220116_exp_out_rs2684788.RData")

# work_dta_subset_2016347 %>% ggplot(aes(x = GG, y = TT)) + 
#   geom_hex(bins = 100) + 
#   theme_classic()

# SNP 2016347
dta_long <- work_dta_subset_2016347 %>% select(eid, GG, GT, TT) %>% gather("genotype2016347", "probability", 2:4) %>% arrange(eid)
dta_long <- dta_long %>% group_by(eid) %>% mutate(maxP = max(probability))
dta_long <- dta_long %>% filter(probability == maxP)
dta_long <- dta_long %>% filter(probability >= 0.95) # assigning based on 0.95 cutoff

dta_long <- dta_long %>% select(eid, genotype2016347)
work_dta_subset_2016347 <- right_join(work_dta_subset_2016347, dta_long, by = "eid") %>% select(-GG, -GT, -TT)


# SNP 2684788

dta_long <- work_dta_subset_2684788 %>% select(eid, CC, CT, TT) %>% gather("genotype2684788", "probability", 2:4) %>% arrange(eid)
dta_long <- dta_long %>% group_by(eid) %>% mutate(maxP = max(probability))
dta_long <- dta_long %>% filter(probability == maxP)
dta_long <- dta_long %>% filter(probability >= 0.95) # assigning based on 0.95 cutoff

dta_long <- dta_long %>% select(eid, genotype2684788)
work_dta_subset_2684788 <- right_join(work_dta_subset_2684788, dta_long, by = "eid") %>% select(-CC, -CT, -TT)


work_dta_subset <- full_join(work_dta_subset_2016347, work_dta_subset_2684788, 
                             by = common_by(by = NULL, work_dta_subset_2016347, work_dta_subset_2684788)$x)

work_dta_subset <- work_dta_subset %>% mutate(education = as.factor(education), 
                                              exercise = as.factor(exercise), 
                                              ethnicity = as.factor(ethnicity))

work_dta_subset <- work_dta_subset %>% 
  mutate(t.allele.num = case_when(genotype2016347 == "GT" ~ 1,
                                  genotype2016347 == "TT" ~ 2,
                                  TRUE ~ 0),
         t.allele.bin = case_when(genotype2016347 %in% c("GT", "TT") ~ 1,
                                  TRUE ~ 0)) %>%
  mutate(t.allele.num = ifelse(is.na(genotype2016347), NA, t.allele.num),
         t.allele.bin = ifelse(is.na(genotype2016347), NA, t.allele.bin))


save(work_dta_subset, file = here("data", "ukb_working_data_sub_n207797_exp_out_cleanSNPs.RData"))

rm(dta_long, work_dta_subset_2016347, work_dta_subset_2684788, bc.insitu, bc.insitu.recs, date.dta.is, reg.date.is, work_dta, work_dta_full)
