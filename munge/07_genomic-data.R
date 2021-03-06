###############################
# Suzanne Dufault
# August 14, 2018
# Opening Data Files from Mark
# GENOMIC DATA ONLY
###############################

#install.packages( "http://www.well.ox.ac.uk/~gav/resources/rbgen_v1.1.4.tgz", repos = NULL, type = "source" )
gen_dta <- bgen.load("/Volumes/Seagate Backup Plus Drive/BiobankData/ukb_imp_chr15_v3.bgen",
                     rsids = c("rs2016347", "rs2684788"))

save(gen_dta, file = "data/genomicDataSNPs.RData") # saving file for future use


# Merging with working dataset
identifiers <- ukb_gen_read_sample("/Volumes/Seagate Backup Plus Drive/BiobankData/ukb37368_imp_chr15_v3_s487371.sample")#, col.name = c("id_1", "id_2", "missing", "sex"))

# Not quite right w.r.t. formatting. 
head(identifiers$id_1)
eids <- unlist(data.table::tstrsplit(identifiers$id_1, split = " ", keep = 1))
head(eids)

gen_dta_rs2016347 <- gen_dta$data['rs2016347',,]
gen_dta_rs2684788 <- gen_dta$data['rs2684788',,]
colnames(gen_dta_rs2016347) <- c("GG", "GT", "TT")
colnames(gen_dta_rs2684788) <- c("CC", "CT", "TT")

dta_rs2016347 <- data.frame(eid = eids, gen_dta_rs2016347, stringsAsFactors = FALSE)
dta_rs2684788 <- data.frame(eid = eids, gen_dta_rs2684788, stringsAsFactors = FALSE)

work_dta_subset_2016347 <- left_join(work_dta_subset, dta_rs2016347, by = "eid")
work_dta_subset_2684788 <- left_join(work_dta_subset, dta_rs2684788, by = "eid")

save(work_dta_subset_2016347, file = "data/ukb_working_data_sub_n220116_exp_out_rs2016347.RData")
save(work_dta_subset_2684788, file = "data/ukb_working_data_sub_n220116_exp_out_rs2684788.RData")

rm(gen_dta, dta_rs2016347, dta_rs2684788, gen_dta_rs2016347, gen_dta_rs2684788, eids, identifiers)
