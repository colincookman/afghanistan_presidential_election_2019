target <- "http://www.iec.org.af/en/voters/vr-statistics"

pdf_links <- getHTMLLinks(target)
pdf_links <- pdf_links[grep("/pdf/vr-list-1397/", pdf_links)]
pdf_links <- paste0("http://www.iec.org.af", pdf_links)
pdf_links <- gsub("\\../", "", pdf_links)

for(i in seq_along(pdf_links)) {
  download.file(pdf_links[i], file.path("./raw/vr_data/", destfile = basename(pdf_links[i])))
  Sys.sleep(1)
}

# -----------------------------------------------------------------------------

rm(list = ls())
file_list <- list.files("./raw/vr_data", pattern = ".pdf")


for(i in 1:length(file_list)) {
  
  # import files on list and convert to text lines
  target <- paste0("./raw/vr_data/", file_list[i])
  
  # pull out province name and code
  province_name <- str_split(target, "-")[[1]][2]
  province_name <- trimws(str_split(province_name, "\\.")[[1]][1])
  province_code <- str_split(target, "-")[[1]][1]
  province_code <- trimws(str_split(province_code, "\\/")[[1]][4])
  #
  province_districts <- unique(district_key$district_or_subdivision_name_dari[district_key$province_code == province_code])
  
  province_import <- pdf_text(target)
  province_string <- toString(province_import)
  province_lines <- read_lines(province_string)

  variables <- grep("مجموع", province_lines)
  variables_2 <- variables + 1
  header_start <- grep("انتخابات", province_lines) 
  headers <- unlist(c(header_start, (header_start + 1), (header_start+2)))
  footer_row <- grep("Page ", province_lines)
  
  district_rows <- grep(list(province_districts), province_lines)
  # remove headers and footers from document
  province_trimmed <- province_lines[- c(headers, variables, variables_2, footer_row)]
  
  province_trimmed_again <- gsub("[^0-9 ]", "", province_trimmed)
  
  data <- Reduce(rbind, strsplit(trimws(province_trimmed_again), "\\s{2,}"))
  rownames(data) <- 1:dim(data)[1]
  data <- as.data.frame(data)
  
    
  
  colnames(data) <- c("ballot_position", "candidate_name_eng", "candidate_code", "votes", "vote_share")
  data$province_code <- province_code
  data$province_name_eng <- str_to_upper(province_name)
  
  if(i == 1) {
    all_out <- data
  }else{
    all_out <- rbind(all_out, data)
  }

}

# -----------------------------------------------------------------------------

vr_summary <- pc_key %>% filter(planned_2018 == "YES") %>% group_by(province_code, province_name_eng) %>% summarize(
  male_old = sum(vr_general_male, na.rm = T),
  fem_old = sum(vr_general_fem, na.rm = T),
  kuchi_old = sum(vr_final_kuchi, na.rm = T),
  sikh_old = sum(vr_final_sikh, na.rm = T))

new_vr$province <- str_to_upper(new_vr$province)
new_vr$province[new_vr$province == "NOORISTAN"] <- "NOORESTAN"
new_vr$province[new_vr$province == "KUNAR"] <- "KUNARHA"
new_vr$province[new_vr$province == "UROZGAN"] <- "URUZGAN"
new_vr$province[new_vr$province == "WARDAK"] <- "MAYDANWARDAK"
new_vr$province[new_vr$province == "SAREPUL"] <- "SARE PUL"

vr_change <- new_vr %>% rename(province_name_eng = province) %>% left_join(vr_summary) %>% rowwise %>% mutate(
    total_old = sum(male_old, fem_old, kuchi_old, sikh_old, na.rm = T),
    male_change = male - male_old,
    male_pct = male_change / male_old,
    fem_change = female - fem_old,
    fem_pct = fem_change / fem_old,
    kuchi_change = kuchi - kuchi_old,
    kuchi_pct = kuchi_change / kuchi_old,
    sikh_change = sikh - sikh_old,
    sikh_pct = sikh_change / sikh_old,
    total_change = total - total_old,
    total_pct = total_change / total_old)

vr_change <- vr_change %>% dplyr::select(
  province_code, province_name_eng,
  total, total_old, total_change, total_pct,
  male, male_old, male_change, male_pct,
  female, fem_old, fem_change, fem_pct,
  kuchi, kuchi_old, kuchi_change, kuchi_pct,
  sikh, sikh_old, sikh_change, sikh_pct) %>% 
  arrange(province_code) %>%
  rename(
    total_2019 = total, total_2018 = total_old,
    male_2019 = male, male_2018 = male_old,
    fem_2019 = female, fem_2018 = fem_old,
    kuchi_2019 = kuchi, kuchi_2018 = kuchi_old,
    sikh_2019 = sikh, sikh_2018 = sikh_old
  )

write.csv(vr_change, "./raw/rough_vr_change.csv", row.names = F)

# -----------------------------------------------------------------------------

pc_df <- pc_key_2018 %>% 
  dplyr::select(
    province_code, district_sub_code, pc_code, pc_name_eng, pc_name_dari,
    planned_2018, prelim_results_reporting, final_results_reporting,
    vr_prelim, vr_general_male, vr_general_fem, vr_final_kuchi, vr_final_sikh, vr_final_all_total) %>% 
  rename(
    planned_18 = planned_2018,
    prelim_results_18 = prelim_results_reporting,
    final_results_18 = final_results_reporting,
    vr_prelim_total_18 = vr_prelim,
    vr_male_18 = vr_general_male,
    vr_fem_18 = vr_general_fem,
    vr_kuchi_18 = vr_final_kuchi,
    vr_sikh_18 = vr_final_sikh,
    vr_final_total_18 = vr_final_all_total
  )

dupe_check <- pc_df %>% group_by(district_sub_code) %>% 
  mutate(double = ifelse(duplicated(pc_name_eng), "YES", "NO")) %>% 
  filter(double == "YES") %>% dplyr::select(pc_name_eng, double) %>%
  right_join(pc_df) %>% dplyr::filter(double == "YES")

new_vr$pc_code <- as.character(str_pad(new_vr$pc_code, 7, pad = "0", side = "left"))
new_vr_join <- new_vr %>% 
  dplyr::select(pc_code, vr_male, vr_fem, vr_kuchi, vr_sikh, vr_total) %>%
  rename(vr_male_19 = vr_male,
         vr_fem_19 = vr_fem,
         vr_kuchi_19 = vr_kuchi,
         vr_sikh_19 = vr_sikh,
         vr_prelim_total_19 = vr_total)

pc_df <- left_join(pc_df, new_vr_join)

pc_votes <- all_data_all_results %>% group_by(pc_code) %>% summarize(
  prelim_votes_18 = sum(votes[results_status == "PRELIMINARY"], na.rm = T),
  final_votes_18 = sum(votes[results_status == "FINAL"], na.rm = T)
)

pc_df <- left_join(pc_df, pc_votes)

pc_df <- pc_df %>% mutate(
  missing_vr_19 = ifelse(!is.na(vr_final_total_18) & is.na(vr_prelim_total_19), "YES", "OK"),
  new_vr_19 = ifelse(is.na(vr_final_total_18) & !is.na(vr_prelim_total_19), "NEW", "NO CHANGE")
)

pc_df <- pc_df %>% mutate(
  vr_male_change = ifelse(is.na(vr_male_19), 0, vr_male_19) - ifelse(is.na(vr_male_18), 0, vr_male_18),
  vr_male_pct_change = vr_male_change / ifelse(is.na(vr_male_18), 0, vr_male_18),
  vr_fem_change = ifelse(is.na(vr_fem_19), 0, vr_fem_19) - ifelse(is.na(vr_fem_18), 0, vr_fem_18),
  vr_fem_pct_change = vr_fem_change / ifelse(is.na(vr_fem_18), 0, vr_fem_18),
  vr_kuchi_change = ifelse(is.na(vr_kuchi_19), 0, vr_kuchi_19) - ifelse(is.na(vr_kuchi_18), 0, vr_kuchi_18),
  vr_kuchi_pct_change = vr_kuchi_change / ifelse(is.na(vr_kuchi_18), 0, vr_kuchi_18),
  vr_sikh_change = ifelse(is.na(vr_sikh_19), 0, vr_sikh_19) - ifelse(is.na(vr_sikh_18), 0, vr_sikh_18),
  vr_sikh_pct_change = vr_sikh_change / ifelse(is.na(vr_sikh_18), 0, vr_sikh_18),
  vr_total_change = ifelse(is.na(vr_prelim_total_19), 0, vr_prelim_total_19) - ifelse(is.na(vr_final_total_18), 0, vr_final_total_18),
  vr_total_pct_change = vr_total_change / ifelse(is.na(vr_final_total_18), 0, vr_final_total_18),
  vr_prelim_total_change = ifelse(is.na(vr_prelim_total_19), 0, vr_prelim_total_19) - ifelse(is.na(vr_prelim_total_18), 0, vr_prelim_total_18),
  vr_prelim_total_change_pct = vr_prelim_total_change / ifelse(is.na(vr_prelim_total_18), 0, vr_prelim_total_18),
  prelim_votes_18_pct_vr_18 = prelim_votes_18 / vr_final_total_18,
  prelim_votes_18_pct_vr_19 = prelim_votes_18 / vr_prelim_total_19,
  final_votes_18_pct_vr_18 = final_votes_18 / vr_final_total_18,
  final_votes_18_pct_vr_19 = final_votes_18 / vr_prelim_total_19
)

pc_df$district_sub_code[pc_df$pc_code == "0101555"] <- "0101-12"
pc_df$district_sub_code[pc_df$pc_code == "0101556"] <- "0101-12"
pc_df$district_sub_code[pc_df$pc_code == "0101559"] <- "0101-22"

output <- pc_df %>% left_join(dplyr::select(province_key, province_code, province_name_eng)) %>% 
  left_join(dplyr::select(district_key, district_sub_code, district_or_subdivision_name_eng)) %>% dplyr::select(
  province_code, province_name_eng, district_sub_code, district_or_subdivision_name_eng, pc_code, pc_name_eng, pc_name_dari,
  planned_18, prelim_results_18, prelim_votes_18, final_results_18, final_votes_18, missing_vr_19, new_vr_19,
  vr_prelim_total_18, vr_final_total_18, vr_male_18, vr_fem_18, vr_kuchi_18, vr_sikh_18,
  vr_prelim_total_19, vr_male_19, vr_fem_19, vr_kuchi_19, vr_sikh_19,
  vr_total_change, vr_total_pct_change, 
  vr_male_change, vr_male_pct_change, vr_fem_change, vr_fem_pct_change, 
  vr_kuchi_change, vr_kuchi_pct_change, vr_sikh_change, vr_sikh_pct_change) %>% 
  arrange(province_code, district_sub_code, pc_code)

write.csv(output, "./raw/af_pc_vr_comparison_18_19.csv", row.names = F)

province_summary <- output %>% group_by(province_code, province_name_eng) %>%
  summarize(
    total_known_pcs = length(pc_code),
    planned_pcs_18 = length(pc_code[planned_18 == "YES"]),
    prelim_results_pcs_18 = length(pc_code[prelim_results_18 == "YES"]),
    final_results_pcs_18 = length(pc_code[final_results_18 == "YES"]),
    pcs_with_vr_18 = length(pc_code[!is.na(vr_final_total_18)]),
    pcs_with_vr_not_planned_18 = length(pc_code[!is.na(vr_final_total_18) & planned_18 == "NO"]),
    pcs_planned_but_missing_vr_18 = length(pc_code[is.na(vr_final_total_18) & planned_18 == "YES"]),
    total_prelim_votes_18 = sum(prelim_votes_18, na.rm = T),
    total_final_votes_18 = sum(final_votes_18, na.rm = T),
    pcs_with_vr_19 = length(pc_code[!is.na(vr_prelim_total_19)]),
    pcs_missing_from_vr_19 = length(pc_code[missing_vr_19 == "YES"]),
    pcs_that_added_vr_19 = length(pc_code[new_vr_19 == "NEW"]),
    prelim_total_vr_18 = sum(vr_prelim_total_18, na.rm = T),
    final_total_vr_18 = sum(vr_final_total_18, na.rm = T),
    final_total_vr_18_at_planned_pcs = sum(vr_final_total_18[planned_18 == "YES"], na.rm = T),
    final_male_vr_18 = sum(vr_male_18, na.rm = T),
    final_male_vr_18_at_planned_pcs = sum(vr_male_18[planned_18 == "YES"], na.rm = T),
    final_fem_vr_18 = sum(vr_fem_18, na.rm = T),
    final_fem_vr_18_at_planned_pcs = sum(vr_fem_18[planned_18 == "YES"], na.rm = T),
    final_kuchi_vr_18 = sum(vr_kuchi_18, na.rm = T),
    final_kuchi_vr_18_at_planned_pcs = sum(vr_kuchi_18[planned_18 == "YES"]),
    final_sikh_vr_18 = sum(vr_sikh_18, na.rm = T),
    final_sikh_vr_18_at_planned_pcs = sum(vr_sikh_18[planned_18 == "YES"]),
    prelim_total_vr_19 = sum(vr_prelim_total_19, na.rm = T),
    prelim_vr_male_19 = sum(vr_male_19, na.rm = T),
    prelim_vr_fem_19 = sum(vr_fem_19, na.rm = T),
    prelim_vr_kuchi_19 = sum(vr_kuchi_19, na.rm = T),
    prelim_vr_sikh_19 = sum(vr_sikh_19, na.rm = T),
    total_vr_net_change = prelim_total_vr_19 - final_total_vr_18,
    total_vr_pct_change = total_vr_net_change / final_total_vr_18,
    vr_male_net_change = prelim_vr_male_19 - final_male_vr_18,
    vr_male_pct_change = vr_male_net_change / final_male_vr_18,
    vr_fem_net_change = prelim_vr_fem_19 - final_fem_vr_18,
    vr_fem_pct_change = vr_fem_net_change / final_fem_vr_18,
    vr_kuchi_net_change = prelim_vr_kuchi_19 / final_kuchi_vr_18,
    vr_kuchi_pct_change = vr_kuchi_net_change / final_kuchi_vr_18,
    vr_sikh_net_change = prelim_vr_sikh_19 - final_sikh_vr_18,
    vr_sikh_pct_change = vr_sikh_net_change / final_sikh_vr_18
  )
province_summary[11,4:35] <- NA

write.csv(province_summary, "./raw/af_province_vr_comparison_18_19.csv", row.names = F)
