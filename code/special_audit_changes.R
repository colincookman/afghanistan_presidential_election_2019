library(tidyverse)
library(lubridate)
options(scipen=999)
setwd("~/Google Drive/GitHub/afghanistan_presidential_election_2019")

rm(list = ls())

final_ps_data <- read_csv("results_data/first_round_final_results/final_af_candidate_ps_data_2019.csv")
prelim_ps_data <- read_csv("results_data/first_round_preliminary_results/prelim_af_candidate_ps_data_2019.csv")

all_data <- full_join(prelim_ps_data, final_ps_data) %>% arrange(province_code, ps_code, ballot_position, results_status)

pc_key_2019 <- read_csv("keyfiles/pc_key_2019.csv")
ps_key_2019 <- read_csv("keyfiles/ps_key_2019.csv")
ps_reporting_status <- read_csv("validity_checks/ps_reporting_status.csv")
ps_summary_report <- read_csv("analysis/ps_summary_report.csv")
ps_audit_target_list <- read_csv("audit_data/ps_audit_target_list.csv")
iec_ecc_special_audit_targets <- read_csv("raw/iec_ecc_special_audit_list/iec_ecc_special_audit_targets.csv")

# UPDATE REPORTING STATUS

ecc_special_audit_targets <- tibble(ps_code = unique(iec_ecc_special_audit_targets$ps_code), ecc_special_audit = "YES")

ps_reporting_status_update <- ps_reporting_status %>% left_join(ecc_special_audit_targets) %>% dplyr::select(
  province_code, pc_code, ps_code, ps_type,
  prelim_results_reporting, final_results_reporting,
  audit_or_recount, ecc_special_audit,
  everything()
  ) %>% rename(
    iec_audit = audit_or_recount,
    iec_invalidated = ps_invalidated,
    prelim_zero_votes = reporting_zero_votes
  )
ps_reporting_status_update$ecc_special_audit[is.na(ps_reporting_status_update$ecc_special_audit)] <- "NO"

write.csv(ps_reporting_status_update, "./validity_checks/ps_reporting_status.csv", row.names = F)

# CREATE NEW PS KEY WITH ALL METADATA
  
initially_quarantined <- iec_ecc_special_audit_targets$ps_code[iec_ecc_special_audit_targets$special_audit_criteria == "initially_quarantined"]
recount <- iec_ecc_special_audit_targets$ps_code[iec_ecc_special_audit_targets$special_audit_criteria == "recount"]
out_of_time <- iec_ecc_special_audit_targets$ps_code[iec_ecc_special_audit_targets$special_audit_criteria == "out_of_time"]

initially_quarantined <- tibble(ps_code = initially_quarantined, ecc_audit_quarantined = "YES")
recount <- tibble(ps_code = recount, ecc_audit_recount = "YES")
out_of_time <- tibble(ps_code = out_of_time, ecc_audit_time = "YES")

ps_report_with_audit <- ps_summary_report %>% 
  left_join(ecc_special_audit_targets) %>%
  left_join(initially_quarantined) %>%
  left_join(out_of_time) %>%
  left_join(recount) %>%
  rename(
    iec_audit = audit_or_recount,
    iec_invalidated = ps_invalidated,
    prelim_zero_votes = reporting_zero_votes
  )

ps_report_with_audit$ecc_special_audit[is.na(ps_report_with_audit$ecc_special_audit)] <- "NO"

ps_metadata <- ps_report_with_audit %>%
    left_join(dplyr::select(
    ps_reporting_status, ps_code, final_results_reporting
  )) %>%
  left_join(
    final_ps_data %>% 
      group_by(ps_code) %>%
      summarize(final_total_votes = sum(votes, na.rm = T))) %>%
  dplyr::select(1:10, 44, 13, 11, 23, 45, 12, 14:22, 40:43) %>% 
  arrange(province_code, pc_code, ps_code) %>%
  rename(
    iec_pre_audit_decision = pre_audit_decision,
    iec_post_audit_decision = post_audit_decision,
    iec_invalidated_results_sheet_total = invalidated_results_sheet_total,
    iec_pre_audit_bvv_total = pre_audit_bvv_total,
    prelim_total_votes = total_votes,
    prelim_missing_bvv_reporting = missing_bvv_reporting,
    prelim_excess_turnout = excess_turnout,
    prelim_pre_post_audit_difference = pre_post_audit_difference
  ) %>% rowwise %>% mutate(
    prelim_final_net_change = sum(final_total_votes, -(prelim_total_votes), na.rm = T)
  ) %>% dplyr::select(
    1:15, prelim_final_net_change, everything()
  ) %>% mutate(
    final_zero_votes = ifelse(final_results_reporting == "YES" & final_total_votes == 0, "YES", 
                              ifelse(final_results_reporting == "YES" & final_total_votes != 0, "NO", NA))
  )


write.csv(ps_metadata, "./keyfiles/ps_key_2019.csv", row.names = F)

# NET CHANGE IN AUDIT

net_change <- all_data %>% group_by(ps_code, candidate_code) %>% summarize(
  prelim_votes = sum(votes[results_status == "PRELIMINARY"]),
  final_votes = sum(votes[results_status == "FINAL"]),
  net_change = final_votes - prelim_votes
  )

write.csv(net_change, "./analysis/prelim_final_net_change.csv", row.names = F)

