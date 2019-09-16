# IMPORT DATA -----------------

final_first_round_2014 <- read_csv("past_elections/presidential_2014/results_data/first_round_final_results/final_af_candidate_ps_data_first_round_2014.csv")
prelim_first_round_2014 <- read_csv("past_elections/presidential_2014/results_data/first_round_preliminary_results/prelim_af_candidate_ps_data_first_round_2014.csv")

final_runoff_2014 <- read_csv("past_elections/presidential_2014/results_data/run_off_final_results/final_af_candidate_ps_data_run_off_2014.csv")
prelim_runoff_2014 <- read_csv("past_elections/presidential_2014/results_data/run_off_preliminary_results/prelim_af_candidate_ps_data_run_off_2014.csv")

final_wj_2018 <- read_csv("~/Google Drive/GitHub/afghanistan_election_results_2018/results_data/final_results/final_af_candidate_ps_data_2018.csv")
prelim_wj_2018 <- read_csv("~/Google Drive/GitHub/afghanistan_election_results_2018/results_data/preliminary_results/prelim_af_candidate_ps_data_2018.csv")

all_data_2014 <- full_join(final_first_round_2014, prelim_first_round_2014) %>% 
  full_join(final_runoff_2014) %>%
  full_join(prelim_runoff_2014)

all_data_2018 <- full_join(final_wj_2018, prelim_wj_2018)

pc_key_2019 <- read_csv("keyfiles/pc_key_2019.csv")
pc_key_2018 <- read_csv("~/Google Drive/GitHub/afghanistan_election_results_2018/keyfiles/pc_key.csv")

district_key_2014 <- read_csv("past_elections/presidential_2014/keyfiles/district_key_2014.csv")

#district_final_vr_net_change <- read_csv("pc_plan/district_final_vr_net_change.csv")

district_code_keyfile_2019 <- read_csv("district_data/district_code_keyfile_2019.csv")
district_code_keyfile_2019$`2019_IEC_province_code` <- str_pad(district_code_keyfile_2019$`2019_IEC_province_code`, width = 2, side = "left", pad = 0)
district_code_keyfile_2019$`2018_IEC_province_code` <- str_pad(district_code_keyfile_2019$`2018_IEC_province_code`, width = 2, side = "left", pad = 0)
district_code_keyfile_2019$`2014_IEC_province_code` <- str_pad(district_code_keyfile_2019$`2014_IEC_province_code`, width = 2, side = "left", pad = 0)
district_code_keyfile_2019$`2012_AGCHO_province_code` <- str_pad(district_code_keyfile_2019$`2012_AGCHO_province_code`, width = 2, side = "left", pad = 0)
district_code_keyfile_2019$`2010_IEC_province_code` <- str_pad(district_code_keyfile_2019$`2010_IEC_province_code`, width = 2, side = "left", pad = 0)
district_code_keyfile_2019$`2009_IEC_province_code` <- str_pad(district_code_keyfile_2019$`2009_IEC_province_code`, width = 2, side = "left", pad = 0)

district_code_keyfile_2019$`2019_IEC_district_code` <- str_pad(district_code_keyfile_2019$`2019_IEC_district_code`, width = 4, side = "left", pad = 0)
district_code_keyfile_2019$`2018_matched_to_2019_IEC_district_code` <- str_pad(district_code_keyfile_2019$`2018_matched_to_2019_IEC_district_code`, width = 4, side = "left", pad = 0)
district_code_keyfile_2019$`2018_IEC_district_code` <- str_pad(district_code_keyfile_2019$`2018_IEC_district_code`, width = 4, side = "left", pad = 0)
district_code_keyfile_2019$`2019_matched_to_2014_IEC_district_code` <- str_pad(district_code_keyfile_2019$`2019_matched_to_2014_IEC_district_code`, width = 4, side = "left", pad = 0)
district_code_keyfile_2019$`2014_IEC_district_code` <- str_pad(district_code_keyfile_2019$`2014_IEC_district_code`, width = 4, side = "left", pad = 0)
district_code_keyfile_2019$`2019_matched_to_2012_AGCHO_district_code` <- str_pad(district_code_keyfile_2019$`2019_matched_to_2012_AGCHO_district_code`, width = 4, side = "left", pad = 0)
district_code_keyfile_2019$`2012_AGCHO_district_code` <- str_pad(district_code_keyfile_2019$`2012_AGCHO_district_code`, width = 4, side = "left", pad = 0)
district_code_keyfile_2019$`2019_matched_to_2010_IEC_district_code` <- str_pad(district_code_keyfile_2019$`2019_matched_to_2010_IEC_district_code`, width = 4, side = "left", pad = 0)
district_code_keyfile_2019$`2010_IEC_district_code` <- str_pad(district_code_keyfile_2019$`2010_IEC_district_code`, width = 4, side = "left", pad = 0)
district_code_keyfile_2019$`2019_matched_to_2009_IEC_district_code` <- str_pad(district_code_keyfile_2019$`2019_matched_to_2009_IEC_district_code`, width = 4, side = "left", pad = 0)
district_code_keyfile_2019$`2009_IEC_district_code` <- str_pad(district_code_keyfile_2019$`2009_IEC_district_code`, width = 4, side = "left", pad = 0)

# COLLAPSE DATA -------------

all_data_2018_with_2014 <- all_data_2018 %>% 
  rename(`2018_IEC_district_code` = district_code) %>%
  left_join(dplyr::select(
    district_code_keyfile_2019, `2018_IEC_district_code`, `2019_matched_to_2014_IEC_district_code` 
  ))

all_data_2018_with_2014$results_status <- gsub("Results Status : Uncertified", "PRELIMINARY", all_data_2018_with_2014$results_status)
all_data_2018_with_2014$results_status <- gsub("Results Status : Certified", "FINAL", all_data_2018_with_2014$results_status)

consolidate_18_to_14 <- all_data_2018_with_2014 %>% group_by(`2019_matched_to_2014_IEC_district_code`) %>%
  summarize(
    prelim_votes_18 = sum(votes[results_status == "PRELIMINARY"], na.rm = T),
    final_votes_18 = sum(votes[results_status == "FINAL"], na.rm = T),
    net_change_prelim_final_18 = final_votes_18 - prelim_votes_18,
    pct_change_prelim_final_18 = net_change_prelim_final_18 / prelim_votes_18,
    prelim_winner_votes_18 = sum(votes[winner == "YES" & results_status == "PRELIMINARY"], na.rm = T),
    prelim_winner_pct_18 = prelim_winner_votes_18 / prelim_votes_18,
    final_winner_votes_18 = sum(votes[winner == "YES" & results_status == "FINAL"]),
    final_winner_pct_18 = final_winner_votes_18 / final_votes_18
  )

consolidate_14 <- all_data_2014 %>% rename(`2014_IEC_district_code` = district_code) %>%
  group_by(`2014_IEC_district_code`) %>%
  summarize(
    prelim_first_round_votes_14 = sum(votes[results_status == "PRELIMINARY" & election_type == "PRESIDENTIAL"], na.rm = T),
    final_first_round_votes_14 = sum(votes[results_status == "FINAL" & election_type == "PRESIDENTIAL"], na.rm = T),
    net_change_first_round_14 = final_first_round_votes_14 - prelim_first_round_votes_14,
    pct_change_first_round_14 = net_change_first_round_14 / prelim_first_round_votes_14,
    prelim_runoff_votes_14 = sum(votes[results_status == "PRELIMINARY" & election_type == "PRESIDENTIAL RUNOFF"], na.rm = T),
    final_runoff_votes_14 = sum(votes[results_status == "FINAL" & election_type == "PRESIDENTIAL RUNOFF" & 
                                      post_audit_status != "Invalidated"], na.rm = T),
    net_change_runoff_14 = final_runoff_votes_14 - prelim_runoff_votes_14,
    pct_change_runoff_14 = net_change_runoff_14 / prelim_runoff_votes_14,
    net_change_final_first_runoff_14 = final_runoff_votes_14 - final_first_round_votes_14,
    pct_change_final_first_runoff_14 = net_change_final_first_runoff_14 / final_first_round_votes_14,
    AG_final_first_round_votes_14 = sum(votes[results_status == "FINAL" & election_type == "PRESIDENTIAL" &
                                           candidate_code == "101_1_13"], na.rm = T),
    AA_final_first_round_votes_14 = sum(votes[results_status == "FINAL" & election_type == "PRESIDENTIAL" &
                                           candidate_code == "100_1_1"], na.rm = T),
    AG_first_round_pct_14 = AG_final_first_round_votes_14 / final_first_round_votes_14,
    AA_first_round_pct_14 = AA_final_first_round_votes_14 / final_first_round_votes_14,
    AG_final_runoff_votes_14 = sum(votes[results_status == "FINAL" & election_type == "PRESIDENTIAL RUNOFF" &
                                           candidate_code == "101_1_13" & post_audit_status != "Invalidated"], na.rm = T),
    AA_final_runoff_votes_14 = sum(votes[results_status == "FINAL" & election_type == "PRESIDENTIAL RUNOFF" &
                                           candidate_code == "100_1_1" & post_audit_status != "Invalidated"], na.rm = T),
    AG_runoff_pct_14 = AG_final_runoff_votes_14 / final_runoff_votes_14,
    AA_runoff_pct_14 = AA_final_runoff_votes_14 / final_runoff_votes_14,
    AG_net_change_first_runoff_14 = AG_final_runoff_votes_14 - AG_final_first_round_votes_14,
    AA_net_change_first_runoff_14 = AA_final_runoff_votes_14 - AA_final_first_round_votes_14,
    AG_pct_change_first_runoff_14 = AG_net_change_first_runoff_14 / AG_final_first_round_votes_14,
    AA_pct_change_first_runoff_14 = AA_net_change_first_runoff_14 / AA_final_first_round_votes_14
    )

vr_19_to_14 <- pc_key_2019 %>% rename(`2019_IEC_district_code` = district_code) %>%
  left_join(
    dplyr::select(district_code_keyfile_2019, `2019_IEC_district_code`, `2019_matched_to_2014_IEC_district_code`)) %>%
  group_by(`2019_matched_to_2014_IEC_district_code`) %>%
  summarize(
  vr_total_final_19 = sum(vr_final_total_19, na.rm = T),
  vr_total_prelim_19 = sum(vr_prelim_total_19, na.rm = T),
  vr_total_final_18 = sum(vr_final_total_18, na.rm = T),
  vr_2019_net_change = vr_total_final_19 - vr_total_prelim_19,
  vr_2019_pct_change = vr_2019_net_change / vr_total_prelim_19, 
  vr_2018_2019_net_change = vr_total_final_19 - sum(vr_final_total_18, na.rm = T),
  vr_2018_2019_pct_change = vr_2018_2019_net_change / vr_total_final_18,
  planned_pcs_19 = length(pc_code[planned_2019 == "YES"]),
  planned_pcs_18 = length(pc_code[planned_2018 == "YES"]),
  pc_change = planned_pcs_19 - planned_pcs_18,
  vr_topup_pcs_19 = length(pc_code[vr_topup_location == "YES"]),
  prelim_pcs_18 = length(pc_code[prelim_results_2018 == "YES"]),
  final_pcs_18 = length(pc_code[final_results_2018 == "YES"])
)

# CREATE MASTER DF ------------------------------

district_data_combined <- vr_19_to_14 %>% 
  rename(`2014_IEC_district_code` = `2019_matched_to_2014_IEC_district_code`) %>% 
  left_join(consolidate_18_to_14 %>% rename(`2014_IEC_district_code` = `2019_matched_to_2014_IEC_district_code`)) %>%
  left_join(consolidate_14) %>% filter(!is.na(`2014_IEC_district_code`)) %>%
  mutate(
    prelim_turnout_18 = prelim_votes_18 / vr_total_final_18,
    final_turnout_18 = final_votes_18 / vr_total_final_18,
    provincial_capital = ifelse(str_sub(`2014_IEC_district_code`, 3, 4) == "01", "YES", "NO"),
    vr_final_19_pct_natl = vr_total_final_19 / sum(vr_total_final_19, na.rm = T),
    vr_final_18_pct_natl = vr_total_final_18 / sum(vr_total_final_18, na.rm = T),
    prelim_votes_18_pct_natl = prelim_votes_18 / sum(prelim_votes_18, na.rm = T),
    final_votes_18_pct_natl = final_votes_18 / sum(final_votes_18, na.rm = T),
    prelim_winner_votes_18_pct_natl = prelim_winner_votes_18 / sum(prelim_winner_votes_18, na.rm = T),
    final_winner_votes_18_pct_natl = final_winner_votes_18 / sum(final_winner_votes_18, na.rm = T),
    prelim_first_round_14_pct_natl = prelim_first_round_votes_14 / sum(prelim_first_round_votes_14, na.rm = T),
    final_first_round_14_pct_natl = final_first_round_votes_14 / sum(final_first_round_votes_14, na.rm = T),
    prelim_runoff_votes_14_pct_natl = prelim_runoff_votes_14 / sum(prelim_runoff_votes_14, na.rm = T),
    final_runoff_votes_14_pct_natl = final_runoff_votes_14 / sum(final_runoff_votes_14, na.rm = T),
    AG_final_first_14_pct_natl = AG_final_first_round_votes_14 / sum(AG_final_first_round_votes_14, na.rm = T),
    AG_final_runoff_14_pct_natl = AG_final_runoff_votes_14 / sum(AG_final_runoff_votes_14, na.rm = T),
    AA_final_first_14_pct_natl = AA_final_first_round_votes_14 / sum(AA_final_first_round_votes_14, na.rm = T),
    AA_final_runoff_14_pct_natl = AA_final_runoff_votes_14 / sum(AA_final_runoff_votes_14, na.rm = T),
    AG_strong_district = ifelse(AG_first_round_pct_14 >= quantile(AG_first_round_pct_14, .75, na.rm = T), "YES", "NO"),
    AA_strong_district = ifelse(AA_first_round_pct_14 >= quantile(AA_first_round_pct_14, .75, na.rm = T), "YES", "NO")
    ) %>% left_join(
      dplyr::select(district_key_2014, province_code, province_name_eng, district_code, district_name_eng) %>% 
        rename(`2014_IEC_district_code` = district_code) %>% unique()) %>%
  dplyr::select(
    province_code, province_name_eng, `2014_IEC_district_code`, district_name_eng, provincial_capital,
    planned_pcs_19, vr_topup_pcs_19, planned_pcs_18, prelim_pcs_18, final_pcs_18,
    vr_total_final_19, vr_total_prelim_19, vr_2019_net_change, vr_2019_pct_change, vr_final_19_pct_natl,
    vr_total_final_18, vr_2018_2019_net_change, vr_2018_2019_pct_change, vr_final_18_pct_natl,
    prelim_votes_18, final_votes_18, net_change_prelim_final_18, pct_change_prelim_final_18,
    prelim_turnout_18, final_turnout_18, prelim_votes_18_pct_natl, final_votes_18_pct_natl,
    prelim_winner_votes_18, prelim_winner_pct_18, final_winner_votes_18, final_winner_pct_18,
    prelim_winner_votes_18_pct_natl, final_winner_votes_18_pct_natl,
    prelim_first_round_votes_14, final_first_round_votes_14, net_change_first_round_14, pct_change_first_round_14,
    prelim_runoff_votes_14, final_runoff_votes_14, net_change_runoff_14, pct_change_runoff_14,
    prelim_first_round_14_pct_natl, final_first_round_14_pct_natl, prelim_runoff_votes_14_pct_natl, final_runoff_votes_14_pct_natl,
    net_change_final_first_runoff_14, pct_change_final_first_runoff_14,
    AG_strong_district,
    AG_final_first_round_votes_14, AG_first_round_pct_14, AG_final_runoff_votes_14, AG_runoff_pct_14,
    AG_net_change_first_runoff_14, AG_pct_change_first_runoff_14, AG_final_first_14_pct_natl, AG_final_runoff_14_pct_natl,
    AA_strong_district,
    AA_final_first_round_votes_14, AA_first_round_pct_14, AA_final_runoff_votes_14, AA_runoff_pct_14,
    AA_net_change_first_runoff_14, AA_pct_change_first_runoff_14, AA_final_first_14_pct_natl, AA_final_runoff_14_pct_natl
  )

write.csv(district_data_combined, "./district_data/district_data_analysis_14_18_19.csv", row.names = F)

AG_supporting_districts <- district_data_combined %>% filter(AG_first_round_pct_14 >= quantile(AG_first_round_pct_14, .75, na.rm = T))

ggplot(data = subset(district_data_combined,
                     vr_2018_2019_pct_change <= 1 & pct_change_prelim_final_18 <= 1
                     ),
       aes(x = pct_change_prelim_final_18, y = vr_2018_2019_pct_change,  color = provincial_capital)) +
  geom_point() +
  geom_point(alpha = 0.65)

very_dumb_model <- district_data_combined %>% filter(!is.infinite(prelim_turnout_18)) %>%
  mutate(
  proj_18_turnout_on_19_vr = (prelim_turnout_18 * vr_total_final_19),
  proj_AG_14_runoff_votes_on_18_turnout_19_vr = AG_runoff_pct_14 * proj_18_turnout_on_19_vr,
  proj_AG_pct_natl = proj_AG_14_runoff_votes_on_18_turnout_19_vr / sum(proj_AG_14_runoff_votes_on_18_turnout_19_vr, na.rm = T),
  proj_WJ_winner_votes_on_19_vr = (prelim_winner_pct_18 * vr_total_final_19),
  proj_WJ_winner_pct_natl = proj_WJ_winner_votes_on_19_vr / sum(proj_WJ_winner_votes_on_19_vr, na.rm = T)
  ) %>% 
  dplyr::select(`2014_IEC_district_code`, 
                proj_18_turnout_on_19_vr, 
                proj_AG_14_runoff_votes_on_18_turnout_19_vr, proj_AG_pct_natl,
                proj_WJ_winner_votes_on_19_vr, proj_WJ_winner_pct_natl)

