library(XML)
library(tidyverse)
library(stringr)
library(rvest)
library(RSelenium)
library(wdman)
library(seleniumPipes)
library(lubridate)
library(RCurl)
options(scipen=999)
setwd("~/Google Drive/GitHub/afghanistan_presidential_election_2019")

rm(list = ls())
all_data <- read_csv("results_data/first_round_preliminary_results/prelim_af_candidate_ps_data_2019.csv")
pc_key <- read_csv("keyfiles/pc_key_2019.csv")
ps_key <- read_csv("keyfiles/ps_key_2019.csv")
ps_status_report_list <- read_csv("./validity_checks/prelim_ps_reporting_status.csv")

#-----------------------------------------------------------------------------------
# PS SUMMARY REPORT

ps_results_long <- all_data %>% group_by(ps_code) %>%
  summarize(
    candidate_1 = sum(votes[ballot_position == "1"], na.rm = T),
    candidate_2 = sum(votes[ballot_position == "2"], na.rm = T),
    candidate_3 = sum(votes[ballot_position == "3"], na.rm = T),
#    candidate_4 = sum(votes[ballot_position == "4"], na.rm = T),
    candidate_5 = sum(votes[ballot_position == "5"], na.rm = T),
    candidate_6 = sum(votes[ballot_position == "6"], na.rm = T),
    candidate_7 = sum(votes[ballot_position == "7"], na.rm = T),
    candidate_8 = sum(votes[ballot_position == "8"], na.rm = T),
    candidate_9 = sum(votes[ballot_position == "9"], na.rm = T),
    candidate_10 = sum(votes[ballot_position == "10"], na.rm = T),
    candidate_11 = sum(votes[ballot_position == "11"], na.rm = T),
    candidate_12 = sum(votes[ballot_position == "12"], na.rm = T),
#    candidate_13 = sum(votes[ballot_position == "13"], na.rm = T),
#    candidate_14 = sum(votes[ballot_position == "14"], na.rm = T),
    candidate_15 = sum(votes[ballot_position == "15"], na.rm = T),
    candidate_16 = sum(votes[ballot_position == "16"], na.rm = T),
    candidate_17 = sum(votes[ballot_position == "17"], na.rm = T),
#    candidate_18 = sum(votes[ballot_position == "18"], na.rm = T),
    total_votes = sum(votes, na.rm = T)
  )

ps_results_long_out <- ps_status_report_list %>% left_join(ps_results_long) %>%
  mutate(pre_post_audit_difference = total_votes - pre_audit_bvv_total,
         missing_bvv_reporting = ifelse(total_votes > 0 & (commission_decision == "Decision 105 Paragraph 1" | 
                                                             commission_decision == "Decision 105 Paragraph 2"), "YES", "NO"),
         reporting_zero_votes = ifelse(total_votes == 0, "YES", "NO"),
         added_votes = ifelse(pre_post_audit_difference > 0, "YES", "NO")
  ) %>% left_join(dplyr::select(pc_key, pc_code, province_code, province_name_eng, district_sub_code, district_or_subdivision_name_eng, provincial_capital)) %>%
  left_join(dplyr::select(ps_key, ps_code, vr_final_total_19)) %>%
  rename(total_vr = vr_final_total_19) %>%
  mutate(turnout = total_votes / total_vr,
         excess_turnout = ifelse(turnout > 1, "YES", "NO")) %>%
  dplyr::select(province_code, province_name_eng, district_sub_code, district_or_subdivision_name_eng, provincial_capital,
                pc_code, ps_code, ps_type, total_vr, prelim_results_reporting, audit_or_recount, commission_decision, missing_bvv_reporting,
                reporting_zero_votes, added_votes, excess_turnout, pre_audit_bvv_total, pre_post_audit_difference, total_votes, everything())

ps_results_long_out$added_votes[is.na(ps_results_long_out$added_votes)] <- "UNKNOWN"
# ps_results_long_out$excess_turnout[is.na(ps_results_long_out$excess_turnout)] <- "NO"

write.csv(ps_results_long_out, "./analysis/ps_summary_report.csv", row.names = F)

province_rollup <- ps_results_long_out %>% group_by(province_code, province_name_eng) %>%
  summarize(
    ps_planned = length(ps_code),
    ps_reporting = length(ps_code[prelim_results_reporting == "YES"]),
    ps_audited = length(ps_code[audit_or_recount == "YES"]),
    missing_bvv_ps_reporting = length(ps_code[missing_bvv_reporting == "YES"]),
    missing_bvv_new_votes = sum(total_votes[missing_bvv_reporting == "YES"], na.rm = T),
    ps_reporting_zero_votes = length(ps_code[reporting_zero_votes == "YES" & prelim_results_reporting == "YES"]),
    ps_reporting_excess_turnout = length(ps_code[excess_turnout == "YES" & prelim_results_reporting == "YES"]),
    ps_added_votes_in_audit = length(ps_code[added_votes == "YES"]),
    known_bvv_votes_under_audit = sum(pre_audit_bvv_total, na.rm = T),
    known_net_change_from_audit = sum(pre_post_audit_difference, na.rm = T),
    total_vr = sum(total_vr, na.rm = T),
    total_preliminary_votes = sum(total_votes, na.rm = T),
    turnout = total_preliminary_votes / total_vr,
    candidate_1 = sum(candidate_1, na.rm = T),
    candidate_2 = sum(candidate_2, na.rm = T),
    candidate_3 = sum(candidate_3, na.rm = T),
    candidate_5 = sum(candidate_5, na.rm = T),
    candidate_6 = sum(candidate_6, na.rm = T),
    candidate_7 = sum(candidate_7, na.rm = T),
    candidate_8 = sum(candidate_8, na.rm = T),
    candidate_9 = sum(candidate_9, na.rm = T),
    candidate_10 = sum(candidate_10, na.rm = T),
    candidate_11 = sum(candidate_11, na.rm = T),
    candidate_12 = sum(candidate_12, na.rm = T),
    candidate_15 = sum(candidate_12, na.rm = T),
    candidate_16 = sum(candidate_16, na.rm = T),
    candidate_17 = sum(candidate_17, na.rm = T)
  )

natl_total <- ps_results_long_out %>% 
  mutate(province_code = "NATIONAL TOTAL",
         province_name_eng = "NATIONAL TOTAL") %>%
  group_by(province_code, province_name_eng) %>%
  summarize(
    ps_planned = length(ps_code),
    ps_reporting = length(ps_code[prelim_results_reporting == "YES"]),
    ps_audited = length(ps_code[audit_or_recount == "YES"]),
    missing_bvv_ps_reporting = length(ps_code[missing_bvv_reporting == "YES"]),
    missing_bvv_new_votes = sum(total_votes[missing_bvv_reporting == "YES"], na.rm = T),
    ps_reporting_zero_votes = length(ps_code[reporting_zero_votes == "YES" & prelim_results_reporting == "YES"]),
    ps_reporting_excess_turnout = length(ps_code[excess_turnout == "YES" & prelim_results_reporting == "YES"]),
    ps_added_votes_in_audit = length(ps_code[added_votes == "YES"]),
    known_bvv_votes_under_audit = sum(pre_audit_bvv_total, na.rm = T),
    known_net_change_from_audit = sum(pre_post_audit_difference, na.rm = T),
    total_vr = sum(total_vr, na.rm = T),
    total_preliminary_votes = sum(total_votes, na.rm = T),
    turnout = total_preliminary_votes / total_vr,
    candidate_1 = sum(candidate_1, na.rm = T),
    candidate_2 = sum(candidate_2, na.rm = T),
    candidate_3 = sum(candidate_3, na.rm = T),
    candidate_5 = sum(candidate_5, na.rm = T),
    candidate_6 = sum(candidate_6, na.rm = T),
    candidate_7 = sum(candidate_7, na.rm = T),
    candidate_8 = sum(candidate_8, na.rm = T),
    candidate_9 = sum(candidate_9, na.rm = T),
    candidate_10 = sum(candidate_10, na.rm = T),
    candidate_11 = sum(candidate_11, na.rm = T),
    candidate_12 = sum(candidate_12, na.rm = T),
    candidate_15 = sum(candidate_12, na.rm = T),
    candidate_16 = sum(candidate_16, na.rm = T),
    candidate_17 = sum(candidate_17, na.rm = T)
  )
  
preliminary_results_prov <- read_csv("raw/preliminary_results_prov.csv")
preliminary_results_prov$province_code <- str_pad(preliminary_results_prov$province_code, side = "left", pad = "0", width = 2)

province_rollup <- province_rollup %>% 
  full_join(natl_total) %>%
  left_join(dplyr::select(preliminary_results_prov, province_code, pre_audit_not_deduplicated_bvv_vote_per_AAN, 
                          pre_audit_deduplicated_bvv_vote_total_per_AAN)) %>%
  rename(
    pre_deduplication_bvv_vote_total = pre_audit_not_deduplicated_bvv_vote_per_AAN,
    post_deduplication_bvv_vote_total = pre_audit_deduplicated_bvv_vote_total_per_AAN
  ) %>% mutate(
    other_change_pre_dedupe_post_audit = total_preliminary_votes - (missing_bvv_new_votes + pre_deduplication_bvv_vote_total + known_net_change_from_audit),
    other_change_post_dedupe_post_audit = total_preliminary_votes - (missing_bvv_new_votes + post_deduplication_bvv_vote_total + known_net_change_from_audit),
    turnout = total_preliminary_votes / total_vr) %>% 
  dplyr::select(
    province_code, province_name_eng, ps_planned, total_vr, ps_reporting, ps_audited, missing_bvv_ps_reporting, ps_reporting_zero_votes, 
    ps_reporting_excess_turnout, ps_added_votes_in_audit,
    pre_deduplication_bvv_vote_total, post_deduplication_bvv_vote_total, known_bvv_votes_under_audit, known_net_change_from_audit,
    missing_bvv_new_votes, other_change_pre_dedupe_post_audit, other_change_post_dedupe_post_audit,
    total_preliminary_votes, turnout, everything()
  )

write.csv(province_rollup, "./analysis/provincial_summary_report.csv", row.names = F)

district_rollup <- ps_results_long_out %>% 
  group_by(province_code, province_name_eng, district_sub_code, district_or_subdivision_name_eng, provincial_capital) %>%
  summarize(
    ps_planned = length(ps_code),
    ps_reporting = length(ps_code[prelim_results_reporting == "YES"]),
    ps_audited = length(ps_code[audit_or_recount == "YES"]),
    missing_bvv_ps_reporting = length(ps_code[missing_bvv_reporting == "YES"]),
    missing_bvv_new_votes = sum(total_votes[missing_bvv_reporting == "YES"], na.rm = T),
    ps_reporting_zero_votes = length(ps_code[reporting_zero_votes == "YES" & prelim_results_reporting == "YES"]),
    ps_reporting_excess_turnout = length(ps_code[excess_turnout == "YES" & prelim_results_reporting == "YES"]),
    ps_added_votes_in_audit = length(ps_code[added_votes == "YES"]),
    known_bvv_votes_under_audit = sum(pre_audit_bvv_total, na.rm = T),
    known_net_change_from_audit = sum(pre_post_audit_difference, na.rm = T),
    total_vr = sum(total_vr, na.rm = T),
    total_preliminary_votes = sum(total_votes, na.rm = T),
    turnout = total_preliminary_votes / total_vr,
    candidate_1 = sum(candidate_1, na.rm = T),
    candidate_2 = sum(candidate_2, na.rm = T),
    candidate_3 = sum(candidate_3, na.rm = T),
    candidate_5 = sum(candidate_5, na.rm = T),
    candidate_6 = sum(candidate_6, na.rm = T),
    candidate_7 = sum(candidate_7, na.rm = T),
    candidate_8 = sum(candidate_8, na.rm = T),
    candidate_9 = sum(candidate_9, na.rm = T),
    candidate_10 = sum(candidate_10, na.rm = T),
    candidate_11 = sum(candidate_11, na.rm = T),
    candidate_12 = sum(candidate_12, na.rm = T),
    candidate_15 = sum(candidate_15, na.rm = T),
    candidate_16 = sum(candidate_16, na.rm = T),
    candidate_17 = sum(candidate_17, na.rm = T)
  )
  
write.csv(district_rollup, "./analysis/district_summary_report.csv", row.names = F)


#-----------------------------------------------------------------------------------
# 2019 DATA 

rm(list = ls())
province_key <- read_csv("keyfiles/province_key_2019.csv")
ps_key <- read_csv("keyfiles/ps_key_2019.csv")
pc_key <- read_csv("keyfiles/pc_key_2019.csv")
ps_audit_target_list <- read_csv("audit_data/ps_audit_target_list.csv")
district_code_keyfile_2019 <- read_csv("district_data/district_code_keyfile_2019.csv")
ps_results <- read_csv("results_data/first_round_preliminary_results/prelim_af_candidate_ps_data_2019.csv")
ps_summary_report <- read_csv("analysis/ps_summary_report.csv")
district_summary_report <- read_csv("analysis/district_summary_report.csv")
provincial_summary_report <- read_csv("analysis/provincial_summary_report.csv")


#-----------------------------------------------------------------------------------

ps_summary_report <- ps_summary_report %>% mutate(
  ghani_pct = candidate_8 / total_votes,
  abdullah_pct = candidate_9 / total_votes,
  other_pct = (total_votes - (candidate_8 + candidate_9)) / total_votes
) %>% left_join(dplyr::select(pc_key, pc_code, provincial_capital))

provincial_summary_report <- provincial_summary_report %>% mutate(
  ghani_pct = candidate_8 / total_preliminary_votes,
  abdullah_pct = candidate_9 / total_preliminary_votes,
  other_pct = (total_preliminary_votes - (candidate_8 + candidate_9)) / total_preliminary_votes
)

district_summary_report <- district_summary_report %>% mutate(
  ghani_pct = candidate_8 / total_preliminary_votes,
  abdullah_pct = candidate_9 / total_preliminary_votes,
  other_pct = (total_preliminary_votes - (candidate_8 + candidate_9)) / total_preliminary_votes,
  vr_pct_natl = total_vr / 9665745,
  votes_pct_natl = total_preliminary_votes / 1824401)

#district_summary_report$UN_OCHA_region_code[district_summary_report$UN_OCHA_region_code == "Kabul"] <- "Central / Kabul"
#district_summary_report$UN_OCHA_region_code[district_summary_report$UN_OCHA_region_code == "Capital"] <- "Central / Kabul"

ghani_med <- median(district_summary_report$ghani_pct, na.rm = T)
abdullah_med <- median(district_summary_report$abdullah_pct, na.rm = T)

ggplot(data = district_summary_report,
       aes(x = ghani_pct, y = abdullah_pct, size = votes_pct_natl, color = provincial_capital)) +
  coord_equal() +
  geom_vline(xintercept = ghani_med, size = 1, color = "black", alpha = .6) +
  geom_hline(yintercept = abdullah_med, size = 1, color = "black", alpha = .6) +
  geom_point(alpha = .65) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1), breaks=seq(0, 1, by=0.05)) +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1), breaks=seq(0, 1, by=0.05)) +
  scale_color_manual(values = c("black", "blue")) +
  labs(x = "Ghani Share of Preliminary Vote", 
       y = "Abdullah Share of Preliminary Vote",
       title = "District Vote Share for Top Two Candidates\nin the 2019 Afghan Presidential Elections",
       subtitle = "Points are districts. X- and Y-intercept lines are national median.",
       caption = "Author: Colin Cookman (Twitter: @colincookman / Email: ccookman@gmail.com or ccookman@usip.org)\nData Sources: https://github.com/colincookman/afghanistan_presidential_election_2019/",
       color = "Provincial capital district",
       size = "Total district votes as percent of total national votes"
   ) +
   theme(
     plot.caption = element_text(hjust = 0),
     legend.position = "bottom"
   )


## 
out <- lm(formula = ghani_pct ~ turnout + audit_or_recount + missing_bvv_reporting + ps_type + excess_turnout + province_name_eng,
          data = subset(ps_summary_report, prelim_results_reporting == "YES"))
summary(out)

out <- lm(formula = audit_or_recount ~ ps_type,
          data = subset(ps_summary_report, prelim_results_reporting == "YES"))
summary(out)

##

ggplot(data = district_summary_report,
       aes(x = abdullah_pct, y = ghani_pct, size = total_preliminary_votes, color = UN_OCHA_region_code)
       ) + 
  geom_point(alpha = .7) +
  scale_color_brewer(palette = "Set1")




# past elections --------------------------------------------------------------
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