library(tidyverse)
library(lubridate)
library(ggrepel)
options(scipen=999)
rm(list = ls())

# IMPORT DATA -----------------

prelim_2019 <- read_csv("results_data/first_round_preliminary_results/prelim_af_candidate_ps_data_2019.csv")
prelim_2019 <- prelim_2019 %>% rename(winner = prelim_winner)
prelim_2019$election_date <- mdy("09-28-2019")

final_2019 <- read_csv("results_data/first_round_final_results/final_af_candidate_ps_data_2019.csv")
final_2019$election_date <- mdy("09-28-2019")

all_data_2019 <- full_join(final_2019, prelim_2019)

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
all_data_2018$results_status <- gsub("Results Status : Uncertified", "PRELIMINARY", all_data_2018$results_status)
all_data_2018$results_status <- gsub("Results Status : Certified", "FINAL", all_data_2018$results_status)
all_data_2018$election_type <- "PARLIAMENTARY"
all_data_2018$election_date <- mdy("10-20-2018")

pc_key_2019 <- read_csv("keyfiles/pc_key_2019.csv")
pc_key_2018 <- read_csv("~/Google Drive/GitHub/afghanistan_election_results_2018/keyfiles/pc_key.csv")

district_key_2014 <- read_csv("past_elections/presidential_2014/keyfiles/district_key_2014.csv")
district_code_keyfile_2019 <- read_csv("district_data/district_code_keyfile_2019.csv")

cso_district_population_estimates_2004_2020 <- read_csv("~/Google Drive/GitHub/afghanistan_district_population_data/cso_district_population_estimates_2004-2020.csv")
cso_codes_key <- read_csv("~/Google Drive/GitHub/afghanistan_district_population_data/cso_codes_key.csv")

# COLLAPSE DATA -------------

merge_18_19 <- all_data_2018 %>% 
  dplyr::select(election_type, results_status, election_date, 
                province_code, district_code, pc_code, 
                candidate_code, ballot_position, candidate_gender, votes, winner) %>%
  full_join(
    dplyr::select(all_data_2019,
                  election_type, results_status, election_date,
                  province_code, district_code, pc_code,
                  candidate_code, ballot_position, candidate_gender, votes, winner)
  ) %>%
  arrange(election_date, election_type, province_code, pc_code) %>%
  rename(`2018_IEC_district_code` = district_code) %>%
  left_join(dplyr::select(
    district_code_keyfile_2019, `2018_IEC_district_code`, `2019_matched_to_2014_IEC_district_code` 
  ))


collapse_18_19_to_14 <- merge_18_19 %>% 
  group_by(
    election_type, results_status, election_date, province_code, `2019_matched_to_2014_IEC_district_code`,
    candidate_code, ballot_position, candidate_gender, winner) %>%
  summarize(votes = sum(votes, na.rm = T))

merge_18_19_to_14 <- all_data_2014 %>%
  rename(`2019_matched_to_2014_IEC_district_code` = district_code) %>%
  group_by(
    election_type, results_status, election_date, province_code, `2019_matched_to_2014_IEC_district_code`,
    candidate_code, ballot_position, candidate_gender) %>%
  summarize(votes = sum(votes, na.rm = T)) %>%
  full_join(collapse_18_19_to_14) %>%
  arrange(
    election_date, election_type, province_code, `2019_matched_to_2014_IEC_district_code`, ballot_position
  )

merge_18_19_to_14$winner[is.na(merge_18_19_to_14$winner)] <- "NO"
merge_18_19_to_14$winner[merge_18_19_to_14$election_type == "PRESIDENTIAL RUNOFF" & merge_18_19_to_14$candidate_code == "101_1_13"] <- "YES"
merge_18_19_to_14$winner[merge_18_19_to_14$election_type == "PRESIDENTIAL" & merge_18_19_to_14$election_date == "2014-06-14" & merge_18_19_to_14$candidate_code == "100_1_1"] <- "YES"

pop_18_19 <- cso_district_population_estimates_2004_2020 %>% filter(
  gregorian_year == "2018-2019" | gregorian_year == "2019-2020") %>%
  dplyr::select(solar_year, gregorian_year, province_code, district_code, 7:17) %>%
  rename(`2018_IEC_district_code` = district_code) %>%
  left_join(dplyr::select(
    district_code_keyfile_2019, `2018_IEC_district_code`, `2019_matched_to_2014_IEC_district_code` 
  )) %>% group_by(solar_year, gregorian_year, province_code, units, `2019_matched_to_2014_IEC_district_code`) %>%
  summarize(rural_fem = sum(rural_fem, na.rm = T),
            rural_male = sum(rural_male, na.rm = T),
            rural_total = sum(rural_total, na.rm = T),
            urban_fem = sum(urban_fem, na.rm = T),
            urban_male = sum(urban_male, na.rm = T),
            urban_total = sum(urban_total, na.rm = T),
            fem_population = sum(fem_population, na.rm = T),
            male_population = sum(male_population, na.rm = T),
            total_population = sum(total_population, na.rm = T)
  )

pop_14 <- cso_district_population_estimates_2004_2020 %>% filter(gregorian_year == "2014-2015") %>%
  dplyr::select(solar_year, gregorian_year, province_code, district_code, 7:17) %>%
  rename(`2019_matched_to_2014_IEC_district_code` = district_code) %>% 
  group_by(solar_year, gregorian_year, province_code, units, `2019_matched_to_2014_IEC_district_code`) %>%
  summarize(rural_fem = sum(rural_fem, na.rm = T),
            rural_male = sum(rural_male, na.rm = T),
            rural_total = sum(rural_total, na.rm = T),
            urban_fem = sum(urban_fem, na.rm = T),
            urban_male = sum(urban_male, na.rm = T),
            urban_total = sum(urban_total, na.rm = T),
            fem_population = sum(fem_population, na.rm = T),
            male_population = sum(male_population, na.rm = T),
            total_population = sum(total_population, na.rm = T)
  )

pop_14_18_19 <- pop_14 %>% full_join(pop_18_19) %>%
  arrange(solar_year, `2019_matched_to_2014_IEC_district_code`)

pop_long <- pop_14_18_19 %>% 
  group_by(`2019_matched_to_2014_IEC_district_code`) %>%
  summarize(total_pop_14 = sum(total_population[solar_year == "1393"]),
            total_pop_18 = sum(total_population[solar_year == "1397"]),
            total_pop_19 = sum(total_population[solar_year == "1398"]),
            urban_pop_14 = sum(urban_total[solar_year == "1393"]),
            urban_pop_18 = sum(urban_total[solar_year == "1397"]),
            urban_pop_19 = sum(urban_total[solar_year == "1398"]),
            rural_pop_14 = sum(rural_total[solar_year == "1393"]),
            rural_pop_18 = sum(rural_total[solar_year == "1397"]),
            rural_pop_19 = sum(rural_total[solar_year == "1398"]),
            male_pop_14 = sum(male_population[solar_year == "1393"]),
            male_pop_18 = sum(male_population[solar_year == "1397"]),
            male_pop_19 = sum(male_population[solar_year == "1398"]),
            fem_pop_14 = sum(fem_population[solar_year == "1393"]),
            fem_pop_18 = sum(fem_population[solar_year == "1397"]),
            fem_pop_19 = sum(fem_population[solar_year == "1398"])
  )

# SUMMARIZE DATA  

district_summary_report <- merge_18_19_to_14 %>%
  group_by(`2019_matched_to_2014_IEC_district_code`) %>%
  summarize(
    total_R114 = sum(votes[results_status == "PRELIMINARY" & election_date == "2014-04-05"], na.rm = T),
    aa_R114 = sum(votes[results_status == "PRELIMINARY" & election_date == "2014-04-05" & candidate_code == "100_1_1"], na.rm = T),
    aa_pct_R114 = aa_R114 / total_R114,
    ag_R114 = sum(votes[results_status == "PRELIMINARY" & election_date == "2014-04-05" & candidate_code == "101_1_13"], na.rm = T),
    ag_pct_R114 = ag_R114 / total_R114,
    other_R114 = total_R114 - (aa_R114 + ag_R114),
    other_pct_R114 = other_R114 / total_R114,
    total_R214 = sum(votes[results_status == "PRELIMINARY" & election_date == "2014-06-14"], na.rm = T),
    aa_R214 = sum(votes[results_status == "PRELIMINARY" & election_date == "2014-06-14" & candidate_code == "100_1_1"], na.rm = T),
    aa_pct_R214 = aa_R214 / total_R214,
    ag_R214 = sum(votes[results_status == "PRELIMINARY" & election_date == "2014-06-14" & candidate_code == "101_1_13"], na.rm = T),
    ag_pct_R214 = ag_R214 / total_R214,
    aa_net_change_14 = aa_R214 - aa_R114,
    aa_pct_change_14 = (aa_R214 - aa_R114) / aa_R114,
    ag_net_change_14 = ag_R214 - ag_R114,
    aa_pct_change_14 = (ag_R214 - ag_R114) / ag_R114,
    total_19 = sum(votes[results_status == "PRELIMINARY" & election_date == "2019-09-28"], na.rm = T),
    aa_19 = sum(votes[results_status == "PRELIMINARY" & election_date == "2019-09-28" & candidate_code == "1052-1-14"], na.rm = T),
    aa_pct_19 = aa_19 / total_19,
    ag_19 = sum(votes[results_status == "PRELIMINARY" & election_date == "2019-09-28" & candidate_code == "1055-5-15"], na.rm = T),
    ag_pct_19 = ag_19 / total_19,
    other_19 = total_19 - (aa_19 + ag_19),
    other_pct_19 = other_19 / total_19,
    pct_natl_19 = total_19 / 1824401,
    aa_net_R114_19 = aa_19 - aa_R114,
    aa_pct_R114_19 = (aa_19 - aa_R114) / aa_R114,
    ag_net_R114_19 = ag_19 - ag_R114,
    ag_pct_R114_19 = (ag_19 - ag_R114) / ag_R114,
    other_net_R114_19 = other_19 - other_R114,
    other_pct_R114_19 = (other_19 - other_R114) / other_R114,
    aa_net_R214_19 = aa_19 - aa_R214,
    aa_pct_R214_19 = (aa_19 - aa_R214) / aa_R214,
    ag_net_R214_19 = ag_19 - ag_R214,
    ag_pct_R214_19 = (ag_19 - ag_R214) / ag_R214,
    total_18 = sum(votes[results_status == "PRELIMINARY" & election_date == "2018-10-20"], na.rm = T),
    winners_18 = sum(votes[results_status == "PRELIMINARY" & election_date == "2018-10-20" & winner == "YES"], na.rm = T),
    winners_pct_18 = winners_18 / total_18,
    other_18 = total_18 - winners_18,
    other_pct_18 = other_18 / total_18
  ) %>% rename(
    district_code = `2019_matched_to_2014_IEC_district_code`
  ) %>% 
  left_join(pop_long %>% 
              rename(district_code = `2019_matched_to_2014_IEC_district_code`)
            ) %>% 
  left_join(
    dplyr::select(
      district_key_2014, district_code, province_code, province_name_eng, district_name_eng, provincial_capital
    ) %>% unique()
  ) %>% 
  dplyr::select(province_code, province_name_eng, district_code, district_name_eng, provincial_capital, everything())

district_summary_report$province_code[is.na(district_summary_report$province_code)] <- "30"
district_summary_report$province_name_eng[is.na(district_summary_report$province_name_eng)] <- "HELMAND"
district_summary_report$district_name_eng[is.na(district_summary_report$district_name_eng)] <- "NAWAMISH"
district_summary_report$provincial_capital[is.na(district_summary_report$provincial_capital)] <- "NO"

district_summary_report <- district_summary_report %>%
  mutate(no_votes_R114 = ifelse(total_R114 == 0, "YES", "NO"),
         no_votes_R214 = ifelse(total_R214 == 0, "YES", "NO"),
         no_votes_18 = ifelse(total_18 == 0, "YES", "NO"),
         no_votes_19 = ifelse(total_19 == 0, "YES", "NO")
  )


ag_q1_R214 <- quantile(district_summary_report$ag_pct_R214, .25, na.rm = T)
ag_q4_R214 <- quantile(district_summary_report$ag_pct_R214, .65, na.rm = T)
ag_q1_19 <- quantile(district_summary_report$ag_pct_19, .25, na.rm = T)
ag_q4_19 <- quantile(district_summary_report$ag_pct_19, .65, na.rm = T)

ghani_district_change <- ggplot(data = subset(district_summary_report,
                                              !is.na(province_code)),
       aes(x = ag_pct_R214, y = ag_pct_19, size = pct_natl_19, color = provincial_capital)) +
  coord_equal() +
  geom_abline(slope = 1, intercept = 0, size = 1, color="gray80") +
  geom_vline(xintercept = .5, size = 1, color = "gray80") +
  geom_hline(yintercept = .5, size = 1, color = "gray80") +
  scale_color_brewer(palette = "Set1") +
  geom_point(alpha = .75) +
  geom_text_repel(data = subset(district_summary_report,
                                !is.na(province_code) &
                                ((ag_pct_R214 >= ag_q4_R214) & (ag_pct_19 <= ag_q1_19)) |
                                ((ag_pct_R214 <= ag_q1_R214) & (ag_pct_19 >= ag_q4_19))
                                ),
                  mapping = aes(label = paste0(district_name_eng, " - ", province_name_eng)),
                  size = 3, 
                  box.padding = unit(0.55, "lines")
  ) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1), breaks=seq(0, 1, by=0.05)) +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1), breaks=seq(0, 1, by=0.05)) +
  labs(x = "Ghani Percent of Preliminary Valid Vote in Second Round 2014", 
       y = "Ghani Percent of Preliminary Valid Vote in First Round 2019",
       title = "Ghani Vote Share Changes by District 2014 - 2019",
       subtitle = "Points are districts; 2019 data has been reaggregated to match 2014 district boundaries.\nPoints below the diagonal line are a loss in vote share in 2019 compared to 2014,\npoints above are an increase in vote share.\nCandidate received 50%+ support in both elections in points in the upper right quadrant,\nless than 50% in both elections in lower left quadrant.",
       caption = "Author: Colin Cookman (Twitter: @colincookman / Email: ccookman@gmail.com or ccookman@usip.org)\nData Sources: https://github.com/colincookman/afghanistan_presidential_election_2019/",
       size = "District votes as % of 2019 national preliminary valid vote",
       color = "Provincial capital district"
   ) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    plot.caption = element_text(hjust = 0)
  )

ggsave("./graphics/ghani_2014_to_2019_districts_plot.png", plot = ghani_district_change, dpi = 300, height = 215.9, width = 279.4, units = "mm")

aa_q1_R214 <- quantile(district_summary_report$aa_pct_R214, .25, na.rm = T)
aa_q4_R214 <- quantile(district_summary_report$aa_pct_R214, .65, na.rm = T)
aa_q1_19 <- quantile(district_summary_report$aa_pct_19, .25, na.rm = T)
aa_q4_19 <- quantile(district_summary_report$aa_pct_19, .65, na.rm = T)

abdullah_district_change <- ggplot(data = subset(district_summary_report,
                                              !is.na(province_code)),
       aes(x = aa_pct_R214, y = aa_pct_19, size = pct_natl_19, color = provincial_capital)) +
  coord_equal() +
  geom_abline(slope = 1, intercept = 0, size = 1, color="gray80") +
  geom_vline(xintercept = .5, size = 1, color = "gray80") +
  geom_hline(yintercept = .5, size = 1, color = "gray80") +  
  scale_color_brewer(palette = "Set1") +
  geom_point(alpha = .75) +
  geom_text_repel(data = subset(district_summary_report,
                                !is.na(province_code) &
                                ((aa_pct_R214 >= aa_q4_R214) & (aa_pct_19 <= aa_q1_19)) |
                                ((aa_pct_R214 <= aa_q1_R214) & (aa_pct_19 >= aa_q4_19))
                                ),
                  mapping = aes(label = paste0(district_name_eng, " - ", province_name_eng)),
                  size = 3, 
                  box.padding = unit(0.55, "lines")
  ) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1), breaks=seq(0, 1, by=0.05)) +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1), breaks=seq(0, 1, by=0.05)) +
  labs(x = "Abdullah Percent of Preliminary Valid Vote in Second Round 2014", 
       y = "Abdullah Percent of Preliminary Valid Vote in First Round 2019",
       title = "Abdullah Vote Share Changes by District 2014 - 2019",
       subtitle = "Points are districts; 2019 data has been reaggregated to match 2014 district boundaries.\nPoints below the diagonal line are a loss in vote share in 2019 compared to 2014,\npoints above are an increase in vote share.\nCandidate received 50%+ support in both elections in points in the upper right quadrant,\nless than 50% in both elections in lower left quadrant.",
       caption = "Author: Colin Cookman (Twitter: @colincookman / Email: ccookman@gmail.com or ccookman@usip.org)\nData Sources: https://github.com/colincookman/afghanistan_presidential_election_2019/",
       size = "District votes as % of 2019 national preliminary valid vote",
       color = "Provincial capital district"
   ) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    plot.caption = element_text(hjust = 0)
  )

ggsave("./graphics/abdullah_2014_to_2019_districts_plot.png", plot = abdullah_district_change, dpi = 300, height = 215.9, width = 279.4, units = "mm")

province_vr <- pc_key_2019 %>% group_by(province_code) %>% 
  summarize(
    vr_19 = sum(vr_final_total_19, na.rm = T),
    vr_18 = sum(vr_final_total_18, na.rm = T)
  )

province_turnout_18_19 <- merge_18_19 %>%
  group_by(province_code) %>%
  summarize(
    total_18 = sum(votes[results_status == "PRELIMINARY" & election_type == "PARLIAMENTARY"], na.rm = T),
    total_19 = sum(votes[results_status == "PRELIMINARY" & election_type == "PRESIDENTIAL"], na.rm = T)) %>%
  left_join(province_vr) %>%
  mutate(
    turnout_18 = total_18 / vr_18,
    turnout_19 = total_19 / vr_19
  ) %>%
  left_join(dplyr::select(province_key_2019,
            province_code, province_name_eng, UN_OCHA_region_code)
  ) %>% mutate(
    pct_natl = total_19 / 1824401
  )

natl_med_turnout_18 <- median(province_turnout_18_19$turnout_18[!is.na(province_turnout_18_19$turnout_18)])
natl_med_turnout_19 <- median(province_turnout_18_19$turnout_19)

turnout_comparison_plot <- ggplot(data = province_turnout_18_19, 
       aes(x = turnout_18, y = turnout_19, color = UN_OCHA_region_code, size = pct_natl)) +
  coord_fixed() +
  geom_hline(yintercept = natl_med_turnout_19, size = 1, color = "gray80") +
  geom_vline(xintercept = natl_med_turnout_18, size = 1, color = "gray80") +
  geom_abline(slope = 1, intercept = 0, size = 1, color="gray80") +
  geom_point() +
#  geom_smooth(method = "lm") +
  scale_x_continuous(limits = c(0, .9), labels = scales::percent_format(accuracy = 1), breaks=seq(0, .9, by=0.05)) +
  scale_y_continuous(limits = c(0, .9), labels = scales::percent_format(accuracy = 1), breaks=seq(0, .9, by=0.05)) +
  geom_text_repel(data = province_turnout_18_19, 
                  mapping = aes(label = province_name_eng), size = 3, box.padding = unit(0.55, "lines")) +
  labs(y = "2019 Votes Against Voter Registration (Preliminary Results)", 
       x = "2018 Votes Against Voter Registration (Preliminary Results)",
       title = "Provincial-Level Turnout Comparisons in Afghanistan Elections",
       subtitle = str_wrap("Diagonal line represents hypothetical perfect correlation; all points below this line represent a relative drop in turnout between 2018 parliamentary and 2019 presidential elections.\n X- and Y-intercept lines are 2019 national medians."),
       caption = "Author: Colin Cookman (Twitter: @colincookman / Email: ccookman@gmail.com or ccookman@usip.org)\nData Sources:\n2018 parliamentary data: https://github.com/colincookman/afghanistan_election_results_2018\n2019 provisional data: https://github.com/colincookman/afghanistan_presidential_election_2019/",
       color = "UNAMA Region Code",
       size = "Share of total 2019 preliminary vote"
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    plot.caption = element_text(hjust = 0)
  )

turnout_comparison_plot

ggsave("./graphics/turnout_18_19_comparison_plot.png", plot = turnout_comparison_plot, dpi = 300, height = 215.9, width = 279.4, units = "mm")



