library(tidyverse)
library(lubridate)
library(ggrepel)
library(mapproj)
library(geosphere)
library(rgdal)
library(viridis)
library(finalfit)
options(scipen=999)
setwd("~/Google Drive/GitHub/afghanistan_presidential_election_2019")
rm(list = ls())

# -----------------------------------------------------------------------------
# PC data

pc_key_2014 <- read_csv("past_elections/presidential_2014/keyfiles/pc_key_2014.csv")
pc_key_2018 <- read_csv("~/Google Drive/GitHub/afghanistan_election_results_2018/keyfiles/pc_key.csv")
pc_key_2019 <- read_csv("keyfiles/pc_key_2019.csv")
ps_key_2019 <- read_csv("keyfiles/ps_key_2019.csv")
district_code_keyfile_2019 <- read_csv("district_data/district_code_keyfile_2019.csv")
district_key_2014 <- read_csv("past_elections/presidential_2014/keyfiles/district_key_2014.csv")
district_key_2019 <- read_csv("keyfiles/district_key_2019.csv")
province_key_2019 <- read_csv("keyfiles/province_key_2019.csv")

pc_gis_2019 <- read_csv("~/Google Drive/Afghan Elections Research/2018 Parliamentary/PC Data/pc_gis.csv")
pc_code_matching_2014_2018 <- read_csv("pc_plan/pc_code_matching_2014_2018.csv")
nearest_replacement_center_2014_2018 <- read_csv("pc_plan/nearest_replacement_center_2014_2018.csv")
cso_district_population_estimates_2004_2020 <- read_csv("~/Google Drive/GitHub/afghanistan_district_population_data/cso_district_population_estimates_2004-2020.csv")

# -----------------------------------------------------------------------------
# results data

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

candidate_key_2018 <- read_csv("~/Google Drive/GitHub/afghanistan_election_results_2018/keyfiles/candidate_key.csv")

pc_results_2014 <- final_runoff_2014 %>%
  group_by(pc_code) %>%
  summarize(aa_valid_votes_14 = sum(votes[ballot_position == 1 & post_audit_status != "Invalidated"]),
            ag_valid_votes_14 = sum(votes[ballot_position == 5 & post_audit_status != "Invalidated"]),
            total_valid_votes_14 = sum(votes[post_audit_status != "Invalidated"]),
            aa_pct_14 = aa_valid_votes_14 / total_valid_votes_14,
            ag_pct_14 = ag_valid_votes_14 / total_valid_votes_14
            ) %>%
  rename(pc_code_14 = pc_code)

pc_results_2018 <- prelim_wj_2018 %>%
  left_join(dplyr::select(candidate_key_2018, candidate_code, final_winner)) %>%
  group_by(pc_code) %>%
  summarize(total_prelim_votes_18 = sum(votes),
            winner_votes_18 = sum(votes[final_winner == "YES"]),
            win_pct_18 = winner_votes_18 / total_prelim_votes_18,
            lose_pct_18 = (1 - win_pct_18)
            ) %>%
  rename(pc_code_18 = pc_code)

pc_results_2019 <- final_2019 %>%
  group_by(province_name_eng, pc_code) %>%
  summarize(aa_valid_votes_19 = sum(votes[ballot_position == 9]),
            ag_valid_votes_19 = sum(votes[ballot_position == 8]),
            other_valid_votes_19 = sum(votes) - (aa_valid_votes_19 + ag_valid_votes_19),
            total_valid_votes_19 = sum(votes),
            aa_pct_19 = aa_valid_votes_19 / total_valid_votes_19,
            ag_pct_19 = ag_valid_votes_19 / total_valid_votes_19,
            other_pct_19 = other_valid_votes_19 / total_valid_votes_19
            ) %>%
  rename(pc_code_18 = pc_code)

# -----------------------------------------------------------------------------

# pc_code_matches_update <- pc_code_matching_2014_2018 %>%
#  rowwise %>% 
#  mutate(matched = ifelse((matched_by != "NO MATCH / DROPPED FROM 2018 PLANNING" & matched_by != "NO MATCH / NEW LOCATION"), "YES", "NO"),
#         site_2014 = ifelse(!is.na(pc_code_14), "YES", "NO"),
#         site_2018 = ifelse(!is.na(pc_code_18), "YES", "NO"),
#         dropped_from_14 = ifelse(site_2014 == "YES" & matched == "NO", "YES", "NO"),
#         center_status = ifelse(dropped_from_14 == "YES", "Dropped After 2014",
#                                          ifelse(site_2018 == "YES" & site_2014 == "NO", "New Site", "Existing Site")
#                                          )
#  ) %>%
#  dplyr::select(
#    pc_code_18, pc_code_14, site_2018, site_2014,
#    matched, matched_by, center_status, dropped_from_14, code_change, merger, everything()
#  )
#
#write_csv(pc_code_matches_update, "pc_plan/pc_code_matching_2014_2018.csv")
#

pc_analysis <- pc_code_matches_update %>% 
  # add reporting status, district, and province codes to centers that were present in both election cycles
  filter(site_2014 == "YES" & site_2018 == "YES") %>%
  left_join(pc_key_2014 %>%
              dplyr::select(
                district_code, pc_code,
                planned_2014, prelim_first_round_results_reporting, final_first_round_results_reporting, 
                prelim_run_off_results_reporting, final_run_off_results_reporting) %>%
              rename(`2014_IEC_district_code` = district_code,
                     pc_code_14 = pc_code)
            ) %>%
  left_join(pc_key_2019 %>%
                  dplyr::select(
                    district_code, pc_code,
                    planned_2019, prelim_results_2019, final_results_2019, pc_closed, vr_topup_location, vr_final_total_19,
                    planned_2018, prelim_results_2018, final_results_2018, vr_final_total_18) %>%
              rename(`2019_IEC_district_code` = district_code,
                     pc_code_18 = pc_code,
                     pc_closed_19 = pc_closed)
            ) %>%
  left_join(district_code_keyfile_2019 %>%
              dplyr::select(`2018_IEC_province_code`,
                            `2014_IEC_district_code`,
                            `2019_matched_to_2014_IEC_district_code`,
                            `2019_matched_to_2012_AGCHO_district_code`)
            ) %>%
  # add reporting status, district, and province codes to centers present only in 2014
  full_join(
    pc_code_matches_update %>% 
      filter(center_status == "Dropped After 2014") %>%
      left_join(pc_key_2014 %>%
              dplyr::select(
                district_code, pc_code,
                planned_2014, prelim_first_round_results_reporting, final_first_round_results_reporting, 
                prelim_run_off_results_reporting, final_run_off_results_reporting) %>%
              rename(`2014_IEC_district_code` = district_code,
                     pc_code_14 = pc_code)
            ) %>%
      left_join(district_code_keyfile_2019 %>%
              dplyr::select(`2018_IEC_province_code`,
                            `2014_IEC_district_code`,
                            `2019_matched_to_2014_IEC_district_code`,
                            `2019_matched_to_2012_AGCHO_district_code`)
      )
  ) %>%
  # add reporting status, district, and province codes to centers present only in 2018-19
  full_join(
    pc_code_matches_update %>% 
      filter(center_status == "New Site") %>%
      left_join(pc_key_2019 %>%
                  dplyr::select(
                    district_code, pc_code,
                    planned_2019, prelim_results_2019, final_results_2019, pc_closed, vr_final_total_19,
                    planned_2018, prelim_results_2018, final_results_2018, vr_final_total_18) %>%
                  rename(`2019_IEC_district_code` = district_code,
                         pc_code_18 = pc_code,
                         pc_closed_19 = pc_closed) %>%
                  left_join(district_code_keyfile_2019 %>%
                              dplyr::select(`2018_IEC_province_code`,
                                            `2019_IEC_district_code`,
                                            `2019_matched_to_2014_IEC_district_code`,
                                            `2019_matched_to_2012_AGCHO_district_code`)
                  )
      )
  ) %>%
  left_join(
    dplyr::select(pc_key_2019,
                  province_code, province_name_eng) %>% unique() %>%
      rename(`2018_IEC_province_code` = province_code)
  ) %>%
  mutate(no_report_14 = ifelse(prelim_first_round_results_reporting != "YES" & final_first_round_results_reporting != "YES" & 
                  prelim_run_off_results_reporting != "YES" & final_run_off_results_reporting != "YES", "YES", "NO"),
         no_report_18 = ifelse(prelim_results_2018 != "YES" & final_results_2018 != "YES", "YES", "NO"),
         no_report_19 = ifelse(prelim_results_2019 != "YES" & final_results_2019 != "YES", "YES", "NO")
         ) %>%
  left_join(pc_results_2014) %>%
  left_join(pc_results_2019) %>%
  left_join(pc_results_2018) %>%
  left_join(
    dplyr::select(pc_gis_2019,
                  pc_code, lat, lon) %>%
      rename(pc_code_18 = pc_code,
             lat_18 = lon,
             lon_18 = lat)
    ) %>%
  mutate(
    current_lat = ifelse(site_2018 == "YES", lat_18, lat_14),
    current_lon = ifelse(site_2018 == "YES", lon_18, lon_14),
    turnout_18 = total_prelim_votes_18 / vr_final_total_18,
    turnout_19 = total_valid_votes_19 / vr_final_total_19
    ) %>%
  dplyr::select(
    `2018_IEC_province_code`, province_name_eng, 
    `2019_matched_to_2014_IEC_district_code`, `2019_matched_to_2012_AGCHO_district_code`,
    pc_code_18, pc_code_14, site_2014, site_2018,
    pc_name_eng_18, pc_name_dari_18, pc_name_eng_14, pc_name_dari_14,
    matched, matched_by, center_status, dropped_from_14,
    code_change, merger, iec_2018_assessment_report_status,
    lat_14, lon_14, lat_18, lon_18, current_lat, current_lon, coord_14_18_discrepancy_in_meters,
    planned_2014, prelim_first_round_results_reporting, final_first_round_results_reporting,
    prelim_run_off_results_reporting, final_run_off_results_reporting, no_report_14,
    planned_2018, prelim_results_2018, final_results_2018, no_report_18,
    planned_2019, prelim_results_2019, final_results_2019, pc_closed_19, no_report_19,
    everything()
  ) %>%
  dplyr::select(-c(`2019_IEC_district_code`, `2014_IEC_district_code`)) %>%
  arrange(pc_code_18)

# write_csv(pc_analysis, "./analysis/pc_analysis.csv")
#

# -----------------------------------------------------------------------------
# REGRESSION TESTS

out <- lm(data = pc_analysis,
          formula = ag_pct_14 ~ dropped_from_14 + province_name_eng)
summary(out)


out <- lm(data = pc_analysis,
           formula = turnout_19 ~ center_status)
summary(out)


out <- lm(data = pc_analysis,
           formula = turnout_18 ~ center_status)
summary(out)


out <- lm(data = pc_analysis,
           formula = winner_votes_18 ~ center_status + province_name_eng)
summary(out)

out <- lm(data = pc_analysis,
           formula = ag_pct_19 ~ center_status + province_name_eng)
summary(out)

out <- lm(data = pc_analysis,
          formula = turnout_19 ~ `2019_matched_to_2014_IEC_district_code`)
summary(out)

out <- lm(data = pc_analysis,
          formula = aa_pct_19 ~ pc_code_18 + province_name_eng)
summary(out)


out <- lm(data = pc_analysis,
          formula = aa_pct_14 ~ planned_2019)
summary(out)


out <- lm(data = pc_results,
          formula = aa_pct_14 ~ planned_2019)
summary(out)

explanatory <- c("dropped_from_14", "province_name_eng")
dependent <- "ag_pct_14"

pc_analysis %>%
  finalfit::summary_factorlist(dependent, explanatory,
                     p = T, add_dependent_label = T)

pc_analysis %>%
  finalfit::finalfit(dependent, explanatory, metrics = T)

# -----------------------------------------------------------------------------
# DAY OF CLOSURES AND GAPS IN COVERAGE

province_eday_closed <- pc_key_2019 %>%
  group_by(province_name_eng) %>% 
  summarize(planned_pc = length(pc_code[planned_2019 == "YES"]), 
            closed_pc = length(pc_code[pc_closed == "YES" & !is.na(pc_closed)]),
            pct_closed = closed_pc / planned_pc)

district_eday_closed <- ps_key_2019 %>%
  left_join(dplyr::select(
    pc_key_2019, district_sub_code, district_code) %>% unique()
    ) %>%
  group_by(province_name_eng, district_code) %>% #, district_sub_code, district_or_subdivision_name_eng) %>% 
  summarize(planned_ps = length(ps_code), 
            closed_ps = length(pc_code[ps_open == "NO" & !is.na(ps_open)]),
            pct_closed = closed_ps / planned_ps,
            total_district_vr = sum(total_vr, na.rm = T),
            closed_vr = sum(total_vr[ps_open == "NO"], na.rm = T),
            vr_pct_closed = closed_vr / total_district_vr)

district_closed_18 <- pc_key_2019 %>%
  group_by(province_name_eng, district_code) %>%
  summarize(planned_pc_18 = length(pc_code[planned_2018 == "YES"]), 
            reporting_pc_18 = length(pc_code[prelim_results_2018 == "YES"]),
            pct_reporting_18 = reporting_pc_18 / planned_pc_18)

district_results_19 <- final_2019 %>% group_by(district_code) %>%
  summarize(total_votes = sum(votes),
            ag_votes = sum(votes[ballot_position == 8]),
            aa_votes = sum(votes[ballot_position == 9]),
            other_votes = total_votes - (ag_votes + aa_votes),
            aa_pct = aa_votes / total_votes,
            ag_pct = ag_votes / total_votes,
            other_pct = other_votes / total_votes
            )

district_projection <- district_eday_closed %>%
  left_join(district_results_19) %>%
  rowwise %>% mutate(turnout = total_votes / total_district_vr,
                     missing_votes = closed_vr * turnout,
                     new_total = missing_votes + total_votes,
                     missing_ag = ag_pct * missing_votes,
                     missing_aa = aa_pct * missing_votes,
                     missing_other = other_pct * missing_votes,
                     new_ag = missing_ag + ag_votes,
                     new_aa = missing_aa + aa_votes,
                     new_other = missing_other + other_votes,
                     new_ag_pct = new_ag / new_total,
                     new_aa_pct = new_aa / new_total,
                     new_other_pct = new_other / new_total) %>%
  arrange(district_sub_code)

district_pop_2019 <- cso_district_population_estimates_2004_2020 %>%
  filter(gregorian_year == "2018-2019") %>%
  dplyr::select(province_code, district_code, rural_total, urban_total, total_population, fem_population) %>% 
  rename(`2019_IEC_district_code` = district_code) %>%
  left_join(
    dplyr::select(district_code_keyfile_2019,
                  `2019_IEC_district_code`, `2019_matched_to_2012_AGCHO_district_code`
                  )
  )

district_closed_pop <- district_pop_2019 %>%
  left_join(district_eday_closed %>% 
              dplyr::select(-province_name_eng) %>%
              rename(`2019_IEC_district_code` = district_code)) %>%
  left_join(district_closed_18 %>% 
              dplyr::select(-province_name_eng) %>%
              rename(`2019_IEC_district_code` = district_code)) %>%
  rowwise %>%
  mutate(no_planned_19 = ifelse(is.na(planned_ps), "YES", "NO"),
         no_open_19 = ifelse(is.na(closed_ps) | pct_closed == 1, "YES", "NO"),
         no_planned_18 = ifelse(planned_pc_18 == 0 | is.na(planned_pc_18), "YES", "NO"),
         no_reporting_18 = ifelse(reporting_pc_18 == 0 | is.na(reporting_pc_18), "YES", "NO"),
         ps_per_400 = planned_ps / (total_population/400),
         ps_per_1000 = planned_ps / (total_population/1000)
  ) %>%
  left_join(
    dplyr::select(district_code_keyfile_2019, `2019_IEC_district_code`, `2019_IEC_district_name_eng`)
  ) %>%
  left_join(district_key_2019 %>%
              rowwise %>% mutate(`2019_IEC_district_code` = str_pad(district_code, side = "left", pad = "0", width = 4)) %>%
              dplyr::select(`2019_IEC_district_code`, provincial_capital) %>% 
              unique()
  )

province_per_1000 <- district_pop_2019 %>%
  group_by(province_code) %>%
  summarize(
    total_population = sum(total_population, na.rm = T),
    fem_population = sum(fem_population, na.rm = T)) %>%
  left_join(dplyr::select(province_key_2019, province_code, province_name_eng)) %>%
  left_join(
    ps_key_2019 %>% group_by(province_code) %>% 
      summarize(planned_ps = length(ps_code)
                )
    ) %>%
  left_join(
    pc_key_2019 %>% group_by(province_code) %>% 
      summarize(total_vr = sum(vr_final_total_19, na.rm = T),
                fem_vr = sum(vr_final_fem_19, na.rm = T)
                )
    ) %>%
  mutate(ps_per_1000_eligible_pop = planned_ps / ((total_population/1000) / 2),
         vr_pct_elgible_pop = total_vr / (total_population / 2),
         vr_per_1000_elgible_pop = total_vr / ((total_population/1000) / 2),
         ps_per_1000_vr = planned_ps / (total_vr / 1000),
         vr_per_1000_pct_ps_per_1000 = vr_per_1000_elgible_pop / ps_per_1000_eligible_pop,
         fem_vr_pct_eligible_fem_pop = fem_vr / fem_population
         )



district_pop_2014 <- cso_district_population_estimates_2004_2020 %>%
  filter(gregorian_year == "2013-2014") %>%
  dplyr::select(district_code, rural_total, urban_total, total_population) %>% 
  rename(`2014_IEC_district_code` = district_code) %>%
  left_join(
    dplyr::select(district_code_keyfile_2019,
                  `2014_IEC_district_code`, `2019_matched_to_2012_AGCHO_district_code`
                  )
  )

district_closed_14 <- pc_key_2014 %>%
  group_by(province_name_eng, district_code) %>%
  summarize(planned_pc_14 = length(pc_code[planned_2014 == "YES"]), 
            reporting_pc_14 = length(pc_code[prelim_first_round_results_reporting == "YES" | prelim_run_off_results_reporting == "YES" |
                                               final_first_round_results_reporting == "YES" | final_run_off_results_reporting == "YES"]),
            pct_reporting_14 = reporting_pc_14 / planned_pc_14) %>%
  rename(`2014_IEC_district_code` = district_code)

district_pop_closed_14 <- district_pop_2014 %>% 
  left_join(district_closed_14) %>%
  mutate(no_open_14 = ifelse(is.na(reporting_pc_14) | reporting_pc_14 == 0, "YES", "NO"))




ps_per_pop_refactor <- district_closed_pop %>%
  left_join(dplyr::select(
    district_code_keyfile_2019, `2019_matched_to_2012_AGCHO_district_code`, `2012_AGCHO_district_code`
  )) %>%
  group_by(`2019_matched_to_2012_AGCHO_district_code`) %>%
  summarize(
    total_population = sum(total_population, na.rm = T),
    ps_planned = sum(planned_ps, na.rm = T),
    ps_per_1000 = ps_planned / (total_population/1000)
  )

ggplot(data = district_closed_pop, 
       aes(x = planned_ps, y = pct_closed, color = provincial_capital)) +
  geom_point(alpha = .75) +
  geom_smooth(method = "lm", aes(group = 1)) +
  facet_wrap(~province_name_eng)


ggplot(data = district_closed_pop, 
       aes(x = log(total_population), y = ps_per_1000, color = provincial_capital)) +
  geom_point(alpha = .75) +
  geom_smooth(method = "lm", aes(group = 1)) +
  facet_wrap(~province_name_eng)

# -----------------------------------------------------------------------------
# MAPPING

district_map <- readOGR(dsn = "~/Google Drive/GitHub/afghanistan_election_results_2018/district_data/administrative_boundaries/afg_admbnda_adm2_agcho_20180522/", 
                        layer = "afg_admbnda_adm2_agcho_20180522")

district_gislayerdata <- mutate(as.data.frame(district_map),
                                id = as.character(row_number())
                                )
district_gislayerdata$id <-  as.character(as.numeric(district_gislayerdata$id) - 1)

district_map_data <- fortify(district_map)
district_map_data <- inner_join(district_map_data, district_gislayerdata, "id")

district_centroids <- data.frame(
  long = coordinates(district_map)[, 1],
  lat = coordinates(district_map)[, 2])

district_centroids[, "ADM2_PCODE"] <- district_map@data[,"ADM2_PCODE"]

district_centroids <- district_centroids %>%
  rename(
    lab_long = long,
    lab_lat = lat
  )
district_map_data <- left_join(district_map_data, district_centroids)

district_map_data <- district_map_data %>%
  mutate(
    `2019_matched_to_2012_AGCHO_district_code` = gsub("AF", "", as.character(ADM2_PCODE)),
    `2018_IEC_province_code` = gsub("AF", "", ADM1_PCODE)
  )

natl_pc_map_plot <- ggplot(data = district_map_data,
         mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(color = "black", size = 0.2, alpha = 1, fill = "gray80") +
  geom_point(data = pc_analysis,
             inherit.aes = F,
             aes(x = current_lon, 
                     y = current_lat, 
                     color = center_status),
             alpha = .55, size = 1) +
#  geom_text(data = district_map_data,
#            mapping = aes(label = ADM2_EN, x = lab_long, y = lab_lat, group = group),
#            size = 3, color = "black") +
#  coord_quickmap() +
  coord_map(projection = "albers", lat0 = 29, lat1 = 39) +
#  facet_wrap(~province_name_eng) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Afghanistan Polling Center Sites Map",
       subtitle = "Polling center coordinates may be approximate.\nIncludes all centers for which coordinate data is available; not all sites were opened on election day in either period due to either planned or unplanned closures.\nDistrict boundaries are based on 399-district UN OCHA dataset, and do not reflect subsequent district administrative splits.",
       caption = "Author: Colin Cookman (Twitter: @colincookman / Email: ccookman@gmail.com or ccookman@usip.org)\nData Sources: https://github.com/colincookman/afghanistan_presidential_election_2019/",
       color = "Polling Center Change in Status 2014-2018"
   ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = .5),
    legend.position = "bottom",
    legend.box = "vertical",
    plot.caption = element_text(hjust = .5)
  ) +
  guides(
    color = guide_legend(title.position = "top", title.hjust = 0.5)
  )

natl_pc_map_plot

ggsave(filename = "./graphics/afghanistan_pc_map_plot.png", 
       plot = natl_pc_map_plot, dpi = 300, height = 450, width = 800, units = "mm")

province_codes <- unique(pc_analysis$`2018_IEC_province_code`)

for(i in 1:length(province_codes)){
  plot_number <- province_codes[i]
  plot_name <- province_key_2019$province_name_eng[province_key_2019$province_code == province_codes[i]]
  plot_obj <- 
    ggplot(data = subset(district_map_data,
                `2018_IEC_province_code` == plot_number),
         mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(color = "black", size = 0.2, alpha = 1, fill = "gray80") +
  geom_point(data = subset(pc_analysis,
                `2018_IEC_province_code` == plot_number),
             inherit.aes = F,
             aes(x = current_lon, 
                     y = current_lat, 
                     color = center_status),
             alpha = .65, size = 2) +
  geom_text(data = subset(district_map_data,
                          `2018_IEC_province_code` == plot_number),
            mapping = aes(label = ADM2_EN, x = lab_long, y = lab_lat, group = group),
            size = 3, color = "black") +
  coord_quickmap() +
#  coord_map(projection = "albers", lat0 = 29, lat1 = 39) +
#  facet_wrap(~province_name_eng) +
  scale_color_brewer(palette = "Set1") +
  labs(title = paste0(plot_name, " Polling Center Sites Map"),
       subtitle = "Polling center coordinates may be approximate.\nIncludes all centers for which coordinate data is available; not all sites were opened on election day in either period due to either planned or unplanned closures.\nDistrict boundaries are based on 399-district UN OCHA dataset, and do not reflect subsequent district administrative splits.",
       caption = "Author: Colin Cookman (Twitter: @colincookman / Email: ccookman@gmail.com or ccookman@usip.org)\nData Sources: https://github.com/colincookman/afghanistan_presidential_election_2019/",
       color = "Polling Center Change in Status 2014-2018"
   ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = .5),
    legend.position = "bottom",
    legend.box = "vertical",
    plot.caption = element_text(hjust = .5)
  ) +
  guides(
    color = guide_legend(title.position = "top", title.hjust = 0.5)
  )

ggsave(filename = paste0("./graphics/pc_maps/", plot_number, "_", plot_name, "_pc_map_plot.png"), 
       plot = plot_obj, dpi = 300, height = 450, width = 800, units = "mm")

}

map_ps_per_pop <- district_map_data %>% 
  left_join(ps_per_pop_refactor)

ggplot(data = map_ps_per_pop,
         mapping = aes(x = long, y = lat, group = group, fill = ps_per_1000)) + 
  geom_polygon(color = "black", size = 0.2, alpha = 1) +
  scale_fill_viridis()

# -----------------------------------------------------------------------------

# SHARE FOR PARLIAMENT WINNERS VS GHANI VOTE SHARE - RERUN THIS AT DISTRICT / PROVINCE LEVEL?

ggplot(data = pc_analysis, aes(x = win_pct_18, y = ag_pct_19)) + 
  geom_point(alpha = .7) + 
  facet_wrap(~province_name_eng) + 
  geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1), breaks=seq(0, 1, by=0.1)) +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1), breaks=seq(0, 1, by=0.1)) +
  labs(x = "Share of Preliminary Vote Cast for Winning Candidates in 2018 Parliamentary Elections", 
       y = "Share of Final Valid Vote Cast for Ashraf Ghani in 2019 Presidential ELection",
       title = "Votes for Winning Candidates in 2018-2019",
       subtitle = "Points are polling centers. Preliminary data used for 2018 parliamentary election results due to discrepancies in final results data for Kabul as published by the Independent Election Commission.",
       caption = "Author: Colin Cookman (Twitter: @colincookman / Email: ccookman@gmail.com or ccookman@usip.org)\nData Sources: https://github.com/colincookman/afghanistan_presidential_election_2019/") +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    plot.caption = element_text(hjust = .5),
    axis.text.x = element_text(angle = 90)
  )

wj_ballot_check <- prelim_wj_2018 %>%
  group_by(constituency_code, ballot_position) %>%
  summarize(votes = sum(votes, na.rm = T))

ggplot(data = wj_ballot_check,
       aes(x = log(ballot_position), y = log(votes))) +
  geom_point(alpha = .65) +
  geom_smooth(method = "lm") +
  facet_wrap(~constituency_code)


out <- lm(data = wj_ballot_check,
          formula = votes ~ ballot_position + constituency_code)
summary(out)


# -----------------------------------------------------------------------------

# PLUG IN REPLACEMENT CENTERS

pc_nearest <- nearest_replacement_center_2014_2018 %>% 
  left_join(pc_analysis %>% 
              dplyr::select(-c(pc_name_eng_14, pc_code_14, coord_14_18_discrepancy_in_meters, matched_by, lat_14, lon_14))
            )

pc_nearest <- 
#  pc_analysis %>%
  filter(matched_by != "NO MATCH / DROPPED FROM 2018 PLANNING") %>%
  full_join(nearest_replacement_center_2014_2018) %>%
  arrange(pc_code_14) %>%
  left_join(pc_key_2014 %>%
              dplyr::select(
                district_code, pc_code, planned_2014,
                prelim_first_round_results_reporting, prelim_run_off_results_reporting) %>%
              rename(pc_code_14 = pc_code, `2014_IEC_district_code` = district_code)
  ) %>%
  left_join(pc_key_2019 %>%
              dplyr::select(
                district_code, pc_code, planned_2019, planned_2018,
                prelim_results_2019, prelim_results_2018) %>%
              rename(pc_code_18 = pc_code, `2019_IEC_district_code` = district_code)
  ) %>%
  left_join(district_code_keyfile_2019 %>%
              dplyr::select(
                `2019_IEC_district_code`, `2019_matched_to_2014_IEC_district_code`, `2019_matched_to_2012_AGCHO_district_code`
              )) %>%
  dplyr::select(
    `2019_IEC_district_code`, `2019_matched_to_2014_IEC_district_code`, `2019_matched_to_2012_AGCHO_district_code`, `2014_IEC_district_code`,
    everything()
  )

pc_results <- pc_changes %>%
  left_join(pc_results_2014) %>%
  left_join(pc_results_2019) %>%
  mutate(center_status = ifelse(matched_by == "NEAREST REPLACEMENT CENTER BY PROXIMITY",
                                ifelse(coord_14_18_discrepancy_in_meters < 500, "relocated_500m",
                                       ifelse(coord_14_18_discrepancy_in_meters >= 500 & coord_14_18_discrepancy_in_meters < 1000, "relocated_500m_1km", "relocated_1km")
                                ), 
                                ifelse(matched_by == "NO MATCH / NEW LOCATION", "new_pc", "existing_pc")
                                )
  )
  
aa_med_14 <-median(pc_results$aa_pct_14, na.rm = T)
aa_med_19 <- median(pc_results$aa_pct_19, na.rm = T)

ggplot(data = subset(pc_results, center_status != "new_pc"),
       aes(x = aa_pct_14, y = aa_pct_19, color = center_status)) +
  scale_color_brewer(palette = "Set1") +
  geom_hline(yintercept = aa_med_19, size = 1, color = "gray80") +
  geom_vline(xintercept = aa_med_14, size = 1, color = "gray80") +
  geom_abline(slope = 1, intercept = 0, size = 1, color="gray80") +
  geom_point(alpha = .25) +
  geom_smooth(method = "lm") + #aes(group = 1)) +
  coord_equal() +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1), breaks=seq(0, 1, by=0.05)) +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1), breaks=seq(0, 1, by=0.05)) +
  facet_grid(cols = vars(center_status))

ag_med_14 <-median(pc_results$ag_pct_14, na.rm = T)
ag_med_19 <- median(pc_results$ag_pct_19, na.rm = T)

out <- lm(data = pc_results,
           formula = aa_pct_14 ~ aa_pct_19 + center_status)
summary(out)


ggplot(data = subset(pc_results, center_status != "new_pc"),
       aes(x = ag_pct_14, y = ag_pct_19, color = center_status)) +
  scale_color_brewer(palette = "Set1") +
  geom_hline(yintercept = ag_med_19, size = 1, color = "gray80") +
  geom_vline(xintercept = ag_med_14, size = 1, color = "gray80") +
  geom_abline(slope = 1, intercept = 0, size = 1, color="gray80") +
  geom_point(alpha = .25) +
#  geom_smooth(method = "lm") +
  geom_smooth(method = "lm", aes(group = 1)) +
  coord_equal() +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1), breaks=seq(0, 1, by=0.05)) +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1), breaks=seq(0, 1, by=0.05)) +
  facet_wrap(~province_name_eng)
#  facet_grid(cols = vars(center_status))


ggplot(data = subset(pc_results, center_status != "new_pc"),
       aes(x = aa_pct_14, y = aa_pct_19, color = center_status)) +
  scale_color_brewer(palette = "Set1") +
  geom_hline(yintercept = aa_med_19, size = 1, color = "gray80") +
  geom_vline(xintercept = aa_med_14, size = 1, color = "gray80") +
  geom_abline(slope = 1, intercept = 0, size = 1, color="gray80") +
  geom_point(alpha = .25) +
#  geom_smooth(method = "lm") +
  geom_smooth(method = "lm", aes(group = 1)) +
  coord_equal() +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1), breaks=seq(0, 1, by=0.05)) +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1), breaks=seq(0, 1, by=0.05)) +
  facet_wrap(~province_name_eng)


out <- lm(data = pc_results,
           formula = aa_pct_14 ~ center_status)
summary(out)

out <- lm(data = pc_results,
           formula = ag_pct_14 ~ center_status)
summary(out)


# -----------------------------------------------------------------------------

district_summary <- pc_changes %>%
  group_by(`2019_matched_to_2014_IEC_district_code`) %>%
  summarize(
    total_14 = length(pc_code_14),
    planned_14 = length(pc_code_14[planned_2014 == "YES"]),
    planned_pct_14 = planned_14 / total_14,
    open_R1_14 = length(pc_code_14[prelim_first_round_results_reporting == "YES"]),
    open_R2_14 = length(pc_code_14[prelim_run_off_results_reporting == "YES"]),
    open_pct_R1 = open_R1_14 / planned_14,
    open_pct_R2 = open_R2_14 / planned_14,
    total_18 = length(pc_code_18),
    planned_18 = length(pc_code_18[planned_2018 == "YES"]),
    planned_pct_18 = planned_18 / total_18,
    planned_19 = length(pc_code_18[planned_2019 == "YES"]),
    open_18 = length(pc_code_18[prelim_results_2018 == "YES"]),
    open_19 = length(pc_code_18[prelim_results_2019 == "YES"]),
    open_pct_18 = open_18 / planned_18,
    open_pct_19 = open_19 / planned_19,
    new_pcs_14_18 = length(pc_code_18[matched_by == "NO MATCH / NEW LOCATION"]),
    closed_14_18_replacement_under_500m = length(pc_code_14[matched_by == "NEAREST REPLACEMENT CENTER BY PROXIMITY" & 
                                                               coord_14_18_discrepancy_in_meters < 500]),
    closed_14_18_replacement_500_1km = length(pc_code_14[matched_by == "NEAREST REPLACEMENT CENTER BY PROXIMITY" & 
                                                           coord_14_18_discrepancy_in_meters >= 500 & 
                                                           coord_14_18_discrepancy_in_meters < 1000]),
    closed_14_18_replacement_over_1km = length(pc_code_14[matched_by == "NEAREST REPLACEMENT CENTER BY PROXIMITY" & 
                                                             coord_14_18_discrepancy_in_meters >= 1000]),
    median_replacement_distance = median(coord_14_18_discrepancy_in_meters[matched_by == "NEAREST REPLACEMENT CENTER BY PROXIMITY"]),
    mean_replacement_distance = mean(coord_14_18_discrepancy_in_meters[matched_by == "NEAREST REPLACEMENT CENTER BY PROXIMITY"]),
    pct_14_planned_relocated = (length(pc_code_14[matched_by == "NEAREST REPLACEMENT CENTER BY PROXIMITY"])) / planned_14,
    pct_18_planned_new = new_pcs_14_18 / planned_18
  ) %>%
  arrange(`2019_matched_to_2014_IEC_district_code`)

district_summary_report <- read_csv("analysis/district_summary_report.csv")

district_summary_report_closures <- district_summary_report %>%
  left_join(
    district_summary %>% rename(district_code = `2019_matched_to_2014_IEC_district_code`) %>%
      dplyr::select(
        district_code, pct_14_planned_relocated, pct_18_planned_new
      )
  )

ggplot(data = district_summary_report_closures,
       aes(x = aa_pct_R214, y = pct_14_planned_relocated, color = provincial_capital)) +
  coord_equal() +
  scale_color_brewer(palette = "Set1") +
  geom_point(alpha = .75) +
  geom_smooth(method = "lm", aes(group = 1)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1), breaks=seq(0, 1, by=0.05)) +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1), breaks=seq(0, 1, by=0.05)) +
  facet_wrap(~province_name_eng)





out <- lm(data = pc_analysis,
          formula = ag_pct_14 ~ province_code_18)
summary(out)


nearest_replacement_center_2014_2018 <- nearest_replacement_center_2014_2018 %>% 
  rowwise %>% 
  mutate(province_code = str_sub(pc_code_18, start = 1, end = 2),
         district_code = str_sub(pc_code_18, start = 1, end = 4))

province_relocation <- nearest_replacement_center_2014_2018 %>%
  group_by(province_code) %>%
  summarize(med_distance = median(coord_14_18_discrepancy_in_meters),
            mean_distance = mean(coord_14_18_discrepancy_in_meters))


