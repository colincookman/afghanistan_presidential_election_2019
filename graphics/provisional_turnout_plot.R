partial_turnout_reports_10_03_19 <- read_csv("raw/partial_turnout_reports_10_03_19.csv")
partial_turnout_reports_10_03_19 <- filter(partial_turnout_reports_10_03_19, !is.na(province_code))
partial_turnout_reports_10_03_19$X24 <- NULL
partial_turnout_reports_10_03_19$`PS Planned (4938 MOI List of Sept 28)` <- NULL
partial_turnout_reports_10_03_19$X40 <- NULL

colnames(partial_turnout_reports_10_03_19) <- c(
  "province_code", "region_name_eng", "province_name_eng",
  "pc_plan_IEC", "ps_plan_IEC", 
  "pc_plan_MOI", "pc_plan_MOI_pct_IEC",
  "pc_count_09_28", "pc_count_09_29", "pc_count_10_01", "pc_count_10_03",
  "pc_reporting_pct_IEC_09_28", "pc_reporting_pct_IEC_09_29", "pc_reporting_pct_IEC_10_01", "pc_reporting_pct_IEC_10_03",
  "pc_reporting_pct_MOI_10_01", "pc_reporting_pct_MOI_10_03", "pc_remaining_pct_MOI_10_01",
  "ps_reporting_09_29", "ps_reporting_pct_IEC_09_29", "ps_reporting_10_03", "ps_reporting_pct_IEC_10_03",
  "partial_votes_09_28", "partial_votes_09_29", "partial_votes_10_01", "partial_votes_10_03",
  "net_change_pc_28_29", "net_change_votes_28_29", 
  "net_change_pc_29_01", "net_change_votes_29_01", 
  "net_change_pc_01_03", "net_change_votes_01_03",
  "avg_votes_pc_09_28", "avg_votes_pc_09_29", "avg_votes_pc_10_01", "avg_votes_pc_10_03",
  "straight_line_projection",
  "vr_total_2019", "turnout_10_03_19", 
  "total_prelim_votes_2018", "pct_change_votes_2018_10_03_19",
  "vr_total_2018", "pct_change_vr_18_19",
  "turnout_2018", "pct_change_turnout_2018_10_03_19",
  "total_prelim_round_one_votes_2014", "pct_change_votes_2014_10_03_19"
)

partial_turnout_reports_10_03_19$straight_line_projection <- NULL

pc_report_18 <- pc_key_2018 %>% group_by(province_code) %>%
  summarize(pc_plan_2018 = length(pc_code[planned_2018 == "YES"]),
            pc_prelim_reporting_2018 = length(pc_code[prelim_results_reporting == "YES"])) %>% 
  mutate(pc_reporting_pct_2018 = pc_prelim_reporting_2018 / pc_plan_2018) %>% filter(province_code != "11")

partial_turnout_reports_10_03_19$province_code <- as.character(str_pad(partial_turnout_reports_10_03_19$province_code, 2, pad = "0", side = "left"))
partial_turnout_reports_10_03_19 <- left_join(partial_turnout_reports_10_03_19, pc_report_18)

partial_turnout_reports_10_03_19 <- partial_turnout_reports_10_03_19 %>% mutate(
  pc_plan_IEC_pct_plan_18 = pc_plan_IEC / pc_plan_2018,
  pc_plan_MOI_pct_plan_18 = pc_plan_MOI / pc_plan_2018,
  pc_plan_IEC_pct_prelim_reporting_18 = pc_plan_IEC / pc_prelim_reporting_2018,
  pc_plan_MOI_pct_prelim_reporting_18 = pc_plan_MOI / pc_prelim_reporting_2018
)

partial_turnout_reports_10_03_19 <- partial_turnout_reports_10_03_19 %>% dplyr::select(
  region_name_eng, province_code, province_name_eng,
  pc_plan_IEC, ps_plan_IEC, pc_plan_MOI, pc_plan_MOI_pct_IEC, 
  pc_plan_2018, pc_prelim_reporting_2018, pc_plan_IEC_pct_plan_18, pc_plan_MOI_pct_plan_18, 
  pc_plan_IEC_pct_prelim_reporting_18, pc_plan_MOI_pct_prelim_reporting_18,
  pc_count_09_28, pc_count_09_29, pc_count_10_01, pc_count_10_03,
  pc_reporting_pct_IEC_09_28, pc_reporting_pct_IEC_09_29, pc_reporting_pct_IEC_10_01, pc_reporting_pct_IEC_10_03, pc_reporting_pct_2018,
  everything()
)

partial_turnout_reports_10_03_19 <- partial_turnout_reports_10_03_19 %>% mutate(
  pct_natl_vote_10_03_19 = partial_votes_10_03 / (sum(partial_turnout_reports_10_03_19$partial_votes_10_03, na.rm = T)),
  pct_natl_vr_19 = vr_total_2019 / (sum(partial_turnout_reports_10_03_19$vr_total_2019, na.rm = T)),
  pct_natl_vote_2018 = total_prelim_votes_2018 / (sum(partial_turnout_reports_10_03_19$total_prelim_votes_2018, na.rm = T)),
  pct_natl_vr_18 = vr_total_2018 / (sum(partial_turnout_reports_10_03_19$vr_total_2018, na.rm = T)),
  pct_natl_vote_2014 = total_prelim_round_one_votes_2014 / (sum(partial_turnout_reports_10_03_19$total_prelim_round_one_votes_2014, na.rm = T))
)

partial_turnout_reports_10_03_19 <- partial_turnout_reports_10_03_19 %>% mutate(
  turnout_10_03_19 = partial_votes_10_03 / vr_total_2019,
  pct_change_votes_2018_10_03_19 = (partial_votes_10_03 - total_prelim_votes_2018) / total_prelim_votes_2018,
  pct_change_vr_18_19 = (vr_total_2019 - vr_total_2018) / vr_total_2018,
  turnout_2018 = total_prelim_votes_2018 / vr_total_2018,
  pct_change_turnout_2018_10_03_19 = (turnout_10_03_19 - turnout_2018) / turnout_2018,
  pct_change_votes_2014_10_03_19 = (partial_votes_10_03 - total_prelim_round_one_votes_2014) / total_prelim_round_one_votes_2014
)


write.csv(partial_turnout_reports_10_03_19, "./raw/partial_turnout_reports_10_03_19.csv", row.names = F)



natl_med_turnout_18 <- median(partial_turnout_reports_10_03_19$turnout_2018[!is.na(partial_turnout_reports_10_03_19$turnout_2018)])
natl_med_turnout_19 <- median(partial_turnout_reports_10_03_19$turnout_10_03_19)


prov_turnout <- ggplot(data = partial_turnout_reports_10_03_19, aes(x = turnout_2018, y = turnout_10_03_19, color = region_name_eng)) +
  coord_fixed(ratio = 1) +
  geom_hline(yintercept = natl_med_turnout_19, size = 1, color = "gray80") +
  geom_vline(xintercept = natl_med_turnout_18, size = 1, color = "gray80") +
  geom_abline(slope = 1, intercept = 0, size = 1, color="gray80") +
  geom_point() +
#  geom_smooth(method = "lm") +
  scale_x_continuous(limits = c(0, .9), labels = scales::percent_format(accuracy = 1), breaks=seq(0, .9, by=0.05)) +
  scale_y_continuous(limits = c(0, .9), labels = scales::percent_format(accuracy = 1), breaks=seq(0, .9, by=0.05)) +
  geom_text_repel(data = partial_turnout_reports_10_03_19, 
                  mapping = aes(label = province_name_eng), size = 3, box.padding = unit(0.55, "lines")) +
  labs(y = "2019 Provisional Votes Against Voter Registration (Provisional Results as of 10-03-19)", 
       x = "2018 Votes Against Voter Registration (Preliminary Results)",
       title = "Provincial-Level Turnout Comparisons in Afghanistan Elections",
       subtitle = "Diagonal line represents hypothetical perfect correlation, x- and y-intercept lines are national median.",
       caption = "Author: Colin Cookman (Twitter: @colincookman / Email: ccookman@gmail.com or ccookman@usip.org)\nData Sources:\n2018 parliamentary data: https://github.com/colincookman/afghanistan_election_results_2018\n2019 provisional data: https://github.com/colincookman/afghanistan_presidential_election_2019/blob/master/raw/Partial%20Turnout%2010-03-19.xlsx",
       color = "UNAMA Region Code"
  ) +
  theme(
    plot.caption = element_text(hjust = 0)
  )

prov_turnout

ggsave("./graphics/provisional_turnout_10_03.png", plot = prov_turnout, dpi = 300, height = 215.9, width = 279.4, units = "mm")


natl_med_vr_18 <- median(partial_turnout_reports_10_03_19$pct_natl_vr_19[!is.na(partial_turnout_reports_10_03_19$pct_natl_vr_19)])
natl_med_open_ps_19 <- median(partial_turnout_reports_10_03_19$ps_reporting_pct_IEC_10_03)


ps_closures <- ggplot(data = partial_turnout_reports_10_03_19, 
                      aes(x = turnout_10_03_19, y = ps_reporting_pct_IEC_10_03, color = region_name_eng)) +
  coord_fixed(ratio = 1) +
  geom_hline(yintercept = natl_med_open_ps_19, size = 1, color = "gray80") +
  geom_vline(xintercept = natl_med_turnout_19, size = 1, color = "gray80") +
#  geom_abline(slope = 1, intercept = 0, size = 1, color="gray80") +
  geom_point() +
#  geom_smooth(method = "lm", color = "dodgerblue4", 
#              data = partial_turnout_reports_10_03_19,
#              aes(x = turnout_10_03_19, y = ps_reporting_pct_IEC_10_03)) +
  scale_x_continuous(limits = c(0, .9), labels = scales::percent_format(accuracy = 1), breaks=seq(0, .9, by=0.05)) +
  scale_y_continuous(limits = c(.4, 1), labels = scales::percent_format(accuracy = 1), breaks=seq(.4, 1, by=0.05)) +
  geom_text_repel(data = partial_turnout_reports_10_03_19, 
                  mapping = aes(label = province_name_eng), size = 3, box.padding = unit(0.55, "lines")) +
  labs(y = "Percent of Planned Polling Stations Reported Open (Provisional Results of 10-03-19)", 
       x = "2019 Provisional Votes Against Voter Registration (Provisional Results as of 10-03-19)",
       title = "Polling Station Closures in the 2019 Afghan Presidential Election",
       subtitle = "X- and y-intercept lines are national median.",
       caption = "Author: Colin Cookman (Twitter: @colincookman / Email: ccookman@gmail.com or ccookman@usip.org)\nData Sources: https://github.com/colincookman/afghanistan_presidential_election_2019/",
       color = "UNAMA Region Code"
  ) +
  theme(
    plot.caption = element_text(hjust = 0)
  )

ps_closures

ggsave("./graphics/ps_closures_turnout_10_03.png", plot = ps_closures, dpi = 300, height = 215.9, width = 279.4, units = "mm")


ps_closures_vr <- ggplot(data = partial_turnout_reports_10_03_19, 
                       aes(y = pct_natl_vr_19, x = ps_reporting_pct_IEC_10_03, color = region_name_eng)) +
   coord_fixed(ratio = 1) +
   geom_vline(xintercept = natl_med_open_ps_19, size = 1, color = "gray80") +
   geom_hline(yintercept = natl_med_vr_18, size = 1, color = "gray80") +
#  geom_abline(slope = 1, intercept = 0, size = 1, color="gray80") +
   geom_point() +
#  geom_smooth(method = "lm") +
   scale_y_continuous(limits = c(0, .2), labels = scales::percent_format(accuracy = 1), breaks=seq(0, .2, by=0.05)) +
   scale_x_continuous(limits = c(.4, 1), labels = scales::percent_format(accuracy = 1), breaks=seq(.4, 1, by=0.05)) +
   geom_text_repel(data = partial_turnout_reports_10_03_19, 
                   mapping = aes(label = province_name_eng), size = 3, box.padding = unit(0.55, "lines")) +
   labs(x = "Percent of Planned Polling Stations Reported Open (Provisional Results of 10-03-19)", 
        y = "Province's Share of Total National 2019 Voter Registration",
        title = "Polling Station Closures in the 2019 Afghan Presidential Election",
        subtitle = "X- and y-intercept lines are national median.",
        caption = "Author: Colin Cookman (Twitter: @colincookman / Email: ccookman@gmail.com or ccookman@usip.org)\nData Sources: https://github.com/colincookman/afghanistan_presidential_election_2019/",
        color = "UNAMA Region Code"
   ) +
   theme(
     plot.caption = element_text(hjust = 0)
   )

ps_closures_vr

ggsave("./graphics/ps_closures_vr_10_03.png", plot = ps_closures_vr, dpi = 300, height = 215.9, width = 279.4, units = "mm")
