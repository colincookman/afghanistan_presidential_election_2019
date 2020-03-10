provincial_summary_report <- final_2019 %>%
  group_by(province_name_eng) %>%
  summarize(candidate_8 = sum(votes[ballot_position == 8], na.rm = T),
            candidate_9 = sum(votes[ballot_position == 9], na.rm = T),
            total_final_votes = sum(votes, na.rm = T)) %>%
  mutate(ghani_pct = candidate_8 / total_final_votes,
         abdullah_pct = candidate_9 / total_final_votes
         )

national_total <- final_2019 %>%
  mutate(province_name_eng = "NATIONAL TOTAL") %>%
  group_by(province_name_eng) %>%
  summarize(candidate_8 = sum(votes[ballot_position == 8], na.rm = T),
            candidate_9 = sum(votes[ballot_position == 9], na.rm = T),
            total_final_votes = sum(votes, na.rm = T)) %>%
  mutate(ghani_pct = candidate_8 / total_final_votes,
         abdullah_pct = candidate_9 / total_final_votes
         )

provincial_summary_report <- provincial_summary_report %>%
  full_join(national_total)

provincial_vote_share <- ggplot(data = provincial_summary_report,
       aes(x = abdullah_pct, y = ghani_pct)) + 
  geom_point(alpha = .65) +
  coord_equal() +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1), breaks=seq(0, 1, by=0.05)) +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1), breaks=seq(0, 1, by=0.05)) +
  geom_text_repel(mapping = aes(label = province_name_eng), size = 3, box.padding = unit(0.55, "lines")) +
  labs(x = "Abdullah Share of Final Valid Vote Total", 
       y = "Ghani Share of Final Valid Vote Total",
       title = "Provincial Vote Share for Top Two Candidates\nin the 2019 Afghan Presidential Elections",
       caption = "Author: Colin Cookman (Twitter: @colincookman / Email: ccookman@gmail.com or ccookman@usip.org)\nData Sources: https://github.com/colincookman/afghanistan_presidential_election_2019/"
   ) +
  theme(
    axis.text.x = element_text(angle = 90),
    plot.caption = element_text(hjust = 0)
   )

provincial_vote_share

ggsave("./graphics/final_provincial_vote_share.png", plot = provincial_vote_share, dpi = 300, height = 215.9, width = 279.4, units = "mm")
