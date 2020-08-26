rm(list = ls())

# 2014 and 2018 pc matching ------------------

district_code_keyfile_2019 <- read_csv("district_data/district_code_keyfile_2019.csv")
pc_key_2018 <- read_csv("~/Google Drive/GitHub/afghanistan_election_results_2018/keyfiles/pc_key.csv")
pc_key_2014 <- read_csv("past_elections/presidential_2014/keyfiles/pc_key_2014.csv")
pc_gis_2019 <- read_csv("~/Google Drive/Afghan Elections Research/2018 Parliamentary/PC Data/pc_gis.csv")


coord_2018 <- dplyr::select(pc_gis_2019, pc_code, lat, lon) %>% 
  #correct lat/lon error
  rename(lon2 = lat, lat2 = lon) %>% 
  rename(lon = lon2, lat = lat2) %>% 
  rowwise %>% 
  mutate(latlon = paste0(str_pad(round(lat, digits = 5), width = 8, side = "right", pad = 0), " ",
                         str_pad(round(lon, digits = 5), width = 8, side = "right", pad = 0))) %>%
  left_join(
    dplyr::select(pc_key_2018,
                  district_code, district_sub_code, pc_code, pc_name_eng, pc_name_dari, assessment_status)) %>%
  rename(pc_code_18 = pc_code,
         `2018_IEC_district_code` = district_code,
         pc_name_eng_18 = pc_name_eng,
         pc_name_dari_18 = pc_name_dari) %>%
  left_join(dplyr::select(
    district_code_keyfile_2019, `2018_IEC_district_code`, `2019_matched_to_2014_IEC_district_code`))

coord_2014 <- dplyr::select(pc_key_2014_updated, district_code, pc_code, pc_name_eng, pc_name_dari, lat, lon) %>% 
  rowwise %>% 
  mutate(latlon = paste0(str_pad(round(lat, digits = 5), width = 8, side = "right", pad = 0), " ",
                         str_pad(round(lon, digits = 5), width = 8, side = "right", pad = 0))) %>%
  rename(pc_code_14 = pc_code,
         pc_name_eng_14 = pc_name_eng,
         pc_name_dari_14 = pc_name_dari,
         `2019_matched_to_2014_IEC_district_code` = district_code)


gis_matches <- coord_2018$pc_code_18[coord_2018$latlon %in% coord_2014$latlon]

gis_matches_df <- tibble(pc_code_18 = gis_matches) %>%
  left_join(coord_2018) %>% 
  left_join(dplyr::select(coord_2014, pc_code_14, pc_name_eng_14, pc_name_dari_14, latlon)) %>% 
  mutate(code_change = ifelse(pc_code_18 != pc_code_14, "YES", "NO"),
         matched_by = "GIS MATCH") %>%
  dplyr::select(`2018_IEC_district_code`, district_sub_code, `2019_matched_to_2014_IEC_district_code`,
                pc_code_18, pc_code_14, pc_name_eng_18, pc_name_eng_14, pc_name_dari_18, pc_name_dari_14,
                latlon, lat, lon, code_change, matched_by, assessment_status)


names_2018 <- dplyr::select(coord_2018, 
                            `2019_matched_to_2014_IEC_district_code`, district_sub_code, 
                            pc_name_dari_18, pc_name_eng_18, pc_code_18) %>%
  rename(pc_name_dari = pc_name_dari_18) %>%
  left_join((dplyr::select(pc_key_2018, pc_code, district_sub_code)) %>% 
              rename(pc_code_18 = pc_code)
            ) %>%
    mutate(`2019_matched_to_2014_sub_code` = ifelse(`2019_matched_to_2014_IEC_district_code` == "0101",
                                                           district_sub_code, `2019_matched_to_2014_IEC_district_code`)) %>%
  dplyr::select(-c(district_sub_code, `2019_matched_to_2014_IEC_district_code`))

names_2018$`2019_matched_to_2014_sub_code`[names_2018$pc_code_18 == "0101555"] <- "0101-12"
names_2018$`2019_matched_to_2014_sub_code`[names_2018$pc_code_18 == "0101556"] <- "0101-12"
names_2018$`2019_matched_to_2014_sub_code`[names_2018$pc_code_18 == "0101559"] <- "0101-22"

names_2014 <- dplyr::select(coord_2014, `2019_matched_to_2014_IEC_district_code`, pc_name_dari_14, pc_name_eng_14, pc_code_14) %>% 
               rename(pc_name_dari = pc_name_dari_14) %>%
               left_join(dplyr::select(pc_key_2014, pc_code, district_sub_code) %>%
                           rename(pc_code_14 = pc_code)
                         ) %>%
  mutate(`2019_matched_to_2014_sub_code` = ifelse(`2019_matched_to_2014_IEC_district_code` == "0101",
                                                           district_sub_code, `2019_matched_to_2014_IEC_district_code`)) %>%
  dplyr::select(-c(district_sub_code, `2019_matched_to_2014_IEC_district_code`))

name_matches_df <- left_join(names_2018, names_2014) %>% 
  filter(!is.na(pc_code_14)) %>%
  mutate(matched_by = "LOCATION NAME AND DISTRICT MATCH",
         code_change = ifelse(pc_code_18 != pc_code_14, "YES", "NO")) %>%
  left_join(dplyr::select(
    coord_2018, pc_code_18, lat, lon, latlon, assessment_status, `2019_matched_to_2014_IEC_district_code`)
  ) %>%
  rename(lat_18 = lat, lon_18 = lon, latlon_18 = latlon) %>%
  left_join(dplyr::select(
    coord_2014, pc_code_14, lat, lon, latlon
  )) %>%
  rename(lat_14 = lat, lon_14 = lon, latlon_14 = latlon) %>%
  dplyr::select(`2019_matched_to_2014_IEC_district_code`, `2019_matched_to_2014_sub_code`, pc_code_18, pc_code_14, 
                pc_name_eng_18, pc_name_eng_14, pc_name_dari,
                latlon_18, latlon_14, lat_18, lat_14, lon_18, lon_14, code_change, matched_by, assessment_status) %>%
  rowwise %>% mutate(vincenty_distance = distVincentyEllipsoid(c(lat_18, lon_18), c(lat_14, lon_14)))

no_other_match <- name_matches_df %>%
  filter(pc_code_18 %in% gis_matches)

known_matches <- gis_matches_df %>% 
  dplyr::select(pc_code_18, pc_code_14, pc_name_eng_18, pc_name_eng_14, pc_name_dari_18, 
                code_change, matched_by, assessment_status) %>% 
  rename(pc_name_dari = pc_name_dari_18) %>% 
  full_join(filter(name_matches_df, !(pc_code_18 %in% gis_matches))) %>% 
  dplyr::select(-c(`2019_matched_to_2014_sub_code`, lat, lon, latlon)) %>%
  arrange(pc_code_18)

# write.csv(known_matches, "./pc_plan/pc_code_matching_2014_2018.csv", row.names = F)

not_matched <- pc_key_2018$pc_code[!pc_key_2018$pc_code %in% c(gis_matches_df$pc_code_18, name_matches_df$pc_code_18)]

not_matched_df <- tibble(pc_code = not_matched) %>% 
  left_join(
    dplyr::select(pc_key_2018,
                  district_code, pc_code, pc_name_eng, pc_location_eng, 
                  pc_name_dari, assessment_status, planned_2018)) %>%
  rename(pc_code_18 = pc_code,
         pc_name_eng_18 = pc_name_eng,
         pc_name_dari_18 = pc_name_dari,
         `2018_IEC_district_code` = district_code) %>%
  left_join(dplyr::select(
    district_code_keyfile_2019, `2018_IEC_district_code`, `2019_matched_to_2014_IEC_district_code`)) %>%
  left_join(dplyr::select(coord_2018, pc_code_18, latlon)) %>%
  mutate(matched_by = NA) %>%
  dplyr::select(
    `2019_matched_to_2014_IEC_district_code`,
    pc_code_18, pc_name_eng_18, pc_name_dari_18, pc_location_eng, matched_by, assessment_status, latlon
  )

# write.csv(not_matched_df, "./pc_plan/unmatched_2018_pcs.csv", row.names = F)

# incorporate manual matching

manual_match <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1_HyneSOtQjoSSENDu6AACwBg5URZF2aJ8-1HBt3cT8g/edit?usp=sharing")

manual_match_df <- manual_match %>%
  filter(!is.na(pc_code_14)) %>%
  rowwise %>% mutate(
    pc_code_18 = str_pad(pc_code_18, side = "left", pad = "0", width = 7),
    pc_code_14 = str_pad(pc_code_14, side = "left", pad = "0", width = 7),
    `2019_matched_to_2014_IEC_district_code` = str_pad(`2019_matched_to_2014_IEC_district_code`, side = "left", pad = "0", width = 4),
    code_change = ifelse(pc_code_18 != pc_code_14, "YES", "NO")
  ) %>% 
  left_join(dplyr::select(coord_2014, pc_code_14, pc_name_eng_14)) %>%
  dplyr::select(pc_code_18, pc_code_14, pc_name_eng_18, pc_name_eng_14, pc_name_dari_18,
                code_change, matched_by, assessment_status, `2019_matched_to_2014_IEC_district_code`)

known_matches_additional <- 
  known_matches %>% rename(pc_name_dari_18 = pc_name_dari) %>%
  full_join(manual_match_df) %>%
  arrange(`2019_matched_to_2014_IEC_district_code`, pc_code_18)

# fix / confirm duplicate codes for mismatched or consolidated polling center locations

dupe_18 <- known_matches_additional %>% filter(duplicated(pc_code_18) | duplicated(pc_code_18, fromLast = T)) %>%
  mutate(dupe = "2018") %>% group_by(pc_name_eng_18) %>%
  mutate(count = n()) %>%
  mutate(correct = ifelse(count > 2, "MISALIGNED", "MERGER"),
         merger = ifelse(correct == "MERGER", "2014 PCS CONSOLIDATED", NA)
  )

misaligned <- dupe_18 %>% filter(correct == "MISALIGNED") %>%
  left_join(dplyr::select(
    coord_2018, pc_code_18, lat, lon, latlon,)
  ) %>%
  rename(lat_18 = lat, lon_18 = lon, latlon_18 = latlon) %>%
  left_join(dplyr::select(
    coord_2014, pc_code_14, lat, lon, latlon
  )) %>%
  rename(lat_14 = lat, lon_14 = lon, latlon_14 = latlon) %>%
  rowwise %>% mutate(vincenty_distance = distVincentyEllipsoid(c(lat_18, lon_18), c(lat_14, lon_14))) %>%
  group_by(pc_code_18) %>%
  mutate(closer_dist = min(vincenty_distance),
         closer_location = ifelse(vincenty_distance == closer_dist, "YES", "NO"))

known_matches_corrected <- known_matches_additional %>%
  filter(!(pc_code_18 %in% misaligned$pc_code_18)) %>%
  full_join(
    misaligned %>% filter(closer_location == "YES") %>%
      dplyr::select(
        pc_code_18, pc_code_14, pc_name_eng_18, pc_name_eng_14, pc_name_dari_18,
        code_change, matched_by, assessment_status, `2019_matched_to_2014_IEC_district_code`
      )
    ) %>%
  filter(!(pc_code_18 %in% dupe_18$pc_code_18[dupe_18$correct == "MERGER"])) %>%
  full_join(dupe_18 %>%
              filter(correct == "MERGER") %>%
              dplyr::select(
                  pc_code_18, pc_code_14, pc_name_eng_18, pc_name_eng_14, pc_name_dari_18,
                  code_change, matched_by, assessment_status, `2019_matched_to_2014_IEC_district_code`, merger
                  )
            ) %>% 
  arrange(`2019_matched_to_2014_IEC_district_code`, pc_code_18)


dupe_14 <- known_matches_corrected %>%
  filter(duplicated(pc_code_14) | duplicated(pc_code_14, fromLast = T)) %>%
  arrange(pc_code_14) %>%
  mutate(dupe = "2014") %>% group_by(pc_name_eng_14) %>%
  mutate(count = n()) %>%
  mutate(correct = ifelse(count > 2, "MISALIGNED", "MERGER"),
         merger = ifelse(correct == "MERGER", "2014 PCS SPLIT", NA)
  )

splits <- dupe_14 %>% filter(correct == "MERGER") %>%
  left_join(dplyr::select(
    coord_2018, pc_code_18, lat, lon, latlon,)
  ) %>%
  rename(lat_18 = lat, lon_18 = lon, latlon_18 = latlon) %>%
  left_join(dplyr::select(
    coord_2014, pc_code_14, lat, lon, latlon
  )) %>%
  rename(lat_14 = lat, lon_14 = lon, latlon_14 = latlon) %>%
  rowwise %>% mutate(vincenty_distance = distVincentyEllipsoid(c(lat_18, lon_18), c(lat_14, lon_14))) %>%
  group_by(pc_code_14) %>%
  mutate(closer_dist = min(vincenty_distance),
         closer_location = ifelse(vincenty_distance == closer_dist, "YES", "NO"))

splits_not_gis <- splits %>% filter(!(pc_code_14 %in% pc_code_14[matched_by == "GIS MATCH"]))


known_matches_more_correx <- known_matches_corrected %>%
  filter(!(pc_code_14 %in% dupe_14$pc_code_14[dupe_14$correct == "MISALIGNED"])) %>%
  full_join(
    dupe_14 %>% filter(correct == "MISALIGNED" & matched_by == "GIS MATCH") %>%
      dplyr::select(pc_code_18, pc_code_14, pc_name_eng_18, pc_name_eng_14, pc_name_dari_18,
                  code_change, matched_by, assessment_status, `2019_matched_to_2014_IEC_district_code`, merger)
  ) %>%
  filter(!(pc_code_14 %in% splits$pc_code_14)) %>%
  full_join(splits %>% 
              filter(matched_by == "GIS MATCH") %>%
              dplyr::select(
                  pc_code_18, pc_code_14, pc_name_eng_18, pc_name_eng_14, pc_name_dari_18,
                  code_change, matched_by, assessment_status, `2019_matched_to_2014_IEC_district_code`, merger
                  )
  ) %>%
  full_join(splits_not_gis %>%
              filter(closer_location == "YES") %>%
              dplyr::select(
                  pc_code_18, pc_code_14, pc_name_eng_18, pc_name_eng_14, pc_name_dari_18,
                  code_change, matched_by, assessment_status, `2019_matched_to_2014_IEC_district_code`, merger
                  )
            ) %>%
  full_join(splits_not_gis %>%
              filter(is.na(closer_location) & matched_by == "LOCATION NAME AND DISTRICT MATCH") %>%
               dplyr::select(
                  pc_code_18, pc_code_14, pc_name_eng_18, pc_name_eng_14, pc_name_dari_18,
                  code_change, matched_by, assessment_status, `2019_matched_to_2014_IEC_district_code`, merger
                  )
            ) %>%
  arrange(pc_code_18, pc_code_14)

#check for distance between non-GIS matches

known_dist_check <- known_matches_more_correx %>% 
  left_join(dplyr::select(
    coord_2018, pc_code_18, lat, lon, latlon,)
  ) %>%
  rename(lat_18 = lat, lon_18 = lon, latlon_18 = latlon) %>%
  left_join(dplyr::select(
    coord_2014, pc_code_14, lat, lon, latlon
  )) %>%
  rename(lat_14 = lat, lon_14 = lon, latlon_14 = latlon) %>%
  rowwise %>% mutate(vincenty_distance = distVincentyEllipsoid(c(lat_18, lon_18), c(lat_14, lon_14)))

#2018 not matched to 2014 = new since 2014?
#2014 not matched to 2018 = closed?
unmatched_18 <- coord_2018[!(coord_2018$pc_code_18 %in% known_dist_check$pc_code_18), ]
unmatched_14 <- coord_2014[!(coord_2014$pc_code_14 %in% known_dist_check$pc_code_14), ]

#check for distance between remaining 2018 unmatched to 2014 unmatched to see if any are very close

mat <- distm(unmatched_18[2:3], unmatched_14[6:5], fun = distVincentyEllipsoid)

unmatched_18 <- unmatched_18 %>%
  rename(lat_18 = lat,
         lon_18 = lon,
         latlon_18 = latlon)

unmatched_14 <- unmatched_14 %>%
  rename(lat_14 = lat,
         lon_14 = lon,
         latlon_14 = latlon)

nearest_neighbors <- cbind(unmatched_18 %>% dplyr::select(pc_code_18, lat_18, lon_18), unmatched_14[max.col(-mat),])

nearest_neighbors <- nearest_neighbors %>%
  dplyr::select(pc_code_18, lat_18, lon_18, pc_code_14, lat_14, lon_14) %>%
  rowwise %>%
  mutate(vincenty_distance = distVincentyEllipsoid(c(lat_18, lon_18), c(lat_14, lon_14)))

additional_gis <- nearest_neighbors %>% filter(vincenty_distance == 0) %>%
  mutate(matched_by = "GIS MATCH") %>%
  left_join(coord_2018 %>%
              dplyr::select(pc_code_18, pc_name_eng_18, pc_name_dari_18, assessment_status, `2019_matched_to_2014_IEC_district_code`,
                            latlon) %>%
              rename(latlon_18 = latlon)
            ) %>%
  left_join(coord_2014 %>%
              dplyr::select(pc_code_14, pc_name_eng_14, latlon) %>%
              rename(latlon_14 = latlon)) %>%
  mutate(code_change = ifelse(pc_code_18 == pc_code_14, "NO", "YES"))
  
known_matches_expanded <- known_dist_check %>% full_join(additional_gis)

known_matches_expanded$merger[known_matches_expanded$merger == "2014 PCS SPLIT"] <- NA

additional_proximity <- nearest_neighbors %>% filter(vincenty_distance > 0 & vincenty_distance < 71.13) %>%
  mutate(matched_by = "PROXIMITY MATCH") %>%
  left_join(coord_2018 %>%
              dplyr::select(pc_code_18, pc_name_eng_18, pc_name_dari_18, assessment_status, `2019_matched_to_2014_IEC_district_code`,
                            latlon) %>%
              rename(latlon_18 = latlon)
            ) %>%
  left_join(coord_2014 %>%
              dplyr::select(pc_code_14, pc_name_eng_14, latlon) %>%
              rename(latlon_14 = latlon)) %>%
  mutate(code_change = ifelse(pc_code_18 == pc_code_14, "NO", "YES")) %>%
  dplyr::select(pc_code_18, pc_code_14, pc_name_eng_18, pc_name_eng_14, assessment_status, vincenty_distance, everything())

additional_proximity_adds <- additional_proximity %>%
  filter(assessment_status != "New" & assessment_status != "Relocated" & !is.na(assessment_status) &
           !(pc_code_18 %in% c("0101074", "0306104", "0306106", "0306107", "0306111", "0306114", "0310162", "0901003", "0623341"))
         ) %>%
  full_join(additional_proximity %>% filter(pc_code_18 %in% c("1001019", "0101019", "2405144", "2408237", "2909183")))

known_matches_expanded_again <- full_join(known_matches_expanded, additional_proximity_adds)

known_matches_expanded_again$merger[known_matches_expanded_again$pc_code_18 == "1218230"] <- "2014 PCS CONSOLIDATED"

known_matches_expanded_again <- known_matches_expanded_again %>%
  filter(!(pc_code_18 == "1218230" & pc_code_14 == "1218245") & !(pc_code_18 == "1218229" & pc_code_14 == "1218246"))

known_matches_expanded_again <- known_matches_expanded_again %>%
  arrange(pc_code_18)

new_2018 <- coord_2018[!(coord_2018$pc_code_18 %in% known_matches_expanded_again$pc_code_18), ]
dropped_2014 <- coord_2014[!(coord_2014$pc_code_14 %in% known_matches_expanded_again$pc_code_14), ]

reported_but_dropped <- dropped_2014 %>%
  left_join(
    dplyr::select(pc_key_2014,
                  pc_code, prelim_first_round_results_reporting, final_first_round_results_reporting,
                  prelim_run_off_results_reporting, final_run_off_results_reporting) %>%
      rename(pc_code_14 = pc_code)
  ) %>%
  rowwise %>% mutate(reported_but_dropped = ifelse(prelim_first_round_results_reporting == "YES" |
                                                     final_first_round_results_reporting == "YES" |
                                                     prelim_run_off_results_reporting == "YES" |
                                                     final_run_off_results_reporting == "YES",
                                                   "YES", "NO")
                     )

matches_final <- known_matches_expanded_again %>%
  left_join(dplyr::select(coord_2014,
                          pc_code_14, pc_name_dari_14)) %>%
  dplyr::select(
    pc_code_18, pc_code_14, pc_name_eng_18, pc_name_eng_14, pc_name_dari_18, pc_name_dari_14,
    matched_by, code_change, merger, assessment_status, lat_14, lon_14, latlon_14, vincenty_distance
  ) %>%
  rename(coord_14_18_discrepancy_in_meters = vincenty_distance,
         iec_2018_assessment_report_status = assessment_status) %>%
  arrange(pc_code_18)

# matches_final$merger[matches_final$pc_code_14 == "1218246"] <- NA

new_2018_final <- new_2018 %>% dplyr::select(
  `2018_IEC_district_code`, district_sub_code, `2019_matched_to_2014_IEC_district_code`,
  pc_code_18, pc_name_eng_18, pc_name_dari_18, assessment_status) %>%
  mutate(matched_by = "NO MATCH / NEW LOCATION") %>%
  rename(`2018_IEC_district_sub_code` = district_sub_code,
         iec_2018_assessment_report_status = assessment_status) %>%
  arrange(`2018_IEC_district_code`, pc_code_18)

dropped_2014 <- dropped_2014 %>% mutate(matched_by = "NO MATCH / DROPPED FROM 2018 PLANNING")
  
write.csv(matches_final, "./pc_plan/pc_code_matching_2014_2018.csv", row.names = F)
write.csv(new_2018_final, "./pc_plan/new_pcs_2018.csv", row.names = F)
write.csv(dropped_2014, "./pc_plan/dropped_pcs_2014.csv", row.names = F)

# amendments and corrections

match_changes <- pc_code_matching_2014_2018 %>% dplyr::select(
  pc_code_18, pc_code_14, matched_by, code_change, merger
)
match_changes$pc_code_14[match_changes$pc_code_18 == "1214189"] <- "1214304"
match_changes$pc_code_14[match_changes$pc_code_18 == "1401014"] <- "1401046"
match_changes$pc_code_14[match_changes$pc_code_18 == "1401013"] <- "1401026"
match_changes$pc_code_14[match_changes$pc_code_18 == "2711162"] <- NA
match_changes$pc_code_14[match_changes$pc_code_18 == "3202090"] <- NA
match_changes$pc_code_14[match_changes$pc_code_18 == "0614311"] <- "0614518"
match_changes$merger[match_changes$pc_code_14 == "1309209"] <- "2014 PC SPLIT"

match_changes <- match_changes %>%
  full_join(tibble(pc_code_18 = "0614311", pc_code_14 = "0614493", 
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES", merger = "2014 PCS CONSOLIDATED")) %>%
  full_join(tibble(pc_code_18 = "0101137", pc_code_14 = "0101145", 
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "0409152", pc_code_14 = "0409047",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "0506071", pc_code_14 = "0506070",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "0605138", pc_code_14 = "0605514",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "0606155", pc_code_14 = "0606187",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "0801018", pc_code_14 = "0802027",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "1001017", pc_code_14 = "1001019",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "1001028", pc_code_14 = "1001169",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "1309145", pc_code_14 = "1309209",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES", merger = "2014 PC SPLIT")) %>%  
  full_join(tibble(pc_code_18 = "1401028", pc_code_14 = "1401161",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "1507055", pc_code_14 = "1507028",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "1507061", pc_code_14 = "1507021",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "1508068", pc_code_14 = "1508060",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "1515134", pc_code_14 = "1515104",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2202044", pc_code_14 = "2202059",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2306179", pc_code_14 = "2306086",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2403076", pc_code_14 = "2403114",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2404089", pc_code_14 = "2404039",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2405124", pc_code_14 = "2405098",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2406152", pc_code_14 = "2406143",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2406157", pc_code_14 = "2406132",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2406161", pc_code_14 = "2406134",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2406169", pc_code_14 = "2406128",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2407171", pc_code_14 = "2407192",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2407178", pc_code_14 = "2407153",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2407180", pc_code_14 = "2407154",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2407182", pc_code_14 = "2407167",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2407183", pc_code_14 = "2407157",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2407185", pc_code_14 = "2407164",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2407195", pc_code_14 = "2407166",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2407198", pc_code_14 = "2407150",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2407211", pc_code_14 = "2407159",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2408220", pc_code_14 = "2408074",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2408234", pc_code_14 = "2408082",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2409240", pc_code_14 = "2409056",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2409241", pc_code_14 = "2409057",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2409243", pc_code_14 = "2409194",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2409244", pc_code_14 = "2409060",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2409253", pc_code_14 = "2409059",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2409256", pc_code_14 = "2409066",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2701014", pc_code_14 = "2717052",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "2701015", pc_code_14 = "2717015",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "3201032", pc_code_14 = "3202071",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "3203110", pc_code_14 = "3203087",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
  full_join(tibble(pc_code_18 = "3301036", pc_code_14 = "3301023",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES")) %>%
# additional corrections made to match 2014 pcs previously dropped
  full_join(dropped_14_pcs_gis_match) %>%
  full_join(dropped_14_pcs_location_match) %>%
  full_join(tibble(pc_code_18 = "1107137", pc_code_14 = "1107227",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES", merger = "2014 PC SPLIT")) %>%  
  full_join(tibble(pc_code_18 = "1107138", pc_code_14 = "1107227",
                   matched_by = "LOCATION NAME AND DISTRICT MATCH", code_change = "YES", merger = "2014 PC SPLIT")) %>%  
  arrange(pc_code_18)


#dropped_14_pcs_gis_match <- pc_key_2014_updated %>%
#  filter(planned_2014 == "NO") %>%
#  dplyr::select(
#    pc_code, pc_name_eng, lat, lon
#  ) %>%
#  rename(pc_code_14 = pc_code, pc_name_eng_14 = pc_name_eng)


missing_18 <- pc_key_2018$pc_code[!(pc_key_2018$pc_code %in% match_changes$pc_code_18)]
missing_14 <- pc_key_2014$pc_code[!(pc_key_2014$pc_code %in% match_changes$pc_code_14)]

match_changes_gaps <- match_changes %>%
  full_join(pc_key_2018 %>% filter(pc_code %in% missing_18) %>% 
              rename(pc_code_18 = pc_code) %>%
              dplyr::select(pc_code_18)) %>%
  left_join(pc_key_2018 %>% 
              dplyr::select(pc_code, pc_name_eng, pc_name_dari, assessment_status) %>%
              rename(pc_code_18 = pc_code)
            ) %>%
  rename(pc_name_eng_18 = pc_name_eng, 
         pc_name_dari_18 = pc_name_dari, 
         iec_2018_assessment_report_status = assessment_status) %>%
  full_join(pc_key_2014 %>% filter(pc_code %in% missing_14) %>% 
              rename(pc_code_14 = pc_code) %>%
              dplyr::select(pc_code_14)) %>%
  left_join(pc_key_2014 %>% 
              dplyr::select(pc_code, pc_name_eng, pc_name_dari) %>%
              rename(pc_code_14 = pc_code)
            ) %>%
  rename(pc_name_eng_14 = pc_name_eng, 
         pc_name_dari_14 = pc_name_dari) %>%
  arrange(pc_code_18, pc_code_14) %>%
  left_join(coord_2014 %>% 
              dplyr::select(pc_code_14, lat, lon) %>%
              rename(lat_14 = lat, lon_14 = lon)
  ) %>%
  left_join(coord_2018 %>% 
              dplyr::select(pc_code_18, lat, lon) %>%
              rename(lat_18 = lat, lon_18 = lon)
  ) 
  
match_changes_gaps$matched_by[is.na(match_changes_gaps$pc_code_14)] <- "NO MATCH / NEW LOCATION"
match_changes_gaps$matched_by[is.na(match_changes_gaps$pc_code_18)] <- "NO MATCH / DROPPED FROM 2018 PLANNING"

match_changes_gaps <- match_changes_gaps %>% rowwise %>%
  mutate(coord_14_18_discrepancy_in_meters = distVincentyEllipsoid(c(lat_18, lon_18), c(lat_14, lon_14)))

# final proximity check

final_unmatched_18 <- match_changes_gaps %>% filter(matched_by == "NO MATCH / NEW LOCATION") %>%
  dplyr::select(lat_18, lon_18, pc_code_18, pc_name_eng_18)
final_unmatched_14 <- match_changes_gaps %>% filter(matched_by == "NO MATCH / DROPPED FROM 2018 PLANNING") %>%
    dplyr::select(lat_14, lon_14, pc_code_14, pc_name_eng_14)

#check for distance between remaining 2018 unmatched to 2014 unmatched to see if any are very close

final_match <- distm(final_unmatched_18[1:2], final_unmatched_14[1:2], fun = distVincentyEllipsoid)

final_nearest_neighbors <- cbind(final_unmatched_18 %>% 
                                   dplyr::select(pc_code_18, lat_18, lon_18, pc_name_eng_18), 
                                 final_unmatched_14[max.col(-final_match),]) %>%
  rowwise %>%
  mutate(vincenty_distance = distVincentyEllipsoid(c(lat_18, lon_18), c(lat_14, lon_14)))

nearest_neighbors_added <- 
    tibble(
      pc_code_18 = c("0623354", "0623355", "0623356", "3207229", "2703062", "0623343", "0623344", "2301029", "0623338", "0606159",
                     "0623345", "3301028", "1111221", "2401022", "2108172"),
      pc_code_14 = c("0615378", "0615354", "0615353", "3207180", "2703068", "0615359", "0615389", "2301004", "0615388", "0606190",
                     "0615390", "3301025", "1111137", "2401003", "2108230"),
      matched_by = c("GIS MATCH", "GIS MATCH", "GIS MATCH", "LOCATION NAME AND DISTRICT MATCH", 
      "PROXIMITY MATCH", "LOCATION NAME AND DISTRICT MATCH", "LOCATION NAME AND DISTRICT MATCH", "PROXIMITY MATCH", 
      "LOCATION NAME AND DISTRICT MATCH", "PROXIMITY MATCH", "LOCATION NAME AND DISTRICT MATCH", "PROXIMITY MATCH", 
      "PROXIMITY MATCH", "PROXIMITY MATCH", "LOCATION NAME AND DISTRICT MATCH"),
      code_change = c("YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES")
    ) %>% 
    left_join(
    pc_key_2018 %>% 
      dplyr::select(pc_code, pc_name_eng, pc_name_dari, assessment_status) %>%
      rename(pc_code_18 = pc_code) %>%
      rename(pc_name_eng_18 = pc_name_eng, pc_name_dari_18 = pc_name_dari, iec_2018_assessment_report_status = assessment_status)
    ) %>% 
    left_join(
    pc_key_2014 %>% 
      dplyr::select(pc_code, pc_name_eng, pc_name_dari) %>%
      rename(pc_code_14 = pc_code) %>%
      rename(pc_name_eng_14 = pc_name_eng, pc_name_dari_14 = pc_name_dari)
    ) %>% 
    left_join(coord_2014 %>% 
              dplyr::select(pc_code_14, lat, lon) %>%
              rename(lat_14 = lat, lon_14 = lon)
              ) %>%
  left_join(coord_2018 %>% 
              dplyr::select(pc_code_18, lat, lon) %>%
              rename(lat_18 = lat, lon_18 = lon)
  ) %>% rowwise %>% mutate(coord_14_18_discrepancy_in_meters = distVincentyEllipsoid(c(lat_18, lon_18), c(lat_14, lon_14)))

match_changes_final <- match_changes_gaps %>%
  filter(!(pc_code_14 %in% nearest_neighbors_added$pc_code_14)) %>%
  full_join(nearest_neighbors_added) %>%
  filter(!(is.na(pc_code_14) & pc_code_18 %in% nearest_neighbors_added$pc_code_18)) %>%
  arrange(pc_code_18)

match_changes_export <- match_changes_final %>% dplyr::select(-c(lat_18, lon_18))
write.csv(match_changes_export, "./pc_plan/pc_code_matching_2014_2018.csv", row.names = F)
#
# find closest replacement polling center catchment for unmatched centers

catch_18 <- match_changes_final %>% filter(!is.na(lat_18)) %>%
  dplyr::select(lat_18, lon_18, pc_code_18, pc_name_eng_18)
catch_14 <- match_changes_final %>% filter(matched_by == "NO MATCH / DROPPED FROM 2018 PLANNING") %>%
    dplyr::select(lat_14, lon_14, pc_code_14, pc_name_eng_14)

#check for distance between remaining 2018 unmatched to 2014 unmatched to see if any are very close

catch_match <- distm(catch_14[1:2], catch_18[1:2], fun = distVincentyEllipsoid)

catch_nearest_neighbors <- cbind(catch_14 %>% 
                                   dplyr::select(pc_code_14, lat_14, lon_14, pc_name_eng_14), 
                                 catch_18[max.col(-catch_match),]) %>%
  rowwise %>%
  mutate(coord_14_18_discrepancy_in_meters = distVincentyEllipsoid(c(lat_18, lon_18), c(lat_14, lon_14)))

catch_final <- catch_nearest_neighbors %>% 
  dplyr::select(pc_code_14, pc_name_eng_14, pc_code_18, pc_name_eng_18, lat_14, lon_14, coord_14_18_discrepancy_in_meters) %>%
  mutate(matched_by = "NEAREST REPLACEMENT CENTER BY PROXIMITY") %>%
  arrange(coord_14_18_discrepancy_in_meters)

write.csv(catch_final, "./pc_plan/nearest_replacement_center_2014_2018.csv", row.names = F)
