library(XML)
library(tidyverse)
library(rvest)
library(wdman)
library(lubridate)
library(pdftools)
library(readr)
library(tabulizer)
library(gsheet)

setwd("~/Google Drive/GitHub/afghanistan_presidential_election_2019")

# CANDIDATE KEY ---------------------------------------------------------------

candidate_key_2019 <- tibble(
  ballot_position = 1:18,
  candidate_code = c("1038-4-12", "1044-6-3", "1012-32-7", "1015-27-17", "1035-8-11", 
                     "1025-5-16", "1033-4-8", "1055-5-15", "1052-1-14", "1037-1-2",
                     "1042-19-10", "1002-17-1", "2021-1-6", "1003-27-13", "1007-2-18",
                     "1029-4-4", "1058-7-9", "1004-1-5"),
#  candidate_name_eng =
  candidate_name_dari = c("رحمت الله نبيل",
                          "سيد نورالله جليلی",
                          "دکتور فرامرز تمنا",
                          "شيدا محمد ابدالی",
                          "احمد ولی مسعود",
                          "نور رحمان لٻوال",
                          "محمد شهاب حکيمی",
                          "محمد اشرف غنی",
                          "دکتور عبدالله عبدالله",
                          "محمد حکيم تورسن",
                          "گلبدين حکمتيار",
                          "عبداللطيف پدرام",
                          "نورالحق علومی",
                          "حاجی محمد ابراهيم الکوزی",
                          "پوهاند پروفيسور دوکتور غلام فاروق نجرابی",
                          "عنايت الله حفيظ",
                          "محمد حنيف اتمر",
                          "داکتر زلمی رسول"),
#  party_name_dari = 
  candidate_gender = "M",
  past_winners = "NO",
  withdrawn = "NO"
)

candidate_key_2019$past_winners[candidate_key_2019$candidate_code == "1055-5-15"] <- "YES"
candidate_key_2019$withdrawn[candidate_key_2019$candidate_code == "1058-7-9" | candidate_key_2019$candidate_code == "1004-1-5"] <- "YES"

write.csv(candidate_key_2019, "./keyfiles/candidate_key_2019.csv", row.names = F)

# DISTRICT AND PROVINCE KEY ---------------------------------------------------

pc_key_2019 <- read_csv("keyfiles/pc_key_2019.csv")

district_key_2019 <- pc_key_2019 %>% 
  dplyr::select(province_code, province_name_eng, 
                district_code, district_name_eng, district_name_dari, 
                district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari,
                provincial_capital) %>% 
  unique()

write.csv(district_key_2019, "./keyfiles/district_key_2019.csv", row.names = F)

province_constituency_key <- read_csv("~/Google Drive/GitHub/afghanistan_election_results_2018/keyfiles/province_constituency_key.csv")

province_key_2019 <- province_constituency_key %>% 
  dplyr::select(province_code, province_name_eng, province_name_dari, province_name_pashto) %>% 
  filter(!is.na(province_code)) %>%
  unique()

write.csv(province_key_2019, "./keyfiles/province_key_2019.csv", row.names = F)

# DISTRICT ROSETTA ------------------------------------------------------------
rm(list = ls())

# district_key_2004
# district_key_2005
district_key_2009 <- read_csv("past_elections/presidential_2009/keyfiles/district_key_2009.csv")
district_key_2010 <- read_csv("~/Google Drive/GitHub/afghanistan_election_results_2018/past_elections/wj_2010/keyfiles/district_key_2010.csv")
district_key_2014 <- read_csv("past_elections/presidential_2014/keyfiles/district_key_2014.csv")
district_key_2018 <- read_csv("~/Google Drive/GitHub/afghanistan_election_results_2018/keyfiles/district_key.csv")
district_key_2019 <- read_csv("./keyfiles/district_key_2019.csv")

province_key_2004 <- read_csv("past_elections/presidential_2004/keyfiles/province_key_2004.csv")
province_key_2005 <- read_csv("~/Google Drive/GitHub/afghanistan_election_results_2018/past_elections/wj_2005/province_key_2005.csv")
province_key_2009 <- read_csv("past_elections/presidential_2009/keyfiles/province_key_2009.csv")
province_key_2010 <- read_csv("~/Google Drive/GitHub/afghanistan_election_results_2018/past_elections/wj_2010/keyfiles/province_constituency_key_2010.csv")
province_key_2014 <- read_csv("past_elections/presidential_2014/keyfiles/province_key_2014.csv")
province_key_2018 <- read_csv("~/Google Drive/GitHub/afghanistan_election_results_2018/keyfiles/province_constituency_key.csv")
province_key_2019 <- read_csv("./keyfiles/province_key_2019.csv")

district_code_keyfile_2018 <- read_csv("~/Google Drive/GitHub/afghanistan_election_results_2018/district_data/district_code_keyfile.csv")

district_code_keyfile_2019 <- district_code_keyfile_2018 %>% dplyr::select(
  `2018_IEC_province_code`, `2012_AGCHO_province_code`, `2010_IEC_province_code`,
  `2018_IEC_district_code`, `2018_IEC_district_name_eng`,
  matched_to_2012_AGCHO_district_code, `2012_AGCHO_district_code`, `2012_AGCHO_district_name_eng`,
  matched_to_2010_IEC_district_code, `2010_IEC_district_code`, `2010_IEC_district_name_eng`, district_name_dari) %>%
  arrange(`2018_IEC_district_code`)

district_code_keyfile_2019$`2018_IEC_province_code` <- str_pad(district_code_keyfile_2019$`2018_IEC_province_code`, width = 2, side = "left", pad = "0")
district_code_keyfile_2019$`2012_AGCHO_province_code` <- str_pad(district_code_keyfile_2019$`2012_AGCHO_province_code`, width = 2, side = "left", pad = "0")
district_code_keyfile_2019$`2010_IEC_province_code` <- str_pad(district_code_keyfile_2019$`2010_IEC_province_code`, width = 2, side = "left", pad = "0")

district_code_keyfile_2019$`2018_IEC_district_code` <- str_pad(district_code_keyfile_2019$`2018_IEC_district_code`, width = 4, side = "left", pad = "0")
district_code_keyfile_2019$`2012_AGCHO_district_code` <- str_pad(district_code_keyfile_2019$`2012_AGCHO_district_code`, width = 4, side = "left", pad = "0")
district_code_keyfile_2019$`2010_IEC_district_code` <- str_pad(district_code_keyfile_2019$`2010_IEC_district_code`, width = 4, side = "left", pad = "0")
district_code_keyfile_2019$matched_to_2012_AGCHO_district_code <- str_pad(district_code_keyfile_2019$matched_to_2012_AGCHO_district_code, width = 4, side = "left", pad = "0")
district_code_keyfile_2019$matched_to_2010_IEC_district_code <- str_pad(district_code_keyfile_2019$matched_to_2010_IEC_district_code, width = 4, side = "left", pad = "0")
district_code_keyfile_2019$`2018_IEC_district_name_eng` <- str_to_upper(district_code_keyfile_2019$`2018_IEC_district_name_eng`)
district_code_keyfile_2019$`2012_AGCHO_district_name_eng` <- str_to_upper(district_code_keyfile_2019$`2012_AGCHO_district_name_eng`)
district_code_keyfile_2019$`2010_IEC_district_name_eng` <- str_to_upper(district_code_keyfile_2019$`2010_IEC_district_name_eng`)

write.csv(district_code_keyfile_2019, "./district_data/district_code_keyfile_2019.csv", row.names = F)

# manual fixes in final
