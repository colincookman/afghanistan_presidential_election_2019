library(XML)
library(tidyverse)
library(rvest)
library(wdman)
library(lubridate)
library(pdftools)
library(readr)
library(tabulizer)
library(gsheet)
library(tesseract)
tesseract_download("fas")

# CREATE CANDIDATE KEY --------------------------------------------------------
rm(list = ls())

# IEC total results pages again ommitting some candidates from the original ballot, so need to create manually

candidate_code <- c("100_1_1", "101_1_19", "100_1_10", "100_1_25", "101_1_13", "101_1_16", "100_1_24", "100_1_7", "100_1_28", "100_1_4", "100_1_37")

candidate_name_dari <- c(
  "داکتر عبدا عبدا",
  "محمد داود سلطان زوی",
  "عبدالرحیم وردک",
  "عبدالقیوم کرزی",
  "محمد اشرف غنی احمدزی",
  "محمد نادر نعیم",
  "زلمی رسول",
  "انجنیر قطب الدین هلل",
  "محمد شفیق گل آغا شیرزی",
  "پروفیسور عبدرب الرسول سیاف",
  "هدایت امین ارسل"
)

candidate_name_eng <- c("Dr. Abdullah Abdullah", "Mohammad Daoud Sultanzoy", "Abdul Rahim Wardak", "Abdul Qayum Karzai", "Mohammad Ashraf Ghani Ahmadzai", 
                        "Mohammad Nader Naeem", "Zalmai Rasul", "Eng. Qutbudin Hilal", "Mohammad Sahfiq Gul Agha Sherzai", "Abdul Rab Rasul Sayaf", 
                        "Hedayat Amin Arsala")

party_name_dari <- c(
  "جمعیت اسلمی افغانستان",
  "خپلواک/مستقل",
  "خپلواک/مستقل",
  "خپلواک/مستقل",
  "خپلواک/مستقل",
  "خپلواک/مستقل",
  "خپلواک/مستقل",
  "خپلواک/مستقل",
  "خپلواک/مستقل",
  "خپلواک/مستقل",
  "خپلواک/مستقل"
)

candidate_key <- as.data.frame(cbind(candidate_code, candidate_name_eng, candidate_name_dari, party_name_dari)) %>% 
  mutate(ballot_position = row_number(),
         candidate_gender = "M",
         first_round_winner = ifelse(candidate_code == "100_1_1", "YES", "NO"),
         run_off_winner = ifelse(candidate_code == "101_1_13", "YES", "NO"),
         past_winners = "NO") %>% dplyr::select(candidate_code, ballot_position, everything()) %>% arrange(ballot_position)

write.csv(candidate_key, "./past_elections/presidential_2014/keyfiles/candidate_key_2014.csv", row.names = F)

# CREATE PC, DISTRICT< AND PROVINCE KEYs --------------------------------------
rm(list = ls())

pc_import <- pdf_text("./past_elections/presidential_2014/raw/first_round_pc_list.pdf")

pc_text <- toString(pc_import)
pc_text_lines <- read_lines(pc_text)

pdf_convert("./past_elections/presidential_2014/raw/first_round_pc_list.pdf", pages = NULL, filenames = NULL, format = "png")
text <- ocr("./past_elections/presidential_2014/raw/scanned_pc_list_pages/first_round_pc_list_1.png", engine = tesseract("fas"))


pc_plan <- gsheet2tbl("https://docs.google.com/spreadsheets/d/12qCIIf1slwz6gDZtableqAhoehZYcODA1eLOAfrwOkA/edit?usp=sharing")
pc_plan$pc_code <- as.character(str_pad(pc_plan$pc_code, 7, pad = "0", side = "left"))

pc_2014 <- pc_plan %>% dplyr::select(pc_code, lat, lon)
pc_2018 <- pc_gis %>% dplyr::select(pc_code, lat, lon) %>% rename(
  lon_corrected = lat,
  lat_corrected = lon) %>% rename(
  lon = lon_corrected,
  lat = lat_corrected
  ) %>% dplyr::select(pc_code, lat, lon) %>% arrange(pc_code)

matched <- pc_2018$pc_code[pc_2018$pc_code %in% pc_2014$pc_code]
new_pcs <- setdiff(pc_2018$pc_code, pc_2014$pc_code)
dropped_pcs <- setdiff(pc_2014$pc_code, pc_2018$pc_code)

matched_by_code <- pc_2018 %>% filter(pc_code %in% matched) %>% mutate(year = "2018") %>% full_join(
  pc_2014 %>% filter(pc_code %in% matched) %>% mutate(year = "2014")
)

# FIRST ROUND PRELIM AND FINAL RESULTS DATA -----------------------------------
rm(list = ls())


# RUNOFF PRELIM AND POST-AUDIT RESULTS DATA -----------------------------------
rm(list = ls())


# UPDATE PC KEY WITH REPORTING STATUS -----------------------------------------
rm(list = ls())


# COMPLETENESS CHECKS AND SUMMARY STATS ---------------------------------------
rm(list = ls())