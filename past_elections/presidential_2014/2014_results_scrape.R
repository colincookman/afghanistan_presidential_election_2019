library(XML)
library(tidyverse)
library(rvest)
library(wdman)
library(lubridate)
library(pdftools)
library(readr)
library(tabulizer)
library(gsheet)
# library(tesseract)
# tesseract_download("fas")
setwd("~/Google Drive/GitHub/afghanistan_presidential_election_2019")

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

# CREATE PC, DISTRICT, AND PROVINCE KEYs --------------------------------------
rm(list = ls())

# pc_import <- pdf_text("./past_elections/presidential_2014/raw/first_round_pc_list.pdf")
# pc_text <- toString(pc_import)
# pc_text_lines <- read_lines(pc_text)

# pdf_convert("./past_elections/presidential_2014/raw/first_round_pc_list.pdf", pages = NULL, filenames = NULL, format = "png")
# text <- ocr("./past_elections/presidential_2014/raw/scanned_pc_list_pages/first_round_pc_list_1.png", engine = tesseract("fas"))

# currently unable to properly parse the IEC's PC plan due to Dari text encoding issues - relying on FineReader OCR parsing with thanks to Eyal Hanfling

pc_raw <- read.csv("./past_elections/presidential_2014/raw/FineReaderOCR_first_round_pc_list.csv", stringsAsFactors = F, header = F)
colnames(pc_raw) <- c("ps_planned_count", "ps_fem", "ps_male", "pc_location_dari", "pc_name_dari", 
                     "district_or_subdivision_name_dari", "district_name_dari", "pc_code")

pc_key <- pc_raw
pc_key$pc_code <- as.character(str_pad(pc_key$pc_code, 7, pad = "0", side = "left"))
pc_key <- pc_key %>% mutate(
  province_code = str_sub(pc_code, 1,2),
  district_code = str_sub(pc_code, 1,4),
  district_number = str_sub(district_code, 3,4),
  pc_number = str_sub(pc_code, 5,7)
)

nahia <- "ناحيه"
pc_key <- pc_key %>% rowwise %>% mutate(
  district_name_dari = ifelse(district_code == "0101", "كابل", district_or_subdivision_name_dari),
  district_sub_code = ifelse(grepl(nahia, district_or_subdivision_name_dari), 
                             paste0(district_code, "-", trimws(str_split(district_or_subdivision_name_dari, nahia)[[1]][2])), district_code)
)

nahia_01 <- "كابل - ناحيه 01"
nahia_02 <- "كابل - ناحيه 02"
nahia_03 <- "كابل - ناحيه 03"
nahia_04 <- "كابل - ناحيه 04"
nahia_05 <- "كابل - ناحيه 05"
nahia_06 <- "كابل - ناحيه 06"
nahia_07 <- "كابل - ناحيه 07"
nahia_08 <- "كابل - ناحيه 08"
nahia_09 <- "كابل - ناحيه 09"
nahia_10 <- "كابل - ناحيه 10"
nahia_11 <- "كابل - ناحيه 11"
nahia_12 <- "كابل - ناحيه 12"
nahia_13 <- "كابل - ناحيه 13"
nahia_14 <- "كابل - ناحيه 14"
nahia_15 <- "كابل - ناحيه 15"
nahia_16 <- "كابل - ناحيه 16"
nahia_17 <- "كابل - ناحيه 17"
nahia_18 <- "كابل - ناحيه 18"
nahia_21 <- "كابل - ناحيه 21"

pc_key$district_or_subdivision_name_dari[pc_key$district_sub_code == "0101-01"] <- nahia_01
pc_key$district_or_subdivision_name_dari[pc_key$district_sub_code == "0101-02"] <- nahia_02
pc_key$district_or_subdivision_name_dari[pc_key$district_sub_code == "0101-03"] <- nahia_03
pc_key$district_or_subdivision_name_dari[pc_key$district_sub_code == "0101-04"] <- nahia_04
pc_key$district_or_subdivision_name_dari[pc_key$district_sub_code == "0101-05"] <- nahia_05
pc_key$district_or_subdivision_name_dari[pc_key$district_sub_code == "0101-06"] <- nahia_06
pc_key$district_or_subdivision_name_dari[pc_key$district_sub_code == "0101-07"] <- nahia_07
pc_key$district_or_subdivision_name_dari[pc_key$district_sub_code == "0101-08"] <- nahia_08
pc_key$district_or_subdivision_name_dari[pc_key$district_sub_code == "0101-09"] <- nahia_09
pc_key$district_or_subdivision_name_dari[pc_key$district_sub_code == "0101-10"] <- nahia_10
pc_key$district_or_subdivision_name_dari[pc_key$district_sub_code == "0101-11"] <- nahia_11
pc_key$district_or_subdivision_name_dari[pc_key$district_sub_code == "0101-12"] <- nahia_12
pc_key$district_or_subdivision_name_dari[pc_key$district_sub_code == "0101-13"] <- nahia_13
pc_key$district_or_subdivision_name_dari[pc_key$district_sub_code == "0101-14"] <- nahia_14
pc_key$district_or_subdivision_name_dari[pc_key$district_sub_code == "0101-15"] <- nahia_15
pc_key$district_or_subdivision_name_dari[pc_key$district_sub_code == "0101-16"] <- nahia_16
pc_key$district_or_subdivision_name_dari[pc_key$district_sub_code == "0101-17"] <- nahia_17
pc_key$district_or_subdivision_name_dari[pc_key$district_sub_code == "0101-18"] <- nahia_18
pc_key$district_or_subdivision_name_dari[pc_key$district_sub_code == "0101-21"] <- nahia_21

pc_key <- pc_key %>% mutate(
  provincial_capital = ifelse(district_number == "01", "YES", "NO")
  ) %>% dplyr::select(
    province_code, 
    district_code, district_name_dari, district_sub_code, district_or_subdivision_name_dari, provincial_capital,
    pc_code, pc_name_dari, pc_location_dari, 
    ps_planned_count, ps_male, ps_fem
  ) %>% arrange(pc_code)

# GET ENGLISH TRANSLATIONS + PROVINCE NAMES FROM FINAL RESULTS FILE


province_key <- pc_key %>% dplyr::select(province_code, province_name_eng, province_name_dari) %>% unique() %>% arrange(province_code)

district_key <- pc_key %>% dplyr::select(province_code, 
                                              district_code, district_name_eng, district_name_dari, 
                                              district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari,
                                              provincial_capital) %>% unique() %>% arrange(district_code)

write.csv(province_key, "./past_elections/presidential_2014/keyfiles/province_key_2014.csv", row.names = F)
write.csv(district_key, "./past_elections/presidential_2014/keyfiles/district_key_2014.csv", row.names = F)
write.csv(pc_key, "./past_elections/presidential_2014/keyfiles/pc_key_2014.csv", row.names = F)

# correct 2014 pc key to account for PCs dropped from plans but captured by NDI Afghan Open Elections Data project

ndi_first <- read_csv("https://raw.githubusercontent.com/nditech/af-elections-data/master/2014-presidential-firstround/downloads/2014_PollingCenters_First.csv")
ndi_runoff <- read_csv("https://raw.githubusercontent.com/nditech/af-elections-data/master/2014-presidential-runoff/downloads/2014_PollingCenters_Runoff.csv")

# key_missing <- pc_key_2014$pc_code[!(all_data_2014$pc_code %in% pc_key_2014$pc_code)] # none
# ndi_runoff_missing <- ndi_runoff[!(ndi_runoff$pc_code %in% pc_key_2014$pc_code), ] # none
ndi_first_missing <- ndi_first[!(ndi_first$pc_code %in% pc_key_2014$pc_code), ]

pc_key_2014_new <- pc_key_2014
pc_key_2014_new$planned_2014 = "YES"

dropped_pcs <- ndi_first_missing %>%
  dplyr::select(iec_prov_id, iec_district_id, pc_code, pc_name, ps_total, ps_male, ps_female, lat, lon) %>%
  rename(province_code = iec_prov_id, district_code = iec_district_id, 
         pc_name_eng = pc_name, ps_planned_count = ps_total, ps_fem = ps_female) %>%
  mutate(district_code = paste0(province_code, district_code),
         planned_2014 = "NO",
         prelim_first_round_results_reporting = "NO",
         final_first_round_results_reporting = "NO",
         prelim_run_off_results_reporting = "NO",
         final_run_off_results_reporting = "NO") %>%
  left_join(
    unique(dplyr::select(pc_key_2014, province_code, province_name_eng, province_name_dari,
                         district_code, district_name_eng, district_name_dari, provincial_capital))
  ) %>%
  mutate(district_sub_code = district_code,
         district_or_subdivision_name_eng = district_name_eng,
         district_or_subdivision_name_dari = district_name_dari
  ) %>%
  arrange(pc_code)

dropped_pcs$provincial_capital[is.na(dropped_pcs$provincial_capital)] <- "NOORISTAN"
dropped_pcs$province_name_eng[is.na(dropped_pcs$province_name_eng)] <- "NOORISTAN"
dropped_pcs$province_name_dari[is.na(dropped_pcs$province_name_dari)] <- "نورستان"
dropped_pcs$district_name_eng[is.na(dropped_pcs$district_name_eng)] <- "MANDOL"
dropped_pcs$district_name_dari[is.na(dropped_pcs$district_name_dari)] <- "منډول"
dropped_pcs$district_or_subdivision_name_eng[is.na(dropped_pcs$district_or_subdivision_name_eng)] <- "MANDOL"
dropped_pcs$district_or_subdivision_name_dari[is.na(dropped_pcs$district_or_subdivision_name_dari)] <- "منډول"

district_key_2014_new <- district_key_2014 %>%
  full_join(
    tibble(
      province_code = "16",
      district_code = "1607",
      district_sub_code = "1607",
      province_name_eng = "NOORISTAN",
      province_name_dari = "نورستان",
      district_name_eng = "MANDOL",
      district_name_dari = "منډول",
      district_or_subdivision_name_eng = "MANDOL",
      district_or_subdivision_name_dari = "منډول",
      provincial_capital = "NO"
    )
  ) %>%
  arrange(district_code, district_sub_code)

write.csv(district_key_2014_new, "./past_elections/presidential_2014/keyfiles/district_key_2014.csv", row.names = F)

pc_key_2014_updated <- pc_key_2014_new %>%
  full_join(dropped_pcs) %>%
  left_join(dplyr::select(ndi_first, pc_code, pc_location)) %>%
  rename(pc_location_eng = pc_location) %>%
  dplyr::select(1:12, 25, 13:14, 24, everything()
    ) %>%
  arrange(pc_code)
  
write.csv(pc_key_2014_updated, "./past_elections/presidential_2014/keyfiles/pc_key_2014.csv", row.names = F)

# FIRST ROUND PRELIM AND FINAL RESULTS DATA -----------------------------------

# Preliminary results 
rm(list = ls())

prelim_raw <- pdf_text("./past_elections/presidential_2014/raw/first_round_preliminary_by_PS.pdf")
prelim_text <- toString(prelim_raw)
prelim_lines <- read_lines(prelim_text)

alpha <- grep("[a-z]", prelim_lines)
footer_row <- grep("\\/", prelim_lines)
totals <- grep("\\d{6,7} Total", prelim_lines)

text_except_totals <- setdiff(alpha, totals)

prelim_trimmed <- prelim_lines[- c(text_except_totals, footer_row)]

data <- Reduce(rbind, strsplit(trimws(prelim_trimmed), "\\s{2,}"))
rownames(data) <- 1:dim(data)[1]
prelim_data <- data.frame(data)
prelim_data$X2 <- as.numeric(as.character(prelim_data$X2))
prelim_data$X3 <- as.numeric(as.character(prelim_data$X3))
prelim_data$X4 <- as.numeric(as.character(prelim_data$X4))
prelim_data$X5 <- as.numeric(as.character(prelim_data$X5))
prelim_data$X6 <- as.numeric(as.character(prelim_data$X6))
prelim_data$X7 <- as.numeric(as.character(prelim_data$X7))
prelim_data$X8 <- as.numeric(as.character(prelim_data$X8))
prelim_data$X9 <- as.numeric(as.character(prelim_data$X9))
prelim_data$X10 <- as.numeric(as.character(prelim_data$X10))
prelim_data$X11 <- as.numeric(as.character(prelim_data$X11))
prelim_data$X12 <- as.numeric(as.character(prelim_data$X12))
prelim_data$X13 <- as.numeric(as.character(prelim_data$X13))

colnames(prelim_data) <- c("id", "Eng. Qutbudin Hilal", "Dr. Abdullah Abdullah", "Zalmai Rasul", "Abdul Rahim Wardak", "Abdul Qayum Karzai",
                    "Abdul Rab Rasul Sayaf", "Mohammad Ashraf Ghani Ahmadzai", "Mohammad Daoud Sultanzoy", "Mohammad Sahfiq Gul Agha Sherzai",
                    "Mohammad Nader Naeem", "Hedayat Amin Arsala", "total")

pc_starts <- grep("\\d{6,7}", prelim_data$id)
pc_ends <- grep("Total", prelim_data$id)
pc_starts <- setdiff(pc_starts, pc_ends)

row_out <- data.frame()
for(i in 1:length(pc_starts)){
  pc_code = as.character(prelim_data$id[pc_starts[i]])
  ps_reporting_count = (pc_ends[i] - pc_starts[i]) - 1
  for(j in 1:ps_reporting_count){
    row_position <- pc_starts[i] + j
    ps_number = as.character(prelim_data$id[row_position])
    for(k in (2:12)){
    candidate_name_eng = names(prelim_data)[k]
    votes = as.numeric(prelim_data[row_position, k])
    ps_cand_row = cbind(pc_code, ps_number, candidate_name_eng, votes)
    row_out <- rbind(row_out, ps_cand_row)
    }
  }
}

prelim_final <- row_out
prelim_final$pc_code <- as.character(prelim_final$pc_code)
prelim_final$ps_number <- as.character(prelim_final$ps_number)
prelim_final$candidate_name_eng <- as.character(prelim_final$candidate_name_eng)
prelim_final$votes <- as.numeric(as.character(prelim_final$votes))
prelim_final$ps_code <- paste0(prelim_final$pc_code, "-", prelim_final$ps_number)

prelim_final <- prelim_final %>% mutate(
  province_code = str_sub(pc_code, 1,2),
  district_code = str_sub(pc_code, 1,4)) %>% left_join(candidate_key_2014) %>%
  mutate(election_date = mdy("04-05-14"), results_date = mdy("04-26-14"), election_type = "PRESIDENTIAL", results_status = "PRELIMINARY") %>%
  dplyr::select(
    election_date, results_date, election_type, results_status, province_code, district_code, pc_code, ps_code, 
    ballot_position, candidate_code, candidate_name_eng, candidate_name_dari, party_name_dari, candidate_gender, votes, 
    first_round_winner, run_off_winner, past_winners) %>%
  arrange(province_code, district_code, pc_code, ps_code, ballot_position)

write.csv(prelim_final, "./past_elections/presidential_2014/results_data/first_round_preliminary_results/prelim_af_candidate_ps_data_first_round_2014.csv", row.names = F)

prelim_lite <- prelim_final %>% dplyr::select(ps_code, candidate_code, votes)
write.csv(prelim_lite, "./past_elections/presidential_2014/results_data/first_round_preliminary_results/prelim_af_candidate_ps_data_first_round_2014_lite.csv", row.names = F)

# check IEC totals -----------------------------------------------------------------

IEC_totals <- data[grep("Total", data), ]
IEC_totals <- data.frame(IEC_totals)
colnames(IEC_totals) <- c("id", "Eng. Qutbudin Hilal", "Dr. Abdullah Abdullah", "Zalmai Rasul", "Abdul Rahim Wardak", "Abdul Qayum Karzai",
                    "Abdul Rab Rasul Sayaf", "Mohammad Ashraf Ghani Ahmadzai", "Mohammad Daoud Sultanzoy", "Mohammad Sahfiq Gul Agha Sherzai",
                    "Mohammad Nader Naeem", "Hedayat Amin Arsala", "total")
IEC_totals <- IEC_totals %>% rowwise %>% mutate(pc_code = str_split(id, " ")[[1]][1])
IEC_totals$pc_code <- as.character(str_pad(IEC_totals$pc_code, 7, pad = "0", side = "left"))
IEC_totals$`Eng. Qutbudin Hilal` <- as.numeric(as.character(IEC_totals$`Eng. Qutbudin Hilal`))
IEC_totals$`Dr. Abdullah Abdullah` <- as.numeric(as.character(IEC_totals$`Dr. Abdullah Abdullah`))
IEC_totals$`Zalmai Rasul` <- as.numeric(as.character(IEC_totals$`Zalmai Rasul`))
IEC_totals$`Abdul Rahim Wardak` <- as.numeric(as.character(IEC_totals$`Abdul Qayum Karzai`))
IEC_totals$`Abdul Qayum Karzai` <- as.numeric(as.character(IEC_totals$`Eng. Qutbudin Hilal`))
IEC_totals$`Abdul Rab Rasul Sayaf` <- as.numeric(as.character(IEC_totals$`Abdul Rab Rasul Sayaf`))
IEC_totals$`Mohammad Ashraf Ghani Ahmadzai` <- as.numeric(as.character(IEC_totals$`Mohammad Ashraf Ghani Ahmadzai`))
IEC_totals$`Mohammad Daoud Sultanzoy` <- as.numeric(as.character(IEC_totals$`Mohammad Daoud Sultanzoy`))
IEC_totals$`Mohammad Sahfiq Gul Agha Sherzai` <- as.numeric(as.character(IEC_totals$`Mohammad Sahfiq Gul Agha Sherzai`))
IEC_totals$`Mohammad Nader Naeem` <- as.numeric(as.character(IEC_totals$`Mohammad Nader Naeem`))
IEC_totals$`Hedayat Amin Arsala` <- as.numeric(as.character(IEC_totals$`Hedayat Amin Arsala`))
IEC_totals$`total` <- as.numeric(as.character(IEC_totals$`total`))

calc_out <- data.frame()
for(i in 1:length(IEC_totals$pc_code)){
  pc_code = IEC_totals$pc_code[i]
  for(j in 2:13){
    candidate_name_eng = names(IEC_totals)[j]
    IEC_total = as.numeric(IEC_totals[i, j])
    calc_row = cbind(pc_code, candidate_name_eng, IEC_total)
    calc_out <- rbind(calc_out, calc_row)
  }
}
calc_out$IEC_total <- as.numeric(as.character(calc_out$IEC_total))
calc_out$pc_code <- as.character(calc_out$pc_code)
calc_out$candidate_name_eng <- as.character(calc_out$candidate_name_eng)

calculated_totals <- prelim_final %>% group_by(pc_code) %>% summarize(calc_votes = sum(votes), candidate_name_eng = "total") %>% 
  left_join(calc_out) %>% mutate(
    calc_error = ifelse(calc_votes != IEC_total, "ERROR", "OK")
  )

# First round final results ---------------------------------------------------

rm(list = ls())
final_raw <- pdf_text("./past_elections/presidential_2014/raw/first_round_final_by_PS.pdf")

final_text <- toString(final_raw)
final_lines <- read_lines(final_text)


alpha <- grep("[a-z]", final_lines)
dari <- grep("محمد", final_lines)

final_trimmed <- final_lines[-c(alpha, dari)]

data <- Reduce(rbind, strsplit(trimws(final_trimmed), "\\s{2,}"))
rownames(data) <- 1:dim(data)[1]
final_data <- data.frame(data)

# write.csv(final_data, "./past_elections/presidential_2014/raw/final_first_round_raw.csv", row.names = F)
# final_data_clean <- read.csv("./past_elections/presidential_2014/raw/final_first_round_raw.csv", stringsAsFactors = F)

# strip hidden non-numeric characters from columns to allow for conversion from factor

final_data$X1 <- as.numeric(gsub("[^0-9]", "", final_data$X1))
final_data$X2 <- as.numeric(gsub("[^0-9]", "", final_data$X2))
final_data$X3 <- as.numeric(gsub("[^0-9]", "", final_data$X3))
final_data$X4 <- as.numeric(gsub("[^0-9]", "", final_data$X4))
final_data$X5 <- as.numeric(gsub("[^0-9]", "", final_data$X5))
final_data$X6 <- as.numeric(gsub("[^0-9]", "", final_data$X6))
final_data$X7 <- as.numeric(gsub("[^0-9]", "", final_data$X7))
final_data$X8 <- as.numeric(gsub("[^0-9]", "", final_data$X8))
final_data$X9 <- as.numeric(gsub("[^0-9]", "", final_data$X9))
final_data$X10 <- as.numeric(gsub("[^0-9]", "", final_data$X10))
final_data$X11 <- as.numeric(gsub("[^0-9]", "", final_data$X11))
final_data$X12 <- as.numeric(gsub("[^0-9]", "", final_data$X12))
final_data$X13 <- as.numeric(gsub("[^0-9]", "", final_data$X13))

final_data_trimmed <- final_data %>% filter(!is.na(X1))
  
colnames(final_data_trimmed) <- c("id", "Eng. Qutbudin Hilal", "Dr. Abdullah Abdullah", "Zalmai Rasul", "Abdul Rahim Wardak", "Abdul Qayum Karzai",
                    "Abdul Rab Rasul Sayaf", "Mohammad Ashraf Ghani Ahmadzai", "Mohammad Daoud Sultanzoy", "Mohammad Sahfiq Gul Agha Sherzai",
                    "Mohammad Nader Naeem", "Hedayat Amin Arsala", "total")

pc_starts <- grep("\\d{6,7}", final_data_trimmed$id)
  
row_out <- data.frame()
for(i in 1:length(pc_starts)){
  pc_code = as.character(str_pad(final_data_trimmed$id[pc_starts[i]], 7, pad = "0", side = "left"))
  ps_reporting_count = (pc_starts[i+1] - pc_starts[i]) - 1
  for(j in 1:ps_reporting_count){
    row_position <- pc_starts[i] + j
    ps_number = as.character(final_data_trimmed$id[row_position])
    for(k in (2:12)){
    candidate_name_eng = names(final_data_trimmed)[k]
    votes = as.numeric(final_data_trimmed[row_position, k])
    ps_cand_row = cbind(pc_code, ps_number, candidate_name_eng, votes)
    row_out <- rbind(row_out, ps_cand_row)
    }
  }
}

final_first_round <- row_out
final_first_round$pc_code <- as.character(final_first_round$pc_code)
final_first_round$ps_number <- as.character(final_first_round$ps_number)
final_first_round$candidate_name_eng <- as.character(final_first_round$candidate_name_eng)
final_first_round$votes <- as.numeric(as.character(final_first_round$votes))
final_first_round$ps_code <- paste0(final_first_round$pc_code, "-", final_first_round$ps_number)

final_first_round <- final_first_round %>% mutate(
  province_code = str_sub(pc_code, 1,2),
  district_code = str_sub(pc_code, 1,4)) %>% left_join(candidate_key_2014) %>%
  mutate(election_date = mdy("04-05-14"), results_date = mdy("05-15-14"), election_type = "PRESIDENTIAL", results_status = "FINAL") %>%
  dplyr::select(
    election_date, results_date, election_type, results_status, province_code, district_code, pc_code, ps_code, 
    ballot_position, candidate_code, candidate_name_eng, candidate_name_dari, party_name_dari, candidate_gender, votes, 
    first_round_winner, run_off_winner, past_winners) %>%
  arrange(province_code, district_code, pc_code, ps_code, ballot_position)


write.csv(final_first_round, "./past_elections/presidential_2014/results_data/first_round_final_results/final_af_candidate_ps_data_first_round_2014.csv", row.names = F)

final_lite <- final_first_round %>% dplyr::select(ps_code, candidate_code, votes)

write.csv(final_lite, "./past_elections/presidential_2014/results_data/first_round_final_results/final_af_candidate_ps_data_first_round_2014_lite.csv", row.names = F)


# RUNOFF PRELIM RESULTS AND POST-AUDIT DATA --------------------------------------------------
rm(list = ls())

candidate_key_2014 <- read.csv("./past_elections/presidential_2014/keyfiles/candidate_key_2014.csv", stringsAsFactors = F)

pc_key_2014 <- read.csv("./past_elections/presidential_2014/keyfiles/pc_key_2014.csv", stringsAsFactors = F)
pc_key_2014$province_code <- as.character(str_pad(pc_key_2014$province_code, 2, pad = "0", side = "left"))
pc_key_2014$district_code <- as.character(str_pad(pc_key_2014$district_code, 4, pad = "0", side = "left"))
pc_key_2014$pc_code <- as.character(str_pad(pc_key_2014$pc_code, 7, pad = "0", side = "left"))


post_audit_import <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1DfakTAyfrgEsPB7qgPzy2NV4FTUb9mG5kMFi3T9Z7h4/edit?usp=sharing")
post_audit <- post_audit_import
post_audit$X19 <- NULL

colnames(post_audit) <- c("province_name_eng", "province_name_dari", "district_name_eng", "pc_code", "pc_name_eng", "pc_name_dari", "ps_type", "ps_number",
                          "ps_code", "results_barcode", "invalid_ballots", "unused_ballots", "spoiled_ballots", "AA_votes", "AG_votes",
                          "total_valid", "discrepancy", "status", "PA_results_barcode", "PA_invalid_ballots", "PA_unused_ballots", "PA_new_unused_ballots",
                          "PA_spoiled_ballots", "PA_AA_votes", "PA_AG_votes", "PA_total_valid", "PA_discrepancy", "PA_status_detail", "PA_status")

# Preliminary runoff results --------------------------------------------------

runoff_prelim <- post_audit %>% 
  dplyr::select(
    province_name_eng, province_name_dari, district_name_eng,
    pc_code, pc_name_eng, pc_name_dari, ps_number, ps_code, ps_type, status, results_barcode,
    total_valid, AA_votes, AG_votes, unused_ballots, spoiled_ballots, invalid_ballots, discrepancy) %>%
  arrange(pc_code) %>% rowwise %>% mutate(
    results_status = "PRELIMINARY",
    election_type = "PRESIDENTIAL RUNOFF",
    election_date = mdy("06-14-14"),
    results_date = mdy("07-07-14"),
    ps_code = paste0(pc_code, "-", str_pad(ps_number, 2, pad = "0", "left")),
    province_code = str_sub(pc_code, 1, 2),
    district_code = str_sub(pc_code, 1, 4)
  ) %>% dplyr::select(-ps_number) %>% 
  dplyr::select(results_status, election_type, election_date, results_date,
                province_code, province_name_eng, province_name_dari,
                district_code, district_name_eng, everything()) %>%
  arrange(province_code, district_code, pc_code, ps_code)

# write.csv(runoff_prelim, "./past_elections/presidential_2014/raw/raw_runoff_preliminary_results_and_extra_ballots_by_ps.csv", row.names = F)

all_out <- data.frame()
vote_names <- c("Total Valid", "Dr. Abdullah Abdullah", "Mohammad Ashraf Ghani Ahmadzai", "Unused Ballots", "Spoiled Ballots",
                 "Invalid Ballots", "Discrepancy")

for(i in 1:length(runoff_prelim$ps_code)){
 metadata <- runoff_prelim[i, 1:16]
 for(j in 1:7){
  row_out <- data.frame()
  votes = as.numeric(runoff_prelim[i, j+16])
  candidate_name_eng = vote_names[j]
  row_out <- data.frame(c(metadata, candidate_name_eng, votes))
  colnames(row_out) <- c("results_status", "election_type", "election_date", "results_date", "province_code", "province_name_eng", "province_name_dari",
                         "district_code", "district_name_eng", "pc_code", "pc_name_eng", "pc_name_dari", "ps_code", "ps_type",
                         "status", "results_barcode", "candidate_name_eng", "votes")
  all_out <- rbind(all_out, row_out)
  }
}

runoff_prelim_output[] <- lapply(all_out, as.character)
runoff_prelim_output$votes <- as.numeric(runoff_prelim_output$votes)
runoff_prelim_output <- runoff_prelim_output %>% left_join(candidate_key_2014) %>% filter(!is.na(ballot_position))

# runoff_prelim_error_check <- runoff_prelim_output %>% group_by(ps_code) %>% summarize(
#  AA_votes = votes[candidate_name_eng == "Dr. Abdullah Abdullah"],
#  AG_votes = votes[candidate_name_eng == "Mohammad Ashraf Ghani Ahmadzai"],
#  total_votes = votes[candidate_name_eng == "Total Valid"],
#  unused_ballots = votes[candidate_name_eng == "Unused Ballots"],
#  spoiled_ballots = votes[candidate_name_eng == "Spoiled Ballots"],
#  invalid_ballots = votes[candidate_name_eng == "Invalid Ballots"],
#  discrepancy = votes[candidate_name_eng == "Discrepancy"],
#  total_check = ifelse(AA_votes + AG_votes != total_votes, "ERROR", "OK"),
#  discrepancy_check = ifelse((600 - (total_votes + unused_ballots + spoiled_ballots + invalid_ballots) != discrepancy),
#                             "ERROR", "OK")
#  )

runoff_prelim_output <- runoff_prelim_output %>% dplyr::select(
  election_type, election_date, results_status, results_date,
  province_code, province_name_eng, province_name_dari, district_code, district_name_eng,
  pc_code, pc_name_eng, pc_name_dari,
  ps_code, ps_type, 
  ballot_position, candidate_code, candidate_name_eng, candidate_name_dari, party_name_dari, candidate_gender,
  votes, first_round_winner, run_off_winner, past_winners, results_barcode
) %>% arrange(province_code, district_code, pc_code, ps_code, ballot_position)

write.csv(runoff_prelim_output, 
          "./past_elections/presidential_2014/results_data/run_off_preliminary_results/prelim_af_candidate_ps_data_run_off_2014.csv", 
          row.names = F)

runoff_prelim_lite <- runoff_prelim_output %>% dplyr::select(ps_code, candidate_code, votes)

write.csv(runoff_prelim_lite, 
          "./past_elections/presidential_2014/results_data/run_off_preliminary_results/prelim_af_candidate_ps_data_run_off_2014_lite.csv", 
          row.names = F)


# Post-audit runoff results ---------------------------------------------------

runoff_post_audit <- post_audit %>% dplyr::select(province_name_eng, province_name_dari, district_name_eng,
                                              pc_code, pc_name_eng, pc_name_dari, ps_number, ps_code, ps_type, PA_status, PA_results_barcode,
                                              PA_total_valid, PA_AA_votes, PA_AG_votes, PA_unused_ballots, 
                                              PA_spoiled_ballots, PA_invalid_ballots, PA_discrepancy) %>%
  arrange(pc_code) %>% rowwise %>% mutate(
    results_status = "FINAL",
    election_type = "PRESIDENTIAL RUNOFF",
    election_date = mdy("06-14-14"),
    ps_code = paste0(pc_code, "-", str_pad(ps_number, 2, pad = "0", "left")),
    province_code = str_sub(pc_code, 1, 2),
    district_code = str_sub(pc_code, 1, 4)
  ) %>% dplyr::select(-ps_number) %>% 
  dplyr::select(results_status, election_type, election_date,
                province_code, province_name_eng, province_name_dari,
                district_code, district_name_eng, everything()) %>% 
  arrange(province_code, district_code, pc_code, ps_code)


all_out <- data.frame()
vote_names <- c("Total Valid", "Dr. Abdullah Abdullah", "Mohammad Ashraf Ghani Ahmadzai", "Unused Ballots", "Spoiled Ballots",
                 "Invalid Ballots", "Discrepancy")

for(i in 1:length(runoff_post_audit$ps_code)){
 metadata <- runoff_post_audit[i, 1:15]
 for(j in 1:7){
  row_out <- data.frame()
  votes = as.numeric(runoff_post_audit[i, j+15])
  candidate_name_eng = vote_names[j]
  row_out <- data.frame(c(metadata, candidate_name_eng, votes))
  colnames(row_out) <- c("results_status", "election_type", "election_date", 
                         "province_code", "province_name_eng", "province_name_dari",
                         "district_code", "district_name_eng", "pc_code", "pc_name_eng", "pc_name_dari", "ps_code", "ps_type",
                         "status", "results_barcode", "candidate_name_eng", "votes")
  all_out <- rbind(all_out, row_out)
  }
}

runoff_post_audit_output <- all_out
runoff_post_audit_output[] <- lapply(all_out, as.character)
runoff_post_audit_output$votes <- as.numeric(runoff_post_audit_output$votes)
runoff_post_audit_output <- runoff_post_audit_output %>% left_join(candidate_key_2014) %>% filter(!is.na(ballot_position))

runoff_post_audit_output <- runoff_post_audit_output %>% dplyr::select(
  election_type, election_date, results_status,
  province_code, province_name_eng, province_name_dari, district_code, district_name_eng,
  pc_code, pc_name_eng, pc_name_dari,
  ps_code, ps_type, results_barcode, status,
  ballot_position, candidate_code, candidate_name_eng, candidate_name_dari, party_name_dari, candidate_gender,
  votes, first_round_winner, run_off_winner, past_winners, results_barcode
) %>% rename(post_audit_status = status) %>% 
  arrange(province_code, district_code, pc_code, ps_code, ballot_position)

write.csv(runoff_post_audit_output, 
          "./past_elections/presidential_2014/results_data/run_off_final_results/final_af_candidate_ps_data_run_off_2014.csv", 
          row.names = F)

runoff_final_lite <- runoff_post_audit_output %>% dplyr::select(ps_code, post_audit_status, candidate_code, votes)

write.csv(runoff_final_lite, 
          "./past_elections/presidential_2014/results_data/run_off_final_results/final_af_candidate_ps_data_run_off_2014_lite.csv", 
          row.names = F)

# get final results from published pdf to confirm -----------------------------

#target <- ("./past_elections/presidential_2014/raw/runoff_final_VotesByPollingStation.pdf")
#final_runoff_import <- pdf_text(target)
#pdf_string <- toString(final_runoff_import)
#pdf_lines <- read_lines(pdf_string)
#
#header_start <- grep("Votes By Polling Stations", pdf_lines)
#header_end <- grep("Barcode", pdf_lines)
#headers <- list()
#for(j in 1:length(header_start)){
#  distance <- header_start[j]:header_end[j]
#  headers <- c(headers, distance)
#}
#headers <- unlist(headers)
#footer_row <- grep("Page ", pdf_lines)
#
#pdf_text <- pdf_lines[- c(headers, footer_row)]
#
#pdf_trimmed <- gsub("[^0-9 ]", "", pdf_text)
#pdf_trimmed <- gsub(" ", "  ", pdf_trimmed)
#
#data <- Reduce(rbind, strsplit(trimws(pdf_trimmed), "\\s{2,}"))
#rownames(data) <- 1:dim(data)[1]
#data <- as.data.frame(data)
##colnames(data) <- c("pc_code", "ps_number", "barcode_number", "votes")
#
#runoff_post_audit_pdf <- data
#runoff_post_audit_pdf$pc_code <- as.character(runoff_post_audit_pdf$pc_code)
#runoff_post_audit_pdf$ps_number <- as.numeric(as.character(runoff_post_audit_pdf$ps_number))
#runoff_post_audit_pdf$barcode_number <- NULL
#runoff_post_audit_pdf$votes <- as.numeric(as.character(runoff_post_audit_pdf$votes))
#runoff_post_audit_pdf$ps_code = paste0(runoff_post_audit_pdf$pc_code, "-", 
#                                       str_pad(runoff_post_audit_pdf$ps_number, width = 2, side = "left", pad = "0"))
#
# runoff_post_audit_pdf$candidate_code <- NA
# runoff_post_audit_pdf$candidate_code[seq(from = 1, to = length(runoff_post_audit_pdf$ps_code), by = 2)] <- "101_1_1"
# runoff_post_audit_pdf$candidate_code[is.na(runoff_post_audit_pdf$candidate_code)] <-  "101_1_13"

# unable to properly parse, using tabulizer to read pdf instead

tabula_runoff_final_VotesByPollingStation <- read_csv("past_elections/presidential_2014/raw/tabula-runoff_final_VotesByPollingStation.csv")
colnames(tabula_runoff_final_VotesByPollingStation) <- c("province_name_eng", "province_name_dari", 
                                                         "district_name_eng", "district_name_dari",
                                                         "pc_code", "pc_name_eng", "pc_name_dari",
                                                         "candidate_name_dari", "ps_type",
                                                         "ps_type_dari", "ps_number", "results_barcode",
                                                         "votes")
header_rows <- grep("Province", tabula_runoff_final_VotesByPollingStation$province_name_eng)
pdf_results <- tabula_runoff_final_VotesByPollingStation[- header_rows, ]
pdf_results$pc_code <- str_pad(pdf_results$pc_code, 7, pad = "0", "left")
pdf_results$ps_code <- paste0(pdf_results$pc_code, "-", str_pad(pdf_results$ps_number, 2, pad = "0", "left"))

post_audit_ps_invalidated <- unique(runoff_post_audit_output$ps_code[runoff_post_audit_output$post_audit_status == "Invalidated"])
post_audit_ps_not_invalidated <- unique(runoff_post_audit_output$ps_code[runoff_post_audit_output$post_audit_status != "Invalidated"])
pdf_ps <- unique(pdf_results$ps_code)

setdiff(pdf_ps, post_audit_ps_not_invalidated)

# confirmed that published PDF matches dataset (but only reports non-invalidated PS)

# discrepancy check ------
#all_out <- data.frame()
#row_out <- data.frame()
#for(i in 1:length(runoff_post_audit$ps_code)){
# metadata <- runoff_prelim[i, 1:16]
# vote_names <- c("Total Valid", "Dr. Abdullah Abdullah", "Mohammad Ashraf Ghani Ahmadzai", "Unused Ballots", "Spoiled Ballots",
#                 "Invalid Ballots", "Discrepancy")
# for(j in 1:7){
#  votes = as.numeric(runoff_prelim[i, j+16])
#  candidate_name_eng = vote_names[j]
#  row_out <- data.frame(c(metadata, candidate_name_eng, votes))
#  colnames(row_out) <- c("results_status", "election_type", "election_date", "results_date", "province_code", "province_name_eng", "province_name_dari",
#                         "district_code", "district_name_eng", "pc_code", "pc_name_eng", "pc_name_dari", "ps_code", "ps_type",
#                         "status", "results_barcode", "candidate_name_eng", "votes")
#  all_out <- rbind(all_out, row_out)
#  }
#}

# CREATE A DISTRICT KEY -------------------------------------------------------

district_list <- pc_key_2014 %>% 
  dplyr::select(province_code, district_code, district_name_dari,
                district_sub_code, district_or_subdivision_name_dari, provincial_capital) %>%
  unique()


english_names <- final_post_audit %>% dplyr::select(province_code, province_name_eng, province_name_dari,
                                                    district_code, district_name_eng) %>% unique()
english_names <- rbind(english_names, c("02", 
                                        "Kapisa", 
                                        "کاپیسا",
                                        "0207",
                                        "Alasay"))

district_list <- district_list %>% left_join(english_names)

# fix nonstandardized district names

dupe_errors <- district_list[duplicated(district_list$district_code), ]

district_list$district_name_dari[district_list$district_code == "0206"] <- "تكاب"
district_list$district_name_dari[district_list$district_code == "0103"] <- "جهار اسياب"
district_list$district_name_dari[district_list$district_code == "0304"] <- "سشدخلل"
district_list$district_name_dari[district_list$district_code == "0306"] <- "سالنكة"
district_list$district_name_dari[district_list$district_code == "0405"] <- "سيد اباد"
district_list$district_name_dari[district_list$district_code == "0407"] <- "حصه اول بهسود"
district_list$district_name_dari[district_list$district_code == "0409"] <- "مركز بهسود"
district_list$district_name_dari[district_list$district_code == "0502"] <- "بركىبرك"
district_list$district_name_dari[district_list$district_code == "0504"] <- "خوشى"
district_list$district_name_dari[district_list$district_code == "0505"] <- "محمد اغه"
district_list$district_name_dari[district_list$district_code == "0601"] <- "جلال آباد"
district_list$district_name_dari[district_list$district_code == "0602"] <- "بهسود"
district_list$district_name_dari[district_list$district_code == "0604"] <- "چپرهار"
district_list$district_name_dari[district_list$district_code == "0605"] <- "کامه"
district_list$district_name_dari[district_list$district_code == "0606"] <- "کوز کنړ"
district_list$district_name_dari[district_list$district_code == "0608"] <- "خوګیاڼي"
district_list$district_name_dari[district_list$district_code == "0609"] <- "بټي کوټ"
district_list$district_name_dari[district_list$district_code == "0611"] <- "پچیر و اګام"
district_list$district_name_dari[district_list$district_code == "0613"] <- "كوت"
district_list$district_name_dari[district_list$district_code == "0616"] <- "شینوار"
district_list$district_name_dari[district_list$district_code == "0618"] <- "لعل پور"
district_list$district_name_dari[district_list$district_code == "0702"] <- "قرغه یي"
district_list$district_name_dari[district_list$district_code == "0703"] <- "علیشنګ"
district_list$district_name_dari[district_list$district_code == "0808"] <- "آبشار"
district_list$district_name_dari[district_list$district_code == "0909"] <- "جلگه"
district_list$district_name_dari[district_list$district_code == "0913"] <- "خوست و فرنگ"
district_list$district_name_dari[district_list$district_code == "1002"] <- "شیبر"
district_list$district_name_dari[district_list$district_code == "1005"] <- "یکاولنگ"
district_list$district_name_dari[district_list$district_code == "1106"] <- "جغتو"
district_list$district_name_dari[district_list$district_code == "1113"] <- "آب بند"
district_list$district_name_dari[district_list$district_code == "1201"] <- "شرن"
district_list$district_name_dari[district_list$district_code == "1203"] <- "یوسف خیل"
district_list$district_name_dari[district_list$district_code == "1204"] <- "یحی خیل"
district_list$district_name_dari[district_list$district_code == "1205"] <- "سروضه"
district_list$district_name_dari[district_list$district_code == "1212"] <- "زیړوک"
district_list$district_name_dari[district_list$district_code == "1302"] <- "احمد آبا"
district_list$district_name_dari[district_list$district_code == "1303"] <- "زرمت"
district_list$district_name_dari[district_list$district_code == "1306"] <- "سید کرم"
district_list$district_name_dari[district_list$district_code == "1401"] <- "خوست"
district_list$district_name_dari[district_list$district_code == "1404"] <- "تڼي"
district_list$district_name_dari[district_list$district_code == "1411"] <- "سپیره"
district_list$district_name_dari[district_list$district_code == "1413"] <- "ځاځي میدان"
district_list$district_name_dari[district_list$district_code == "1501"] <- "اسعد آباد"
district_list$district_name_dari[district_list$district_code == "1506"] <- "شيكل و شيلتن"
district_list$district_name_dari[district_list$district_code == "1511"] <- "غازی آباد"
district_list$district_name_dari[district_list$district_code == "1601"] <- "پارون"
district_list$district_name_dari[district_list$district_code == "1701"] <- "فیض آباد"
district_list$district_name_dari[district_list$district_code == "1703"] <- "ارغنجخواه"
district_list$district_name_dari[district_list$district_code == "1704"] <- "یفتل سفلا"
district_list$district_name_dari[district_list$district_code == "1713"] <- "شهر بزرگ"
district_list$district_name_dari[district_list$district_code == "1716"] <- "وردوج"
district_list$district_name_dari[district_list$district_code == "1717"] <- "تگاب"
district_list$district_name_dari[district_list$district_code == "1721"] <- "کوف آب"
district_list$district_name_dari[district_list$district_code == "1801"] <- "تالقان"
district_list$district_name_dari[district_list$district_code == "1806"] <- "نمک آب"
district_list$district_name_dari[district_list$district_code == "1812"] <- "دشت قلعه"
district_list$district_name_dari[district_list$district_code == "1815"] <- "درقد"
district_list$district_name_dari[district_list$district_code == "1816"] <- "چا آب"
district_list$district_name_dari[district_list$district_code == "1903"] <- "علی آباد"
district_list$district_name_dari[district_list$district_code == "1904"] <- "خان آباد"
district_list$district_name_dari[district_list$district_code == "2005"] <- "روی دوآب"
district_list$district_name_dari[district_list$district_code == "2112"] <- "شور تیپه"
district_list$district_name_dari[district_list$district_code == "2206"] <- "گوسفندی"
district_list$district_name_dari[district_list$district_code == "2301"] <- "چغچران"
district_list$district_name_dari[district_list$district_code == "2305"] <- "پسابند"
district_list$district_name_dari[district_list$district_code == "2306"] <- "شهرک"
district_list$district_name_dari[district_list$district_code == "2406"] <- "کیتی"
district_list$district_name_dari[district_list$district_code == "2408"] <- "سنگ تخت"
district_list$district_name_dari[district_list$district_code == "2503"] <- "چوره"
district_list$district_name_dari[district_list$district_code == "2504"] <- "شهید حساس"
district_list$district_name_dari[district_list$district_code == "2601"] <- "قلات"
district_list$district_name_dari[district_list$district_code == "2602"] <- "ترنک و جلدک"
district_list$district_name_dari[district_list$district_code == "2603"] <- "شینکې"
district_list$district_name_dari[district_list$district_code == "2607"] <- "دایچوپان"
district_list$district_name_dari[district_list$district_code == "2610"] <- "شملزائی"
district_list$district_name_dari[district_list$district_code == "2704"] <- "پنجوایې"
district_list$district_name_dari[district_list$district_code == "2705"] <- "ژیړی"
district_list$district_name_dari[district_list$district_code == "2711"] <- "سپین بولدک"
district_list$district_name_dari[district_list$district_code == "2801"] <- "شبرغان"
district_list$district_name_dari[district_list$district_code == "2805"] <- "قوش تیپه"
district_list$district_name_dari[district_list$district_code == "2806"] <- "خم آب"
district_list$district_name_dari[district_list$district_code == "2807"] <- "آقچه"
district_list$district_name_dari[district_list$district_code == "2808"] <- "فیض آباد"
district_list$district_name_dari[district_list$district_code == "2810"] <- "قرقین"
district_list$district_name_dari[district_list$district_code == "2811"] <- "درز آب"
district_list$district_name_dari[district_list$district_code == "2902"] <- "پشتون کوت"
district_list$district_name_dari[district_list$district_code == "2905"] <- "بل چراغ"
district_list$district_name_dari[district_list$district_code == "2907"] <- "قیصار"
district_list$district_name_dari[district_list$district_code == "2909"] <- "دولت آباد"
district_list$district_name_dari[district_list$district_code == "3008"] <- "سنگین قلعه"
district_list$district_name_dari[district_list$district_code == "3011"] <- "ریگ خانشین"
district_list$district_name_dari[district_list$district_code == "3101"] <- "قلعه نو"
district_list$district_name_dari[district_list$district_code == "3102"] <- "آبکمری"
district_list$district_name_dari[district_list$district_code == "3206"] <- "پشتون زرغون"
district_list$district_name_dari[district_list$district_code == "3208"] <- "گلران"
district_list$district_name_dari[district_list$district_code == "3210"] <- "کشک کهنه"
district_list$district_name_dari[district_list$district_code == "3214"] <- "شیندند"
district_list$district_name_dari[district_list$district_code == "3215"] <- "فرسی"
district_list$district_name_dari[district_list$district_code == "3216"] <- "چشت شریف"
district_list$district_name_dari[district_list$district_code == "3303"] <- "خاک سفید"
district_list$district_name_dari[district_list$district_code == "3305"] <- "شیب کوه"
district_list$district_name_dari[district_list$district_code == "3311"] <- "پرچمن"
  
district_list <- district_list %>% 
  dplyr::select(- district_or_subdivision_name_dari) %>% 
  unique()

district_list_with_subdivisions <- district_list %>% left_join(
  pc_key_2014 %>% filter(district_code == "0101") %>%
  dplyr::select(district_sub_code, district_or_subdivision_name_dari) %>% unique()
)

district_list_with_subdivisions$district_or_subdivision_name_dari[is.na(district_list_with_subdivisions$district_or_subdivision_name_dari)] <- 
  district_list_with_subdivisions$district_name_dari[is.na(district_list_with_subdivisions$district_or_subdivision_name_dari)]

kabul_subdivisions <- district_list_with_subdivisions %>% filter(district_code == "0101") %>% rowwise %>%
  mutate(
    district_or_subdivision_name_eng = paste0("Kabul Nahia ", str_split(district_sub_code, "-")[[1]][2]))

district_key <- district_list_with_subdivisions %>% filter(district_code != "0101") %>%
  full_join(kabul_subdivisions) %>% arrange(province_code, district_code, district_sub_code) %>% dplyr::select(
    province_code, province_name_eng, province_name_dari,
    district_code, district_name_eng, district_name_dari,
    district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, 
    provincial_capital
  ) %>% 
  mutate(province_name_eng = str_to_upper(province_name_eng),
         district_name_eng = str_to_upper(district_name_eng),
         district_or_subdivision_name_eng = str_to_upper(district_or_subdivision_name_eng))

district_key$district_or_subdivision_name_eng[is.na(district_key$district_or_subdivision_name_eng)] <-
  district_key$district_name_eng[is.na(district_key$district_or_subdivision_name_eng)]

write.csv(district_key, "./past_elections/presidential_2014/keyfiles/district_key_2014.csv", row.names = F)

# CREATE A PROVINCE KEY

province_key <- district_key %>% dplyr::select(province_code, province_name_eng, province_name_dari) %>% unique()
write.csv(province_key, "./past_elections/presidential_2014/keyfiles/province_key_2014.csv", row.names = F)

# UPDATE PC KEY WITH REPORTING STATUS -----------------------------------------
rm(list = ls())

district_key_2014 <- read.csv("./past_elections/presidential_2014/keyfiles/district_key_2014.csv", stringsAsFactors = F)
district_key_2014$district_code <- str_pad(district_key_2014$district_code, 4, pad = "0", "left")
district_key_2014$province_code <- str_pad(district_key_2014$province_code, 2, pad = "0", "left")

pc_key_2014 <- read.csv("./past_elections/presidential_2014/keyfiles/pc_key_2014.csv", stringsAsFactors = F)
pc_key_2014$pc_code <- str_pad(pc_key_2014$pc_code, 7, pad = "0", "left")
pc_key_2014$district_code <- str_pad(pc_key_2014$district_code, 4, pad = "0", "left")
pc_key_2014$province_code <- str_pad(pc_key_2014$province_code, 2, pad = "0", "left")

final_post_audit <- read.csv("./past_elections/presidential_2014/results_data/run_off_final_results/final_af_candidate_ps_data_run_off_2014.csv", stringsAsFactors = F)
final_post_audit$pc_code <- str_pad(final_post_audit$pc_code, 7, pad = "0", "left")
final_post_audit$district_code <- str_pad(final_post_audit$district_code, 4, pad = "0", "left")
final_post_audit$province_code <- str_pad(final_post_audit$province_code, 2, pad = "0", "left")

prelim_post_audit <- read.csv("./past_elections/presidential_2014/results_data/run_off_preliminary_results/prelim_af_candidate_ps_data_run_off_2014.csv", stringsAsFactors = F)
prelim_post_audit$pc_code <- str_pad(prelim_post_audit$pc_code, 7, pad = "0", "left")
prelim_post_audit$district_code <- str_pad(prelim_post_audit$district_code, 4, pad = "0", "left")
prelim_post_audit$province_code <- str_pad(prelim_post_audit$province_code, 2, pad = "0", "left")

final_round_one <- read.csv("./past_elections/presidential_2014/results_data/first_round_final_results/final_af_candidate_ps_data_first_round_2014.csv", stringsAsFactors = F)
final_round_one$pc_code <- str_pad(final_round_one$pc_code, 7, pad = "0", "left")
final_round_one$district_code <- str_pad(final_round_one$district_code, 4, pad = "0", "left")
final_round_one$province_code <- str_pad(final_round_one$province_code, 2, pad = "0", "left")

prelim_round_one <- read.csv("./past_elections/presidential_2014/results_data/first_round_preliminary_results/prelim_af_candidate_ps_data_first_round_2014.csv", stringsAsFactors = F)
prelim_round_one$pc_code <- str_pad(prelim_round_one$pc_code, 7, pad = "0", "left")
prelim_round_one$district_code <- str_pad(prelim_round_one$district_code, 4, pad = "0", "left")
prelim_round_one$province_code <- str_pad(prelim_round_one$province_code, 2, pad = "0", "left")

aodp_first_round_pcs <- read.csv("https://2014.afghanistanelectiondata.org/data/polling/2014_iec_polling_center_locations_all_march31.csv", stringsAsFactors = F)
colnames(aodp_first_round_pcs) <- c("closed", "district_name_eng", "district_number", "province_number", "lat", "lon",
                                    "map_accuracy", "pc_code", "pc_location_eng", "pc_name_eng", "province_name_eng",
                                    "ps_fem", "ps_male", "ps_planned_count", "verification_note")
aodp_first_round_pcs$pc_code <- str_pad(aodp_first_round_pcs$pc_code, 7, pad = "0", "left")

aodp_runoff_pcs <- read.csv("https://2014.afghanistanelectiondata.org/data/polling/iec_runoff_polling_centers_en_jun2014.csv", stringsAsFactors = F)
colnames(aodp_runoff_pcs) <- c("lon", "lat", "ps_male", "pc_name_eng", "pc_code", "province_name_eng", "ps_planned_count",
                               "district_name_eng", "ps_fem", "pc_location_eng", "province_number", "map_accuracy", "district_number")
aodp_runoff_pcs$pc_code <- str_pad(aodp_runoff_pcs$pc_code, 7, pad = "0", "left")

post_audit_import <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1DfakTAyfrgEsPB7qgPzy2NV4FTUb9mG5kMFi3T9Z7h4/edit?usp=sharing")


prelim_first_pcs <- unique(prelim_round_one$pc_code)
final_first_pcs <- unique(final_round_one$pc_code)
prelim_runoff_pcs <- unique(prelim_post_audit$pc_code)
final_runoff_pcs <- unique(final_post_audit$pc_code)
current_pc_key <- unique(pc_key_2014$pc_code)

prelim_first_not_in_key <- setdiff(prelim_first_pcs, current_pc_key)
final_first_not_in_key <- setdiff(final_first_pcs, current_pc_key)
prelim_runoff_not_in_key <- setdiff(prelim_runoff_pcs, current_pc_key)
final_runoff_not_in_key <- setdiff(final_runoff_pcs, current_pc_key)

missing_from_key <- unique(c(prelim_first_not_in_key, final_first_not_in_key, prelim_runoff_not_in_key, final_runoff_not_in_key))

missing_in_final <- final_post_audit %>%
  dplyr::select(province_code, district_code, district_name_eng, pc_code, pc_name_dari) %>% unique() %>%
  filter(pc_code %in% missing_from_key)

# FILL IN MISSING PC KEY INFO

missing_in_final <- missing_in_final %>% mutate(district_sub_code = district_code) %>% 
  dplyr::select(- c(province_code, district_code, district_name_eng))

new_pc_key <- pc_key_2014 %>% dplyr::select(district_sub_code, pc_code, pc_name_dari, pc_location_dari,
                                            ps_planned_count, ps_male, ps_fem) %>% full_join(missing_in_final) %>% 
  arrange(pc_code)

new_pc_key <- left_join(new_pc_key, district_key_2014)

known_pc_names <- final_post_audit %>% dplyr::select(pc_code, pc_name_eng, pc_name_dari) %>% unique()
missing_names_pc <- setdiff(new_pc_key$pc_code, known_pc_names$pc_code)
missing_pc_names <- known_pc_names %>% filter(pc_code %in% missing_names_pc)

new_pc_key_with_known_names <- new_pc_key %>% rename(pdf_name_dari = pc_name_dari) %>% left_join(known_pc_names)
new_pc_key_with_known_names$pc_name_dari[is.na(new_pc_key_with_known_names$pc_name_dari)] <- 
  new_pc_key_with_known_names$pc_name_dari[is.na(new_pc_key_with_known_names$pc_name_dari)]

still_missing_pc_names <- new_pc_key_with_known_names %>% filter(is.na(pc_name_eng) | is.na(pc_name_dari))

from_prelim <- aodp_first_round_pcs %>% filter(pc_code %in% still_missing_pc_names$pc_code) %>% dplyr::select(
  pc_code, pc_name_eng
)

new_pc_key_with_more_names <- new_pc_key_with_known_names %>% filter(pc_code %in% from_prelim$pc_code) %>% 
  dplyr::select(- c(pc_name_eng)) %>%
  left_join(from_prelim) %>%
  mutate(pc_name_dari = pdf_name_dari)

new_additions <- new_pc_key_with_more_names$pc_code
already_known <- new_pc_key_with_known_names$pc_code[!(new_pc_key_with_known_names$pc_code %in% new_additions)]

updated_new_pc_key <- new_pc_key_with_known_names %>% filter(pc_code %in% already_known) %>%
  full_join(new_pc_key_with_more_names) %>% arrange(pc_code)

updated_new_pc_key$pc_name_eng[updated_new_pc_key$pc_code == "2301040"] <- "Istarman Mosque"
updated_new_pc_key$pc_name_dari[updated_new_pc_key$pc_code == "2301040"] <- "مسجد استرمان"

updated_new_pc_key <- updated_new_pc_key %>% dplyr::select(
  province_code, province_name_eng, province_name_dari,
  district_code, district_name_eng, district_name_dari,
  district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, 
  provincial_capital, 
  pc_code, pc_name_eng, pc_name_dari, pc_location_dari, 
  ps_planned_count, ps_male, ps_fem
) %>% arrange(pc_code)

prelim_first_pcs <- data.frame(prelim_first_pcs) %>% mutate(prelim_first_round_results_reporting = "YES") %>%
  rename(pc_code = prelim_first_pcs)

final_first_pcs <- data.frame(final_first_pcs) %>% mutate(final_first_round_results_reporting = "YES") %>%
  rename(pc_code = final_first_pcs)

prelim_runoff_pcs <- data.frame(prelim_runoff_pcs) %>% mutate(prelim_run_off_results_reporting = "YES") %>%
  rename(pc_code = prelim_runoff_pcs)

final_runoff_pcs <- data.frame(final_runoff_pcs) %>% mutate(final_run_off_results_reporting = "YES") %>%
  rename(pc_code = final_runoff_pcs)

updated_new_pc_key <- updated_new_pc_key %>% left_join(prelim_first_pcs) %>%
  left_join(final_first_pcs) %>% left_join(prelim_runoff_pcs) %>% left_join(final_runoff_pcs)

updated_new_pc_key$prelim_first_round_results_reporting[is.na(updated_new_pc_key$prelim_first_round_results_reporting)] <- "NO"
updated_new_pc_key$final_first_round_results_reporting[is.na(updated_new_pc_key$final_first_round_results_reporting)] <- "NO"
updated_new_pc_key$prelim_run_off_results_reporting[is.na(updated_new_pc_key$prelim_run_off_results_reporting)] <- "NO"
updated_new_pc_key$final_run_off_results_reporting[is.na(updated_new_pc_key$final_run_off_results_reporting)] <- "NO"

write.csv(updated_new_pc_key, "./past_elections/presidential_2014/keyfiles/pc_key_2014.csv", row.names = F)

IEC_reported_closed <- read_lines(toString(pdf_text("./past_elections/presidential_2014/raw/runoff_closedPollingStations.pdf")))

IEC_reported_closed_PCs <- as.data.frame(str_match(IEC_reported_closed, "(\\d{6,7})")[,1]) %>% filter(!is.na(.)) %>% unique()
colnames(IEC_reported_closed_PCs) <- "pc_code"

setdiff(IEC_reported_closed_PCs$pc_code, updated_new_pc_key$pc_code[updated_new_pc_key$prelim_run_off_results_reporting == "NO" | 
                                                                      updated_new_pc_key$final_run_off_results_reporting == "NO"])

closed_runoff_PCs_not_in_key <- setdiff(IEC_reported_closed_PCs$pc_code, updated_new_pc_key$pc_code)
closed_runoff_PCs_not_in_key <- data.frame(closed_runoff_PCs_not_in_key)
closed_runoff_PCs_not_in_key$pc_name_eng <- c("Saparay Secondary School", "New Balady School", "Masmod Village Mosque",
                                              "Mosque", "Sarigul", "Khenj Mosque", "Khwaja Gawhar", "Ghalbala", 
                                              "Hotaki Mosque", "Khwaja Aspalan Mosque", "Sharshar Village", "Nagharah Khan Village",
                                              "Kabulzai", "Tarnawe", "Dara Bast Mosque", "Sangbar Mosque")
closed_runoff_PCs_not_in_key$pc_name_dari <- c(
  "ری متوسطه",
  "نيو بالا دی مکتب",
  "د ماسمود د کلي جومات",
  "مسجد",
  "اريگل مسجد",
  "مسجد خينج",
  "خواجه گوهر",
  "غلبله",
  "مسجد هوتکی",
  "مسجد خواجه اسپلان",
  "قريه شرشر",
  "قريه نغاره خان",
  "کابل زی",
  "ترناوی",
  "مسجد دره بست",
  "مسجد سنگبر"
)
# closed_runoff_PCs_not_in_key$pc_location_dari

closed_runoff_PCs_not_in_key$ps_planned_count <- c(2, 2, 3, 3, 2, 3, 4, 3, 2, 5, 2, 3, 2, 2, 3, 3)
closed_runoff_PCs_not_in_key$ps_male <- c(1, 1, 2, 2, 1, 2, 2, 2, 1, 2, 1, 2, 1, 1, 2, 1)
closed_runoff_PCs_not_in_key$ps_fem <- c(1, 1, 1, 1, 1, 1, 2, 1, 1, 3, 1, 1, 1, 1, 1, 2)
closed_runoff_PCs_not_in_key$prelim_first_round_results_reporting <- "NO"
closed_runoff_PCs_not_in_key$final_first_round_results_reporting <- "NO"
closed_runoff_PCs_not_in_key$prelim_run_off_results_reporting <- "NO"
closed_runoff_PCs_not_in_key$final_run_off_results_reporting <- "NO"

pc_key <- read.csv("./past_elections/presidential_2014/keyfiles/pc_key_2014.csv", stringsAsFactors = F)
pc_key$pc_code <- str_pad(pc_key$pc_code, 7, pad = "0", "left")
pc_key$district_code <- str_pad(pc_key$district_code, 4, pad = "0", "left")
pc_key$province_code <- str_pad(pc_key$province_code, 2, pad = "0", "left")

# correct ps number errors from pdf scan

pc_key$ps_male[pc_key$pc_code == "1101034"] <- 7
pc_key$ps_male[pc_key$pc_code == "1114341"] <- 9
pc_key$ps_male[pc_key$pc_code == "2701003"] <- 9
pc_key$ps_male[pc_key$pc_code == "2701018"] <- 7
pc_key$ps_male[pc_key$pc_code == "2801002"] <- 7
pc_key$ps_male[pc_key$pc_code == "3001010"] <- 7
pc_key$ps_fem[pc_key$pc_code == "0601002"] <- 7
pc_key$ps_fem[pc_key$pc_code == "0601004"] <- 7
pc_key$ps_fem[pc_key$pc_code == "0601010"] <- 7
pc_key$ps_fem[pc_key$pc_code == "0601011"] <- 7
pc_key$ps_fem[pc_key$pc_code == "2701029"] <- 7
pc_key$ps_fem[pc_key$pc_code == "3201431"] <- 7
pc_key$ps_planned_count[pc_key$ps_planned_count == "٦"] <- 7
bad_nine <- pc_key$ps_planned_count[pc_key$pc_code == "0101087"]
pc_key$ps_planned_count[pc_key$ps_planned_count == bad_nine] <- 9

pc_key$ps_planned_count <- as.numeric(pc_key$ps_planned_count)
pc_key$ps_male <- as.numeric(pc_key$ps_male)
pc_key$ps_fem <- as.numeric(pc_key$ps_fem)

pc_key_with_closed <- pc_key %>% 
  full_join(
    rename(closed_runoff_PCs_not_in_key, pc_code = closed_runoff_PCs_not_in_key) %>%
      mutate(
        province_code = str_sub(pc_code, 1,2),
        district_code = str_sub(pc_code, 1,4)
        ) %>% left_join(district_key_2014)
    ) %>% arrange(pc_code)

write.csv(pc_key_with_closed, "./past_elections/presidential_2014/keyfiles/pc_key_2014.csv", row.names = F)

# check list of disqualified PS to make sure no ommitted PCs

disqualified_ps <- read_lines(toString(pdf_text("./past_elections/presidential_2014/raw/runoff_PS-Disqualified-By-IEC-&-IECC-Segragated-By-Gender.pdf")))
disqualified_PCs <- as.data.frame(str_match(disqualified_ps, "(\\d{6,7})")[,1]) %>% filter(!is.na(.)) %>% unique()
colnames(disqualified_PCs) <- "pc_code"
disqualified_PCs <- disqualified_PCs %>% arrange(pc_code)

disqualified_not_in_key <- setdiff(disqualified_PCs$pc_code, pc_key_with_closed$pc_code)

# check against Afghan Open Elections Data lists
rm(list = ls())
pc_key <- read.csv("./past_elections/presidential_2014/keyfiles/pc_key_2014.csv", stringsAsFactors = F)
pc_key$pc_code <- str_pad(pc_key$pc_code, 7, pad = "0", "left")
pc_key$district_code <- str_pad(pc_key$district_code, 4, pad = "0", "left")
pc_key$province_code <- str_pad(pc_key$province_code, 2, pad = "0", "left")

aodp_first_round_pcs <- read.csv("https://2014.afghanistanelectiondata.org/data/polling/2014_iec_polling_center_locations_all_march31.csv", stringsAsFactors = F)
colnames(aodp_first_round_pcs) <- c("closed", "district_name_eng", "district_number", "province_number", "lat", "lon",
                                    "map_accuracy", "pc_code", "pc_location_eng", "pc_name_eng", "province_name_eng",
                                    "ps_fem", "ps_male", "ps_planned_count", "verification_note")
aodp_first_round_pcs$pc_code <- str_pad(aodp_first_round_pcs$pc_code, 7, pad = "0", "left")

aodp_runoff_pcs <- read.csv("https://2014.afghanistanelectiondata.org/data/polling/iec_runoff_polling_centers_en_jun2014.csv", stringsAsFactors = F)
colnames(aodp_runoff_pcs) <- c("lon", "lat", "ps_male", "pc_name_eng", "pc_code", "province_name_eng", "ps_planned_count",
                               "district_name_eng", "ps_fem", "pc_location_eng", "province_number", "map_accuracy", "district_number")
aodp_runoff_pcs$pc_code <- str_pad(aodp_runoff_pcs$pc_code, 7, pad = "0", "left")

aodp_pcs <- unique(c(aodp_first_round_pcs$pc_code, aodp_runoff_pcs$pc_code))

AODP_open_not_in_keyfile <- setdiff(aodp_pcs[(aodp_pcs %in% aodp_first_round_pcs$pc_code[aodp_first_round_pcs$closed != "1"])],
                                    pc_key$pc_code)

AODP_closed_not_in_keyfile <- setdiff(aodp_pcs[(aodp_pcs %in% aodp_first_round_pcs$pc_code[aodp_first_round_pcs$closed == "1"])],
                                    pc_key$pc_code)


# AODP missing polling centers came from a now inactive ARCGIS map - probably dropped in security review / pre-election planning
# process as these do not seem to appear in other IEC files, at least that are publicly accessible at this point.
# https://github.com/developmentseed/aodp-data/tree/master/data/2014_president_election/polling-center-locations/first-round/original

# add lat / lon for known PCs

first_gis <- aodp_first_round_pcs %>% dplyr::select(pc_code, lat, lon)
runoff_gis <- aodp_runoff_pcs %>% filter(!(pc_code %in% first_gis$pc_code)) %>% dplyr::select(pc_code, lat, lon)
all_gis <- full_join(first_gis, runoff_gis) %>% arrange(pc_code)

pc_key_with_gis <- left_join(pc_key, all_gis)

write.csv(pc_key_with_gis, "./past_elections/presidential_2014/keyfiles/pc_key_2014.csv", row.names = F)

# PS KEY AND REPORTING / DISQUALIFICATION STATUS ------------------------------------
rm(list = ls())
pc_key_2014 <- read_csv("past_elections/presidential_2014/keyfiles/pc_key_2014.csv")

ps_planned <- data.frame()
for(i in 1:length(pc_key_2014$pc_code)){
  pc_code = as.character(pc_key_2014$pc_code[i])
  ps_planned_count = pc_key_2014$ps_planned_count[i]
  ps_male = pc_key_2014$ps_male[i]
  ps_fem = pc_key_2014$ps_fem[i]
  row_out <- data.frame()
  
  if(!is.na(ps_male) & ps_male > 0){
  for(j in 1:ps_male){
    ps_code = as.character(paste0(pc_code, "-", as.character(str_pad(j, 2, pad = "0", side = "left"))))
    ps_type = "M"
    row = data.frame(pc_code, ps_code, ps_type)
    row_out <- rbind(row_out, row)
  }} else {NULL}
  
  if(!is.na(ps_fem) & ps_fem > 0){
  for(k in 1:ps_fem){
    ps_code = as.character(paste0(pc_code, "-", as.character(str_pad(sum(k, as.numeric(ps_male), na.rm = T), 2, pad = "0", side = "left"))))
    ps_type = "F"
    row = data.frame(pc_code, ps_code, ps_type)
    row_out <- rbind(row_out, row)
  }} else {NULL}
  
  ps_planned <- rbind(ps_planned, row_out)
}

write.csv(ps_planned, "./past_elections/presidential_2014/keyfiles/ps_key_2014.csv", row.names = F)


final_post_audit <- read.csv("./past_elections/presidential_2014/results_data/run_off_final_results/final_af_candidate_ps_data_run_off_2014.csv", stringsAsFactors = F)
final_post_audit$pc_code <- str_pad(final_post_audit$pc_code, 7, pad = "0", "left")
final_post_audit$district_code <- str_pad(final_post_audit$district_code, 4, pad = "0", "left")
final_post_audit$province_code <- str_pad(final_post_audit$province_code, 2, pad = "0", "left")

prelim_post_audit <- read.csv("./past_elections/presidential_2014/results_data/run_off_preliminary_results/prelim_af_candidate_ps_data_run_off_2014.csv", stringsAsFactors = F)
prelim_post_audit$pc_code <- str_pad(prelim_post_audit$pc_code, 7, pad = "0", "left")
prelim_post_audit$district_code <- str_pad(prelim_post_audit$district_code, 4, pad = "0", "left")
prelim_post_audit$province_code <- str_pad(prelim_post_audit$province_code, 2, pad = "0", "left")

final_round_one <- read.csv("./past_elections/presidential_2014/results_data/first_round_final_results/final_af_candidate_ps_data_first_round_2014.csv", stringsAsFactors = F)
final_round_one$pc_code <- str_pad(final_round_one$pc_code, 7, pad = "0", "left")
final_round_one$district_code <- str_pad(final_round_one$district_code, 4, pad = "0", "left")
final_round_one$province_code <- str_pad(final_round_one$province_code, 2, pad = "0", "left")
# correct ps coding error

final_round_one <- final_round_one %>% rowwise %>%
  mutate(ps_code = paste0(pc_code, "-", str_pad(str_split(ps_code, "-")[[1]][2], 2, pad = "0", "left")))

write.csv(final_round_one, "./past_elections/presidential_2014/results_data/first_round_final_results/final_af_candidate_ps_data_first_round_2014.csv", row.names = F)

prelim_round_one <- read.csv("./past_elections/presidential_2014/results_data/first_round_preliminary_results/prelim_af_candidate_ps_data_first_round_2014.csv", stringsAsFactors = F)
prelim_round_one$pc_code <- str_pad(prelim_round_one$pc_code, 7, pad = "0", "left")
prelim_round_one$district_code <- str_pad(prelim_round_one$district_code, 4, pad = "0", "left")
prelim_round_one$province_code <- str_pad(prelim_round_one$province_code, 2, pad = "0", "left")

ps_reporting <- ps_planned %>% left_join(
  dplyr::select(prelim_round_one, ps_code) %>% unique() %>%
    mutate(prelim_first_round_results_reporting = "YES")) %>%
  left_join(
    dplyr::select(final_round_one, ps_code) %>% unique() %>%
      mutate(final_first_round_results_reporting = "YES")) %>%
  left_join(
    dplyr::select(prelim_post_audit, ps_code) %>% unique() %>%
      mutate(prelim_run_off_results_reporting = "YES")) %>%
  left_join(
    dplyr::select(final_post_audit, ps_code, post_audit_status, results_barcode) %>% unique() %>%
      mutate(final_run_off_results_reporting = "YES")
  )

ps_reporting$prelim_first_round_results_reporting[is.na(ps_reporting$prelim_first_round_results_reporting)] <- "NO"
ps_reporting$final_first_round_results_reporting[is.na(ps_reporting$final_first_round_results_reporting)] <- "NO"
ps_reporting$prelim_run_off_results_reporting[is.na(ps_reporting$prelim_run_off_results_reporting)] <- "NO"
ps_reporting$final_run_off_results_reporting[is.na(ps_reporting$final_run_off_results_reporting)] <- "NO"

ps_reporting <- ps_reporting %>% dplyr::select(
  pc_code, ps_code, ps_type, 
  prelim_first_round_results_reporting, final_first_round_results_reporting,
  prelim_run_off_results_reporting, final_run_off_results_reporting, post_audit_status, results_barcode
) %>% arrange(pc_code, ps_code)

write.csv(ps_reporting, "./past_elections/presidential_2014/keyfiles/ps_key_2014.csv", row.names = F)

# RE-AGGREGATE RESULTS
rm(list = ls())

pc_key_2014 <- read.csv("./past_elections/presidential_2014/keyfiles/pc_key_2014.csv", stringsAsFactors = F)
pc_key_2014$pc_code <- str_pad(pc_key_2014$pc_code, 7, pad = "0", "left")
pc_key_2014$district_code <- str_pad(pc_key_2014$district_code, 4, pad = "0", "left")
pc_key_2014$province_code <- str_pad(pc_key_2014$province_code, 2, pad = "0", "left")

ps_key_2014 <- read.csv("./past_elections/presidential_2014/keyfiles/ps_key_2014.csv", stringsAsFactors = F)
ps_key_2014$pc_code <- str_pad(ps_key_2014$pc_code, 7, pad = "0", "left")

district_key_2014 <- read.csv("./past_elections/presidential_2014/keyfiles/district_key_2014.csv", stringsAsFactors = F)
district_key_2014$district_code <- str_pad(district_key_2014$district_code, 4, pad = "0", "left")
district_key_2014$province_code <- str_pad(district_key_2014$province_code, 2, pad = "0", "left")

final_post_audit <- read.csv("./past_elections/presidential_2014/results_data/run_off_final_results/final_af_candidate_ps_data_run_off_2014.csv", stringsAsFactors = F)
final_post_audit$pc_code <- str_pad(final_post_audit$pc_code, 7, pad = "0", "left")
final_post_audit$district_code <- str_pad(final_post_audit$district_code, 4, pad = "0", "left")
final_post_audit$province_code <- str_pad(final_post_audit$province_code, 2, pad = "0", "left")
final_post_audit <- final_post_audit %>% dplyr::select(- c(province_code, province_name_eng, province_name_dari, 
                                                           district_code, district_name_eng,
                                                           pc_name_eng, pc_name_dari)) %>%
  left_join(dplyr::select(
    pc_key_2014, -c(ps_planned_count, ps_male, ps_fem, 
                    prelim_first_round_results_reporting, final_first_round_results_reporting,
                    prelim_run_off_results_reporting, final_run_off_results_reporting,
                    pc_location_dari, lat, lon))) %>% 
  dplyr::select(
    election_type, election_date, results_status,
    province_code, province_name_eng, province_name_dari,
    district_code, district_name_eng, district_name_dari,
    district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital,
    pc_code, pc_name_eng, pc_name_dari,
    everything()
  )

write.csv(final_post_audit,"./past_elections/presidential_2014/results_data/run_off_final_results/final_af_candidate_ps_data_run_off_2014.csv", 
          row.names = F)

prelim_post_audit <- read.csv("./past_elections/presidential_2014/results_data/run_off_preliminary_results/prelim_af_candidate_ps_data_run_off_2014.csv", stringsAsFactors = F)
prelim_post_audit$pc_code <- str_pad(prelim_post_audit$pc_code, 7, pad = "0", "left")
prelim_post_audit$district_code <- str_pad(prelim_post_audit$district_code, 4, pad = "0", "left")
prelim_post_audit$province_code <- str_pad(prelim_post_audit$province_code, 2, pad = "0", "left")

prelim_post_audit <- prelim_post_audit %>% dplyr::select(- c(province_code, province_name_eng, province_name_dari, 
                                                           district_code, district_name_eng,
                                                           pc_name_eng, pc_name_dari)) %>%
  left_join(dplyr::select(
    pc_key_2014, -c(ps_planned_count, ps_male, ps_fem, 
                    prelim_first_round_results_reporting, final_first_round_results_reporting,
                    prelim_run_off_results_reporting, final_run_off_results_reporting,
                    pc_location_dari, lat, lon))) %>% 
  dplyr::select(
    election_type, election_date, results_status, results_date,
    province_code, province_name_eng, province_name_dari,
    district_code, district_name_eng, district_name_dari,
    district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital,
    pc_code, pc_name_eng, pc_name_dari,
    ps_code, ps_type, results_barcode,
    everything()
  )

write.csv(prelim_post_audit,"./past_elections/presidential_2014/results_data/run_off_preliminary_results/prelim_af_candidate_ps_data_run_off_2014.csv", 
          row.names = F)

final_round_one <- read.csv("./past_elections/presidential_2014/results_data/first_round_final_results/final_af_candidate_ps_data_first_round_2014.csv", stringsAsFactors = F)
final_round_one$pc_code <- str_pad(final_round_one$pc_code, 7, pad = "0", "left")
final_round_one$district_code <- str_pad(final_round_one$district_code, 4, pad = "0", "left")
final_round_one$province_code <- str_pad(final_round_one$province_code, 2, pad = "0", "left")

final_round_one <- final_round_one %>% dplyr::select(- c(province_code, district_code)) %>%
  left_join(dplyr::select(
    pc_key_2014, -c(ps_planned_count, ps_male, ps_fem, 
                    prelim_first_round_results_reporting, final_first_round_results_reporting,
                    prelim_run_off_results_reporting, final_run_off_results_reporting,
                    pc_location_dari, lat, lon))) %>% 
  left_join(dplyr::select(
    ps_key_2014, ps_code, ps_type)) %>%
  dplyr::select(
    election_type, election_date, results_status, results_date,
    province_code, province_name_eng, province_name_dari,
    district_code, district_name_eng, district_name_dari,
    district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital,
    pc_code, pc_name_eng, pc_name_dari,
    ps_code, ps_type,
    everything()
  )

write.csv(final_round_one,"./past_elections/presidential_2014/results_data/first_round_final_results/final_af_candidate_ps_data_first_round_2014.csv", 
          row.names = F)

prelim_round_one <- read.csv("./past_elections/presidential_2014/results_data/first_round_preliminary_results/prelim_af_candidate_ps_data_first_round_2014.csv", stringsAsFactors = F)
prelim_round_one$pc_code <- str_pad(prelim_round_one$pc_code, 7, pad = "0", "left")
prelim_round_one$district_code <- str_pad(prelim_round_one$district_code, 4, pad = "0", "left")
prelim_round_one$province_code <- str_pad(prelim_round_one$province_code, 2, pad = "0", "left")

prelim_round_one <- prelim_round_one %>% dplyr::select(- c(province_code, district_code)) %>%
  left_join(dplyr::select(
    pc_key_2014, -c(ps_planned_count, ps_male, ps_fem, 
                    prelim_first_round_results_reporting, final_first_round_results_reporting,
                    prelim_run_off_results_reporting, final_run_off_results_reporting,
                    pc_location_dari, lat, lon))) %>% 
  left_join(dplyr::select(
    ps_key_2014, ps_code, ps_type)) %>%
  dplyr::select(
    election_type, election_date, results_status, results_date,
    province_code, province_name_eng, province_name_dari,
    district_code, district_name_eng, district_name_dari,
    district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital,
    pc_code, pc_name_eng, pc_name_dari,
    ps_code, ps_type,
    everything()
  )

write.csv(prelim_round_one, "./past_elections/presidential_2014/results_data/first_round_preliminary_results/prelim_af_candidate_ps_data_first_round_2014.csv", 
          row.names = F)

# REAGGREGATE RESULTS ---------------------------------------------------------

# PRELIMINARY ROUND ONE -------------------------
# PC level

prelim_round_one_pc <- prelim_round_one %>% 
  group_by(election_type, election_date, results_status, results_date,
           province_code, province_name_eng, province_name_dari,
           district_code, district_name_eng, district_name_dari,
           district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital,
           pc_code, pc_name_eng, pc_name_dari, 
           ballot_position, candidate_code, candidate_name_eng, candidate_name_dari,
           party_name_dari, candidate_gender, first_round_winner, run_off_winner, past_winners
           ) %>% 
  summarize(male_ps_votes = sum(votes[ps_type == "M"], na.rm = T),
            fem_ps_votes = sum(votes[ps_type == "F"], na.rm = T),
            votes = sum(votes, na.rm = T)
            ) %>% arrange(province_code, district_code, pc_code, ballot_position)

write.csv(prelim_round_one_pc, "./past_elections/presidential_2014/results_data/first_round_preliminary_results/prelim_af_candidate_pc_data_first_round_2014.csv", row.names = F)

prelim_round_one_pcs_lite <- prelim_round_one_pc %>% dplyr::select(pc_code, candidate_code, votes) %>% 
  group_by(pc_code, candidate_code) %>% summarize(votes = sum(votes))

write.csv(prelim_round_one_pcs_lite, "./past_elections/presidential_2014/results_data/first_round_preliminary_results/prelim_af_candidate_pc_data_first_round_2014_lite.csv", row.names = F)

# District level
prelim_round_one_district <- prelim_round_one %>% 
  group_by(election_type, election_date, results_status, results_date,
           province_code, province_name_eng, province_name_dari,
           district_code, district_name_eng, district_name_dari,
           district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital,
           ballot_position, candidate_code, candidate_name_eng, candidate_name_dari,
           party_name_dari, candidate_gender, first_round_winner, run_off_winner, past_winners
           ) %>% 
  summarize(male_ps_votes = sum(votes[ps_type == "M"], na.rm = T),
            fem_ps_votes = sum(votes[ps_type == "F"], na.rm = T),
            votes = sum(votes, na.rm = T)
            ) %>% arrange(province_code, district_code, ballot_position)

write.csv(prelim_round_one_district, "./past_elections/presidential_2014/results_data/first_round_preliminary_results/prelim_af_candidate_district_data_first_round_2014.csv", row.names = F)

# Province level
prelim_round_one_province <- prelim_round_one %>% 
  group_by(election_type, election_date, results_status, results_date,
           province_code, province_name_eng, province_name_dari,
           ballot_position, candidate_code, candidate_name_eng, candidate_name_dari,
           party_name_dari, candidate_gender, first_round_winner, run_off_winner, past_winners
           ) %>% 
  summarize(male_ps_votes = sum(votes[ps_type == "M"], na.rm = T),
            fem_ps_votes = sum(votes[ps_type == "F"], na.rm = T),
            votes = sum(votes, na.rm = T)
            ) %>% arrange(province_code, ballot_position)

write.csv(prelim_round_one_district, "./past_elections/presidential_2014/results_data/first_round_preliminary_results/prelim_af_candidate_province_data_first_round_2014.csv", row.names = F)

# FINAL ROUND ONE -------------------------
# PC level

final_round_one_pc <- final_round_one %>% 
  group_by(election_type, election_date, results_status, results_date,
           province_code, province_name_eng, province_name_dari,
           district_code, district_name_eng, district_name_dari,
           district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital,
           pc_code, pc_name_eng, pc_name_dari, 
           ballot_position, candidate_code, candidate_name_eng, candidate_name_dari,
           party_name_dari, candidate_gender, first_round_winner, run_off_winner, past_winners
           ) %>% 
  summarize(male_ps_votes = sum(votes[ps_type == "M"], na.rm = T),
            fem_ps_votes = sum(votes[ps_type == "F"], na.rm = T),
            votes = sum(votes, na.rm = T)
            ) %>% arrange(province_code, district_code, pc_code, ballot_position)

write.csv(final_round_one_pc, "./past_elections/presidential_2014/results_data/first_round_final_results/final_af_candidate_pc_data_first_round_2014.csv", row.names = F)

final_round_one_pcs_lite <- final_round_one_pc %>% dplyr::select(pc_code, candidate_code, votes) %>% 
  group_by(pc_code, candidate_code) %>% summarize(votes = sum(votes))

write.csv(final_round_one_pcs_lite, "./past_elections/presidential_2014/results_data/first_round_final_results/final_af_candidate_pc_data_first_round_2014_lite.csv", row.names = F)

# District level
final_round_one_district <- final_round_one %>% 
  group_by(election_type, election_date, results_status, results_date,
           province_code, province_name_eng, province_name_dari,
           district_code, district_name_eng, district_name_dari,
           district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital,
           ballot_position, candidate_code, candidate_name_eng, candidate_name_dari,
           party_name_dari, candidate_gender, first_round_winner, run_off_winner, past_winners
           ) %>% 
  summarize(male_ps_votes = sum(votes[ps_type == "M"], na.rm = T),
            fem_ps_votes = sum(votes[ps_type == "F"], na.rm = T),
            votes = sum(votes, na.rm = T)
            ) %>% arrange(province_code, district_code, ballot_position)

write.csv(final_round_one_district, "./past_elections/presidential_2014/results_data/first_round_final_results/final_af_candidate_district_data_first_round_2014.csv", row.names = F)

# Province level
final_round_one_province <- final_round_one %>% 
  group_by(election_type, election_date, results_status, results_date,
           province_code, province_name_eng, province_name_dari,
           ballot_position, candidate_code, candidate_name_eng, candidate_name_dari,
           party_name_dari, candidate_gender, first_round_winner, run_off_winner, past_winners
           ) %>% 
  summarize(male_ps_votes = sum(votes[ps_type == "M"], na.rm = T),
            fem_ps_votes = sum(votes[ps_type == "F"], na.rm = T),
            votes = sum(votes, na.rm = T)
            ) %>% arrange(province_code, ballot_position)

write.csv(final_round_one_province, "./past_elections/presidential_2014/results_data/first_round_final_results/final_af_candidate_province_data_first_round_2014.csv", row.names = F)

# PRELIM RUNOFF -------------------------
# PC level

prelim_runoff_pc <- prelim_post_audit %>% 
  group_by(election_type, election_date, results_status, results_date,
           province_code, province_name_eng, province_name_dari,
           district_code, district_name_eng, district_name_dari,
           district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital,
           pc_code, pc_name_eng, pc_name_dari, 
           ballot_position, candidate_code, candidate_name_eng, candidate_name_dari,
           party_name_dari, candidate_gender, first_round_winner, run_off_winner, past_winners
           ) %>% 
  summarize(male_ps_votes = sum(votes[ps_type == "M"], na.rm = T),
            fem_ps_votes = sum(votes[ps_type == "F"], na.rm = T),
            votes = sum(votes, na.rm = T)
            ) %>% arrange(province_code, district_code, pc_code, ballot_position)

write.csv(prelim_runoff_pc, "./past_elections/presidential_2014/results_data/run_off_preliminary_results/prelim_af_candidate_pc_data_run_off_2014.csv", row.names = F)

prelim_runoff_pcs_lite <- prelim_runoff_pc %>% dplyr::select(pc_code, candidate_code, votes) %>% 
  group_by(pc_code, candidate_code) %>% summarize(votes = sum(votes))

write.csv(prelim_runoff_pcs_lite, "./past_elections/presidential_2014/results_data/run_off_preliminary_results/prelim_af_candidate_pc_data_run_off_2014_lite.csv", row.names = F)

# District level
prelim_runoff_district <- prelim_post_audit %>% 
  group_by(election_type, election_date, results_status, results_date,
           province_code, province_name_eng, province_name_dari,
           district_code, district_name_eng, district_name_dari,
           district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital,
           ballot_position, candidate_code, candidate_name_eng, candidate_name_dari,
           party_name_dari, candidate_gender, first_round_winner, run_off_winner, past_winners
           ) %>% 
  summarize(male_ps_votes = sum(votes[ps_type == "M"], na.rm = T),
            fem_ps_votes = sum(votes[ps_type == "F"], na.rm = T),
            votes = sum(votes, na.rm = T)
            ) %>% arrange(province_code, district_code, district_sub_code, ballot_position)

write.csv(prelim_runoff_district, "./past_elections/presidential_2014/results_data/run_off_preliminary_results/prelim_af_candidate_district_data_run_off_2014.csv", row.names = F)

# Province level
prelim_runoff_province <- prelim_post_audit %>% 
  group_by(election_type, election_date, results_status, results_date,
           province_code, province_name_eng, province_name_dari,
           ballot_position, candidate_code, candidate_name_eng, candidate_name_dari,
           party_name_dari, candidate_gender, first_round_winner, run_off_winner, past_winners
           ) %>% 
  summarize(male_ps_votes = sum(votes[ps_type == "M"], na.rm = T),
            fem_ps_votes = sum(votes[ps_type == "F"], na.rm = T),
            votes = sum(votes, na.rm = T)
            ) %>% arrange(province_code, ballot_position)

write.csv(prelim_runoff_province, "./past_elections/presidential_2014/results_data/run_off_preliminary_results/prelim_af_candidate_province_data_run_off_2014.csv", row.names = F)

# FINAL RUNOFF -------------------------
# PC level

final_runoff_pc <- final_post_audit %>% 
  group_by(election_type, election_date, results_status,
           province_code, province_name_eng, province_name_dari,
           district_code, district_name_eng, district_name_dari,
           district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital,
           pc_code, pc_name_eng, pc_name_dari, 
           ballot_position, candidate_code, candidate_name_eng, candidate_name_dari,
           party_name_dari, candidate_gender, first_round_winner, run_off_winner, past_winners
           ) %>% 
  summarize(male_ps_valid_votes = sum(ifelse(ps_type == "M" & post_audit_status != "Invalidated", votes, 0), na.rm = T),
            fem_ps_valid_votes = sum(ifelse(ps_type == "F" & post_audit_status != "Invalidated", votes, 0), na.rm = T),
            male_ps_invalid_votes = sum(ifelse(ps_type == "M" & post_audit_status == "Invalidated", votes, 0), na.rm = T),
            fem_ps_invalid_votes = sum(ifelse(ps_type == "F" & post_audit_status == "Invalidated", votes, 0), na.rm = T),
            votes = sum(c(male_ps_valid_votes, fem_ps_valid_votes)),
            total_invalid_votes = sum(c(male_ps_invalid_votes, fem_ps_invalid_votes), na.rm = T)
            ) %>% arrange(province_code, district_code, district_sub_code, pc_code, ballot_position)

write.csv(final_runoff_pc, "./past_elections/presidential_2014/results_data/run_off_final_results/final_af_candidate_pc_data_run_off_2014.csv", row.names = F)

final_runoff_pcs_lite <- final_runoff_pc %>% dplyr::select(pc_code, candidate_code, votes) %>% 
  group_by(pc_code, candidate_code) %>% summarize(votes = sum(votes))

write.csv(final_runoff_pcs_lite, "./past_elections/presidential_2014/results_data/run_off_final_results/final_af_candidate_pc_data_run_off_2014_lite.csv", row.names = F)

# District level
final_runoff_district <- final_post_audit %>% 
  group_by(election_type, election_date, results_status,
           province_code, province_name_eng, province_name_dari,
           district_code, district_name_eng, district_name_dari,
           district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital,
           ballot_position, candidate_code, candidate_name_eng, candidate_name_dari,
           party_name_dari, candidate_gender, first_round_winner, run_off_winner, past_winners
           ) %>% 
  summarize(male_ps_valid_votes = sum(votes[ps_type == "M" & post_audit_status != "Invalidated"], na.rm = T),
            fem_ps_valid_votes = sum(votes[ps_type == "F" & post_audit_status != "Invalidated"], na.rm = T),
            male_ps_invalid_votes = sum(votes[ps_type == "M" & post_audit_status == "Invalidated"], na.rm = T),
            fem_ps_invalid_votes = sum(votes[ps_type == "F" & post_audit_status == "Invalidated"], na.rm = T),
            votes = sum(c(male_ps_valid_votes, fem_ps_valid_votes)),
            total_invalid_votes = sum(c(male_ps_invalid_votes, fem_ps_invalid_votes), na.rm = T)
            ) %>% arrange(province_code, district_code, district_sub_code, ballot_position)

write.csv(final_runoff_district, "./past_elections/presidential_2014/results_data/run_off_final_results/final_af_candidate_district_data_run_off_2014.csv", row.names = F)

# Province level
final_runoff_province <- final_post_audit %>% 
  group_by(election_type, election_date, results_status,
           province_code, province_name_eng, province_name_dari,
           ballot_position, candidate_code, candidate_name_eng, candidate_name_dari,
           party_name_dari, candidate_gender, first_round_winner, run_off_winner, past_winners
           ) %>% 
  summarize(male_ps_valid_votes = sum(votes[ps_type == "M" & post_audit_status != "Invalidated"], na.rm = T),
            fem_ps_valid_votes = sum(votes[ps_type == "F" & post_audit_status != "Invalidated"], na.rm = T),
            male_ps_invalid_votes = sum(votes[ps_type == "M" & post_audit_status == "Invalidated"], na.rm = T),
            fem_ps_invalid_votes = sum(votes[ps_type == "F" & post_audit_status == "Invalidated"], na.rm = T),
            votes = sum(c(male_ps_valid_votes, fem_ps_valid_votes)),
            total_invalid_votes = sum(c(male_ps_invalid_votes, fem_ps_invalid_votes), na.rm = T)
            ) %>% arrange(province_code, ballot_position)

write.csv(final_runoff_province, "./past_elections/presidential_2014/results_data/run_off_final_results/final_af_candidate_province_data_run_off_2014.csv", row.names = F)

# UPDATE AND CORRECT THE PS KEY -----------------------------------------------------------------

# correct errors in prelim round one

prelim_round_one_corrected <- prelim_round_one %>% rowwise %>% 
  mutate(ps_code = paste0(pc_code, "-", str_pad(str_split(ps_code, "-")[[1]][2], width = 2, side = "left", pad = "0"))) %>% 
  dplyr::select(- ps_type) %>%
  left_join(dplyr::select(ps_key_2014, ps_code, ps_type)) %>% dplyr::select(
    election_type, election_date, results_status, results_date,
    province_code, province_name_eng, province_name_dari,
    district_code, district_name_eng, district_name_dari,
    district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital,
    pc_code, pc_name_eng, pc_name_dari,
    ps_code, ps_type, everything()
  )

missing_type <- prelim_round_one_corrected %>% dplyr::filter(is.na(ps_type)) %>% dplyr::select(ps_code) %>% unique()
missing_type$planned_ps <- "NO"
missing_type <- missing_type %>% rowwise() %>% mutate(pc_code = str_split(ps_code, "-")[[1]][1])
ps_key_2014_with_added <- ps_key_2014 %>% full_join(missing_type) %>% dplyr::select(pc_code, ps_code, planned_ps, everything())
ps_key_2014_with_added$planned_ps[is.na(ps_key_2014_with_added$planned_ps)] <- "YES"


ps_key_2014_updated_reporting <- ps_key_2014_with_added %>% 
  dplyr::select(- c(prelim_first_round_results_reporting, final_first_round_results_reporting,
                    prelim_run_off_results_reporting, final_run_off_results_reporting)) %>%
  left_join(
  dplyr::select(prelim_round_one, ps_code) %>% unique() %>%
    mutate(prelim_first_round_results_reporting = "YES")) %>%
  left_join(
    dplyr::select(final_round_one, ps_code) %>% unique() %>%
      mutate(final_first_round_results_reporting = "YES")) %>%
  left_join(
    dplyr::select(prelim_post_audit, ps_code) %>% unique() %>%
      mutate(prelim_run_off_results_reporting = "YES")) %>%
  left_join(
    dplyr::select(final_post_audit, ps_code, post_audit_status, results_barcode) %>% unique() %>%
      mutate(final_run_off_results_reporting = "YES")
  )

ps_key_2014_updated_reporting$prelim_first_round_results_reporting[is.na(ps_key_2014_updated_reporting$prelim_first_round_results_reporting)] <- "NO"
ps_key_2014_updated_reporting$final_first_round_results_reporting[is.na(ps_key_2014_updated_reporting$final_first_round_results_reporting)] <- "NO"
ps_key_2014_updated_reporting$prelim_run_off_results_reporting[is.na(ps_key_2014_updated_reporting$prelim_run_off_results_reporting)] <- "NO"
ps_key_2014_updated_reporting$final_run_off_results_reporting[is.na(ps_key_2014_updated_reporting$final_run_off_results_reporting)] <- "NO"

# nope! still wrong / gaps

type_and_status_from_audit <- final_post_audit %>% dplyr::select(ps_code, ps_type, post_audit_status, results_barcode) %>% unique()
type_and_status_from_audit$ps_type <- gsub("Male", "M", type_and_status_from_audit$ps_type)
type_and_status_from_audit$ps_type <- gsub("Female", "F", type_and_status_from_audit$ps_type)
type_and_status_from_audit <- type_and_status_from_audit %>% rowwise() %>% mutate(pc_code = str_split(ps_code, "-")[[1]][1])

new_key <- ps_key_2014_updated_reporting %>% dplyr::select(pc_code, ps_code, ps_type, post_audit_status, results_barcode) %>%
  filter(!(ps_code %in% type_and_status_from_audit$ps_code)) %>%
  full_join(type_and_status_from_audit) %>% arrange(pc_code, ps_code)

new_key_with_reporting <- new_key %>% 
  left_join(
  dplyr::select(prelim_round_one, ps_code) %>% unique() %>%
    mutate(prelim_first_round_results_reporting = "YES")) %>%
  left_join(
    dplyr::select(final_round_one, ps_code) %>% unique() %>%
      mutate(final_first_round_results_reporting = "YES")) %>%
  left_join(
    dplyr::select(prelim_post_audit, ps_code) %>% unique() %>%
      mutate(prelim_run_off_results_reporting = "YES")) %>%
  left_join(
    dplyr::select(final_post_audit, ps_code, post_audit_status, results_barcode) %>% unique() %>%
      mutate(final_run_off_results_reporting = "YES")
  )

new_key_with_reporting$prelim_first_round_results_reporting[is.na(new_key_with_reporting$prelim_first_round_results_reporting)] <- "NO"
new_key_with_reporting$final_first_round_results_reporting[is.na(new_key_with_reporting$final_first_round_results_reporting)] <- "NO"
new_key_with_reporting$prelim_run_off_results_reporting[is.na(new_key_with_reporting$prelim_run_off_results_reporting)] <- "NO"
new_key_with_reporting$final_run_off_results_reporting[is.na(new_key_with_reporting$final_run_off_results_reporting)] <- "NO"

ps_planned <- data.frame()
for(i in 1:length(pc_key_2014$pc_code)){
  pc_code = as.character(pc_key_2014$pc_code[i])
  ps_planned_count = pc_key_2014$ps_planned_count[i]
  ps_male = pc_key_2014$ps_male[i]
  ps_fem = pc_key_2014$ps_fem[i]
  row_out <- data.frame()
  
  if(!is.na(ps_male) & ps_male > 0){
  for(j in 1:ps_male){
    ps_code = as.character(paste0(pc_code, "-", as.character(str_pad(j, 2, pad = "0", side = "left"))))
    ps_type = "M"
    row = data.frame(pc_code, ps_code, ps_type)
    row_out <- rbind(row_out, row)
  }} else {NULL}
  
  if(!is.na(ps_fem) & ps_fem > 0){
  for(k in 1:ps_fem){
    ps_code = as.character(paste0(pc_code, "-", as.character(str_pad(sum(k, as.numeric(ps_male), na.rm = T), 2, pad = "0", side = "left"))))
    ps_type = "F"
    row = data.frame(pc_code, ps_code, ps_type)
    row_out <- rbind(row_out, row)
  }} else {NULL}
  
  ps_planned <- rbind(ps_planned, row_out)
}

new_key_with_reporting <- new_key_with_reporting %>% left_join(
  dplyr::select(ps_planned, ps_code) %>% mutate(planned_ps = "YES")
)
new_key_with_reporting$planned_ps[is.na(new_key_with_reporting$planned_ps)] <- "NO"

new_key_with_reporting <- new_key_with_reporting %>% dplyr::select(
  pc_code, ps_code, ps_type, planned_ps, everything()) %>%
  rename(ps_planned_before_first_round = planned_ps)

write.csv(new_key_with_reporting, "./past_elections/presidential_2014/keyfiles/ps_key_2014.csv", row.names = F)

# FIX THE PS TYPE INFO IN PS-LEVEL FILES -------------------------------------------------

prelim_round_one_corrected_again <- prelim_round_one_corrected %>% 
  dplyr::select(- ps_type) %>%
  left_join(dplyr::select(new_key_with_reporting, ps_code, ps_type)) %>%
  left_join(dplyr::select(ps_key_2014, ps_code, ps_type)) %>% dplyr::select(
    election_type, election_date, results_status, results_date,
    province_code, province_name_eng, province_name_dari,
    district_code, district_name_eng, district_name_dari,
    district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital,
    pc_code, pc_name_eng, pc_name_dari,
    ps_code, ps_type, everything()
  )

write.csv(prelim_round_one_corrected_again, "./past_elections/presidential_2014/results_data/first_round_preliminary_results/prelim_af_candidate_ps_data_first_round_2014.csv", row.names = F)

final_round_one_corrected <- final_round_one %>% 
  dplyr::select(- ps_type) %>%
  left_join(dplyr::select(new_key_with_reporting, ps_code, ps_type)) %>%
  left_join(dplyr::select(ps_key_2014, ps_code, ps_type)) %>% dplyr::select(
    election_type, election_date, results_status, results_date,
    province_code, province_name_eng, province_name_dari,
    district_code, district_name_eng, district_name_dari,
    district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital,
    pc_code, pc_name_eng, pc_name_dari,
    ps_code, ps_type, everything()
  )

write.csv(final_round_one_corrected, "./past_elections/presidential_2014/results_data/first_round_final_results/final_af_candidate_ps_data_first_round_2014.csv", row.names = F)

prelim_runoff_corrected <- prelim_post_audit %>% 
  dplyr::select(- ps_type) %>%
  left_join(dplyr::select(new_key_with_reporting, ps_code, ps_type)) %>%
  left_join(dplyr::select(ps_key_2014, ps_code, ps_type)) %>% dplyr::select(
    election_type, election_date, results_status, results_date,
    province_code, province_name_eng, province_name_dari,
    district_code, district_name_eng, district_name_dari,
    district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital,
    pc_code, pc_name_eng, pc_name_dari,
    ps_code, ps_type, everything()
  )

write.csv(prelim_runoff_corrected, "./past_elections/presidential_2014/results_data/run_off_preliminary_results/prelim_af_candidate_ps_data_run_off_2014.csv", row.names = F)

final_runoff_corrected <- final_post_audit %>% 
  dplyr::select(- ps_type) %>%
  left_join(dplyr::select(new_key_with_reporting, ps_code, ps_type)) %>%
  left_join(dplyr::select(ps_key_2014, ps_code, ps_type)) %>% dplyr::select(
    election_type, election_date, results_status,
    province_code, province_name_eng, province_name_dari,
    district_code, district_name_eng, district_name_dari,
    district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital,
    pc_code, pc_name_eng, pc_name_dari,
    ps_code, ps_type, everything()
  )

write.csv(final_runoff_corrected, "./past_elections/presidential_2014/results_data/run_off_final_results/final_af_candidate_ps_data_run_off_2014.csv", row.names = F)


# RE-AGGREGATE THE RESULTS AGAIN, PER ABOVE



# FINAL COMPLETENESS CHECKS AND SUMMARY STATS ---------------------------------------
rm(list = ls())
pc_key_2014 <- read.csv("./past_elections/presidential_2014/keyfiles/pc_key_2014.csv", stringsAsFactors = F)
pc_key_2014$pc_code <- str_pad(pc_key_2014$pc_code, 7, pad = "0", "left")
pc_key_2014$district_code <- str_pad(pc_key_2014$district_code, 4, pad = "0", "left")
pc_key_2014$province_code <- str_pad(pc_key_2014$province_code, 2, pad = "0", "left")

ps_key_2014 <- read.csv("./past_elections/presidential_2014/keyfiles/ps_key_2014.csv", stringsAsFactors = F)
ps_key_2014$pc_code <- str_pad(ps_key_2014$pc_code, 7, pad = "0", "left")

district_key_2014 <- read.csv("./past_elections/presidential_2014/keyfiles/district_key_2014.csv", stringsAsFactors = F)
district_key_2014$district_code <- str_pad(district_key_2014$district_code, 4, pad = "0", "left")
district_key_2014$province_code <- str_pad(district_key_2014$province_code, 2, pad = "0", "left")

final_post_audit <- read.csv("./past_elections/presidential_2014/results_data/run_off_final_results/final_af_candidate_ps_data_run_off_2014.csv", stringsAsFactors = F)
final_post_audit$pc_code <- str_pad(final_post_audit$pc_code, 7, pad = "0", "left")
final_post_audit$district_code <- str_pad(final_post_audit$district_code, 4, pad = "0", "left")
final_post_audit$province_code <- str_pad(final_post_audit$province_code, 2, pad = "0", "left")

prelim_post_audit <- read.csv("./past_elections/presidential_2014/results_data/run_off_preliminary_results/prelim_af_candidate_ps_data_run_off_2014.csv", stringsAsFactors = F)
prelim_post_audit$pc_code <- str_pad(prelim_post_audit$pc_code, 7, pad = "0", "left")
prelim_post_audit$district_code <- str_pad(prelim_post_audit$district_code, 4, pad = "0", "left")
prelim_post_audit$province_code <- str_pad(prelim_post_audit$province_code, 2, pad = "0", "left")

final_round_one <- read.csv("./past_elections/presidential_2014/results_data/first_round_final_results/final_af_candidate_ps_data_first_round_2014.csv", stringsAsFactors = F)
final_round_one$pc_code <- str_pad(final_round_one$pc_code, 7, pad = "0", "left")
final_round_one$district_code <- str_pad(final_round_one$district_code, 4, pad = "0", "left")
final_round_one$province_code <- str_pad(final_round_one$province_code, 2, pad = "0", "left")

prelim_round_one <- read.csv("./past_elections/presidential_2014/results_data/first_round_preliminary_results/prelim_af_candidate_ps_data_first_round_2014.csv", stringsAsFactors = F)
prelim_round_one$pc_code <- str_pad(prelim_round_one$pc_code, 7, pad = "0", "left")
prelim_round_one$district_code <- str_pad(prelim_round_one$district_code, 4, pad = "0", "left")
prelim_round_one$province_code <- str_pad(prelim_round_one$province_code, 2, pad = "0", "left")

all_data <- full_join(filter(final_post_audit, post_audit_status != "Invalidated"), prelim_post_audit) %>% 
  full_join(final_round_one) %>% 
  full_join(prelim_round_one)

ps_count_first_round <- all_data %>% filter(election_type == "PRESIDENTIAL") %>%
  group_by(province_code, election_type, results_status) %>% summarize(
  pc_count = length(unique(pc_code)),
  ps_count = length(unique(ps_code)),
  candidate_count = length(unique(candidate_code)),
  vote_count = sum(votes, na.rm = TRUE)
)

ps_count_run_off <- all_data %>% filter(election_type == "PRESIDENTIAL RUNOFF") %>%
  group_by(province_code, election_type, results_status) %>% summarize(
  pc_count = length(unique(pc_code)),
  ps_count = length(unique(ps_code)),
  candidate_count = length(unique(candidate_code)),
  vote_count = sum(votes, na.rm = TRUE)
)

write.csv(ps_count_first_round, "./past_elections/presidential_2014/validity_checks/first_round_pc_ps_candidate_vote_counts.csv", row.names = F)
write.csv(ps_count_run_off, "./past_elections/presidential_2014/validity_checks/run_off_pc_ps_candidate_vote_counts.csv", row.names = F)

# tabulation check

iec_final_first_round <- data.frame(read_html("http://www.iec.org.af/results_2014/en/finalresults/presidential/2") %>% 
  html_nodes(".table-striped") %>% html_table(fill = TRUE, trim = TRUE, header = TRUE))
colnames(iec_final_first_round) <- c("ballot_position", "candidate_name_eng", "votes", "pct")
write.csv(iec_final_first_round, "./past_elections/presidential_2014/raw/iec_final_first_round_totals.csv", row.names = F)

iec_prelim_first_round <- data.frame(read_html("http://www.iec.org.af/results_2014/en/elections/presidential/2") %>% 
  html_nodes(".table-striped") %>% html_table(fill = TRUE, trim = TRUE, header = TRUE))
colnames(iec_prelim_first_round) <- c("ballot_position", "candidate_name_eng", "votes", "pct")
write.csv(iec_prelim_first_round, "./past_elections/presidential_2014/raw/iec_prelim_first_round_totals.csv", row.names = F)

iec_final_runoff <- data.frame(read_html("http://www.iec.org.af/results_2014/en/final_runoff") %>% 
  html_nodes(".table-striped") %>% html_table(fill = TRUE, trim = TRUE, header = TRUE))
colnames(iec_final_runoff) <- c("province_name_eng", "AG_votes", "AG_pct", "AA_votes", "AA_pct")
iec_final_runoff <- iec_final_runoff[-1, ]
write.csv(iec_final_runoff, "./past_elections/presidential_2014/raw/iec_final_run_off_totals.csv", row.names = F)

iec_prelim_runoff <- data.frame(read_html("http://www.iec.org.af/results_2014/en/runoff") %>% 
  html_nodes(".table-striped") %>% html_table(fill = TRUE, trim = TRUE, header = TRUE))
colnames(iec_prelim_runoff) <- c("province_name_eng", "AA_votes", "AA_pct", "AG_votes", "AG_pct")
iec_prelim_runoff <- iec_prelim_runoff[-1, ]
write.csv(iec_prelim_runoff, "./past_elections/presidential_2014/raw/iec_prelim_run_off_totals.csv", row.names = F)

national_sums <- all_data %>% group_by(election_type, results_status, candidate_code, ballot_position) %>% 
  summarize(votes = sum(votes, na.rm = TRUE))

iec_final_runoff_totals <- tibble(
  ballot_position = as.character(c(1, 5)),
  votes = c(sum(as.numeric(iec_final_runoff$AA_votes)), sum(as.numeric(iec_final_runoff$AG_votes))),
  election_type = "PRESIDENTIAL RUNOFF",
  results_status = "FINAL"
)

iec_prelim_runoff_totals <- tibble(
  ballot_position = as.character(c(1, 5)),
  votes = c(sum(as.numeric(iec_prelim_runoff$AA_votes)), sum(as.numeric(iec_prelim_runoff$AG_votes))),
  election_type = "PRESIDENTIAL RUNOFF",
  results_status = "PRELIMINARY"
)

iec_totals <- iec_final_first_round %>% 
  dplyr::select(ballot_position, votes) %>% 
  filter(!is.na(as.numeric(as.character(ballot_position)))) %>% 
  mutate(election_type = "PRESIDENTIAL", results_status = "FINAL") %>%
  full_join(
    dplyr::select(iec_prelim_first_round, ballot_position, votes) %>%
      filter(!is.na(as.numeric(as.character(ballot_position)))) %>% 
      mutate(election_type = "PRESIDENTIAL", results_status = "PRELIMINARY")
    ) %>%
  full_join(iec_prelim_runoff_totals) %>%
  full_join(iec_final_runoff_totals)

iec_totals$ballot_position <- as.numeric(iec_totals$ballot_position)
iec_totals <- iec_totals %>% rename(iec_vote_total = votes)
national_sums <- national_sums %>% rename(calculated_vote_total = votes)

# NOTE IEC DID NOT REPORT TOTAL RESULTS FOR WARDAK, KARZAI, OR NAEEM DESPITE THEIR RECEIVING VOTES

tabulation_check <- left_join(iec_totals, national_sums)
tabulation_check <- tabulation_check %>% dplyr::select(- candidate_code) %>% 
  left_join(dplyr::select(candidate_key_2014, ballot_position, candidate_code, candidate_name_eng)) %>%
  dplyr::select(election_type, results_status, ballot_position, candidate_code, candidate_name_eng, calculated_vote_total, iec_vote_total)

tabulation_check$error <- ifelse(tabulation_check$calculated_vote_total != tabulation_check$iec_vote_total, "ERROR", "OK")
tabulation_check <- tabulation_check %>% mutate(difference = calculated_vote_total - iec_vote_total)

write.csv(tabulation_check, "./past_elections/presidential_2014/validity_checks/iec_tabulation_check.csv", row.names = F)



