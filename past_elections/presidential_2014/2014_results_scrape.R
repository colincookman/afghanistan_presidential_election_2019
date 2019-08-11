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


# ------------------------------------------------------------------------------

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
row_out <- data.frame()
for(i in 1:length(runoff_post_audit$ps_code)){
 metadata <- runoff_prelim[i, 1:16]
 vote_names <- c("Total Valid", "Dr. Abdullah Abdullah", "Mohammad Ashraf Ghani Ahmadzai", "Unused Ballots", "Spoiled Ballots",
                 "Invalid Ballots", "Discrepancy")
 for(j in 1:7){
  votes = as.numeric(runoff_prelim[i, j+16])
  candidate_name_eng = vote_names[j]
  row_out <- data.frame(c(metadata, candidate_name_eng, votes))
  colnames(row_out) <- c("results_status", "election_type", "election_date", "results_date", "province_code", "province_name_eng", "province_name_dari",
                         "district_code", "district_name_eng", "pc_code", "pc_name_eng", "pc_name_dari", "ps_code", "ps_type",
                         "status", "results_barcode", "candidate_name_eng", "votes")
  all_out <- rbind(all_out, row_out)
  }
}

# UPDATE PC KEY WITH REPORTING STATUS -----------------------------------------
rm(list = ls())


# COMPLETENESS CHECKS AND SUMMARY STATS ---------------------------------------
rm(list = ls())


# quick comparison of 2014 and 2018 coordinates - not matching ------------------
# pc_plan <- gsheet2tbl("https://docs.google.com/spreadsheets/d/12qCIIf1slwz6gDZtableqAhoehZYcODA1eLOAfrwOkA/edit?usp=sharing")
# pc_plan$pc_code <- as.character(str_pad(pc_plan$pc_code, 7, pad = "0", side = "left"))

# pc_2014 <- pc_plan %>% dplyr::select(pc_code, lat, lon)
# pc_2018 <- pc_gis %>% dplyr::select(pc_code, lat, lon) %>% rename(
#  lon_corrected = lat,
#  lat_corrected = lon) %>% rename(
#  lon = lon_corrected,
#  lat = lat_corrected
#  ) %>% dplyr::select(pc_code, lat, lon) %>% arrange(pc_code)

# matched <- pc_2018$pc_code[pc_2018$pc_code %in% pc_2014$pc_code]
# new_pcs <- setdiff(pc_2018$pc_code, pc_2014$pc_code)
# dropped_pcs <- setdiff(pc_2014$pc_code, pc_2018$pc_code)

# matched_by_code <- pc_2018 %>% filter(pc_code %in% matched) %>% mutate(year = "2018") %>% full_join(
#  pc_2014 %>% filter(pc_code %in% matched) %>% mutate(year = "2014")
 )