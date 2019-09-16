library(XML)
library(tidyverse)
library(rvest)
library(wdman)
library(lubridate)
library(pdftools)
library(readr)
library(RSelenium)
library(gsheet)
setwd("~/Google Drive/GitHub/afghanistan_presidential_election_2019")

# CREATE CANDIDATE KEY --------------------------------------------------------

eng_final_candidate_total_url <- "http://www.iec.org.af/results_2009/resultsBallotOrder.html"
dari_final_candidate_total_url <- "http://www.iec.org.af/results_2009/Dari/resultsBallotOrder.html"
pashto_final_candidate_total_url <- "http://www.iec.org.af/results_2009/Pashto/resultsBallotOrder.html"

eng_candidate_list <- read_html(eng_final_candidate_total_url) %>% html_nodes("table") %>% html_table(fill = TRUE, trim = TRUE, header = TRUE)
eng_candidates <- data.frame(eng_candidate_list[[3]])
eng_candidates <- eng_candidates %>% filter(!is.na(Ballot.Order)) 
colnames(eng_candidates) <- c("ballot_position", "candidate_name_eng", "unchallenged_votes", "added_votes", "total_votes", "pct_total")

eng_candidates$unchallenged_votes <- as.numeric(gsub(",", "", eng_candidates$unchallenged_votes))
eng_candidates$added_votes <- as.numeric(gsub(",", "", eng_candidates$added_votes))
eng_candidates$total_votes <- as.numeric(gsub(",", "", eng_candidates$total_votes))

write.csv(eng_candidates, "./past_elections/presidential_2009/raw/final_results_candidates.csv", row.names = F)

eng_candidates$ballot_position[eng_candidates$ballot_position == "41"] <- "31"

dari_candidate_list <- read_html(dari_final_candidate_total_url) %>% html_nodes("table") %>% html_table(fill = TRUE, trim = TRUE, header = TRUE)
dari_candidates <- data.frame(dari_candidate_list[[3]])
dari_candidates <- dari_candidates[-33, ]
dari_candidates <- dari_candidates %>% mutate(ballot_position = row_number())
colnames(dari_candidates) <- c("ballot_position_dari", "candidate_name_dari", "unchallenged_votes_dari", "added_votes_dari", "total_votes_dari", "pct_total_dari", "ballot_position")

pashto_candidate_list <- read_html(pashto_final_candidate_total_url) %>% html_nodes("table") %>% html_table(fill = TRUE, trim = TRUE, header = TRUE)
pashto_candidates <- data.frame(pashto_candidate_list[[3]])
pashto_candidates <- pashto_candidates[-33, ]
pashto_candidates <- pashto_candidates %>% mutate(ballot_position = row_number())
colnames(pashto_candidates) <- c("ballot_position_pashto", "candidate_name_pashto", "unchallenged_votes_pashto", "added_votes_pashto", "total_votes_pashto", "pct_total_pashto", "ballot_position")

eng_candidates$ballot_position <- as.numeric(eng_candidates$ballot_position)

candidate_key <- eng_candidates %>% left_join(dplyr::select(dari_candidates, ballot_position, candidate_name_dari)) %>% 
  left_join(dplyr::select(pashto_candidates, ballot_position, candidate_name_pashto)) %>% dplyr::select(
    ballot_position, candidate_name_eng, candidate_name_dari, candidate_name_pashto
  )

# IEC only reporting partial candidate results on its website? checking against 41-candidate pre-election list

candidate_name_eng <- c("Haji Rahim Jan Shinzad", "Mohammad Yasin Safi", "Mohammad Sarwar Ahmadzai", "Eng. Moin-ul-din Ulfati",
                         "Dr. Habib Mangal", "Zabih-U-llah Ghazi Noristani", "Sayed Jalal Karim", "Mirwais Yasini", "Bismillah Shir",
                         "Bashir Ahmad Bizhan", "Motasim Billah Mazhabi", "Ashraf Ghani Ahmadzai", "Abdul Latif Pedram", "Shahnawaz Tanai",
                         "Dr. Mohammad Nasir Anis", "Mulla Abdul Salam Rakity", "Zia-ul-haq Hafizi", "Mohammad Akbar Oria", "Baz Mohammad Kofi",
                         "Sangin Mohammad Rahmani", "Mahbob-U-lah Koshani", "Mohammad Hakim Torsan", "Abdul Hasib Arian", "Mulla Ghulam Mohammad Rigi",
                         "Ramazan Bashardost", "Mawlawi Mohammad Sayed Hashimi", "Abdul Majid Samim", "Nasrullah Baryalai Arsalai", "Alhaj Shah Mahmood Popal",
                         "Mrs. Shahla Ata", "Dr. Ghulam Faroq Nijrabi", "Alhaj Abdul Ghafor Zori", "Mohammad Hashim Tawfiqi", "Haji Hasan Ali Sultani",
                         "Hamed Karzai", "Mawlana Abdul Qadir Imami Ghori", "Dr. Abdullah Abdullah", "Dr. Frozan Fana", "Abdul Jabar Sabit", "Hidayat Amin Arsala",
                         "Gul Ahmmad Yama")
                         
candidate_name_dari <- c(
  "الحاج رحيم جان شيرزاد )فی سبيل الله(",
  "محمد ياسين ) صافی (",
  "محمد سرور احمدزی",
  "انجنير معين الدين الفتی",
  "داکتر حبيب منگل",
  "ذبيح الله غازی نورستانی",
  "سيد جلال کريم",
  "ميرويس ياسينی",
  "بسم الله شير",
  "بشير احمد بيژن",
  "معتصم با􏲽 مذھبی",
  "اشرف غنی احمدزی",
  "عبداللطيف ) پدرام (",
  "شھنواز تڼی",
  "دوکتور محمد نصير انيس",
  "ملا عبد السلام ) راکتی (",
  "ضيا الحق ) حافظی (",
  "محمد اکبر )اوريا (",
  "باز محمد ) کوفی(",
  "سنگين محمد ) رحمانی (",
  "محبوب الله ) کوشانی(",
  "محمد حکيم تورسن",
  "عبدالحسيب ) آرين (",
  "ملا غلام محمد )ريگی(",
  "رمضان بشردوست",
  "مولوی محمد سعيد ھاشمی",
  "عبدالمجيد ) صميم (",
  "نصرالله بريالی )ارسلائی(",
  "الحاج شاه محمود پوپل",
  "شھلا ) عطا (",
  "پروفيسور دوکتور غلام فاروق ) نجرابی(",
  "الحاج عبد الغفور )زوری(",
  "محمد ھاشم توفيقی",
  "الحاج حسن علی )سلطانی(",
  "حامد کرزی",
  "مولانا عبدالقادر امامی غوری",
  "داکتر عبد الله ) عبدالله (",
  "داکتر فروزان ) فنا (",
  "عبدالجبار ثابت",
  "ھدايت امين ارسلا",
  "گل احمد )يما("
)

candidate_code <- c("1095147", "5552641", "6359413", "4657143", "6350379", "7822442", "7770491", "6359075", "3907996", "2287256",
                                  "8894180", "6359797", "860263", "3010205", "6350728", "6189430", "6350715", "6189294", "3907754", "977770",
                                  "3010270", "926537", "3788581", "6178408", "997138", "6368755", "12223481", "6188208", "6358686", "6370916",
                                  "6358651", "7263746", "6155279", "6136166", "983418", "6359633", "1004871", "6371180", "5109208", "11348474",
                                  "7849401")

party_name_dari <- c("مستقل",
                     "مستقل",
                     "مستقل",
                     "مستقل",
                     "حزب نھضت فراگير دموکراسی وترقی افغانستان",
                     "مستقل",
                     "مستقل",
                     "مستقل",
                     "مستقل",
                     "مستقل",
                     "مستقل",
                     "مستقل",
                     "حزب کنگره ملی افغانستان",
                     "د افغانستان د سولی غورځنګ ګوند",
                     "مستقل",
                     "مستقل",
                     "مستقل",
                     "مستقل",
                     "مستقل",
                     "مستقل",
                     "حزب آزادگان افغانستان",
                     "مستقل",
                     "مستقل",
                     "مستقل",
                     "مستقل",
                     "حزب حرکت انقلاب اسلامی و ملی افغانستان",
                     "مستقل",
                     "مستقل",
                     "حزب صلح ملی اسلامی افغانستان",
                     "مستقل",
                     "حزب استقلال افغانستان",
                     "مستقل",
                     "مستقل",
                     "مستقل",
                     "مستقل",
                     "مستقل",
                     "مستقل",
                     "مستقل",
                     "مستقل",
                     "مستقل",
                     "مستقل"
                     )

candidate_list <- as.data.frame(cbind(candidate_code, candidate_name_eng, candidate_name_dari, party_name_dari)) %>% 
  mutate(ballot_position = row_number()) %>% dplyr::select(ballot_position, everything())

candidate_list$candidate_gender[candidate_list$ballot_position == "30" | candidate_list$ballot_position == "38"] <- "F"
candidate_list$candidate_gender[is.na(candidate_list$candidate_gender)] <- "M"
candidate_list$final_winner[candidate_list$ballot_position == "35"] <- "YES"
candidate_list$final_winner[is.na(candidate_list$final_winner)] <- "NO"
candidate_list$past_winner[candidate_list$ballot_position == "35"] <- "YES"
candidate_list$past_winner[is.na(candidate_list$past_winner)] <- "NO"

write.csv(candidate_list, "./past_elections/presidential_2009/keyfiles/candidate_key_2009.csv", row.names = F)

# CREATE PC, DISTRICT< AND PROVINCE KEYs --------------------------------------
rm(list = ls())

pc_raw <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1msRgeZ57qO8mDLVBBARocXRrif7LyV9I2n3ZHIjs9cw/edit?usp=sharing")

pc_raw$province_name_eng <- str_to_upper(pc_raw$province_name_eng)
pc_raw$district_or_subdivision_name_eng <- str_to_upper(pc_raw$district_or_subdivision_name_eng)
pc_raw$pc_code <- as.character(str_pad(pc_raw$pc_code, 7, pad = "0", side = "left"))

pc_key_2009 <- pc_raw %>% mutate(
  province_code = str_sub(pc_code, 1,2),
  district_code = str_sub(pc_code, 1,4),
  district_number = str_sub(district_code, 3,4),
  pc_number = str_sub(pc_code, 5,7),
  district_name_eng = ifelse(grepl("NAHIA", district_or_subdivision_name_eng),
                             trimws(str_split(district_or_subdivision_name_eng, "NAHIA")[[1]][1]),
                             district_or_subdivision_name_eng)
)

pc_key_2009$district_name_eng <- gsub("PROVINCIAL CENTER", "", pc_key_2009$district_name_eng)
pc_key_2009$district_name_eng <- gsub("\\(", "", pc_key_2009$district_name_eng)
pc_key_2009$district_name_eng <- gsub("\\)", "", pc_key_2009$district_name_eng)

pc_key_2009$district_or_subdivision_name_eng <- gsub("PROVINCIAL CENTER", "", pc_key_2009$district_or_subdivision_name_eng)
pc_key_2009$district_or_subdivision_name_eng <- gsub("\\(", "", pc_key_2009$district_or_subdivision_name_eng)
pc_key_2009$district_or_subdivision_name_eng <- gsub("\\)", "", pc_key_2009$district_or_subdivision_name_eng)

nahia <- "ناحيه"

pc_key_2009 <- pc_key_2009 %>% rowwise %>% mutate(
  district_or_subdivision_name_eng = ifelse(grepl("NAHIA", district_or_subdivision_name_eng),
                             paste0(trimws(str_split(district_or_subdivision_name_eng, "NAHIA")[[1]][1]),
                                    " NAHIA ", 
                                    str_pad(trimws(str_split(district_or_subdivision_name_eng, "NAHIA")[[1]][2]), 2, pad = "0", side = "left")),
                             district_or_subdivision_name_eng),
  district_name_dari = ifelse(grepl(nahia, district_or_subdivision_name_dari),
                              "کابل", district_or_subdivision_name_dari),
  district_or_subdivision_name_dari = ifelse(grepl(nahia, district_or_subdivision_name_dari),
                              paste0("کابل", " ", nahia, " ", str_pad(trimws(str_split(district_or_subdivision_name_eng, "NAHIA")[[1]][2]), 2, pad = 0, side = "left")),
                              district_or_subdivision_name_dari),
  district_sub_code = ifelse(grepl("NAHIA", district_or_subdivision_name_eng), 
                             paste0(district_code, "-", trimws(str_split(district_or_subdivision_name_eng, "NAHIA")[[1]][2])), district_code),
  provincial_capital = ifelse(district_number == "01", "YES", "NO")
  ) %>% dplyr::select(
    province_code, province_name_eng, province_name_dari, 
    district_code, district_name_eng, district_name_dari, district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari,
    provincial_capital,
    pc_code, everything(), -c(district_number, pc_number)
  )

province_key <- pc_key_2009 %>% dplyr::select(province_code, province_name_eng, province_name_dari) %>% unique() %>% arrange(province_code)

district_key <- pc_key_2009 %>% dplyr::select(province_code, 
                                              district_code, district_name_eng, district_name_dari, 
                                              district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari,
                                              provincial_capital) %>% unique() %>% arrange(district_code)

write.csv(province_key, "./past_elections/presidential_2009/keyfiles/province_key_2009.csv", row.names = F)
write.csv(district_key, "./past_elections/presidential_2009/keyfiles/district_key_2009.csv", row.names = F)
write.csv(pc_key_2009, "./past_elections/presidential_2009/keyfiles/pc_key_2009.csv", row.names = F)

# RESULTS DATA ---------------------------------------------------------------

rm(list = ls())
results_url <- "http://www.iec.org.af/results_2009/PollingStation/PollingStationbyProvince.html"
  
page_links <- read_html(results_url) %>% html_nodes("a") %>% html_attr("href")
province_links <- page_links[grepl("Polling\\d{2}", page_links)]
province_links <- paste0("http://www.iec.org.af/results_2009/PollingStation/", province_links)
  
pc_link_list <- list()
# get list of individual PC results links from the province pages
for(i in 1:length(province_links)){
  # get list of polling center links
  province_page_links <- read_html(province_links[i]) %>% html_nodes("a") %>% html_attr("href")
  pc_links <- province_page_links[grepl("\\d{6,7}.html", province_page_links)] %>% unique()
  pc_links <- paste0("http://www.iec.org.af/results_2009/PollingStation/", pc_links)
  pc_link_list <- c(pc_link_list, pc_links)
  Sys.sleep(2)
}
    
pc_out <- data.frame()
error_log <- list()
  
# settings for silent background browsing
eCaps <- list(chromeOptions = list(
  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
))
  
driver <- rsDriver(browser="chrome", port = 4563L, chromever="74.0.3729.6", verbose = FALSE
                     , extraCapabilities = eCaps
                     )
      
for(k in 1:length(pc_link_list)){

    link_target <- pc_link_list[k][[1]][1]
    driver$client$navigate(link_target)
    
    pc_code <- str_split(str_split(pc_link_list[k], "/")[[1]][6], "\\.")[[1]][1]
      
    Sys.sleep(2)
    table_present <- NULL
    page_error <- "NO"
    wait_count <- 0
    while(is.null(table_present)){
      if(wait_count >= 3){
        page_error <- "YES"
        table_present <- "ERROR"
      } else {
        
        table_present <- tryCatch(
        {driver$client$findElement(using = 'id', value = "brtbl")},
          error = function(error){
            Sys.sleep (1)
          }
        )
        wait_count <- (wait_count + 1)
      }
    }
      
    # either get the page or log the error
    if(page_error != "YES"){  
    pc_html <- driver$client$getPageSource()[[1]] %>% read_html() %>% html_nodes("table")
    pc_table <- pc_html[3] %>% html_table(fill = TRUE, trim = TRUE, header = TRUE) %>% data.frame()
    pc_table$pc_code <- str_pad(pc_code, 7, pad = "0", side = "left")
     pc_table$province_code = str_sub(pc_code, 1,2)
    pc_out <- rbind(pc_out, pc_table)
      
    } else {
      error_log <- c(error_log, link_target)
      }
  
    Sys.sleep(5)
  
}
  
driver$client$close()
driver$server$stop()
 

write.csv(pc_out, "./past_elections/presidential_2009/raw/raw_pc_results.csv", row.names = F)
write.csv(tibble(unlist(error_log)), "./past_elections/presidential_2009/raw/broken_pc_links.csv", row.names = F)


## clean up the scraped results data ------------------------------------------
rm(list = ls())

pc_out <- read.csv("./past_elections/presidential_2009/raw/raw_pc_results.csv", stringsAsFactors = F)

pc_cleanup <- pc_out #%>% filter(pc_code == "101001" | pc_code == "101002" | pc_code == "101003" | pc_code == "101004" | pc_code == "101005")

colnames(pc_cleanup) <- c("old_province", "old_pc_code", "candidate_name", paste0("ps_number_", (1:100)), "pc_code", "province_code")

pc_cleanup$pc_code <- as.character(str_pad(pc_cleanup$pc_code, 7, pad = "0", side = "left"))
pc_cleanup$province_code <- as.character(str_pad(pc_cleanup$province_code, 2, pad = "0", side = "left"))

pc_trimmed <- pc_cleanup[-grep("Grand Total", pc_cleanup$candidate_name), ]
pc_trimmed <- pc_trimmed %>% dplyr::select(-c("old_province", "old_pc_code"))
pc_trimmed <- pc_trimmed[pc_trimmed$candidate_name != "", ]
rownames(pc_trimmed) <- 1:dim(pc_trimmed)[1]

pc_trimmed$ps_count <- (104 - as.numeric((apply(pc_trimmed, 1, function(x) sum(is.na(x)))))) - 5

system.time({
ps_out <- data.frame()
for(i in 1:length(pc_trimmed$pc_code)){
  province_code <- as.character(pc_trimmed$province_code[i])
  pc_code <- as.character(pc_trimmed$pc_code[i])
  candidate_name <- as.character(pc_trimmed$candidate_name[i])
  ps_count <- as.numeric(pc_trimmed$ps_count[i])
  for(j in 1:ps_count){
    ps_number <- j
    votes <- as.numeric(pc_trimmed[i, j+1])
    row <- tibble(province_code, pc_code, ps_number, candidate_name, votes)
    ps_out <- rbind(ps_out, row)
  }
}
})

ps_out <- ps_out %>% rowwise %>% mutate(
  ps_code = paste0(pc_code, "-", as.character(str_pad(ps_number, 2, pad = "0", side = "left"))),
  province_code_corrected = str_sub(pc_code, 1,2),
  candidate_name_eng = trimws(str_split(candidate_name, "/")[[1]][1]),
  candidate_name_dari = trimws(str_split(candidate_name, "/")[[1]][2])
  ) %>% 
  dplyr::select(-province_code) %>%
  rename(province_code = province_code_corrected) %>% dplyr::select(
    province_code, pc_code, ps_code, ps_number, candidate_name_eng, candidate_name_dari, votes
  ) %>% arrange(province_code, pc_code, ps_code, candidate_name_eng)

write.csv(ps_out, "./past_elections/presidential_2009/raw/raw_ps_results.csv", row.names = F)

ps_out_corrected <- read.csv("./past_elections/presidential_2009/raw/raw_ps_results.csv", stringsAsFactors = F, strip.white = T)

ps_out_corrected$candidate_name_eng <- str_trim(ps_out_corrected$candidate_name_eng)
ps_out_corrected$candidate_name_dari <- str_trim(ps_out_corrected$candidate_name_dari)

candidate_key <- read.csv("./past_elections/presidential_2009/keyfiles/candidate_key_2009.csv", stringsAsFactors = F)
pc_key <- read.csv("./past_elections/presidential_2009/keyfiles/pc_key_2009.csv", stringsAsFactors = F)

candidate_key <- candidate_key %>% mutate(
  dropped_from_final_ballot = ifelse(candidate_name_eng %in% 
                                        setdiff(candidate_key$candidate_name_eng, unique(ps_out_corrected$candidate_name_eng)), "YES", "NO")
  )

# write.csv(candidate_key, "./past_elections/presidential_2009/keyfiles/candidate_key_2009.csv", row.names = F)

# strip parentheses from candidate names

results_dari_names <- unique(ps_out_corrected$candidate_name_dari)
key_dari_names <- candidate_key$candidate_name_dari[candidate_key$dropped_from_final_ballot == "NO"]

setdiff(key_dari_names, results_dari_names)

# results_dari_names_cleanup <- gsub("\\)", "", results_dari_names)
# results_dari_names_cleanup <- gsub("\\(", "", results_dari_names_cleanup)
# results_dari_names_cleanup <- gsub("  ", " ", results_dari_names_cleanup)
# results_dari_names_cleanup <- str_trim(results_dari_names_cleanup)

# key_dari_names_cleanup <- gsub("\\)", "", key_dari_names)
# key_dari_names_cleanup <- gsub("\\(", "", key_dari_names_cleanup)
# key_dari_names_cleanup <- gsub("  ", " ", key_dari_names_cleanup)
# key_dari_names_cleanup <- str_trim(key_dari_names_cleanup)

ps_out_corrected$candidate_name_dari <- gsub("\\)", "", ps_out_corrected$candidate_name_dari)
ps_out_corrected$candidate_name_dari <- gsub("\\(", "", ps_out_corrected$candidate_name_dari)
ps_out_corrected$candidate_name_dari <- gsub("  ", " ", ps_out_corrected$candidate_name_dari)
ps_out_corrected$candidate_name_dari <- gsub("  ", " ", ps_out_corrected$candidate_name_dari)
ps_out_corrected$candidate_name_dari <- str_trim(ps_out_corrected$candidate_name_dari)

results_dari_names <- dplyr::select(ps_out_corrected, candidate_name_eng, candidate_name_dari) %>% unique()
candidate_key_corrected <- candidate_key %>% filter(dropped_from_final_ballot != "YES") %>% 
  dplyr::select(-(candidate_name_dari)) %>% left_join(results_dari_names) %>% 
  full_join(candidate_key %>% filter(dropped_from_final_ballot == "YES")) %>% arrange(ballot_position) %>%
  dplyr::select(ballot_position, candidate_code, candidate_name_eng, candidate_name_dari, everything())

candidate_key_corrected$candidate_name_dari <- gsub("\\)", "", candidate_key_corrected$candidate_name_dari)
candidate_key_corrected$candidate_name_dari <- gsub("\\(", "", candidate_key_corrected$candidate_name_dari)
candidate_key_corrected$candidate_name_dari <- gsub("  ", " ", candidate_key_corrected$candidate_name_dari)
candidate_key_corrected$candidate_name_dari <- str_trim(candidate_key_corrected$candidate_name_dari)

write.csv(candidate_key, "./past_elections/presidential_2009/keyfiles/candidate_key_2009.csv", row.names = F)

ps_out_with_metadata <- left_join(ps_out_corrected, pc_key)
ps_out_with_metadata <- left_join(ps_out_with_metadata, candidate_key_corrected)
ps_out_with_metadata$election_date <- mdy("08-20-2009")
ps_out_with_metadata$results_date <- mdy("10-21-2009")
ps_out_with_metadata$election_type <- "PRESIDENTIAL"
ps_out_with_metadata$results_status <- "FINAL"

ps_final <- ps_out_with_metadata %>% dplyr::select(
  election_date, results_date, election_type, results_status,
  province_code, province_name_eng, province_name_dari, 
  district_code, district_name_eng, district_name_dari, district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital,
  pc_code, pc_name_eng, pc_name_dari,
  ps_code, candidate_code, ballot_position, candidate_name_eng, candidate_name_dari, candidate_gender, party_name_dari, past_winner,
  votes,  final_winner
) %>% arrange(province_code, district_code, pc_code, ps_code, ballot_position)

write.csv(ps_final, "./past_elections/presidential_2009/results_data/final_af_candidate_ps_data_2009.csv", row.names = F)

# get PC results from broken link PCs from pdf of final results instead -----
error_log <- read.csv("./past_elections/presidential_2009/raw/broken_pc_links.csv", stringsAsFactors = F)
colnames(error_log) <- "broken_link"
error_log <- error_log %>% rowwise %>% mutate(
  missing_pc = str_split(str_split(broken_link, "/")[[1]][6], "\\.")[[1]][1]
)

missing_pc_data <- read.csv("./past_elections/presidential_2009/raw/pcs_missing_from_web.csv", stringsAsFactors = F)

missing_pc_data_restructured <- data.frame()
for(i in 1:length(missing_pc_data$pc_code)){
  pc_code <- as.character(missing_pc_data$pc_code[i])
  candidate_name <- as.character(missing_pc_data$candidate_name[i])
  ps_count <- as.numeric(missing_pc_data$ps_count[i])
  for(j in 1:ps_count){
    ps_number <- j
    votes <- as.numeric(missing_pc_data[i, j+2])
    row <- tibble(pc_code, ps_number, candidate_name, votes)
    missing_pc_data_restructured <- rbind(missing_pc_data_restructured, row)
  }
}

missing_pc_data_restructured$pc_code <- as.character(str_pad(missing_pc_data_restructured$pc_code, 7, pad = "0", side = "left"))

missing_pc_data_restructured <- missing_pc_data_restructured %>% rowwise %>% mutate(
  ps_code = paste0(pc_code, "-", as.character(str_pad(ps_number, 2, pad = "0", side = "left"))),
  province_code = str_sub(pc_code, 1,2)) %>% 
  rename(candidate_name_eng = candidate_name) %>%
  dplyr::select(province_code, pc_code, ps_code, ps_number, candidate_name_eng, votes) %>%
  arrange(province_code, pc_code, ps_code, candidate_name_eng)

missing_pc_data_restructured$candidate_name_eng[missing_pc_data_restructured$candidate_name_eng == "Eng. Moin‐ul‐din Ulfati"] <- "Eng. Moin-ul-din Ulfati"
missing_pc_data_restructured$candidate_name_eng[missing_pc_data_restructured$candidate_name_eng == "Gul Ahmmad Yama "] <- "Gul Ahmmad Yama"
missing_pc_data_restructured$candidate_name_eng[missing_pc_data_restructured$candidate_name_eng == "Mahbob‐U‐lah Koshani"] <- "Mahbob-U-lah Koshani"
missing_pc_data_restructured$candidate_name_eng[missing_pc_data_restructured$candidate_name_eng == "Zabih‐U‐llah Ghazi Noristani"] <- "Zabih-U-llah Ghazi Noristani"
missing_pc_data_restructured$candidate_name_eng[missing_pc_data_restructured$candidate_name_eng == "Zia‐ul‐haq Hafizi"] <- "Zia-ul-haq Hafizi"

pc_key$province_code <- as.character(str_pad(pc_key$province_code, 2, pad = "0", side = "left"))
pc_key$district_code <- as.character(str_pad(pc_key$district_code, 4, pad = "0", side = "left"))
pc_key$pc_code <- as.character(str_pad(pc_key$pc_code, 7, pad = "0", side = "left"))

ps_final$province_code <- as.character(str_pad(ps_final$province_code, 2, pad = "0", side = "left"))
ps_final$district_code <- as.character(str_pad(ps_final$district_code, 4, pad = "0", side = "left"))
ps_final$pc_code <- as.character(str_pad(ps_final$pc_code, 7, pad = "0", side = "left"))
  
missing_pc_with_metadata <- left_join(missing_pc_data_restructured, pc_key)
missing_pc_with_metadata <- left_join(missing_pc_with_metadata, candidate_key_corrected)
missing_pc_with_metadata$election_date <- mdy("08-20-2009")
missing_pc_with_metadata$results_date <- mdy("10-21-2009")
missing_pc_with_metadata$election_type <- "PRESIDENTIAL"
missing_pc_with_metadata$results_status <- "FINAL"

all_pcs_data <- full_join(ps_final, missing_pc_with_metadata) %>% dplyr::select(
  election_date, results_date, election_type, results_status,
  province_code, province_name_eng, province_name_dari, 
  district_code, district_name_eng, district_name_dari, district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital,
  pc_code, pc_name_eng, pc_name_dari,
  ps_code, candidate_code, ballot_position, candidate_name_eng, candidate_name_dari, candidate_gender, party_name_dari, past_winner,
  votes,  final_winner
) %>% arrange(province_code, district_code, pc_code, ps_code, ballot_position)

# fill in missing pc data for one pc that reported results but did not appear in key

all_pcs_data$province_name_eng[all_pcs_data$pc_code == "2406471"] <- "HERAT"
all_pcs_data$province_name_dari[all_pcs_data$pc_code == "2406471"] <- "ھرات"
all_pcs_data$district_code[all_pcs_data$pc_code == "2406471"] <- "2406"
all_pcs_data$district_name_eng[all_pcs_data$pc_code == "2406471"] <- "KUSHK"
all_pcs_data$district_name_dari[all_pcs_data$pc_code == "2406471"] <- "کشک )رباط سنگی("
all_pcs_data$district_sub_code[all_pcs_data$pc_code == "2406471"] <- "2406"
all_pcs_data$district_or_subdivision_name_eng[all_pcs_data$pc_code == "2406471"] <- "KUSHK"
all_pcs_data$district_or_subdivision_name_dari[all_pcs_data$pc_code == "2406471"] <- "کشک )رباط سنگی("
all_pcs_data$provincial_capital[all_pcs_data$pc_code == "2406471"] <- "NO"
all_pcs_data$pc_name_eng[all_pcs_data$pc_code == "2406471"] <- "Shah Ghulam School"
all_pcs_data$pc_name_dari[all_pcs_data$pc_code == "2406471"] <- "مکتب شاه غلام"

write.csv(all_pcs_data, "./past_elections/presidential_2009/results_data/final_af_candidate_ps_data_2009.csv", row.names = F)

final_ps_lite <- all_pcs_data %>% dplyr::select(ps_code, candidate_code, votes)

write.csv(final_ps_lite, "./past_elections/presidential_2009/results_data/final_af_candidate_ps_data_2009_lite.csv", row.names = F)

all_data <- all_pcs_data

ps_count <- all_data %>% group_by(province_code) %>% summarize(
  pc_count = length(unique(pc_code)),
  ps_count = length(unique(ps_code)),
  candidate_count = length(unique(candidate_code)),
  vote_count = sum(votes, na.rm = TRUE)
)

write.csv(ps_count, "./past_elections/presidential_2009/validity_checks/final_pc_ps_candidate_vote_counts.csv", row.names = F)



#-----------------------------------------------------------------------------------
# RE-AGGREGATION OF RESULTS
rm(list = ls())

all_data <- read.csv("./past_elections/presidential_2009/results_data/final_af_candidate_ps_data_2009.csv", stringsAsFactors = F)

# PC level
all_data_pcs <- all_data %>% group_by(election_date, results_date, election_type, results_status, province_code, province_name_eng, province_name_dari,
  district_code, district_name_eng, district_name_dari, district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital,
  pc_code, pc_name_eng, pc_name_dari,
  candidate_code, ballot_position, candidate_name_eng, candidate_name_dari, candidate_gender, party_name_dari, past_winner, final_winner) %>% 
  summarize(male_ps_votes = sum(votes[ps_type == "M"], na.rm = T),
            fem_ps_votes = sum(votes[ps_type == "F"], na.rm = T),
            kuchi_ps_votes = sum(votes[ps_type == "K"], na.rm = T),
            votes = sum(votes, na.rm = T)
            ) %>% arrange(results_status, province_code, district_code, pc_code, ballot_position)

write.csv(all_data_pcs, "./past_elections/presidential_2009/results_data/final_af_candidate_pc_data_2009.csv", row.names = F)

all_data_pcs_lite <- all_data_pcs %>% dplyr::select(pc_code, candidate_code, votes) %>% 
  group_by(pc_code, candidate_code) %>% summarize(votes = sum(votes))

write.csv(all_data_pcs_lite, "./past_elections/presidential_2009/results_data/final_af_candidate_pc_data_2009_lite.csv", row.names = F)

# District level
all_data_districts <- all_data %>% group_by(election_date, results_date, election_type, results_status, province_code, province_name_eng, province_name_dari,
  district_code, district_name_eng, district_name_dari, district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital,
  candidate_code, ballot_position, candidate_name_eng, candidate_name_dari, candidate_gender, party_name_dari, past_winner, final_winner) %>% 
  summarize(male_ps_votes = sum(votes[ps_type == "M"], na.rm = T),
            fem_ps_votes = sum(votes[ps_type == "F"], na.rm = T),
            kuchi_ps_votes = sum(votes[ps_type == "K"], na.rm = T),
            votes = sum(votes, na.rm = T)
            ) %>% arrange(results_status, province_code, district_code, ballot_position)

write.csv(all_data_districts, "./past_elections/presidential_2009/results_data/final_af_candidate_district_data_2009.csv", row.names = F)

# Province level
all_data_provinces <- all_data %>% group_by(election_date, results_date, election_type, results_status, province_code, province_name_eng, province_name_dari,
  candidate_code, ballot_position, candidate_name_eng, candidate_name_dari, candidate_gender, party_name_dari, past_winner, final_winner) %>% 
  summarize(male_ps_votes = sum(votes[ps_type == "M"], na.rm = T),
            fem_ps_votes = sum(votes[ps_type == "F"], na.rm = T),
            kuchi_ps_votes = sum(votes[ps_type == "K"], na.rm = T),
            votes = sum(votes, na.rm = T)
            ) %>% arrange(results_status, province_code, ballot_position)

write.csv(all_data_provinces, "./past_elections/presidential_2009/results_data/final_af_candidate_province_data_2009.csv", row.names = F)

# GAP INVESTIGATIONS, PS KEY, REPORTING STATUS -------------------------------
rm(list = ls())

all_data <- read.csv("./past_elections/presidential_2009/results_data/final_af_candidate_ps_data_2009.csv", stringsAsFactors = F)
candidate_key <- read.csv("./past_elections/presidential_2009/keyfiles/candidate_key_2009.csv", stringsAsFactors = F)
pc_key <- read.csv("./past_elections/presidential_2009/keyfiles/pc_key_2009.csv", stringsAsFactors = F)

all_data$pc_code <- as.character(str_pad(all_data$pc_code, 7, pad = "0", side = "left"))


ps_reporting <- all_data %>% dplyr::select(pc_code, ps_code) %>% unique() %>% mutate(
  final_results_reporting = "YES"
)

pc_key_check <- pc_key %>% rowwise %>% mutate(
  ps_check = ifelse(ps_planned_count != sum(c(ps_male, ps_fem, ps_kuchi), na.rm = TRUE), "ERROR", "OK")
  )

pc_key_errors <- pc_key %>% filter(ps_check == "ERROR")
pc_key_errors$ps_fem[pc_key_errors$pc_code == "2707147"] <- 1
pc_key_errors$ps_fem[pc_key_errors$pc_code == "2707152"] <- 1
pc_key_errors$ps_fem[pc_key_errors$pc_code == "2707153"] <- 1
pc_key_errors$ps_fem[pc_key_errors$pc_code == "2707154"] <- 1
pc_key_errors$ps_fem[pc_key_errors$pc_code == "718247"] <- as.numeric(NA)
pc_key_errors$ps_fem[pc_key_errors$pc_code == "718248"] <- 3
pc_key_errors$ps_fem[pc_key_errors$pc_code == "718249"] <- as.numeric(NA)
pc_key_errors$ps_fem[pc_key_errors$pc_code == "718250"] <- 3
pc_key_errors$ps_fem[pc_key_errors$pc_code == "718251"] <- as.numeric(NA)
pc_key_errors$ps_fem[pc_key_errors$pc_code == "718252"] <- 3
pc_key_errors$ps_fem[pc_key_errors$pc_code == "718253"] <- as.numeric(NA)
pc_key_errors$ps_fem[pc_key_errors$pc_code == "718254"] <- 5
pc_key_errors$ps_fem[pc_key_errors$pc_code == "718255"] <- as.numeric(NA)
pc_key_errors$ps_fem[pc_key_errors$pc_code == "718256"] <- 3
pc_key_errors$ps_fem[pc_key_errors$pc_code == "718257"] <- as.numeric(NA)
pc_key_errors$ps_fem[pc_key_errors$pc_code == "718258"] <- 3
pc_key_errors$ps_fem[pc_key_errors$pc_code == "718259"] <- as.numeric(NA)
pc_key_errors$ps_fem[pc_key_errors$pc_code == "718260"] <- 2
pc_key_errors$ps_fem[pc_key_errors$pc_code == "718261"] <- as.numeric(NA)
pc_key_errors$ps_fem[pc_key_errors$pc_code == "718262"] <- 2
pc_key_errors$ps_fem[pc_key_errors$pc_code == "718263"] <- as.numeric(NA)
pc_key_errors$ps_fem[pc_key_errors$pc_code == "718264"] <- 2
pc_key_errors$ps_fem[pc_key_errors$pc_code == "718265"] <- as.numeric(NA)
pc_key_errors$ps_fem[pc_key_errors$pc_code == "718266"] <- 2
pc_key_errors$ps_fem[pc_key_errors$pc_code == "718303"] <- as.numeric(NA)
pc_key_errors$ps_fem[pc_key_errors$pc_code == "718304"] <- 2
pc_key_errors$ps_fem[pc_key_errors$pc_code == "719267"] <- as.numeric(NA)
pc_key_errors$ps_fem[pc_key_errors$pc_code == "719268"] <- 3
pc_key_errors$ps_fem[pc_key_errors$pc_code == "719269"] <- as.numeric(NA)
pc_key_errors$ps_fem[pc_key_errors$pc_code == "719270"] <- 5
pc_key_errors$ps_fem[pc_key_errors$pc_code == "719271"] <- as.numeric(NA)
pc_key_errors$ps_fem[pc_key_errors$pc_code == "719272"] <- 3
pc_key_errors$ps_fem[pc_key_errors$pc_code == "719273"] <- as.numeric(NA)
pc_key_errors$ps_fem[pc_key_errors$pc_code == "719274"] <- 2
pc_key_errors$ps_fem[pc_key_errors$pc_code == "719275"] <- as.numeric(NA)
pc_key_errors$ps_fem[pc_key_errors$pc_code == "719276"] <- 3
pc_key_errors$ps_fem[pc_key_errors$pc_code == "719277"] <- as.numeric(NA)
pc_key_errors$ps_fem[pc_key_errors$pc_code == "719278"] <- 3
pc_key_errors$ps_fem[pc_key_errors$pc_code == "719279"] <- as.numeric(NA)
pc_key_errors$ps_fem[pc_key_errors$pc_code == "719280"] <- 4

pc_key_corrected <- pc_key %>% filter(ps_check != "ERROR") %>% full_join(pc_key_errors) %>% dplyr::select(-ps_check) %>%
  arrange(province_code, district_code, pc_code)

# write.csv(pc_key_corrected, "./past_elections/presidential_2009/keyfiles/pc_key_2009.csv", row.names = F)

# pc reporting

pc_reporting <- all_data %>% dplyr::select(pc_code) %>% unique() %>% mutate(final_results_reporting = "YES")
pc_reporting$pc_code <- as.character(str_pad(pc_reporting$pc_code, 7, pad = "0", side = "left"))
pc_key_reporting_status <- left_join(pc_key, pc_reporting)

ps_reporting_count <- all_data %>% group_by(pc_code) %>% summarize(
  ps_reporting_count = length(unique(ps_code))
)
ps_reporting_count$pc_code <- as.character(str_pad(ps_reporting_count$pc_code, 7, pad = "0", side = "left"))

pc_key_reporting_status <- pc_key_reporting_status %>% left_join(ps_reporting_count) %>% dplyr::select(
  province_code, province_name_eng, province_name_dari,
  district_code, district_name_eng, district_name_dari,
  district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital, 
  pc_code, pc_name_eng, pc_name_dari,
  final_results_reporting, ps_count, ps_reporting_count, ps_male, ps_fem, ps_kuchi, est_voters, access_notes
) %>% rename(ps_planned_count = ps_count) %>% arrange(province_code, district_code, pc_code)

# write.csv(pc_key_reporting_status, "./past_elections/presidential_2009/keyfiles/pc_key_2009.csv", row.names = F)

# errors here

pc_key <- read.csv("./past_elections/presidential_2009/keyfiles/pc_key_2009.csv", stringsAsFactors = F)
pc_key_fixes <- pc_key %>% rename(province_name_eng = ps_planned_count) %>% left_join(dplyr::select(pc_key_old, pc_code, ps_count)) %>%
  rename(ps_planned_count = ps_count)

pc_key_fixes <- pc_key_fixes %>% dplyr::select(
  province_code, province_name_eng, province_name_dari,
  district_code, district_name_eng, district_name_dari,
  district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital, 
  pc_code, pc_name_eng, pc_name_dari,
  final_results_reporting, ps_planned_count, ps_reporting_count, ps_male, ps_fem, ps_kuchi, est_voters, access_notes
)
pc_key_fixes$final_results_reporting[is.na(pc_key_fixes$final_results_reporting)] <- "NO"

# write.csv(pc_key_fixes, "./past_elections/presidential_2009/keyfiles/pc_key_2009.csv", row.names = F)
pc_key <- read.csv("./past_elections/presidential_2009/keyfiles/pc_key_2009.csv", stringsAsFactors = F)

pc_key$pc_code <- as.character(str_pad(pc_key$pc_code, 7, pad = "0", side = "left"))
ps_reporting$pc_code <- as.character(str_pad(ps_reporting$pc_code, 7, pad = "0", side = "left"))
pc_key$ps_reporting_count[pc_key$final_results_reporting == "NO"] <- 0
 
ps_planned <- data.frame()
for(i in 1:length(pc_key$pc_code)){
  pc_code = as.character(pc_key$pc_code[i])
  ps_count = pc_key$ps_planned_count[i]
  ps_male = pc_key$ps_male[i]
  ps_fem = pc_key$ps_fem[i]
  ps_kuchi = pc_key$ps_kuchi[i]
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
  
  if(!is.na(ps_kuchi) & ps_kuchi > 0){  
  for(l in 1:ps_kuchi){
    ps_code = as.character(paste0(pc_code, "-", as.character(str_pad(sum(l, as.numeric(ps_male), as.numeric(ps_fem), na.rm = T), 2, pad = "0", side = "left"))))
    ps_type = "K"
    row = data.frame(pc_code, ps_code, ps_type)
    row_out <- rbind(row_out, row)
  }} else {NULL}
  
  ps_planned <- rbind(ps_planned, row_out)
}

ps_planned$pc_code <- as.character(ps_planned$pc_code)
ps_planned$ps_code <- as.character(ps_planned$ps_code)
ps_planned$ps_type <- as.character(ps_planned$ps_type)

ps_reporting_status <- ps_planned %>% left_join(ps_reporting)
ps_reporting_status$final_results_reporting[is.na(ps_reporting_status$final_results_reporting)] <- "NO"

ps_reporting_not_planned <- all_data %>% dplyr::select(pc_code, ps_code) %>% unique() %>% anti_join(ps_planned) %>% mutate(
  final_results_reporting = "YES",
  planned_ps = "NO"
)

ps_reporting_status_with_new <- full_join(ps_reporting_status, ps_reporting_not_planned) %>% arrange(pc_code, ps_code)
ps_reporting_status_with_new$planned_ps[is.na(ps_reporting_status_with_new$planned_ps)] <- "YES"
ps_reporting_status_with_new$ps_type[is.na(ps_reporting_status_with_new$ps_type)] <- "UNKNOWN"

# duplicated_ps <- ps_reporting_status_with_new[duplicated(ps_reporting_status_with_new$ps_code), ]
# setdiff(unique(all_data$ps_code), ps_reporting_status_with_new$ps_code)

write.csv(ps_reporting_status_with_new, "./past_elections/presidential_2009/keyfiles/ps_key_2009.csv", row.names = F)

all_data_with_ps_type <- left_join(all_data, ps_reporting_status_with_new) %>% dplyr::select(
  election_date, results_date, election_type, results_status, province_code, province_name_eng, province_name_dari,
  district_code, district_name_eng, district_name_dari, district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital,
  pc_code, pc_name_eng, pc_name_dari, ps_code, ps_type, planned_ps,
  candidate_code, ballot_position, candidate_name_eng, candidate_name_dari, candidate_gender, party_name_dari, past_winner, votes, final_winner) %>% arrange(
    province_code, district_code, pc_code, ps_code, ballot_position
  )

write.csv(all_data_with_ps_type, "./past_elections/presidential_2009/results_data/final_af_candidate_ps_data_2009.csv", row.names = F)

# ADD PS DISQUALIFICATION INFO
rm(list = ls())
ps_key <- read.csv("./past_elections/presidential_2009/keyfiles/ps_key_2009.csv", stringsAsFactors = F)
all_data <- read.csv("./past_elections/presidential_2009/results_data/final_af_candidate_ps_data_2009.csv", stringsAsFactors = F)

disqualifed_by_ecc <- pdf_text("./past_elections/presidential_2009/raw/20091021_PollingStations_Disqualified_ByECCBasedOnComplaints.pdf")
disq_ecc_text <- toString(disqualifed_by_ecc)
disq_ecc_lines <- read_lines(disq_ecc_text)
disq_ecc_codes <- disq_ecc_lines[-(1:4)]
disq_ecc_codes <- Reduce(rbind, strsplit(trimws(disq_ecc_codes), " "))
ecc_codes <- disq_codes[grepl("\\d{9}", disq_ecc_codes)]

disqualified_ps_by_ecc <- tibble(paste0(str_sub(ecc_codes, 1, 7), "-", str_sub(ecc_codes, 8,9)))
colnames(disqualified_ps_by_ecc) <- "ps_code"
disqualified_ps_by_ecc <- disqualified_ps_by_ecc %>% mutate(ps_status = "Disqualifed as a results of ECC decisions on complaints")

disqualified_by_iec_and_ecc <- pdf_text("./past_elections/presidential_2009/raw/20091021_PollingStations_QuarintinedAndDisqualified.pdf")
disq_iec_text <- toString(disqualified_by_iec_and_ecc)
disq_iec_lines <- read_lines(disq_iec_text)
disq_iec_codes <- disq_iec_lines[-(1:3)]
disq_iec_codes <- gsub(",", "", disq_iec_codes)
disq_iec_codes <- trimws(disq_iec_codes)
disq_iec_codes <- Reduce(rbind, strsplit(trimws(disq_iec_codes), "\\s{1,}"))
iec_codes <- data.frame(disq_iec_codes) %>% dplyr::select(X2)
iec_codes$X2 <- as.character(iec_codes$X2)
iec_codes$X2 <- as.character(str_pad(iec_codes$X2, 9, pad = "0", side = "left"))

disqualified_ps_by_iec_and_ecc <- tibble(paste0(str_sub(iec_codes$X2, 1, 7), "-", str_sub(iec_codes$X2, 8,9)))
colnames(disqualified_ps_by_iec_and_ecc) <- "ps_code"
disqualified_ps_by_iec_and_ecc <- disqualified_ps_by_iec_and_ecc %>% mutate(ps_status = "Quarintined by IEC and Disqualified by ECC")

ps_key_with_ecc_disq <- left_join(ps_key, disqualified_ps_by_ecc) %>% filter(!is.na(ps_status))
ps_key_with_iec_disq <- left_join(ps_key, disqualified_ps_by_iec_and_ecc) %>% filter(!is.na(ps_status))
ps_key_with_disq <- ps_key %>% anti_join(ps_key_with_ecc_disq) %>% anti_join(ps_key_with_iec_disq) %>% 
  full_join(ps_key_with_ecc_disq) %>% full_join(ps_key_with_iec_disq) %>% arrange(pc_code, ps_code)
ps_key_with_disq$ps_status[is.na(ps_key_with_disq$ps_status)] <- "PS Validated"

write.csv(ps_key_with_disq, "./past_elections/presidential_2009/keyfiles/ps_key_2009.csv", row.names = F)

results_with_disq <- left_join(all_data, ps_key_with_disq) %>% dplyr::select(
  election_date, results_date, election_type, results_status, 
  province_code, province_name_eng, province_name_dari, 
  district_code, district_name_eng, district_name_dari, district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, provincial_capital,
  pc_code, pc_name_eng, pc_name_dari, ps_code, ps_type, planned_ps, ps_status,
  candidate_code, ballot_position, candidate_name_eng, candidate_name_dari, candidate_gender, party_name_dari, past_winner, votes, final_winner)

write.csv(results_with_disq, "./past_elections/presidential_2009/results_data/final_af_candidate_ps_data_2009.csv", row.names = F)

# TABULATION CHECK -------------------------
rm(list = ls())

all_data <- read.csv("./past_elections/presidential_2009/results_data/final_af_candidate_ps_data_2009.csv", stringsAsFactors = F)
candidate_key <- read.csv("./past_elections/presidential_2009/keyfiles/candidate_key_2009.csv", stringsAsFactors = F)

national_sums <- all_data %>% group_by(candidate_code, ballot_position) %>% summarize(votes = sum(votes, na.rm = TRUE))
iec_totals <- read.csv("./past_elections/presidential_2009/raw/final_results_candidates.csv", stringsAsFactors = F)

# correct IEC ballot positions, which are wrong on the web
iec_totals$ballot_position <- c(1, 3:14, 16:18, 20:21, 23:25, 27, 30:33, 35, 37:41)

tabulation_check <- left_join(iec_totals, national_sums)
tabulation_check$error <- ifelse(tabulation_check$votes != tabulation_check$total_votes, "ERROR", "OK")
tabulation_check <- tabulation_check %>% filter(error == "ERROR")
tabulation_check <- select(tabulation_check, candidate_code, candidate_name_eng, total_votes, votes)
tabulation_check <- tabulation_check %>% rename(
  iec_vote_total = total_votes,
  calculated_vote_total = votes
)
tabulation_check <- tabulation_check %>% mutate(difference = calculated_vote_total - iec_vote_total)
tabulation_check <- tabulation_check %>% dplyr::select(candidate_code, candidate_name_eng, everything())

write.csv(tabulation_check, "./past_elections/presidential_2009/validity_checks/final_iec_tabulation_check.csv", row.names = F)

# ATTEMPT TO PARSE PRELIMINARY RESULTS PDFS -----------------------------------

rm(list = ls())

prelim_raw <- pdf_text("./past_elections/presidential_2009/raw/final_uncertified_ps_results_16_09_2009.pdf")
prelim_text <- toString(prelim_raw)
prelim_lines <- read_lines(prelim_text)

# strip bad parse of dari and other characters
prelim_lines <- gsub("\002", " ", prelim_lines)
prelim_lines <- gsub("\\(  \\\003\\\004\\\005\\\006 \\\a\\\b)       \\\004 \\\016\\\017\\\020 \\\021\\\004\\\022 \\\023\\\017\\\022\\\024", "", prelim_lines)
prelim_lines <- gsub("\\\025 \\\026\\\022 \\\027 \\\006 \\\026\\\022\\\026", "", prelim_lines)
prelim_lines <- gsub("\\\a\\\030\\\031\\\024 \\\016\\\004 \\\024 \\\016\\\004\\\032\\\026 \\\004\\\033\\\020\\\033", "", prelim_lines)
prelim_lines <- gsub("\\\003\\\034\\\033\\\026 \\\035\\\004\\\005\\\022 \\\030\\\036", "", prelim_lines)
prelim_lines <- gsub("\\\a\\\033\\\017\\\030\\\006 \\\027\\\033 \\\025 \\\017\\\037   \\!\\\004\\\005", "", prelim_lines)
prelim_lines <- gsub("\\\021\\\004 \\\036 \\\003\\#\\\020 \\\004\\\006", "", prelim_lines)
prelim_lines <- gsub("\\\a\\\033\\\004\\\006\\\017\\\004 \\$\\\004\\\027 \\\004\\\026", "", prelim_lines)
prelim_lines <- gsub("\\\004   \\\021\\\006\\\005", "", prelim_lines)
prelim_lines <- gsub("\\\016\\%\\\004\\\005 \\\026\\\022 \\\004 \\\005", "", prelim_lines)
prelim_lines <- gsub("\\\a\\\005\\&\"\\\026 \'\\\017\\\005 \\\021\\(\\\030\\\032\\\026", "", prelim_lines)
prelim_lines <- gsub("\\\025 \\\026\\\022 \\\a\\\033\\\037 \\)", "", prelim_lines)
prelim_lines <- gsub("\\( \\\021 \\* \\) \\)\\\004\\+\\,\\\024 \\\005\\-", "", prelim_lines)
prelim_lines <- gsub("\\\a\\.\\\030 \\\027\\\033\\/", "", prelim_lines)
prelim_lines <- gsub("\\( \\\a\\\030\\\036  \\) \\\021\\#\\\006\\\024 \\\005\\- \\#\\\026", "", prelim_lines)
prelim_lines <- gsub("\\( \\\a0\\\b\\\017\\\022 \\) 1\\\022\\\024 \\\017\\\0042", "", prelim_lines)
prelim_lines <- gsub("\\( \\\017\\\004 \\\027 \\) \\\005\\\036 \\\026\\\022\\\026", "", prelim_lines)
prelim_lines <- gsub("\\( \\\a\\\033\\\017\\\026\\\022  \\) \\\026\\\022\\\026 \\\016\\\004\\\034\\\033\\\006", "", prelim_lines)
prelim_lines <- gsub("\\(\\\a\\\033\\\017 \\\027\\\036 \\)   \\\035\\\027\\\005\\\022\\\026", "", prelim_lines)
prelim_lines <- gsub("\\( \\\016\\\004 3 \\) \\\035\\\004\\\006\\\022\\\024 \\\005\\-", "", prelim_lines)
prelim_lines <- gsub("\\(\\\a\\\034\\\004 \\) \\\026\\\022\\\026 \\\021\\#\\\037 \\#\\\026", "", prelim_lines)
prelim_lines <- gsub("4\\\006\\\027 \\\005 \\\016\\\0172\\\026", "", prelim_lines)
prelim_lines <- gsub("\\( \\\021\\\004\\\026\\( \\) \\\004\\\020\\\026\\\024 \\\005\\-", "", prelim_lines)
prelim_lines <- gsub("\\( \\\017\\+\\- \\) \\#\\/", "", prelim_lines)
prelim_lines <- gsub("\\(\\\a\\\005 \\\020\\\033 \\) 1\\\027 \\\017\\\b \\\021\\#\\\037 \\\027\\\030\\\036\\\027 \\\027\\\006\\\004\\\b\\\027 \\*", "", prelim_lines)
prelim_lines <- gsub("\\(\\\025 \\\027 \\) \\\027\\\0315\\\024 \\\005\\- \\\023\\\017\\\022\\\024", "", prelim_lines)
prelim_lines <- gsub(" \\\a6\\\004\\\b\\\027\\\030 \\\021 \\\017\\& \\\026\\\022\\\026", "", prelim_lines)
prelim_lines <- gsub("\\\025 \\\036 \\\026\\\017\\\022", "", prelim_lines)
prelim_lines <- gsub("\\(   \\\005\\- \\)   \\\005\\- \\\030\\\036", "", prelim_lines)
prelim_lines <- gsub("\\( \\\017\\\033\\\b \\) \\\016 \\\027 \\\b \\\030\\\036", "", prelim_lines)
prelim_lines <- gsub("4\\\005\\\0177 \\\017\\\005\\\020\024 \\\005\\-", "", prelim_lines)
prelim_lines <- gsub("\\#\\\006 \\\016\\\004\\\026 4\\\004 \\&", "", prelim_lines)
prelim_lines <- gsub("\\(\\\017\\\026\\\004\\) \\\026\\\022 \\\0038", "", prelim_lines)
prelim_lines <- gsub("\\\025 \\\027         \\\027\\\0315\\\024 \\\005\\- \\\023\\\017\\\022\\\024", "", prelim_lines)
prelim_lines <- gsub("\\\003", "-", prelim_lines)
prelim_lines <- gsub("\\(", "", prelim_lines)
prelim_lines <- gsub("\\)", "", prelim_lines)
prelim_lines <- gsub('\\"', "", prelim_lines)

headers <- c((1:5), grep("Independent Election Commission", prelim_lines))
totals <- grep("Total", prelim_lines)
prelim_trimmed <- prelim_lines[- c(headers, totals)]

pc_start <- grep("\\d{6,7} Haji", prelim_trimmed)
# pc_end <- grep("\\d{6,7} Total", prelim_trimmed)
pc_repeat <- setdiff(grep("\\d{6,7}", prelim_trimmed), c(pc_start))
prelim_trimmed[pc_repeat] <- gsub("\\d{6,7}", " ", prelim_trimmed[pc_repeat])

# strip out province headers
prelim_trimmed <- gsub("Kabul", "", prelim_trimmed)
prelim_trimmed <- gsub("Kapisa", "", prelim_trimmed)
prelim_trimmed <- gsub("Parwan", "", prelim_trimmed)
prelim_trimmed <- gsub("Wardak", "", prelim_trimmed)
prelim_trimmed <- gsub("Logar", "", prelim_trimmed)
prelim_trimmed <- gsub("Ghazni", "", prelim_trimmed)
prelim_trimmed <- gsub("Paktika", "", prelim_trimmed)
prelim_trimmed <- gsub("Paktia", "", prelim_trimmed)
prelim_trimmed <- gsub("Khost", "", prelim_trimmed)
prelim_trimmed <- gsub("Nangerhar", "", prelim_trimmed)
prelim_trimmed <- gsub("Kunarha", "", prelim_trimmed)
prelim_trimmed <- gsub("Kapisa", "", prelim_trimmed)
prelim_trimmed <- gsub("Laghman", "", prelim_trimmed)
prelim_trimmed <- gsub("Nooristan", "", prelim_trimmed)
prelim_trimmed <- gsub("Badakhshan", "", prelim_trimmed)
prelim_trimmed <- gsub("Takhar", "", prelim_trimmed)
prelim_trimmed <- gsub("Baghlan", "", prelim_trimmed)
prelim_trimmed <- gsub("Kunduz", "", prelim_trimmed)
prelim_trimmed <- gsub("Samangan", "", prelim_trimmed)
prelim_trimmed <- gsub("Balkh", "", prelim_trimmed)
prelim_trimmed <- gsub("Juzjan", "", prelim_trimmed)
prelim_trimmed <- gsub("Sar-i-Pul", "", prelim_trimmed)
prelim_trimmed <- gsub("Faryab", "", prelim_trimmed)
prelim_trimmed <- gsub("Badghis", "", prelim_trimmed)
prelim_trimmed <- gsub("Herat", "", prelim_trimmed)
prelim_trimmed <- gsub("Farah", "", prelim_trimmed)
prelim_trimmed <- gsub("Nimroz", "", prelim_trimmed)
prelim_trimmed <- gsub("Helmand", "", prelim_trimmed)
prelim_trimmed <- gsub("Kandahar", "", prelim_trimmed)
prelim_trimmed <- gsub("Zabul", "", prelim_trimmed)
prelim_trimmed <- gsub("Urozgan", "", prelim_trimmed)
prelim_trimmed <- gsub("Ghor", "", prelim_trimmed)
prelim_trimmed <- gsub("Bamyan", "", prelim_trimmed)
prelim_trimmed <- gsub("Panjshir", "", prelim_trimmed)
prelim_trimmed <- gsub("Daikondi", "", prelim_trimmed)


pc_codes <- str_extract(prelim_trimmed[pc_start], "\\d{6,7}")

pc_code_applied <- list()
for(n in 1:length(pc_codes)){
  for(m in 1:32){
    count = n
    pc_code_applied = c(pc_code_applied, count)
  }
}

# this is very slow!
ps_out <- data.frame()

for(i in (1:(length(prelim_trimmed)-1))){
  line = prelim_trimmed[i]
  # strip any pc code at the start of the line
  new_line <- gsub("^(.*)\\d{6,7}", " ", line)
  # find the appropriate pc code
  pc_code_location <- as.numeric(pc_code_applied[i])
  # add the pc code to every line
  new_line_pc <- paste0(pc_codes[pc_code_location], "  ", new_line)
  # make sure there is a proper gap between vote numbers so that columns are properly detected
  new_line_filled <- gsub("(?<=\\d) ", perl = T, replacement = "  ", x = new_line_pc)
  # split up into separate columns
  new_line_split <- str_split(new_line_filled, "\\s{2,}")
  
  # now, process each column as a new ps-candidate row
  for(j in (3:(length(new_line_split[[1]])-1))){
    pc_code = str_pad(as.character(new_line_split[[1]][1]), width = 7, side = "left", pad = "0"))
    candidate_name_eng = new_line_split[[1]][2]
    ps_code = paste0(pc_code, "-", as.character(str_pad(as.character(j-2), width = 2, side = "left", pad = "0")))
    votes = new_line_split[[1]][j]
    ps_row = data.frame(pc_code, candidate_name_eng, ps_code, votes)
    ps_out <- rbind(ps_out, ps_row)
  }
}

prelim_ps_2009 <- ps_out
prelim_ps_2009$pc_code <- as.character(str_pad(as.character(prelim_ps_2009$pc_code), 7, pad = "0", side = "left"))
prelim_ps_2009 <- prelim_ps_2009 %>% rowwise %>% mutate(ps_code_new = paste0(pc_code, "-", str_split(ps_code, "-")[[1]][2]))
prelim_ps_2009 <- prelim_ps_2009 %>% dplyr::select(- ps_code) %>% rename(ps_code = ps_code_new)
ps_cut <- prelim_ps_2009[- grep("Alhaj Abdul Ghafor Zori", prelim_ps_2009$candidate_name_eng), ]
prelim_ps_2009_fixed <- full_join(ps_cut, ps_out) %>% arrange(pc_code, ps_code, candidate_name_eng)

prelim_ps_2009_fixed$candidate_name_eng <- as.character(prelim_ps_2009$candidate_name_eng)
prelim_ps_2009_fixed$ps_code <- as.character(prelim_ps_2009$ps_code)
prelim_ps_2009_fixed$votes <- as.numeric(as.character(prelim_ps_2009_fixed$votes))

prelim_ps_2009_fixed <- dplyr::select(prelim_ps_2009_fixed, pc_code, ps_code, candidate_name_eng, votes) %>%
  arrange(pc_code, ps_code, candidate_name_eng)

write.csv(prelim_ps_2009_fixed, "./past_elections/presidential_2009/raw/rough_prelim_2009.csv", row.names = F)

# parse final results pdf
rm(list = ls())

final_raw <- pdf_text("./past_elections/presidential_2009/raw/final_certified_ps_results_03_11_2009.pdf")
final_text <- toString(final_raw)
final_lines <- read_lines(final_text)


final_lines <- gsub("\\(", "", final_lines)
final_lines <- gsub("\\)", "", final_lines)
final_lines <- gsub('\\"', "", final_lines)
final_lines <- gsub(str_split(final_lines[5], "\\s{2,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[6], "\\s{2,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[7], "\\s{2,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[8], "\\s{2,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[9], "\\s{2,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[10], "\\s{2,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[11], "\\s{2,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[12], "\\s{2,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[13], "\\s{2,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[14], "\\s{2,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[15], "\\s{2,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[16], "\\s{3,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[17], "\\s{2,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[18], "\\s{3,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[19], "\\s{3,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[20], "\\s{2,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[21], "\\s{3,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[22], "\\s{3,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[23], "\\s{3,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[24], "\\s{2,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[25], "\\s{2,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[26], "\\s{3,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[27], "\\s{3,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[28], "\\s{3,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[29], "\\s{2,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[30], "\\s{2,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[31], "\\s{2,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[32], "\\s{3,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[33], "\\s{3,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[34], "\\s{2,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[35], "\\s{2,}")[[1]][3], "", final_lines)
final_lines <- gsub(str_split(final_lines[36], "\\s{2,}")[[1]][3], "", final_lines)

headers <- c((1:4), grep("Independent Election Commission", final_lines))
totals <- grep("Total", final_lines)
final_trimmed <- final_lines[- c(headers, totals)]

pc_start <- grep("\\d{6,7} Haji", final_trimmed)
pc_repeat <- setdiff(grep("\\d{6,7}", final_trimmed), c(pc_start))
final_trimmed[pc_repeat] <- gsub("\\d{6,7}", " ", final_trimmed[pc_repeat])

# strip out province headers
final_trimmed <- gsub("Kabul", "", final_trimmed)
final_trimmed <- gsub("Kapisa", "", final_trimmed)
final_trimmed <- gsub("Parwan", "", final_trimmed)
final_trimmed <- gsub("Wardak", "", final_trimmed)
final_trimmed <- gsub("Logar", "", final_trimmed)
final_trimmed <- gsub("Ghazni", "", final_trimmed)
final_trimmed <- gsub("Paktika", "", final_trimmed)
final_trimmed <- gsub("Paktia", "", final_trimmed)
final_trimmed <- gsub("Khost", "", final_trimmed)
final_trimmed <- gsub("Nangerhar", "", final_trimmed)
final_trimmed <- gsub("Kunarha", "", final_trimmed)
final_trimmed <- gsub("Kapisa", "", final_trimmed)
final_trimmed <- gsub("Laghman", "", final_trimmed)
final_trimmed <- gsub("Nooristan", "", final_trimmed)
final_trimmed <- gsub("Badakhshan", "", final_trimmed)
final_trimmed <- gsub("Takhar", "", final_trimmed)
final_trimmed <- gsub("Baghlan", "", final_trimmed)
final_trimmed <- gsub("Kunduz", "", final_trimmed)
final_trimmed <- gsub("Samangan", "", final_trimmed)
final_trimmed <- gsub("Balkh", "", final_trimmed)
final_trimmed <- gsub("Juzjan", "", final_trimmed)
final_trimmed <- gsub("Sar-i-Pul", "", final_trimmed)
final_trimmed <- gsub("Sar‐i‐Pul", "", final_trimmed)
final_trimmed <- gsub("Faryab", "", final_trimmed)
final_trimmed <- gsub("Badghis", "", final_trimmed)
final_trimmed <- gsub("Herat", "", final_trimmed)
final_trimmed <- gsub("Farah", "", final_trimmed)
final_trimmed <- gsub("Nimroz", "", final_trimmed)
final_trimmed <- gsub("Helmand", "", final_trimmed)
final_trimmed <- gsub("Kandahar", "", final_trimmed)
final_trimmed <- gsub("Zabul", "", final_trimmed)
final_trimmed <- gsub("Urozgan", "", final_trimmed)
final_trimmed <- gsub("Ghor", "", final_trimmed)
final_trimmed <- gsub("Bamyan", "", final_trimmed)
final_trimmed <- gsub("Panjshir", "", final_trimmed)
final_trimmed <- gsub("Daikondi", "", final_trimmed)

pc_codes <- str_extract(final_trimmed[pc_start], "\\d{6,7}")

pc_code_applied <- list()
for(n in 1:length(pc_codes)){
  for(m in 1:32){
    count = n
    pc_code_applied = c(pc_code_applied, count)
  }
}

# this is very slow!
ps_out <- data.frame()

for(i in (151846:(length(final_trimmed)-1))){
  line = final_trimmed[i]
  # strip any pc code at the start of the line
  new_line <- gsub("^(.*)\\d{6,7}", " ", line)
  # find the appropriate pc code
  pc_code_location <- as.numeric(pc_code_applied[i])
  # add the pc code to every line
  new_line_pc <- paste0(pc_codes[pc_code_location], "  ", new_line)
  # make sure there is a proper gap between vote numbers so that columns are properly detected
  new_line_filled <- gsub("(?<=\\d) ", perl = T, replacement = "  ", x = new_line_pc)
  # split up into separate columns
  new_line_split <- str_split(new_line_filled, "\\s{2,}")
  
  # now, process each column as a new ps-candidate row
  for(j in (3:(length(new_line_split[[1]])-1))){
    pc_code = str_pad(as.character(new_line_split[[1]][1]), width = 7, side = "left", pad = "0")
    candidate_name_eng = new_line_split[[1]][2]
    ps_code = paste0(pc_code, "-", as.character(str_pad(as.character(j-2), width = 2, side = "left", pad = "0")))
    votes = new_line_split[[1]][j]
    ps_row = data.frame(pc_code, candidate_name_eng, ps_code, votes)
    ps_out <- rbind(ps_out, ps_row)
  }
}



final_ps_2009 <- ps_out
final_ps_2009$pc_code <- as.character(final_ps_2009$pc_code)
final_ps_2009$candidate_name_eng <- as.character(final_ps_2009$candidate_name_eng)
final_ps_2009$ps_code <- as.character(final_ps_2009$ps_code)
final_ps_2009$votes <- as.numeric(as.character(final_ps_2009$votes))

final_ps_2009 <- dplyr::select(final_ps_2009, pc_code, ps_code, candidate_name_eng, votes) %>%
  arrange(pc_code, ps_code, candidate_name_eng)

write.csv(final_ps_2009, "./past_elections/presidential_2009/raw/rough_final_2009.csv", row.names = F)

# UPDATE PC KEY AND PS KEY AND ADD METADATA TO RESULTS, RE-AGGREGATE AGAIN --------------

rm(list = ls())

ps_key_2009 <- read_csv("past_elections/presidential_2009/keyfiles/ps_key_2009.csv")
ps_key_2009$pc_code <- str_pad(as.character(ps_key_2009$pc_code), width = 7, side = "left", pad = "0")
pc_key_2009 <- read_csv("past_elections/presidential_2009/keyfiles/pc_key_2009.csv")
candidate_key_2009 <- read_csv("past_elections/presidential_2009/keyfiles/candidate_key_2009.csv")
rough_prelim_2009 <- read_csv("past_elections/presidential_2009/raw/rough_prelim_2009.csv")

setdiff(unique(rough_prelim_2009$pc_code), pc_key_2009$pc_code)
setdiff(unique(rough_prelim_2009$ps_code), ps_key_2009$ps_code)

# one PC missing from pre-election plan (2406471) - add to PC key


prelim_2009_full <- rough_prelim_2009 %>% mutate(
  election_date = mdy("08-20-2009"),
  results_date = mdy("09-16-2009"),
  election_type = "PRESIDENTIAL",
  results_status = "PRELIMINARY"
  ) %>%
  left_join(
    dplyr::select(pc_key_2009,
            province_code, province_name_eng, province_name_dari, 
            district_code, district_name_eng, district_name_dari,
            district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari,
            provincial_capital, 
            pc_code, pc_name_eng, pc_name_dari)) %>%
  mutate(prelim_results_reporting = "YES") %>%
  left_join(ps_key_2009) %>% 
  left_join(candidate_key_2009) %>% 
  dplyr::select(election_date, results_date, election_type, results_status,
                province_code, province_name_eng, province_name_dari,
                district_code, district_name_eng, district_name_dari, 
                district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari,
                provincial_capital,
                pc_code, pc_name_eng, pc_name_dari, 
                ps_code, ps_type, planned_ps, ps_status,
                candidate_code, ballot_position, candidate_name_eng, candidate_name_dari, candidate_gender,
                party_name_dari, past_winner, votes, final_winner) %>%
  arrange(pc_code, ps_code, ballot_position)
  


# fix row count


