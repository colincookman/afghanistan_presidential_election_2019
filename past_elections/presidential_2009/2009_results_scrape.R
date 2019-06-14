library(XML)
library(tidyverse)
library(rvest)
library(wdman)
library(lubridate)
library(pdftools)
library(readr)
library(RSelenium)
#library(tabulizer)
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

# UPDATE PC KEY WITH REPORTING STATUS ----------------------------------------

# COMPLETENESS CHECKS AND SUMMARY STATS --------------------------------------

