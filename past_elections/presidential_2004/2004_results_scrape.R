library(XML)
library(tidyverse)
library(rvest)
library(wdman)
library(lubridate)

# CREATE CANDIDATE KEY --------------------------------------------------------

eng_final_candidate_total_url <- "http://www.iec.org.af/public_html/Election%20Results%20Website/english/results_enwww/results_enwww_4.5.html"
dari_final_candidate_total_url <- "http://www.iec.org.af/public_html/Election%20Results%20Website/dari/results_dawww/results_dawww_4.5.html"
pashto_final_candidate_total_url <- "http://www.iec.org.af/public_html/Election%20Results%20Website/pashtu/results_pawww/results_pawww_4.5.html"

eng_candidate_list <- read_html(eng_final_candidate_total_url) %>% html_nodes("table") %>% html_table(fill = TRUE, trim = TRUE, header = TRUE)
eng_candidates <- data.frame(eng_candidate_list[[4]])
eng_candidates <- eng_candidates[-c(1, 20:24), ]
rownames(eng_candidates) <- 1:dim(eng_candidates)[1]
colnames(eng_candidates) <- c("candidate_name_eng", "party_name_eng_transliterated", "votes", "pct_total")

eng_candidates <- eng_candidates %>% mutate(ballot_position = row_number())
eng_candidates$votes <- as.numeric(gsub(",", "", eng_candidates$votes))
eng_candidates$pct_total <- NULL


dari_candidate_list <- read_html(dari_final_candidate_total_url) %>% html_nodes("table") %>% html_table(fill = TRUE, trim = TRUE, header = TRUE)
dari_candidates <- data.frame(dari_candidate_list[[4]])
dari_candidates <- dari_candidates[-c(1, 20:24), ]
rownames(dari_candidates) <- 1:dim(dari_candidates)[1]
colnames(dari_candidates) <- c("pct_total", "votes", "party_name_dari", "candidate_name_dari")

dari_candidates <- dari_candidates %>% mutate(ballot_position = row_number())
dari_candidates <- dari_candidates %>% dplyr::select("ballot_position", "candidate_name_dari", "party_name_dari")

pashto_candidate_list <- read_html(pashto_final_candidate_total_url) %>% html_nodes("table") %>% html_table(fill = TRUE, trim = TRUE, header = TRUE)
pashto_candidates <- data.frame(pashto_candidate_list[[4]])
pashto_candidates <- pashto_candidates[-c(1, 20:24), ]
rownames(pashto_candidates) <- 1:dim(pashto_candidates)[1]
colnames(pashto_candidates) <- c("pct_total", "votes", "party_name_pashto", "candidate_name_pashto")

pashto_candidates <- pashto_candidates %>% mutate(ballot_position = row_number())
pashto_candidates <- pashto_candidates %>% dplyr::select("ballot_position", "candidate_name_pashto", "party_name_pashto")

candidate_key <- eng_candidates %>% left_join(dari_candidates) %>% left_join(pashto_candidates) %>% 
  arrange(ballot_position) %>% dplyr::select(
    ballot_position, candidate_name_eng, candidate_name_dari, candidate_name_pashto, party_name_eng_transliterated, party_name_dari, party_name_pashto
  )

candidate_key$final_winner[candidate_key$ballot_position == "2"] <- "YES"
candidate_key$final_winner[is.na(candidate_key$final_winner)] <- "NO"
candidate_key$candidate_gender[candidate_key$ballot_position == "16"] <- "F"
candidate_key$candidate_gender[is.na(candidate_key$candidate_gender)] <- "M"

write.csv(candidate_key, "./past_elections/presidential_2004/keyfiles/candidate_key_2004.csv", row.names = F)

# GET PROVINCIAL-LEVEL RESULTS ------------------------------------------------

eng_province_results_url <- "http://www.iec.org.af/public_html/Election%20Results%20Website/english/map.htm"
province_links <- getHTMLLinks(eng_province_results_url)
province_names <- read_html(eng_province_results_url) %>% html_nodes("a") %>% html_text()

province_results <- data.frame()
for(i in 1:length(province_links)){
  url_target <- paste0("http://www.iec.org.af/public_html/Election%20Results%20Website/english/", province_links[i])
  province_page <- read_html(url_target) %>% html_nodes("table") %>% html_table(fill = TRUE, trim = TRUE, header = TRUE)
  province_table <- data.frame(province_page[[5]])
  province_table <- province_table[-c(1, 20:24), ]
  rownames(province_table) <- 1:dim(province_table)[1]
  colnames(province_table) <- c("candidate_name_eng", "party_name_eng_transliterated", "votes", "pct_total")
  province_table <- province_table %>% mutate(
    province_code = str_pad(i, 2, pad = "0", side = "left"),
    province_name_eng = province_names[i]) %>% dplyr::select(province_code, province_name_eng, everything())
  province_results <- rbind(province_results, province_table)
  Sys.sleep(2)
}

province_results$votes <- as.numeric(gsub(",", "", province_results$votes))

province_results_with_candidates <- left_join(province_results, candidate_key)

province_results_final <- province_results_with_candidates %>% dplyr::select(
  province_code, province_name_eng, ballot_position, candidate_name_eng, party_name_eng_transliterated, candidate_gender, final_winner, votes
) %>% arrange(province_code)


province_results_final$province_name_eng <- gsub(" \r\n        ", " - ", province_results_final$province_name_eng)

province_results_final$election_date <- dmy("09-10-2004")
province_results_final$results_date <- dmy("03-11-2004")
province_results_final$results_status <- "FINAL"
province_results_final$election_type <- "PRESIDENTIAL"
province_results_final <- province_results_final %>% dplyr::select(election_date, results_date, election_type, results_status, everything()) %>% 
  arrange(province_code, ballot_position)

write.csv(province_results_final, "./past_elections/presidential_2004/keyfiles/final_af_candidate_province_data_2004.csv", row.names = F)

# PROVINCE KEY ----------------------------------------------------------------

province_key <- province_results_final %>% dplyr::select(province_code, province_name_eng) %>% unique()
province_key$province_name_eng[35] <- "Out-of-Country - Iran"
province_key$province_name_eng[36] <- "Out-of-Country - Pakistan"

# surprise! dari and pashto provinces are listed in a different order than english, so will have to just do manually

#dari_province_results_url <- "http://www.iec.org.af/public_html/Election%20Results%20Website/dari/provinces.htm"
#dari_province_names <- read_html(dari_province_results_url) %>% html_nodes("a") %>% html_text()
#dari_province_names_df <- dari_province_names %>% as.data.frame() %>% 
#  mutate(province_code = str_pad(row_number(), 2, pad = "0", side = "left"))
#colnames(dari_province_names_df) <- c("province_name_dari", "province_code")
#dari_province_names_df$province_name_dari <- as.character(dari_province_names_df$province_name_dari)
#dari_province_names_df$province_name_dari <- gsub("\r\n        ", "", dari_province_names_df$province_name_dari)

#pashto_province_results_url <- "http://www.iec.org.af/public_html/Election%20Results%20Website/pashtu/pshprovinces.htm"
#pashto_province_names <- read_html(pashto_province_results_url) %>% html_nodes("a") %>% html_text()
#pashto_province_names_df <- pashto_province_names %>% as.data.frame() %>% 
#  mutate(province_code = str_pad(row_number(), 2, pad = "0", side = "left"))
#colnames(pashto_province_names_df) <- c("province_name_pashto", "province_code")
#pashto_province_names_df$province_name_pashto <- as.character(pashto_province_names_df$province_name_pashto)
#pashto_province_names_df$province_name_pashto <- gsub("\r\n        ", "", pashto_province_names_df$province_name_pashto)

iran <- dari_province_names_df$province_name_dari[35]
iran_pashto <- pashto_province_names_df$province_name_pashto[35]
pakistan <- dari_province_names_df$province_name_dari[36]
pakistan_pashto <- pashto_province_names_df$province_name_pashto[36]

# write.csv(province_key, "./past_elections/presidential_2004/keyfiles/province_key_2004.csv", row.names = F)
province_key <- read.csv("./past_elections/presidential_2004/keyfiles/province_key_2004.csv", stringsAsFactors = F)
province_key$province_name_dari[35] <- iran
province_key$province_name_dari[36] <- pakistan
province_key$province_name_pashto[35] <- iran_pashto
province_key$province_name_pashto[36] <- pakistan_pash
write.csv(province_key, "./past_elections/presidential_2004/keyfiles/province_key_2004.csv", row.names = F)
