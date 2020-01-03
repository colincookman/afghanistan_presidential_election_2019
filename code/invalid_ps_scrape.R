library(XML)
library(tidyverse)
library(stringr)
library(rvest)
library(RSelenium)
library(wdman)
library(seleniumPipes)
library(lubridate)
library(RCurl)
options(scipen=999)
setwd("~/Google Drive/GitHub/afghanistan_presidential_election_2019")

rm(list = ls())

#-----------------------------------------------------------------------------------

ps_scan_links <- list()
province_data_list <- list()

url <- "http://www.iec.org.af/results/en/invalid/invalid_by_pc"

# settings for silent background browsing
eCaps <- list(chromeOptions = list(
  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
))

driver <- rsDriver(browser="chrome", port = 4444L, chromever="78.0.3904.11", verbose = FALSE
#                     , extraCapabilities = eCaps
  )
driver$client$navigate(url)

Sys.sleep(3)

province_list <- xml2::read_html(driver$client$getPageSource()[[1]]) %>%
    rvest::html_nodes("#province") %>%
    rvest::html_children() %>%
    rvest::html_text() %>%
    dplyr::data_frame(province = .)
  
province_list <- province_list %>%
    dplyr::mutate(list_position = 1:length(province_list$province),
                  x = stringr::str_c("#province > option:nth-child(", list_position, ")")
                  )
province_list <- province_list[-1, ]

for(i in (1:length(province_list$x))){
  # make sure the province page has fully loaded by checking to see if the initial results table has loaded, loop until it is found

    province_target <- as.character(province_list[i, 3])
    element <- driver$client$findElement(using = 'css selector', province_target)
    element$clickElement()
    
    # wait to make sure the page has loaded properly
    Sys.sleep(10)
    
    scan_links <- driver$client$getPageSource()[[1]] %>% read_html() %>% 
    html_nodes("a") %>% html_attr("href")
      
    # get a list of all PS scans on the PC page
    scan_links <- scan_links[grepl(".png|.jpg", scan_links)]
    
    ps_scan_links <- c(ps_scan_links, scan_links)
    
    Sys.sleep(5)
    
    page_source <- driver$client$getPageSource()
    province_html <- page_source[[1]] %>% read_html()
    province_data_list_item <- province_html %>% html_nodes(".table-condensed") %>% html_text()
      
    # add html data to a master list      
    province_data_list <- c(province_data_list, province_data_list_item)
      
    Sys.sleep(5)
    
}

driver$client$close()
driver$server$stop()

# UNPACK SCRAPE RESULTS

backup <- province_data_list

table_out <- data.frame()
parse_error <- list()
for(a in (1:length(province_data_list))){ 
  # parse the html of each scraped province page

    split_rows <- str_split(province_data_list[a], "\n\n\n\n")
    rows_trimmed <- gsub("No\n  \tPolling Center\n  \tPolling Stations\n  \tResult Sheet Votes\n    Details\n    Results Sheet\n\n", "",
                         split_rows[[1]])
    rows_trimmed <- rows_trimmed[-length(rows_trimmed)]
    for(b in 1:(length(rows_trimmed))){
      cell_split <- str_split(rows_trimmed[b], "\n")
      if(length(cell_split[[1]] >= 7)){
      row_out <- tibble(serial_number = trimws(cell_split[[1]][1]),
                        pc_code = str_pad(trimws(str_split(cell_split[[1]][3], " - ")[[1]][2]), side = "left", pad = "0", width = 7),
                        ps_number = trimws(str_split(cell_split[[1]][4], "-")[[1]][1]),
                        ps_code = paste0(pc_code, "-", str_pad(ps_number, side = "left", pad = "0", width = 2)),
                        votes = trimws(cell_split[[1]][5]),
                        commission_decision_or_status = trimws(cell_split[[1]][6])
#                        zero_votes = cell_split[[1]][7]
                        )
      table_out <- rbind(table_out, row_out)} else
      {parse_error <- c(parse_error, cell_split)}
    }
}
  


table_out <- table_out %>% filter(!is.na(serial_number))

write.csv(table_out, "./audit_data/invalidated_ps_data.csv", row.names = F)


# INVALID PS SCANS --------------------------------------------------------------------

ps_scan_links_out <- tibble(target_url = ps_scan_links)
ps_scan_links_out <- ps_scan_links_out %>% rowwise %>%
  mutate(
    province_code = str_split(target_url, "/")[[1]][5],
    pc_code = str_split(target_url, "/")[[1]][7],
    scan_filename = str_split(target_url, "/")[[1]][8],
    ps_code = paste0(pc_code, "-", str_split(target_url, "-")[[1]][2])
  ) %>% dplyr::select(
    province_code, pc_code, ps_code, scan_filename, target_url
  ) %>% data.frame()

ps_scan_links_out$province_code <- str_pad(ps_scan_links_out$province_code, pad = "0", side = "left", width = 2)
ps_scan_links_out$pc_code <- str_pad(ps_scan_links_out$pc_code, pad = "0", side = "left", width = 7)

ps_scan_links_write = data.frame(lapply(ps_scan_links_out, as.character), stringsAsFactors=FALSE)

write.csv(ps_scan_links_write, "./audit_data/invalidated_ps_scan_links.csv", row.names = F)

scan_links_list <- read_csv("./audit_data/invalidated_ps_scan_links.csv")

# create a subdirectory for scraped polling station scans if it doesn't already exist
ifelse(!dir.exists(file.path("./ps_scans/invalidated_ps_scans")), dir.create(file.path("./audit_data/invalidated_ps_scans")), FALSE)

ps_scan_error_log <- list()

for(n in 1:length(scan_links_list$target_url)){
  link <- scan_links_list[[5]][[n]]
  
  prov <- scan_links_list$province_code[n]
  pc <- scan_links_list$pc_code[n]
  
  prov_path <- paste0("./ps_scans/invalidated_ps_scans/", prov)
  pc_path <- paste0(prov_path, "/", pc)
  
  # create the appropriate province subdirectory if it doesn't already exist                   
  ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
  
  # create the appropriate pc subdirectory if it doesn't already exist                   
  ifelse(!dir.exists(file.path(pc_path)), dir.create(file.path(pc_path)), FALSE)
  
  tryCatch(download.file(link, file.path(pc_path, basename(link)), mode = "wb", quiet = FALSE),
           error = function(e){ps_scan_error_log <- c(ps_scan_error_log, link)} # this is not logging failed downloads correctly?
  )
  Sys.sleep(4)
}

# second pass 
downloaded_ps <- list.files("./ps_scans/invalidated_ps_scans/", pattern = "png", recursive = T, full.names = T)
download_filenames <- tibble(ps_path = downloaded_ps) %>% rowwise %>% mutate(scan_filename = str_split(ps_path, "/")[[1]][7])

missing_downloads <- scan_links_list$scan_filename[!(scan_links_list$scan_filename %in% download_filenames$scan_filename)]
missing_download_targets <- scan_links_list[scan_links_list$scan_filename %in% missing_downloads, ]

for(n in 1:length(missing_download_targets$target_url)){
  link <- missing_download_targets[[5]][[n]]
  
  prov <- missing_download_targets$province_code[n]
  pc <- missing_download_targets$pc_code[n]
  
  prov_path <- paste0("./ps_scans/invalidated_ps_scans/", prov)
  pc_path <- paste0(prov_path, "/", pc)
  
  # create the appropriate province subdirectory if it doesn't already exist                   
  ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
  
  # create the appropriate pc subdirectory if it doesn't already exist                   
  ifelse(!dir.exists(file.path(pc_path)), dir.create(file.path(pc_path)), FALSE)
  
  tryCatch(download.file(link, file.path(pc_path, basename(link)), mode = "wb", quiet = FALSE),
           error = function(e){ps_scan_error_log <- c(ps_scan_error_log, link)} # this is not logging failed downloads correctly?
  )
  Sys.sleep(4)
}

missing_download_targets$link_broken <- "YES"
write.csv(missing_download_targets, "./audit_data/invalidated_ps_scans_broken_links.csv", row.names = F)


# UPDATE PS STATUS ------------------------------------------------------------
rm(list = ls())

prelim_af_candidate_ps_data_2019_lite <- read_csv("results_data/first_round_preliminary_results/prelim_af_candidate_ps_data_2019_lite.csv")
invalidated_ps <- read_csv("./audit_data/invalidated_ps_data.csv")
ps_audit_target_list <- read_csv("audit_data/ps_audit_target_list.csv")

prelim_ps_reporting_status <- read_csv("validity_checks/prelim_ps_reporting_status.csv")
ps_summary_report <- read_csv("analysis/ps_summary_report.csv")

# invalidated_in_prelim <- prelim_af_candidate_ps_data_2019_lite[prelim_af_candidate_ps_data_2019_lite$ps_code %in% invalidated_ps$ps_code, ]
# all invalidated polling stations in preliminary results report zero votes (not all of zero vote decision stations appear in results)

closed_ps <- invalidated_ps$ps_code[invalidated_ps$commission_decision_or_status == "بسته"]
closed_ps <- tibble(ps_code = closed_ps, ps_open = "NO")

ps_status_update <- prelim_ps_reporting_status %>% 
  dplyr::select(province_code, pc_code, ps_code, ps_type, audit_or_recount, prelim_results_reporting) %>%
  left_join(closed_ps)

invalidated_ps_list <- invalidated_ps %>% filter(commission_decision_or_status != "بسته") %>%
  dplyr::select(ps_code) %>% mutate(ps_invalidated = "YES")

ps_status_update <- ps_status_update %>% left_join(invalidated_ps_list)
ps_status_update$ps_open[ps_status_update$prelim_results_reporting == "YES"] <- "YES"
ps_status_update$ps_open[ps_status_update$ps_invalidated == "YES"] <- "YES"
ps_status_update$ps_invalidated[ps_status_update$audit_or_recount == "YES" & is.na(ps_status_update$ps_invalidated)] <- "NO"
ps_status_update <- ps_status_update %>% 
  dplyr::select(province_code, pc_code, ps_code, ps_type, audit_or_recount, prelim_results_reporting,
                ps_invalidated, ps_open)
ps_status_update$reporting_zero_votes <- NA
ps_status_update$reporting_zero_votes[ps_status_update$prelim_results_reporting == "YES" & ps_status_update$ps_invalidated == "YES"] <- "YES"
ps_status_update$reporting_zero_votes[ps_status_update$prelim_results_reporting == "YES" & is.na(ps_status_update$reporting_zero_votes)] <- "NO"
ps_status_update$ps_open[is.na(ps_status_update$ps_open)] <- "UNKNOWN"

ps_status_update <- ps_status_update %>%
  dplyr::select(province_code, pc_code, ps_code, ps_type, audit_or_recount, prelim_results_reporting,
                ps_invalidated, reporting_zero_votes, ps_open)

missing_open_status <- ps_status_update %>% filter(ps_open == "UNKNOWN") %>% 
  left_join(dplyr::select(ps_audit_target_list, ps_code, commission_decision))

write.csv(ps_status_update, "./validity_checks/prelim_ps_reporting_status.csv", row.names = F)

write.csv(missing_open_status, "./validity_checks/prelim_ps_missing_status.csv", row.names = F)

ps_summary_report_update <- ps_summary_report %>% left_join(ps_status_update) %>%
  rename(pre_audit_decision = commission_decision) %>% left_join((invalidated_ps %>% rename(invalidated_results_sheet_total = votes))) %>%
  rename(post_audit_decision = commission_decision_or_status) %>%
  dplyr::select(1:9, ps_open, accompanying_ps_scan, audit_or_recount, ps_invalidated, pre_audit_decision, post_audit_decision, 10:17, invalidated_results_sheet_total,
                everything()) %>%
  dplyr::select(-c(serial_number, ps_number))

ps_summary_report_update$post_audit_decision[is.na(ps_summary_report_update$post_audit_decision) & ps_summary_report_update$ps_invalidated == "NO"] <- "Not invalidated in audit or recount"

write.csv(ps_summary_report_update, "./analysis/ps_summary_report.csv", row.names = F)

ps_summary_report$added_votes[ps_summary_report$prelim_results_reporting == "NO" & ps_summary_report$invalidated_results_sheet_total == 0] <- "NO"
ps_summary_report$added_votes[ps_summary_report$prelim_results_reporting == "YES" & ps_summary_report$invalidated_results_sheet_total == 0] <- "NO"
ps_summary_report$added_votes[ps_summary_report$ps_invalidated == "YES"] <- "NO"
ps_summary_report$added_votes[ps_summary_report$added_votes == "UNKNOWN" & ps_summary_report$total_votes == 0] <- "NO"
ps_summary_report$added_votes[ps_summary_report$added_votes == "UNKNOWN" & ps_summary_report$prelim_results_reporting == "NO" & ps_summary_report$audit_or_recount == "YES"] <- "NO"
