#-----------------------------------------------------------------------------------
# DOWNLOAD PS SCANS FROM LIST

scan_links_list <- read_csv("./scan_reports/ps_scan_links.csv")

# create a subdirectory for scraped polling station scans if it doesn't already exist
ifelse(!dir.exists(file.path("./ps_scans")), dir.create(file.path("./ps_scans")), FALSE)

ps_scan_error_log <- list()

for(n in 1:length(scan_links_list$target_url)){
  link <- scan_links_list[[5]][[n]]
  
  prov <- scan_links_list$province_code[n]
  pc <- scan_links_list$pc_code[n]
  
  prov_path <- paste0("./ps_scans/", prov)
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


# second pass on original list target to ensure that all downloads went through ----
scan_links_list <- read_csv("./scan_reports/ps_scan_links.csv")

downloaded_ps <- list.files("./ps_scans/", pattern = "png", recursive = T, full.names = T)
#downloaded_ps <- file.info(list.files("./ps_scans/", pattern = "png", recursive = T, full.names = T)) 
#target <- tibble(ps_path = rownames(downloaded_ps[downloaded_ps$size < 100000, ])) %>% 
#                   rowwise %>% mutate(scan_filename = str_split(ps_path, "/")[[1]][6])

download_filenames <- tibble(ps_path = downloaded_ps) %>% rowwise %>% mutate(scan_filename = str_split(ps_path, "/")[[1]][6])

missing_downloads <- scan_links_list$scan_filename[!(scan_links_list$scan_filename %in% download_filenames$scan_filename)]
missing_download_targets <- scan_links_list[scan_links_list$scan_filename %in% missing_downloads, ]

for(n in 1:length(missing_download_targets$target_url)){
  link <- missing_download_targets[[5]][[n]]
  
  prov <- missing_download_targets$province_code[n]
  pc <- missing_download_targets$pc_code[n]
  
  prov_path <- paste0("./ps_scans/", prov)
  pc_path <- paste0(prov_path, "/", pc)
  
  # create the appropriate province subdirectory if it doesn't already exist                   
  ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
  
  # create the appropriate pc subdirectory if it doesn't already exist                   
  ifelse(!dir.exists(file.path(pc_path)), dir.create(file.path(pc_path)), FALSE)
  
  tryCatch(download.file(link, file.path(pc_path, basename(link)), mode = "wb", quiet = FALSE),
           error = function(e){ps_scan_error_log <- c(ps_scan_error_log, link)}
  )
  Sys.sleep(4)
}


# BROKEN LINKS

missing_download_targets$link_broken <- "YES"
write.csv(missing_download_targets, "./scan_reports/ps_scans_broken_links.csv", row.names = F)

# GAPS VS REPORTING
rm(list = ls())

downloaded_ps <- list.files("./ps_scans/", pattern = "png", recursive = T, full.names = T)
downloaded_invalid_ps <- list.files("./ps_scans/invalidated_ps_scans/", pattern = "png", recursive = T, full.names = T)

download_filenames <- tibble(ps_path = downloaded_ps) %>% 
  rowwise %>% mutate(scan_filename = str_split(ps_path, "/")[[1]][6])

download_invalid_filenames <- tibble(ps_path = downloaded_invalid_ps) %>% 
  rowwise %>% mutate(scan_filename = str_split(ps_path, "/")[[1]][7],
                     invalidated_ps = "YES")

all_downloads <- download_filenames %>% full_join(download_invalid_filenames) %>%
  rowwise %>% 
  mutate(ps_code = paste0(str_split(scan_filename, "-")[[1]][1], "-", str_split(scan_filename, "-")[[1]][2]))
all_downloads$invalidated_ps[is.na(all_downloads$invalidated_ps)] <- "NO"

has_scan <- all_downloads %>% dplyr::select(ps_code) %>% mutate(accompanying_ps_scan = "YES")

ps_status_update <- left_join(prelim_ps_reporting_status, has_scan)
ps_status_update$accompanying_ps_scan[is.na(ps_status_update$accompanying_ps_scan) & ps_status_update$prelim_results_reporting == "YES"] <- "NO"

ps_summary_report_update <- ps_summary_report %>% left_join(has_scan) %>% 
  dplyr::select(
    1:10, accompanying_ps_scan, everything()
  )
ps_summary_report_update$accompanying_ps_scan[is.na(ps_summary_report_update$accompanying_ps_scan) & ps_summary_report_update$prelim_results_reporting == "YES"] <- "NO"

write.csv(ps_status_update, "./validity_checks/prelim_ps_reporting_status.csv", row.names = F)
write.csv(ps_summary_report_update, "./analysis/ps_summary_report.csv", row.names = F)

ps_scans_reporting_check <- ps_status_update %>% group_by(province_code) %>%
  summarize(
  ps_reporting_count = length(ps_code[prelim_results_reporting == "YES"]),
  ps_invalidated_count = length(ps_code[ps_invalidated == "YES" & !is.na(ps_invalidated)]),
  ps_scan_count = length(ps_code[accompanying_ps_scan == "YES" & ps_invalidated != "YES" & prelim_results_reporting == "YES"]),
  ps_invalidated_scan_count = length(ps_code[accompanying_ps_scan == "YES" & ps_invalidated == "YES" & !is.na(ps_invalidated)]),
  ps_difference = ps_scan_count - ps_reporting_count,
  ps_invalidated_difference = ps_invalidated_scan_count - ps_invalidated_count
  ) %>% left_join(dplyr::select(province_key_2019, province_code, province_name_eng)) %>%
  dplyr::select(province_code, province_name_eng, everything())

write.csv(ps_scans_reporting_check, "./scan_reports/ps_scans_reporting_check.csv", row.names = F)
