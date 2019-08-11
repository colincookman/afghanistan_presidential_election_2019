library(XML)
library(tidyverse)
library(stringr)
library(rvest)
library(RSelenium)
library(wdman)
library(seleniumPipes)
library(lubridate)
library(RCurl)
library(gsheet)
rm(list = ls())

# GET THE PC LISTS FROM THE IEC WEBSITE ---------------------------------------

pc_url <- "http://www.iec.org.af/en/polling-center"
province_list <- list()
pc_list <- list()

# settings for silent background browsing
#eCaps <- list(chromeOptions = list(
#  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
#))

driver <- rsDriver(browser="chrome", port = 4444L, chromever="76.0.3809.68", verbose = FALSE
#                     , extraCapabilities = eCaps
                   )
driver$client$navigate(pc_url)
  
province_list <- xml2::read_html(driver$client$getPageSource()[[1]]) %>%
    rvest::html_nodes("#ProvinceId") %>%
    rvest::html_children() %>%
    rvest::html_text() %>%
    dplyr::data_frame(province = .)
  
province_list <- province_list %>%
    dplyr::mutate(list_position = 1:length(province_list$province),
                  x = stringr::str_c("#ProvinceId > option:nth-child(", list_position, ")")
                  )
province_list <- province_list[-1, ]

for(j in (1:length(province_list$x))){
    # navigate to each province
    province_target <- as.character(province_list[j, 3])
    element <- driver$client$findElement(using = 'css selector', province_target)
    element$clickElement()
    
    # make sure the pc table is present, loop until it is found
    pc_present <- NULL
    while(is.null(pc_present)){
    pc_present <- tryCatch({driver$client$findElement(using = 'id', value = "tbl_pc")},
    error = function(e){Sys.sleep(1)})
    }
    
    # wait a bit more to make sure the table has loaded properly
    Sys.sleep(3.5)
    
    # get the full table of PCs
    page_source <- driver$client$getPageSource()
    pc_html <- page_source[[1]] %>% read_html()
    pc_data_list_item <- pc_html %>% html_nodes("#pc.table") %>% html_table(fill = TRUE, trim = TRUE, header = TRUE)
          
    # add html data to a master list      
    pc_list <- c(pc_list, pc_data_list_item)
      
}

driver$client$close()
driver$server$stop()

# UNPACK THE HTML -------------------------------------------------------------

pc_master_list <- data.frame()
for(a in (1:length(pc_list))){ 
  # parse the html of each scraped polling center page
  
  pc_page <- as.data.frame(pc_list[a])
  pc_master_list <- rbind(pc_master_list, pc_page)
}

pc_master_list <- pc_master_list %>% rename(
  serial_number = No,
  province_name_eng = Province,
  district_name_eng = District,
  pc_name_eng = PC.Name,
  pc_code = Code,
  pc_location_eng = Location) %>%
  arrange(pc_code)

pc_master_list$pc_code <- as.character(str_pad(pc_master_list$pc_code, 7, pad = "0", side = "left"))

# NEW - check to see if there was an update in late July / early August

pre_election_pc_list <- read_csv("./raw/pre_election_pc_list.csv")

not_in_new_pc_list <- setdiff(pre_election_pc_list$pc_code, pc_master_list$pc_code)
not_in_new_pc_list <- as.data.frame(not_in_new_pc_list) %>% mutate(planned_2019 = "NO") %>% 
  rename(pc_code = not_in_new_pc_list)

new_pc_plan_2019 <- pre_election_pc_list %>% left_join(not_in_new_pc_list)
new_pc_plan_2019$planned_2019[is.na(new_pc_plan_2019$planned_2019)] <- "YES"

af_pc_vr_comparison_18_19 <- read_csv("Google Drive/GitHub/afghanistan_presidential_election_2019/raw/af_pc_vr_comparison_18_19.csv")

vr_and_2018 <- af_pc_vr_comparison_18_19 %>% dplyr::select(
  pc_code, planned_18, prelim_results_18, final_results_18, 
  vr_prelim_total_18, vr_final_total_18, vr_male_18, vr_fem_18, vr_kuchi_18, vr_sikh_18,
  vr_prelim_total_19, vr_male_19, vr_fem_19, vr_kuchi_19, vr_sikh_19
)

new_pc_plan_2019 <- new_pc_plan_2019 %>% left_join(vr_and_2018) %>% rename(planned_2018 = planned_18)

pc_key_2018 <- read_csv("./pc_plan/pc_key_2018.csv")

new_pc_plan_2019 <- new_pc_plan_2019 %>% left_join(dplyr::select(
  pc_key_2018, pc_code, district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari,
  provincial_capital, prelim_ps_count, final_ps_count)) %>% rename(prelim_ps_count_18 = prelim_ps_count, final_ps_count_18 = final_ps_count) %>%
  dplyr::select(province_code, province_name_eng, province_name_dari, 
                district_code, district_name_eng, district_name_dari, 
                district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari,
                provincial_capital, 
                pc_code, pc_name_eng, pc_name_dari, pc_name_pashto,
                pc_location_eng, pc_location_dari, pc_location_pashto,
                planned_2019, planned_2018, prelim_results_18, final_results_18,
                prelim_ps_count_18, final_ps_count_18,
                vr_prelim_total_19, vr_male_19, vr_fem_19, vr_kuchi_19, vr_sikh_19,
                vr_prelim_total_18, vr_final_total_18, vr_male_18, vr_fem_18, vr_kuchi_18, vr_sikh_18) %>% 
  arrange(province_code, district_code, pc_code)

write.csv(new_pc_plan_2019, "./keyfiles/pc_key_2019.csv", row.names = F)

# OLD INITIAL SCRAPE AND CLEANUP TO GET ORIGINAL UNIVERSE OF PCS --------------
# GET THE DARI NAMES ----------------------------------------------------------

pc_url <- "http://www.iec.org.af/prs/pc-lists-prs"
province_list <- list()
pc_list <- list()

# settings for silent background browsing
eCaps <- list(chromeOptions = list(
  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
))

driver <- rsDriver(browser="chrome", port = 4444L, chromever="74.0.3729.6", verbose = FALSE
                     , extraCapabilities = eCaps
                   )
driver$client$navigate(pc_url)
  
province_list <- xml2::read_html(driver$client$getPageSource()[[1]]) %>%
    rvest::html_nodes("#ProvinceId") %>%
    rvest::html_children() %>%
    rvest::html_text() %>%
    dplyr::data_frame(province = .)
  
province_list <- province_list %>%
    dplyr::mutate(list_position = 1:length(province_list$province),
                  x = stringr::str_c("#ProvinceId > option:nth-child(", list_position, ")")
                  )
province_list <- province_list[-1, ]

for(j in (1:length(province_list$x))){
    # navigate to each province
    province_target <- as.character(province_list[j, 3])
    element <- driver$client$findElement(using = 'css selector', province_target)
    element$clickElement()
    
    # make sure the pc table is present, loop until it is found
    pc_present <- NULL
    while(is.null(pc_present)){
    pc_present <- tryCatch({driver$client$findElement(using = 'id', value = "tbl_pc")},
    error = function(e){Sys.sleep(1)})
    }
    
    # wait a bit more to make sure the table has loaded properly
    Sys.sleep(3.5)
    
    # get the full table of PCs
    page_source <- driver$client$getPageSource()
    pc_html <- page_source[[1]] %>% read_html()
    pc_data_list_item <- pc_html %>% html_nodes("#pc.table") %>% html_table(fill = TRUE, trim = TRUE, header = TRUE)
          
    # add html data to a master list      
    pc_list <- c(pc_list, pc_data_list_item)
      
}

driver$client$close()
driver$server$stop()

# UNPACK THE HTML -------------------------------------------------------------

pc_master_list_dari <- data.frame()
for(a in (1:length(pc_list))){ 
  # parse the html of each scraped polling center page
  
  pc_page <- as.data.frame(pc_list[a])
  pc_master_list_dari <- rbind(pc_master_list_dari, pc_page)
}

colnames(pc_master_list_dari) <- c("serial_number", "province_name_dari", "district_name_dari", "pc_name_dari", "pc_code", "pc_location_dari")
pc_master_list_dari <- pc_master_list_dari %>% arrange(pc_code)
pc_master_list_dari$pc_code <- as.character(str_pad(pc_master_list_dari$pc_code, 7, pad = "0", side = "left"))
pc_master_list_dari$serial_number <- NULL
pc_master_list$serial_number <- NULL

pc_master_list_combined <- left_join(pc_master_list, pc_master_list_dari)

# GET THE PASTHO NAMES ----------------------------------------------------------

pc_url <- "http://www.iec.org.af/pas/pc-lists-pas"
province_list <- list()
pc_list <- list()

# settings for silent background browsing
eCaps <- list(chromeOptions = list(
  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
))

driver <- rsDriver(browser="chrome", port = 4444L, chromever="74.0.3729.6", verbose = FALSE
                     , extraCapabilities = eCaps
                   )
driver$client$navigate(pc_url)
  
province_list <- xml2::read_html(driver$client$getPageSource()[[1]]) %>%
    rvest::html_nodes("#ProvinceId") %>%
    rvest::html_children() %>%
    rvest::html_text() %>%
    dplyr::data_frame(province = .)
  
province_list <- province_list %>%
    dplyr::mutate(list_position = 1:length(province_list$province),
                  x = stringr::str_c("#ProvinceId > option:nth-child(", list_position, ")")
                  )
province_list <- province_list[-1, ]

for(j in (1:length(province_list$x))){
    # navigate to each province
    province_target <- as.character(province_list[j, 3])
    element <- driver$client$findElement(using = 'css selector', province_target)
    element$clickElement()
    
    # make sure the pc table is present, loop until it is found
    pc_present <- NULL
    while(is.null(pc_present)){
    pc_present <- tryCatch({driver$client$findElement(using = 'id', value = "tbl_pc")},
    error = function(e){Sys.sleep(1)})
    }
    
    # wait a bit more to make sure the table has loaded properly
    Sys.sleep(3.5)
    
    # get the full table of PCs
    page_source <- driver$client$getPageSource()
    pc_html <- page_source[[1]] %>% read_html()
    pc_data_list_item <- pc_html %>% html_nodes("#pc.table") %>% html_table(fill = TRUE, trim = TRUE, header = TRUE)
          
    # add html data to a master list      
    pc_list <- c(pc_list, pc_data_list_item)
      
}

driver$client$close()
driver$server$stop()

# UNPACK THE HTML -------------------------------------------------------------

pc_master_list_pashto <- data.frame()
for(a in (1:length(pc_list))){ 
  # parse the html of each scraped polling center page
  
  pc_page <- as.data.frame(pc_list[a])
  pc_master_list_pashto <- rbind(pc_master_list_pashto, pc_page)
}

colnames(pc_master_list_pashto) <- c("serial_number", "province_name_pashto", "district_name_pashto", "pc_name_pashto", "pc_code", "pc_location_pashto")
pc_master_list_pashto <- pc_master_list_pashto %>% arrange(pc_code)
pc_master_list_pashto$pc_code <- as.character(str_pad(pc_master_list_pashto$pc_code, 7, pad = "0", side = "left"))
pc_master_list_pashto$serial_number <- NULL

pc_master_list_combined <- left_join(pc_master_list_combined, pc_master_list_pashto)

pc_master_list_combined <- pc_master_list_combined %>% mutate(
  province_code = str_sub(pc_code, 1,2),
  district_code = str_sub(pc_code, 1,4),
) %>% dplyr::select(province_code, province_name_eng, province_name_dari, province_name_pashto, 
                    district_code, district_name_eng, district_name_dari, district_name_pashto,
                    pc_code, pc_name_eng, pc_name_dari, pc_name_pashto,
                    pc_location_eng, pc_location_dari, pc_location_pashto) %>% arrange(pc_code)

write.csv(pc_master_list_combined, "pre_election_pc_list.csv", row.names = F)

join_info <- pc_master_list_combined %>% dplyr::select(-c(province_code, province_name_eng, province_name_dari, province_name_pashto, 
                                                          district_name_eng, pc_name_eng, pc_location_eng))

pc_key_with_dari_pash <- left_join(pc_key, join_info)

pc_key_new <- pc_key_with_dari_pash %>% dplyr::select(province_code, province_name_eng, 
                                                                 district_code, district_name_eng, district_or_subdivision_name,
                                                                 district_name_dari, district_name_pashto,
                                                                 provincial_capital, pc_code, 
                                                                 pc_name_eng, pc_name_dari, pc_name_pashto,
                                                                 pc_location, pc_location_dari, pc_location_pashto, everything()) %>%
  rename(
    district_or_subdivision_name_eng = district_or_subdivision_name,
    pc_location_eng = pc_location
  )


missing <- pc_key_new[is.na(pc_key_new$pc_name_dari), ]

missing_with_dari_pash <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1OZ2EAcY1X2ODWVU6Ozu5N3jzDLlh7qG9hBE9CQ3S30o/edit?usp=sharing")
missing_with_dari_pash$pc_code <- as.character(str_pad(missing_with_dari_pash$pc_code, 7, pad = "0", side = "left"))
missing_with_dari_pash$province_code <- as.character(str_pad(missing_with_dari_pash$province_code, 2, pad = "0", side = "left"))
missing_with_dari_pash$district_code <- as.character(str_pad(missing_with_dari_pash$district_code, 4, pad = "0", side = "left"))

pc_key_new_corrected <- pc_key_new %>% anti_join(missing) %>% full_join(missing_with_dari_pash) %>% arrange(pc_code)

missing_districts <- pc_key_new_corrected[is.na(pc_key_new_corrected$district_name_dari), ]

write.csv(missing_districts, "./data/keyfiles/missing_districts.csv", row.names = F)


district_key_new <- pc_key_with_dari_pash %>% 
  dplyr::select(district_code, district_or_subdivision_name, district_name_dari, district_name_pashto) %>%
  unique()

district_missing <- district_key_new[is.na(district_key_new$district_name_dari), ]

missing_district_with_dari_pash <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1UrAIpANHWtFgWitGyA6UeCZfJhb_D7_20QqUhe9xvaY/edit?usp=sharing")
missing_district_with_dari_pash$province_code <- as.character(str_pad(missing_district_with_dari_pash$province_code, 2, pad = "0", side = "left"))
missing_district_with_dari_pash$district_code <- as.character(str_pad(missing_district_with_dari_pash$district_code, 4, pad = "0", side = "left"))
join_missing_district_with_dari_pash <- missing_district_with_dari_pash %>% 
  dplyr::select(district_code, district_or_subdivision_name_eng, district_name_dari, district_name_pashto) %>% rename(
    district_or_subdivision_name = district_or_subdivision_name_eng
  )

district_key_new_corrected <- district_key_new %>% anti_join(district_missing) %>% 
  full_join(join_missing_district_with_dari_pash) %>% 
  unique() %>% arrange(district_code)

district_key_new_corrected_with_eng <- district_key_new_corrected %>% left_join(
  dplyr::select(district_key, district_code, district_or_subdivision_name, district_name_eng)
) %>% dplyr::select(district_code, district_name_eng, district_or_subdivision_name, district_name_dari, district_name_pashto)

district_key_new_corrected_with_eng <- district_key_new_corrected_with_eng %>% rowwise %>% mutate(
  district_or_subdivision_name_eng = ifelse(grepl("ناحیه", district_name_dari), 
                                            paste0(district_name_eng, " NAHIA ", str_split(district_name_dari, " ")[[1]][2]), district_name_eng)
  
  ) %>% mutate(
    district_or_subdivision_name_dari = district_name_dari,
    district_or_subdivision_name_pashto = district_name_pashto
  ) %>% dplyr::select(-district_or_subdivision_name)
  

district_key_new_corrected_with_eng$district_name_dari[district_key_new_corrected_with_eng$district_code == "0101"] <- "كابل"
district_key_new_corrected_with_eng$district_name_pashto[district_key_new_corrected_with_eng$district_code == "0101"] <- "كابل"
district_key_new_corrected_with_eng$district_name_dari[district_key_new_corrected_with_eng$district_code == "0601"] <- "جلال آباد"
district_key_new_corrected_with_eng$district_name_pashto[district_key_new_corrected_with_eng$district_code == "0601"] <- "جلال آباد"
district_key_new_corrected_with_eng$district_name_dari[district_key_new_corrected_with_eng$district_code == "2101"] <- "مزار شریفد"
district_key_new_corrected_with_eng$district_name_pashto[district_key_new_corrected_with_eng$district_code == "2101"] <- "مزار شریفد"
district_key_new_corrected_with_eng$district_name_dari[district_key_new_corrected_with_eng$district_code == "2701"] <- "کندهار"
district_key_new_corrected_with_eng$district_name_pashto[district_key_new_corrected_with_eng$district_code == "2701"] <- "کندهار"
district_key_new_corrected_with_eng$district_name_dari[district_key_new_corrected_with_eng$district_code == "3201"] <- "هرات"
district_key_new_corrected_with_eng$district_name_pashto[district_key_new_corrected_with_eng$district_code == "3201"] <- "هرات"

district_key_new_corrected_with_eng <- district_key_new_corrected_with_eng %>% mutate(
    district_or_subdivision_name_dari = ifelse(grepl("ناحیه", district_or_subdivision_name_dari), 
                                            paste0(district_name_dari, " ", district_or_subdivision_name_dari), district_or_subdivision_name_dari),
    district_or_subdivision_name_pashto = ifelse(grepl("ناحیه", district_or_subdivision_name_pashto), 
                                            paste0(district_name_pashto, " ", district_or_subdivision_name_pashto), district_or_subdivision_name_pashto)
    )

nahia <- "ناحیه"

district_key_new_corrected_with_eng <- district_key_new_corrected_with_eng %>% rowwise %>% mutate(
  district_sub_code = ifelse(grepl("ناحیه", district_or_subdivision_name_dari), 
                                            paste0(district_code, "-", trimws(str_split(district_or_subdivision_name_dari, nahia)[[1]][2])), district_code)
) %>% dplyr::select(district_code, district_name_eng, district_name_dari, district_name_pashto, district_sub_code, everything()) %>% 
  arrange(district_code, district_sub_code)

district_key_final_new <- left_join(district_key_new_corrected_with_eng, dplyr::select(district_key, district_code, provincial_capital)) %>% unique()

write.csv(district_key_final_new, "./data/keyfiles/district_key.csv", row.names = F)

#-----

pc_key_final_new <- pc_key_new_corrected %>% rowwise %>% mutate(
  district_or_subdivision_name_eng = ifelse(grepl("ناحیه", district_name_dari), 
                                            paste0(district_name_eng, " NAHIA ", str_split(district_name_dari, " ")[[1]][2]), district_name_eng)
  
  ) %>% mutate(
    district_or_subdivision_name_dari = district_name_dari,
    district_or_subdivision_name_pashto = district_name_pashto
  )
  

pc_key_final_new$district_name_dari[pc_key_final_new$district_code == "0101"] <- "كابل"
pc_key_final_new$district_name_pashto[pc_key_final_new$district_code == "0101"] <- "كابل"
pc_key_final_new$district_name_dari[pc_key_final_new$district_code == "0601"] <- "جلال آباد"
pc_key_final_new$district_name_pashto[pc_key_final_new$district_code == "0601"] <- "جلال آباد"
pc_key_final_new$district_name_dari[pc_key_final_new$district_code == "2101"] <- "مزار شریفد"
pc_key_final_new$district_name_pashto[pc_key_final_new$district_code == "2101"] <- "مزار شریفد"
pc_key_final_new$district_name_dari[pc_key_final_new$district_code == "2701"] <- "کندهار"
pc_key_final_new$district_name_pashto[pc_key_final_new$district_code == "2701"] <- "کندهار"
pc_key_final_new$district_name_dari[pc_key_final_new$district_code == "3201"] <- "هرات"
pc_key_final_new$district_name_pashto[pc_key_final_new$district_code == "3201"] <- "هرات"

pc_key_final_new <- pc_key_final_new %>% mutate(
    district_or_subdivision_name_dari = ifelse(grepl("ناحیه", district_or_subdivision_name_dari), 
                                            paste0(district_name_dari, " ", district_or_subdivision_name_dari), district_or_subdivision_name_dari),
    district_or_subdivision_name_pashto = ifelse(grepl("ناحیه", district_or_subdivision_name_pashto), 
                                            paste0(district_name_pashto, " ", district_or_subdivision_name_pashto), district_or_subdivision_name_pashto)
    )

nahia <- "ناحیه"

pc_key_final_new <- pc_key_final_new %>% rowwise %>% mutate(
  district_sub_code = ifelse(grepl("ناحیه", district_or_subdivision_name_dari), 
                                            paste0(district_code, "-", trimws(str_split(district_or_subdivision_name_dari, nahia)[[1]][2])), district_code)
) %>% dplyr::select(province_code, province_name_eng, district_code, district_name_eng, district_name_dari, district_name_pashto, district_sub_code, 
                    district_or_subdivision_name_eng, district_or_subdivision_name_dari, district_or_subdivision_name_pashto, everything()) %>% 
  arrange(pc_code)

missing_pc_final <- pc_key_final_new[is.na(pc_key_final_new$district_or_subdivision_name_dari), ]
write.csv(missing_pc_final, "missing_pc_final.csv", row.names = F)

missing_pc_final_corrected <- gsheet2tbl("https://docs.google.com/spreadsheets/d/11AX6UFDnLYT5yjKK-wk0-ZxIB41HxI8B0KpjDJFMJJg/edit?usp=sharing")
missing_pc_final_corrected$province_code <- as.character(str_pad(missing_pc_final_corrected$province_code, 2, pad = "0", side = "left"))
missing_pc_final_corrected$district_code <- as.character(str_pad(missing_pc_final_corrected$district_code, 4, pad = "0", side = "left"))
missing_pc_final_corrected$district_sub_code <- as.character(str_pad(missing_pc_final_corrected$district_sub_code, 4, pad = "0", side = "left"))
missing_pc_final_corrected$pc_code <- as.character(str_pad(missing_pc_final_corrected$pc_code, 7, pad = "0", side = "left"))

pc_key_final_final <- pc_key_final_new %>% anti_join(missing_pc_final) %>% full_join(missing_pc_final_corrected) %>% arrange(pc_code)
write.csv(pc_key_final_final, "./data/keyfiles/pc_key.csv", row.names = F)
