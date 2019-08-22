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

#settings for silent background browsing
eCaps <- list(chromeOptions = list(
  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
))

driver <- rsDriver(browser="chrome", port = 4444L, chromever="76.0.3809.68", verbose = FALSE
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

# GET THE DARI NAMES ----------------------------------------------------------

pc_url <- "http://www.iec.org.af/prs/pc-lists-prs"
province_list <- list()
pc_list <- list()

# settings for silent background browsing
eCaps <- list(chromeOptions = list(
  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
))

driver <- rsDriver(browser="chrome", port = 4444L, chromever="76.0.3809.68", verbose = FALSE
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

driver <- rsDriver(browser="chrome", port = 4444L, chromever="76.0.3809.68", verbose = FALSE
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

# CREATE PC KEY BASED ON NEW FINAL (?) LIST OF PLANNED PCS --------------------

planned_2019 <- pc_master_list_combined

pre_election_pc_list <- read_csv("./raw/pre_election_pc_list.csv")
pc_key_2018 <- read_csv("./pc_plan/pc_key_2018.csv")
af_pc_vr_comparison_18_19 <- read_csv("./raw/af_pc_vr_comparison_18_19.csv")
vr_and_2018 <- af_pc_vr_comparison_18_19 %>% dplyr::select(
  pc_code, planned_18, prelim_results_18, final_results_18, 
  vr_prelim_total_18, vr_final_total_18, vr_male_18, vr_fem_18, vr_kuchi_18, vr_sikh_18,
  vr_prelim_total_19, vr_male_19, vr_fem_19, vr_kuchi_19, vr_sikh_19
)

planned_pcs <- planned_2019 %>% dplyr::select(pc_code) %>% mutate(planned_2019 = "YES")

new_pcs_in_final <- setdiff(planned_2019$pc_code, vr_and_2018$pc_code)
new_pcs_in_final <- planned_2019 %>% filter(pc_code %in% new_pcs_in_final)
new_pcs_in_final <- new_pcs_in_final %>% rowwise %>% mutate(
  pc_name_eng = trimws(str_split(pc_name_eng, "-")[[1]][2]),
  pc_name_dari = trimws(str_split(pc_name_dari, "-")[[1]][2]),
  pc_name_pashto = trimws(str_split(pc_name_pashto, "-")[[1]][2]),
  planned_2019 = "YES",
  district_sub_code = district_code,
  district_or_subdivision_name_eng = district_name_eng,
  district_or_subdivision_name_dari = district_name_dari,
  district_or_subdivision_name_pashto = district_name_pashto,
  provincial_capital = "NO"
)

new_pcs_in_final <- new_pcs_in_final %>% dplyr::select(
  province_code, province_name_eng,
  district_code, district_name_eng, district_name_dari, district_name_pashto,
  district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, district_or_subdivision_name_pashto,
  provincial_capital, 
  pc_code, pc_name_eng, pc_name_dari, pc_name_pashto, planned_2019
)

new_pc_plan_2019 <- pc_key_2018 %>% dplyr::select(
  province_code, province_name_eng, 
  district_code, district_name_eng, district_name_dari, district_name_pashto,
  district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari, district_or_subdivision_name_pashto,
  provincial_capital,
  pc_code, pc_name_eng, pc_name_dari, pc_name_pashto,
  pc_location_eng, pc_location_dari, pc_location_pashto,
  prelim_ps_count, final_ps_count
  ) %>% left_join(planned_pcs) %>%
  rename(
    prelim_ps_count_2018 = prelim_ps_count,
    final_ps_count_2018 = final_ps_count
  )
new_pc_plan_2019 <- new_pc_plan_2019 %>% full_join(new_pcs_in_final)

new_pc_plan_2019$planned_2019[is.na(new_pc_plan_2019$planned_2019)] <- "NO"

pc_plan_19_with_vr <- new_pc_plan_2019 %>% left_join(vr_and_2018) %>% rename(planned_2018 = planned_18)

new_pc_plan_2019_final <- pc_plan_19_with_vr %>%
  dplyr::select(province_code, province_name_eng, 
                district_code, district_name_eng, district_name_dari, 
                district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari,
                provincial_capital, 
                pc_code, pc_name_eng, pc_name_dari, pc_name_pashto,
                pc_location_eng, pc_location_dari, pc_location_pashto,
                planned_2019, planned_2018, prelim_results_18, final_results_18,
                prelim_ps_count_2018, final_ps_count_2018,
                vr_prelim_total_19, vr_male_19, vr_fem_19, vr_kuchi_19, vr_sikh_19,
                vr_prelim_total_18, vr_final_total_18, vr_male_18, vr_fem_18, vr_kuchi_18, vr_sikh_18) %>% 
  arrange(province_code, district_code, pc_code) %>% rename(
    prelim_results_2018 = prelim_results_18,
    final_results_2018 = final_results_18
  )

write.csv(new_pc_plan_2019_final, "./keyfiles/pc_key_2019.csv", row.names = F)

# PARSE NEW (FINAL?) PC / PS LISTS PUBLISHED 08-19-19 -------------------------
rm(list = ls())

pc_key_2019 <- read.csv("./keyfiles/pc_key_2019.csv", stringsAsFactors = F)

# PC list is slightly messier so leaving for now ------------------------------
# target <- ("./raw/081919 PC List.pdf")
# final_pc_list <- pdf_text(target)
# pdf_string <- toString(final_pc_list)
# pdf_lines <- read_lines(pdf_string)
#
# header_start <- grep("None 400", pdf_lines)
# header_end <- grep("STATION    STATION", pdf_lines)
# headers <- list()
# for(j in 1:length(header_start)){
#  distance <- header_start[j]:header_end[j]
#  headers <- c(headers, distance)
#}
#headers <- unlist(headers)
#footer_row <- grep("Page ", pdf_lines)
#
#pdf_trimmed <- pdf_lines[- c(headers, footer_row)]
#pdf_numbers_only <- gsub("[^0-9 ]", "", pdf_trimmed)
# pdf_trimmed <- gsub(" ", "  ", pdf_trimmed)
#
#data <- Reduce(rbind, strsplit(trimws(pdf_numbers_only), "\\s{2,}"))
#rownames(data) <- 1:dim(data)[1]
#data <- as.data.frame(data)
#colnames(data) <- c("pc_code", "ps_number", "barcode_number", "votes")
#
# PS list ---------------------------------------------------------------------
target <- ("./raw/081919 PS List.pdf")
final_ps_list <- pdf_text(target)
pdf_string <- toString(final_ps_list)
pdf_lines <- read_lines(pdf_string)

headers <- grep("Province District", pdf_lines)
footer_row <- grep("Page ", pdf_lines)

pdf_trimmed <- pdf_lines[- c(headers, footer_row)]

trimmed <- str_match(pdf_trimmed, "(\\d{6,7})(.*)")[,1]
trimmed <- gsub(" M", "  M", trimmed)
trimmed <- gsub(" F", "  F", trimmed)
trimmed <- gsub(" ", "  ", trimmed)
data <- Reduce(rbind, strsplit(trimws(trimmed), "\\s{2,}"))
rownames(data) <- 1:dim(data)[1]
data <- as.data.frame(data)
data <- data %>% filter(!is.na(V1))

colnames(data) <- c("pc_code", "vr_final_total_19", "vr_final_male_19", "vr_final_fem_19", "vr_final_kuchi_19",
                    "ps_type", "ps_number", "ps_est_voters")

final_ps_list_df <- data
final_ps_list_df$pc_code <- as.character(final_ps_list_df$pc_code)
final_ps_list_df$ps_type <- as.character(final_ps_list_df$ps_type)
final_ps_list_df$vr_final_total_19 <- as.numeric(as.character(final_ps_list_df$vr_final_total_19))
final_ps_list_df$vr_final_male_19 <- as.numeric(as.character(final_ps_list_df$vr_final_male_19))
final_ps_list_df$vr_final_fem_19 <- as.numeric(as.character(final_ps_list_df$vr_final_fem_19))
final_ps_list_df$vr_final_kuchi_19 <- as.numeric(as.character(final_ps_list_df$vr_final_kuchi_19))
final_ps_list_df$ps_number <- as.numeric(as.character(final_ps_list_df$ps_number))
final_ps_list_df$ps_est_voters <- as.numeric(as.character(final_ps_list_df$ps_est_voters))

final_ps_list_df$pc_code <- as.character(str_pad(final_ps_list_df$pc_code, 7, pad = "0", side = "left"))

final_ps_list_pcs <- unique(final_ps_list_df$pc_code)

final_ps_list_df <- final_ps_list_df %>% mutate(
  ps_code = paste0(pc_code, "-", str_pad(ps_number, 2, pad = "0", "left")),
  province_code = str_sub(pc_code, 1, 2),
  district_code = str_sub(pc_code, 1, 4)
  ) %>% arrange(pc_code, ps_code)

final_pc_list <- final_ps_list_df %>% group_by(pc_code) %>% summarize(
  ps_planned_count = length(ps_code)) %>% 
  right_join(dplyr::select(final_ps_list_df,
                           pc_code, vr_final_total_19, vr_final_male_19, vr_final_fem_19, vr_final_kuchi_19)) %>%
  unique() %>% 
  dplyr::select(pc_code, ps_planned_count,
                vr_final_total_19, vr_final_male_19, vr_final_fem_19, vr_final_kuchi_19) %>%
  arrange(pc_code) %>% mutate(final_planned_19 = "YES")

final_against_key <- pc_key_2019 %>% left_join(final_pc_list) %>%
  dplyr::select(
    province_code, province_name_eng, 
    district_code, district_name_eng, district_name_dari,
    district_sub_code, district_or_subdivision_name_eng, district_or_subdivision_name_dari,
    provincial_capital,
    pc_code, pc_name_eng, pc_name_dari, pc_name_pashto,
    pc_location_eng, pc_location_dari, pc_location_pashto,
    final_planned_19, planned_2018, prelim_results_2018, final_results_2018,
    ps_planned_count, prelim_ps_count_2018, final_ps_count_2018,
    vr_final_total_19, vr_final_male_19, vr_final_fem_19, vr_final_kuchi_19,
    vr_prelim_total_19, vr_male_19, vr_fem_19, vr_kuchi_19, vr_sikh_19,
    vr_final_total_18, vr_male_18, vr_fem_18, vr_kuchi_18, vr_sikh_18,
    vr_prelim_total_18
  ) %>% rename(
    planned_2019 = final_planned_19,
    planned_ps_count_2019 = ps_planned_count,
    vr_prelim_male_19 = vr_male_19,
    vr_prelim_fem_19 = vr_fem_19,
    vr_prelim_kuchi_19 = vr_kuchi_19,
    vr_prelim_sikh_19 = vr_sikh_19,
    vr_final_male_18 = vr_male_18,
    vr_final_fem_18 = vr_fem_18,
    vr_final_kuchi_18 = vr_kuchi_18,
    vr_final_sikh_18 = vr_sikh_18
  ) %>% arrange(province_code, district_code, district_sub_code, pc_code)

final_against_key$planned_2019[is.na(final_against_key$planned_2019)] <- "NO"
final_against_key$province_code <- as.character(str_pad(final_against_key$province_code, 2, pad = "0", side = "left"))
final_against_key$district_code <- as.character(str_pad(final_against_key$district_code, 4, pad = "0", side = "left"))

final_against_key$district_sub_code[final_against_key$pc_code == "0101555"] <- "0101-12"
final_against_key$district_sub_code[final_against_key$pc_code == "0101556"] <- "0101-12"
final_against_key$district_sub_code[final_against_key$pc_code == "0101559"] <- "0101-22"
final_against_key$district_or_subdivision_name_eng[final_against_key$pc_code == "0101555"] <- "KABUL CITY NAHIA 12"
final_against_key$district_or_subdivision_name_eng[final_against_key$pc_code == "0101556"] <- "KABUL CITY NAHIA 12"
final_against_key$district_or_subdivision_name_eng[final_against_key$pc_code == "0101559"] <- "KABUL CITY NAHIA 22"
final_against_key$district_or_subdivision_name_dari[final_against_key$pc_code == "0101555"] <- "كابل ناحیه 12"
final_against_key$district_or_subdivision_name_dari[final_against_key$pc_code == "0101556"] <- "كابل ناحیه 12"
final_against_key$district_or_subdivision_name_dari[final_against_key$pc_code == "0101559"] <- "كابل ناحیه 22"

pc_key_update <- final_against_key %>% arrange(pc_code)

pc_key_vr_change  <- pc_key_update %>% mutate(
  vr_2019_net_change = vr_final_total_19 - vr_prelim_total_19,
  vr_2019_pct_change = vr_2019_net_change / vr_prelim_total_19, 
  vr_2018_2019_net_change = vr_final_total_19 - vr_final_total_18,
  vr_2018_2019_pct_change = vr_2018_2019_net_change / vr_final_total_18
)

district_vr_change  <- pc_key_update %>% 
  group_by(province_code, province_name_eng, district_sub_code, district_or_subdivision_name_eng) %>%
  summarize(
  vr_total_final_19 = sum(vr_final_total_19, na.rm = T),
  vr_total_prelim_19 = sum(vr_prelim_total_19, na.rm = T),
  vr_total_final_18 = sum(vr_final_total_18, na.rm = T),
  vr_2019_net_change = vr_total_final_19 - vr_total_prelim_19,
  vr_2019_pct_change = vr_2019_net_change / vr_total_prelim_19, 
  vr_2018_2019_net_change = vr_total_final_19 - sum(vr_final_total_18, na.rm = T),
  vr_2018_2019_pct_change = vr_2018_2019_net_change / vr_total_final_18
)


province_vr_change  <- pc_key_update %>% 
  group_by(province_code, province_name_eng) %>%
  summarize(
  vr_total_final_19 = sum(vr_final_total_19, na.rm = T),
  vr_total_prelim_19 = sum(vr_prelim_total_19, na.rm = T),
  vr_total_final_18 = sum(vr_final_total_18, na.rm = T),
  vr_2019_net_change = vr_total_final_19 - vr_total_prelim_19,
  vr_2019_pct_change = vr_2019_net_change / vr_total_prelim_19, 
  vr_2018_2019_net_change = vr_total_final_19 - sum(vr_final_total_18, na.rm = T),
  vr_2018_2019_pct_change = vr_2018_2019_net_change / vr_total_final_18
)

write.csv(district_vr_change, "./pc_plan/district_final_vr_net_change.csv", row.names = F)
write.csv(province_vr_change, "./pc_plan/province_final_vr_net_change.csv", row.names = F)

write.csv(pc_key_update, "./keyfiles/pc_key_2019.csv", row.names = F)

ps_key <- final_ps_list_df %>% dplyr::select(pc_code, ps_code, ps_type, ps_est_voters)

write.csv(ps_key, "./keyfiles/ps_key_2019.csv", row.names = F)


# a bit more analysis of 2019 VR data

target <- ("./raw/VRtopuppc.pdf")
topup_pcs <- pdf_text(target)
pdf_string <- toString(topup_pcs)
pdf_lines <- read_lines(pdf_string)

pc_codes <- str_match(pdf_lines, "(\\d{6,7})")[,1]
pc_codes <- as.data.frame(pc_codes) %>% filter(!is.na(pc_codes)) %>% mutate(vr_topup_location = "YES") %>% rename(pc_code = pc_codes)
pc_codes$pc_code <- as.character(pc_codes$pc_code)

pc_key_with_topup <- pc_key_2019 %>% left_join(pc_codes)
pc_key_with_topup$vr_topup_location[is.na(pc_key_with_topup$vr_topup_location)] <- "NO"

pc_key_with_topup_resort <- pc_key_with_topup %>% dplyr::select(names(pc_key_2019)[1:23], vr_topup_location, everything())

write.csv(pc_key_with_topup_resort, "./keyfiles/pc_key_2019.csv", row.names = F)

pc_key <- pc_key_with_topup_resort

pc_key <- pc_key %>% mutate(
  vr_2019_net_change = vr_final_total_19 - vr_prelim_total_19,
  vr_2019_pct_change = vr_2019_net_change / vr_prelim_total_19, 
  vr_2018_2019_net_change = vr_final_total_19 - vr_final_total_18,
  vr_2018_2019_pct_change = vr_2018_2019_net_change / vr_final_total_18,
)

pc_key <- pc_key %>% group_by(district_sub_code) %>% mutate(
  district_threshold = quantile(vr_final_total_19, .75, na.rm = T)
)
pc_key <- pc_key %>% group_by(province_code) %>% mutate(
  province_threshold = quantile(vr_final_total_19, .75, na.rm = T)
)

pc_key <- pc_key %>% mutate(
  district_outlier = ifelse(vr_final_total_19 >= district_threshold, "YES", "NO"),
  province_outlier = ifelse(vr_final_total_19 >= province_threshold, "YES", "NO"),
  prelim_turnout_18 = prelim_total_votes_2018 / vr_final_total_18,
  final_turnout_18 = final_total_votes_2018 / vr_final_total_18
)

ggplot(data = subset(pc_key, prelim_turnout_18 <= 1 & vr_2018_2019_pct_change <= 1 & vr_topup_location == "YES"),
       aes(x = prelim_turnout_18, y = vr_2018_2019_pct_change)) +
  geom_point() +
  geom_point(alpha = 0.65) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Total Preliminary Votes Recorded in 2018 Parliamentary Election\nas a Share of Total Voter Registration (\"Turnout\")",
       y = "Percent Increase in Total Voter Registration Between 2018 and 2019",
       title = "2018 Turnout and 2019 Voter Registration",
       subtitle = "Author: Colin Cookman (Twitter: @colincookman)\n\nPoints are polling centers where the IEC conducted a June 2019 voter registration \"top-up\" exercise.\nExcludes polling centers with greater than 100% turnout or greater than 100% increase in voter registration. (n = 10)",
       caption = "Data Source: Afghanistan Independent Election Commission\nCaveat: No guarantees are made as to underlying accuracy of this data.")

