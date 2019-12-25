target <- "http://www.iec.org.af/en/auditlist-2019"

pdf_links <- getHTMLLinks(target)
pdf_links <- pdf_links[grep("/pdf/ps audit list/", pdf_links)]
pdf_links <- paste0("http://www.iec.org.af", pdf_links)
pdf_links <- gsub("\\../", "", pdf_links)
pdf_links <- gsub(" ", "%20", pdf_links)

for(i in seq_along(pdf_links)) {
  download.file(pdf_links[i], file.path("./raw/audit_target_list/", destfile = basename(pdf_links[i])))
  Sys.sleep(1)
}

# -----------------------------------------------------------------------------

rm(list = ls())
file_list <- list.files("./raw/audit_target_list", pattern = ".pdf")
