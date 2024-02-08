update_DNA_datasets <- function(repo = "Data_Novotny/AmpliconSeqAnalysis") {
  
  # This function should update the repository with all necessary files from Google Drive.
  # No data other than what is imported in this function should be used for this data project.
  
  require(googledrive)
  repo = "Data_Novotny/AmpliconSeqAnalysis"
  file.path(repo, "Taxtab_COI.rds")
  
  ##############################
  # Download COI data components
  
  drive_download(file.path(repo, "Taxtab_COI.rds"),
                 "Data_import/COI/Taxtab_COI.rds",
                 overwrite = TRUE)
  
  drive_download(file.path(repo, "ASV_COI.rds"),
                 "Data_import/COI/ASV_COI.rds",
                 overwrite = TRUE)
  
  drive_download(file.path(repo, "Samptab_COI.rds"),
                 "Data_import/COI/Samptab_COI.rds",
                 overwrite = TRUE)
  
  
  ##############################
  # Download 12S data components
  
  drive_download(file.path(repo, "Taxtab_12S.rds"),
                 "Data_import/12S/Taxtab_12S.rds",
                 overwrite = TRUE)
  
  drive_download(file.path(repo, "ASV_12S.rds"),
                 "Data_import/12S/ASV_12S.rds",
                 overwrite = TRUE)
  
  drive_download(file.path(repo, "Samptab_12S.rds"),
                 "Data_import/12S/Samptab_12S.rds",
                 overwrite = TRUE)
  
}


update_Contributed_data <- function(repo = "Data_Novotny/ContributedDNAprojects") {
  
  require(googledrive)
  
  
  #################################
  # Download 18S time series data
  drive_download(file.path(repo, "18S_Caterina/ps_18S.rds"),
                 "Data_import/Contributed18S/ps_18S.rds",
                 overwrite = TRUE)
  
}