files <- list.files()
if("remove_log.sh" %in% files & "Kepler_KOI_DV_wget.bat" %in% files) {
  system("sh remove_log.sh")
  dir.create("data", showWarnings = FALSE)
  system("mv Kepler_KOI_DV_wget.bat data/")
  system("mv remove_log.sh data/")
}

setwd("data")

files <- list.files(pattern = ".tbl")
if(length(files) == 0) {
  system("sh Kepler_KOI_DV_wget.bat")
}
