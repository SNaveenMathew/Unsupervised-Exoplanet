# Cross-platform Kepler bulk light curve downloader helper

files <- list.files()
dir.create("data", showWarnings = FALSE)

if("remove_log.sh" %in% files) {
  file.rename("remove_log.sh", "data/remove_log.sh")
}

if("Kepler_KOI_DV_wget.bat" %in% files) {
  file.rename("Kepler_KOI_DV_wget.bat", "data/Kepler_KOI_DV_wget.bat")
}

tbl_files <- list.files("data", pattern = "\\.tbl$")
if(length(tbl_files) == 0) {
  message("No .tbl files found in data/. To download light curves, execute data/Kepler_KOI_DV_wget.bat or wget.")
} else {
  message(sprintf("Found %d Kepler .tbl light curve files in data/.", length(tbl_files)))
}
