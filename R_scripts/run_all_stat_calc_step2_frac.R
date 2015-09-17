# Run the statistical calculation second step (see stat_calc_step2_institutions.R) for all
# files we can find in the "input" directory.

rm(list=ls())

fracMode = TRUE
source("common.R")

for (d in list.files(paste0(dataDir, "/input"))) {
  institutionFiles = list.files(paste0(dataDir, "/input/", d), pattern = "\\.txt$", ignore.case = TRUE)
  institutions = c()
  for (institutionFile in institutionFiles) {
    # remove ".txt"
    institution = substring(institutionFile, 1, nchar(institutionFile)-4)
    institutions = c(institutions, institution)
  }
  corpus = d
  source("stat_calc_step2_institutions.R")
}

