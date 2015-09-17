# Run the statistical calculation first step (see stat_calc_step1_world.R) for all
# files we can find in the "world" directory.

rm(list=ls())

fracMode = TRUE
source("common.R")

for (d in list.files(paste0(dataDir, "/world"), pattern = "\\.txt$", ignore.case = TRUE)) {
  corpus <- substring(d, 1, nchar(d)-4)
  source("stat_calc_step1_world.R")
}

