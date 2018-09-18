############################# Example Code: Create Radiology from RDS.. #############################

dirpath <- "/home/ohdsi/Documents/DICOM-header"

dirs <- list.dirs(path = dirpath, full.names = TRUE, recursive = FALSE)
tpb <- txtProgressBar(min = 0, max = length(dirs), style = 3)

for(i in 1:length(dirs)) {
  files <- list.files(path = dirs[i], pattern = "\\.rds$", recursive = TRUE)
  if(length(files) == 0) next
  setTxtProgressBar(tpb, i)
  df <- createRadiologyOccurrence(dirs[i])
  View(df)
}

######################################## Example Code: END ##########################################
