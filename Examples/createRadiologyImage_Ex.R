############################# Example Code: Create Radiology from RDS.. #############################

dirpath <- "/home/ohdsi/Documents/DICOM-header"

files <- list.files(path = dirpath, full.names = TRUE, recursive = TRUE, pattern = "\\.rds$")
tpb <- txtProgressBar(min = 0, max = length(files), style = 3)

for(i in 1:length(files)) {
  setTxtProgressBar(tpb, i)
  df <- createRadiologyImage(data = readRDS(files[i]))
  View(df)
}

######################################## Example Code: END ##########################################
