############################# Example Code: Read DICOM RDS File   ###################################

rdsFile <- list.files(path = "~/Radiology/rds_test", pattern = "\\.rds$", full.names = T, recursive = T)

# If single data (only one metadata in dicom file )
dcmRDS <- DicomRDS$new(data[[1]])

# Create Occurrence ID for radiology image
roID <- dcmRDS$createOccurrenceID()

# Get PatientID
patientid <- dcmRDS$getPatientID()
dcmRDS$getDirectoryID()

# This radiology contast or non-contrast
isContrast <- dcmRDS$isPost4BrainCT()

# If Multi data
data <- readRDS(file = rdsFile[5])
for(i in 1:length(data)) {
  dcmRDS <- DicomRDS$new(data[[i]])
  roID <- dcmRDS$createOccurrenceID()
  patientid <- dcmRDS$getDirectoryID()
  isContrast <- dcmRDS$isPost4BrainCT()
  df <- data.frame(roID, patientid, isContrast)
}
df

######################################## Example Code: END ##########################################
