############################# Example Code: Read DICOM RDS File   ###################################

rdsFile <- "/home/ohdsi/DICOM-header/header.rds"

data <- readRDS(file = rdsFile)

# If single data (only one metadata in dicom file )
dcmRDS <- DicomRDS$new(data)

# Create Occurrence ID for radiology image
roID <- dcmRDS$createOccurrenceID()

# Get PatientID
patientid <- dcmRDS$getPatientID()

# This radiology contast or non-contrast
isContrast <- dcmRDS$isPost4BrainCT()

# If Multi data
for(i in 1:length(data)) {
  dcmRDS <- DicomRDS$new(data[[i]])
  roID <- dcmRDS$createOccurrenceID()
  patientid <- dcmRDS$getPatientID()
  isContrast <- dcmRDS$isPost4BrainCT()
  df <- data.frame(roID, patientid, isContrast)
}

View(df)

######################################## Example Code: END ##########################################
