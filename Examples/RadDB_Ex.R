############################# Example Code: Convert Radiology CDM  ###################################

# Create Radiology_Occurrence table for rds Path
rdsPath <- "/home/ohdsi/Radiology/rdsfiles"

# Create RadDB object,,
# If using pararell, require pararell package
RDB <- RadDB$new(core = parallel::detectCores() - 1)

# Get Radiology_Occurrence table
occur <- RDB$createRadiologyOccurrence(path = rdsPath)

# Create Radiology_Image table for read RDS file
rdsFile <- "/home/ohdsi/DICOM-header/header.rds"
data <- readRDS(file = rdsFile)

# Get Radiology_Image table
img <- RDB$createRadiologyImage(data = data)

######################################## Example Code: END ##########################################
