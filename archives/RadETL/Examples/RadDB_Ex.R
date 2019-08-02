############################# Example Code: Convert Radiology CDM  ###################################

# Create Radiology_Occurrence table for rds Path
rdsPath <- "~/Radiology/rds_test"

# Create RadDB object,,
# If using pararell, require pararell package
RDB <- RadDB$new(core = parallel::detectCores() - 1)

# Get Radiology_Occurrence table
occur <- RDB$createRadiologyOccurrence(path = rdsPath)

# Create Radiology_Image table for read RDS file
rdsFile <- list.files(path = rdsPath, pattern = "\\.rds$", full.names = T, recursive = T)
data <- readRDS(file = rdsFile[2])

# Get Radiology_Image table
img <- RDB$createRadiologyImage(data = data)

# Detach core
RDB$finalize()

######################################## Example Code: END ##########################################
