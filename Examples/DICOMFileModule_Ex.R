############################# Example Code: Create DICOM RDS File ###################################

# Path is directory folder
path <- "~/Radiology/quer.ai_test"

# savePathRoot is directory folder
savePathRoot <- "~/Radiology/rds_test"

# Parallel core
core <- parallel::detectCores() - 1

DFM <- DcmFileModule$new(path = path, savePathRoot = savePathRoot, core = core)

# rootPathCount is a parameter that determines which of the current DICOM file paths to cut.
# If you do not want to cut this parameter, you can omit this parameter.
DFM$dcmToRDS(rootPathCount = 4)

######################################## Example Code: END ##########################################
