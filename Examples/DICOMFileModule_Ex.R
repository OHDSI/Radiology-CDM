############################# Example Code: Create DICOM RDS File ###################################

# Path is directory folder
path <- "/home/ohdsi/Pictures/Dicom-Images/DCM-12411"

# savePathRoot is directory folder
savePathRoot <- "/home/ohdsi/DICOM-header"

DFM <- DcmFileModule$new(path = path, savePathRoot = savePathRoot)

# rootPathCount is a parameter that determines which of the current DICOM file paths to cut.
# If you do not want to cut this parameter, you can omit this parameter.
DFM$dcmToRDS(rootPathCount = 4, verbose = FALSE)

######################################## Example Code: END ##########################################
