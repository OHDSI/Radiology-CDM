library(RadETL)

# Image unpacking -> save RDS file
path <- '~/signedrangeimages/IMAGES'
savePathRoot <- '~/stored_data/signedrangeimages'
dfm <- DcmFileModule$new(path = path, savePathRoot = savePathRoot, core = parallel::detectCores() - 1)
res <- dfm$dcmToRDS(rootPathCount = 4, verbose = FALSE)

# createRadiologyDB
Rdb <- RadDB$new(core = parallel::detectCores() - 1)
rcdm <- rds$createRadiologyDB(path = path, idp = 2, o_start = 1)

print.data.frame(rcdm[[1]], quote = TRUE) # Occurrence
print.data.frame(rcdm[[2]], quote = TRUE) # Image
