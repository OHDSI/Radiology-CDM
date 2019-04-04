library(RadETL)

# Image unpacking -> save RDS file
path <- '~/signedrangeimages/IMAGES'
savePathRoot <- '~/stored_data/signedrangeimages'
dfm <- DcmFileModule$new(path = path, savePathRoot = savePathRoot, core = parallel::detectCores() - 1)
res <- dfm$dcmToRDS(rootPathCount = 4, verbose = FALSE)

# createRadiologyOccurrence
Rdb <- RadDB$new(core = parallel::detectCores() - 1)
occur <- Rdb$createRadiologyOccurrence(path = savePathRoot, idp = 2)

# createRadiologyImage
files <- list.files(path = savePathRoot, pattern = '\\.rds$', full.names = TRUE, recursive = TRUE)
i <- sample.int(length(files), 1)
data <- readRDS(file = files[i])
dRDS <- DicomRDS$new(data = data)
img <- Rdb$createRadiologyImage(data = data)

print.data.frame(occur, quote = TRUE)
print.data.frame(img, quote = TRUE)
