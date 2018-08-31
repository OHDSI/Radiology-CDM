# Package for Papayar Widgets
# Connecting SQL Server,,
# After connection, input want informations..
#' @export
showImages <- function(path, debug = FALSE) {
  # Required PixelData
  imgs <- readDICOM(path = path, verbose = debug)
  nif <- dicom2nifti(imgs)
  papaya(nif)
}

# Integrated readDICOM Func...
#' @export
readDCM <- function(path, debug = FALSE, view = FALSE) {
  if(view) {
    if(is.list(path))
      files <- as.character(path)
    else if(is.vector(path))
      files <- path
    else
      files <- list.files(path = path, full.names = TRUE, include.dirs = FALSE, recursive = FALSE, pattern = "\\.dcm$")
    count <- 0
    nfiles <- length(files)
    headers <- images <- vector("list", nfiles)
    if(debug) {
      cat(" ", nfiles, "files to be processed by readDCM()", fill = TRUE)
      tpb <- txtProgressBar(min = 0, max = nfiles, style = 3)
    }
    for(i in 1:nfiles) {
      if(debug) setTxtProgressBar(tpb, i)
      tryCatch({
        dcm <- readDICOM(path = files[i])
        images[[i]] <- dcm$img
        headers[[i]] <- dcm$hdr
        count <- count + 1
      }, error = function(e) {
        errComment <- c("readDICOM func error: ", e)
        print(Reduce(pasteNormal, errComment))
      })
    }
    cat("\n", count, "read successes of", nfiles, "files", fill = TRUE)
    if(debug) close(tpb)
    return(list(hdr = headers, img = images))
  } else {
    resImg <- tryCatch({
      print(path)
      readDICOM(path = path, verbose = debug)
    }, error = function(e) {
      errComment <- c("readDICOM func error: ", e)
      errFile <- c("Change func readDICOMFile: ", path)

      print(Reduce(pasteNormal, errComment))
      print(Reduce(pasteNormal, errFile))

      # Retry not parse pixelData function..
      resImg <- readDICOMFile(fname = path, pixelData = FALSE)
      assign("resImg", resImg, envir = .GlobalEnv)
    })
    return(resImg)
  }
}

if(!require(lambda.r))
  install.packages("lambda.r")
library(lambda.r)

# File is DICOM ?
isDicom(file) %as% {
  ext <- substr(file, nchar(file) - 2, nchar(file))
  return(if(ext == 'dcm') TRUE else FALSE)
}
