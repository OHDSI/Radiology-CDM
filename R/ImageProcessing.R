# Integrated readDICOM Func...
library(oro.dicom)

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
      # print(path)
      resImg <- readDICOM(path = path, verbose = debug)
    }, error = function(e) {
        if(debug) {
          errComment <- c("readDICOM func error: ", e)
          errFile <- c("Change func readDICOMFile: ", path)

          print(Reduce(pasteNormal, errComment))
          print(Reduce(pasteNormal, errFile))
        }

      # Retry not parse pixelData function..
      resImg <- readDICOMFile(fname = path, pixelData = FALSE)
      # assign("resImg", resImg, envir = .GlobalEnv)
    })
    return(resImg)
  }
}

convertNifti <- function(filePath) {
  files <- list.files(path = filePath, pattern = "\\.dcm$", full.names = TRUE)
  nifList <- NA

  for(i in 1:length(files)) {
    tryCatch({
      img <- oro.dicom::readDICOM(path = files[i], recursive = TRUE)
      nif <- oro.dicom::dicom2nifti(dcm = img)
      if(is.na(nifList))
        nifList <- nif
      else if(length(dim(nifList)) == 3)
        nifList <- abind::abind(nifList, nif, along = 1)
      else if(length(dim(nifList)) == 2)
        nifList <- abind::abind(nifList, nif, along = 0)
    }, error = function(e) {
      print(Reduce(pasteNormal, c("Error for ", files[i])))
    })
  }
  return(nifList)
}
