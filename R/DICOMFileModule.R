################################ Function Zone #############################################

# Return DICOM file all headers...
extractHdrData(path, rootPathCount = 1, debugging) %as% {
  # If see processbar, verbose set TRUE, don't see verbose set FALSE // DANGER R Studio crashing...
  dicomImg <- readDCM(path, debug = debugging)
  header <- dicomImg$hdr[[1]]

  # RootPathCount is specify relative path count
  # Default is 1 compatiable for Windows Operating system..
  sp <- strsplit(path, "/")
  filePath <- Reduce(pastePath, tail(unlist(sp), -rootPathCount))

  # Return data frame..
  return(data.frame(path = filePath, group = header['group'], element = header['element'], name = header['name'], code = header['code'], length = header['length'], value = header['value']))
}

# Integrated readDICOM Func...
readDCM <- function(path, debug = FALSE, view = FALSE) {
  if(view) {
    files <- list.files(path = path, full.names = TRUE, include.dirs = FALSE, recursive = FALSE, pattern = "\\.dcm$")
    count <- 0
    nfiles <- length(files)
    headers <- images <- vector("list", nfiles)
    cat(" ", nfiles, "files to be processed by readDCM()", fill = TRUE)
    tpb <- txtProgressBar(min = 0, max = nfiles, style = 3)
    for(i in 1:nfiles) {
      setTxtProgressBar(tpb, i)
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
    close(tpb)
    return(list(hdr = headers[sapply(headers, is.null)], img = images[sapply(images, is.null)]))
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

# All dcm files convert to RDS
dcmToRDS(path, savePathRoot, rootPathCount = 1, verbose = FALSE) %as% {
  print("Read Directories....")
  allList <- list.dirs(path, full.names = FALSE)

  # Create All directory...
  # Explore Files...
  for(i in 1:length(allList)) {
    if(!is.empty(allList[i])) {
      print("Create Directories....")
      createDir(allList[i])

      # Load DICOM files
      loadPath <- Reduce(pastePath, c(path, allList[i]))
      listFile <- list.files(loadPath, full.names = TRUE, include.dirs = FALSE, recursive = FALSE, pattern = "\\.dcm$")

      # FileList is not empty...
      if(!isEmpty(listFile)) {
        data <- list()
        for(j in 1:length(listFile))
          data[[j]] <- extractHdrData(listFile[j], rootPathCount, debugging = verbose)
        if(!isEmpty(data)) {
          fileName <- strsplit(allList[i], split = "/")

          # folderPath/fileName.rds
          rdsName <- c(savePathRoot, allList[i], Reduce(pasteNormal, c(tail(fileName[[1]], 1), ".rds")))
          savePath <- Reduce(pastePath, rdsName)
          objectToRDS(data, savePath)
        }
      }
    }
  }
}

#################################### END ###################################################
