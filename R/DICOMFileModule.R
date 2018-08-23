################################ DcmFileModule Class #############################################
DcmFileModule <- R6::R6Class(classname = "DcmFileModule",
  private = list(
    path = NULL,
    savePathRoot = NULL,

    # Return DICOM file all headers...
    extractHdrData = function(path, rootPathCount, debugging) {
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
  ),

  public = list(
    initialize = function(path, savePathRoot) {
      private$path <- path
      private$savePathRoot <- savePathRoot
    },

    # All dcm files convert to RDS
    dcmToRDS = function(rootPathCount = 1, verbose = FALSE) {
      print("Read Directories....")
      allList <- list.dirs(private$path, full.names = FALSE)

      # Create All directory...
      # Explore Files...
      for(i in 1:length(allList)) {
        if(!is.empty(allList[i])) {
          print("Create Directories....")
          createDir(allList[i])

          # Load DICOM files
          loadPath <- Reduce(pastePath, c(private$path, allList[i]))
          listFile <- list.files(loadPath, full.names = TRUE, include.dirs = FALSE, recursive = FALSE, pattern = "\\.dcm$")

          # FileList is not empty...
          if(!isEmpty(listFile)) {
            data <- list()
            for(j in 1:length(listFile))
              data[[j]] <- private$extractHdrData(listFile[j], private$rootPathCount, debugging = verbose)
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
    },
    finalize = function() {}
  )
)

#################################### END ###################################################
