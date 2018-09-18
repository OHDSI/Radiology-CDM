################################ DcmFileModule Class #############################################
#' DcmFileModule Class
#'
#' This class creates a dataset by compressing a batch of DICOM files into a single RDS file.
#' Each DICOM file is so large that it takes a lot of time to read it, and it is a function that extracts only its metadata.
#'
#' @param path Path with DICOM image file
#' @param savePathRoot Top path to store extracted metadata
#' @usage DcmFileModule$new(path, savePathRoot)
#' @author Neon K.I.D
#' @example Examples/DICOMFileModule_Ex.R
#' @export
DcmFileModule <- R6::R6Class(classname = "DcmFileModule",
  private = list(
    path = NULL,
    savePathRoot = NULL,

    # Return DICOM file all headers...
    extractHdrData = function(path, rootPathCount, debugging) {
      # If see processbar, verbose set TRUE, don't see verbose set FALSE // DANGER R Studio crashing...
      dicomImg <- readDCM(path, debug = debugging)
      header <- dicomImg$hdr

      # RootPathCount is specify relative path count
      # Default is 1 compatiable for Windows Operating system..
      sp <- strsplit(path, "/")
      filePath <- Reduce(pastePath, tail(unlist(sp), -abs(rootPathCount)))

      if(!is.data.frame(header))
        res <- data.frame(group = header[[1]]['group'], element = header[[1]]['element'], name = header[[1]]['name'], code = header[[1]]['code'], length = header[[1]]['length'], value = header[[1]]['value'], path = filePath, stringsAsFactors = FALSE)
      else
        res <- data.frame(group = header['group'], element = header['element'], name = header['name'], code = header['code'], length = header['length'], value = header['value'], path = filePath, stringsAsFactors = FALSE)
      # Return data frame..
      return(res)
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
          if(!is.empty(listFile)) {
            data <- list()
            for(j in 1:length(listFile))
              data[[j]] <- private$extractHdrData(listFile[j], rootPathCount = rootPathCount, debugging = verbose)
            if(length(data) != 0) {
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
