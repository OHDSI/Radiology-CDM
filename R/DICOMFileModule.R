require(foreach)
require(rapportools)

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
    cl = NULL,
    needFunc = c('createDir', 'pastePath', 'pasteNormal', 'private'),
    needPkg = c('rapportools', 'oro.dicom'),

    objectToRDS = function(data, savePath) {
      # Save RDS..
      saveRDS(object = data, file = savePath)
      msg <- c(Sys.time(), " -> Saved: ", savePath)
      print(Reduce(pasteNormal, msg))
    },

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
    initialize = function(path, savePathRoot, core) {
      private$path <- path
      private$savePathRoot <- savePathRoot

      # Parallel Processing
      private$cl <- parallel::makePSOCKcluster(core)
      doSNOW::registerDoSNOW(private$cl)
    },

    # All dcm files convert to RDS
    dcmToRDS = function(rootPathCount = 1, verbose = FALSE) {
      allList <- list.dirs(private$path, full.names = FALSE)
      writeLines('Metadata and pixeldata are combined into a single DB....')
      pb <- txtProgressBar(min = 0, max = length(allList), style = 3)

      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress=progress)

      # Create All directory...
      # Explore Files...
      dtor <- foreach(i = 1:length(allList), .options.snow = opts, .packages = private$needPkg, .export = private$needFunc) %dopar% {
        if(!is.empty(allList[i])) {
          createDir(private$savePathRoot, allList[i])

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
              rdsName <- c(private$savePathRoot, allList[i], Reduce(pasteNormal, c(tail(fileName[[1]], 1), ".rds")))
              savePath <- Reduce(pastePath, rdsName)
              private$objectToRDS(data, savePath)
            }
          }
        }
      }
      return(Filter(Negate(is.null), dtor))
    },
    finalize = function() {
      parallel::stopCluster(cl = private$cl)  # Requirement
    }
  )
)

#################################### END ###################################################
