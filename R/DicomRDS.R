#################################### DicomRDS Class #############################################
#' DicomRDS Class
#'
#' This class is a class that imports various metadata by reading an RDS file containing DICOM information.
#' There are basically functions that import data related to Radiology CDM.
#'
#' @param data Data frame imported from DICOM RDS file
#' @seealso https://github.com/OHDSI/Radiology-CDM/wiki
#' @author Neon K.I.D
#' @example Examples/DicomRDS_Ex.R
#' @export
DicomRDS <- R6::R6Class(classname = "DicomRDS",
  private = list(
    # Parameter name is TagName
    getTagValue = function(name) {
      tryCatch({
        res <- self$data$value[which(ifelse(self$data$name %in% name, TRUE, FALSE))]
        if(is.empty(x = res))
          res <- NA
      }, error = function(e) {
        res <- NA
        assign("res", res, envir = .GlobalEnv)
      })
      return(res)
    },
    isValidTag = function(name) if(is.boolean(which(ifelse(self$data$name %in% name, TRUE, FALSE)) == 0)) TRUE else FALSE,
    getTagLength = function(name) {
      tryCatch({
        len <- as.numeric(self$data$length[which(ifelse(self$data$name %in% name, TRUE, FALSE))])
        if(is.empty(x = len))
          len <- NA
      }, error = function(e) {
        len <- NA
        assign("len", len, envir = .GlobalEnv)
      })
      return(len)
    }
  ),

  public = list(
    data = NULL,
    idp = NULL,
    initialize = function(data, idp = 2) {
      library(stringr)
      self$data = data
      self$idp = idp
    },

    # isContrast for Brain-CT
    isPost4BrainCT = function() {
      tryCatch({
        if(private$isValidTag(name = "ContrastBolusAgent"))
          return(TRUE)
        else if(private$isValidTag(name = "PhotometricInterpretation")) {
          colorVal <- private$getTagValue(name = "PhotometricInterpretation")
          if(pmatch(x = colorVal, "RGB", nomatch = FALSE) == 1)
            return(TRUE)
        }
        if(private$isValidTag(name = "ContrastBolusRoute"))
          return(TRUE)
        else
          return(FALSE)
      }, error = function(e) {
        return(FALSE)
      })
    },

    # Creation Radiology ID
    createOccurrenceID = function() {
      seriesID <- unlist(strsplit(x = private$getTagValue("SeriesInstanceUID"), split = '[.]'))
      studyID <- unlist(strsplit(x = private$getTagValue("StudyInstanceUID"), split = '[.]'))

      x <- seriesID[length(seriesID)]
      y <- as.numeric(
        substr(x = studyID[length(studyID)], start = nchar(studyID[length(studyID)]) - 4, stop = nchar(studyID[length(studyID)]))
      )

      directoryID <- self$getDirectoryID()
      i <- directoryID
      z <- as.numeric(substr(x = directoryID, start = nchar(directoryID) - 2, stop = nchar(directoryID)))

      # Numbering...
      if(nchar(x) > 7)
        x <- as.numeric(substr(x = x, start = nchar(x) - 5, stop = nchar(x)))
      else
        x <- as.numeric(x)

      max.val <- i + abs(y - nchar(trunc(x)) - x)
      count <- nchar(trunc(max.val))

      size <- paste("%0", count, "d", sep = "")
      set.seed(i)
      # lets <- toupper(sample(letters,x, replace = TRUE))
      nums <- sprintf(ifelse(length(size) > 1, size[1], size), sample(1:max.val)[1:nchar(trunc(z))])
      res <- paste(nums, sep = "")
      return(sum(as.numeric(res)))
    },

    getStudyDate = function() return(private$getTagValue("StudyDate")),
    getStudyTime = function() return(private$getTagValue("StudyTime")),

    # Get Image shot dateTime
    getStudyDateTime = function() {
      studyDate <- private$getTagValue("StudyDate")
      studyTime <- private$getTagValue("StudyTime")

      return(getDateTime(studyDate, studyTime))
    },

    getDuringTime = function(studyDateTime) {
      tryCatch({
        if(private$isValidTag("AcquisitionDate")) {
          acqDate <- private$getTagValue("AcquisitionDate")
          acqTime <- private$getTagValue("AcquisitionTime")
          acqDatetime <- getDateTime(acqDate, acqTime)
          duringTime <- getDiffTime(studyDateTime, acqDatetime)
          return(duringTime)
        }
      }, error = function(e) {
        return(NA)
      })
    },

    getPatientID = function() return(private$getTagValue("PatientID")),
    getDeviceID = function() return(private$getTagValue("DeviceSerialNumber")),
    getModality = function() {
      modal <- private$getTagValue("Modality")
      if(!is.na(modal) && !is.null(modal)) switch(modal, CT = 10321, MR = 10312, modal)
      else NA
    },
    getOrientation = function() return(private$getTagValue("PatientOrientation")),
    getPosition = function() {
      pos <- private$getTagValue("PatientPosition")
      if(!is.na(pos) && !is.null(pos)) switch(pos, HFS = 10421, pos)
      else NA
    },

    getComment = function() return(private$getTagValue("ImageComments")),
    getDosageunit = function(modality) if(modality == 10312) return("Tesla") else return("kVp"),
    getDosage = function(dosageUnit) {
      sha = private$getTagValue(dosageUnit)
      if(is.empty(sha)) return(NA) else return(sha)
    },

    getSourceID = function() return(private$getTagValue("SOPInstanceUID")),
    getPersonID = function() return(private$getTagValue("PatientID")),
    getStudyID = function() return(private$getTagValue("StudyID")),
    getDirectoryID = function() {
      sp <- strsplit(as.character(self$data$path[length(self$data$path)]), '/')
      shortPath <- tail(x = unlist(sp), -1)
      nVec <- unlist(stringr::str_extract_all(string = shortPath[self$idp], pattern = "\\-*\\d+\\.*\\d*"))
      num <- ifelse(length(nVec) > 1, Reduce(pasteNormal, c(abs(as.numeric(nVec[1])), abs(as.numeric(nVec[2])))), nVec)
      return(as.numeric(num))
    },
    getImageType = function() {
      exType <- private$getTagValue("ImageType")
      if(!is.na(exType)) {
        if(stringr::str_detect(string = exType, pattern = "ORIGINAL"))
          imType <- "PRIMARY"
        else if(is.boolean(str_detect(exType, "DERIVED")))
          imType <- "SECONDARY"
      } else {
        imType <- exType
      }
      return(imType)
    },

    getThickness = function() return(private$getTagValue(name = "SliceThickness")),
    getImgRows = function() return(private$getTagValue(name = "Rows")),
    getImgCols = function() return(private$getTagValue(name = "Columns")),
    getWindowCenter = function() return(private$getTagValue(name = "WindowCenter")),
    getWindowWidth = function() return(private$getTagValue(name = "WindowWidth")),

    isPixelData = function() return(private$getTagLength("PixelData") != -1),

    # free func in C...
    # finalize method in Java...
    finalize = function() {}
  )
)
