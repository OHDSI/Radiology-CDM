#################################### DicomRDS Class #############################################
DicomRDS <- R6Class(classname = "DicomRDS",
  private = list(
    # Parameter name is TagName
    # Parameter data is RDS Data...
    getTagValue = function(name) {
      tryCatch({
        res <- self$data$value[which(ifelse(self$data$name %in% name, TRUE, FALSE))]
        if(is.empty(x = res))
          res <- ''
      }, error = function(e) {
        res <- ''
        assign("res", res, envir = .GlobalEnv)
      })
      return(res)
    },
    isValidTag = function(name) if(is.boolean(which(ifelse(self$data$name %in% name, TRUE, FALSE)) == 0)) TRUE else FALSE
  ),

  public = list(
    data = NULL,
    initialize = function(data) {
      self$data = data
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
        return(FALSE)
      }, error = function(e) {
        return(FALSE)
      })
    },

    # Creation Radiology ID
    createOccurrenceID = function() {
      seriesID <- private$getTagValue("SeriesInstanceUID")
      studyID <- private$getTagValue("StudyID")

      return(Reduce(pasteSQL, c(seriesID, studyID)))
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
        return('')
      })
    },

    getPatientID = function() return(private$getTagValue("PatientID")),
    getDeviceID = function() return(private$getTagValue("DeviceSerialNumber")),
    getModality = function() return(private$getTagValue("Modality")),
    getOrientation = function() {
      pocID <- private$getTagValue("PatientPosition")
      if(is.empty(pocID))
        pocID <- private$getTagValue("PatientOrientation")
      return(pocID)
    },

    getComment = function() return(private$getTagValue("ImageComments")),
    getDosageunit = function(modality) if(equals(modality, "MRI")) return("Tesla") else return("kVp"),
    getDosage = function(dosageUnit) {
      sha = private$getTagValue(dosageUnit)
      if(is.empty(sha)) return('') else return(sha)
    },

    getSourceID = function() return(private$getTagValue("SOPInstanceUID")),
    getPersonID = function() return(private$getTagValue("PatientID")),
    getImageType = function() {
      exType <- private$getTagValue("ImageType")
      if(!is.na(exType)) {
        if(str_detect(string = exType, pattern = "ORIGINAL"))
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

    # Image Type is SECONDARY DERIVED..
    isDERIVED = function() {
      # Todo: Write image type description...
    },

    # free func in C...
    # finalize method in Java...
    finalize = function() {}
  )
)

# Available multiple RDS file
# maybe seems to parallel processing...
getCommonHdr <- function(path, verbose = FALSE) {
  fileList <- list.files(path = path, recursive = TRUE, full.names = TRUE)
  print(Reduce(pasteNormal, c("Common Header Extracting: ", path)))
  pb <- if(verbose) txtProgressBar(min = 0, max = length(fileList), style = 3, width = 100)

  for(k in 1:length(fileList)) {
    data <- readRDS(file = fileList[k])
    minor <- c()

    for(i in 1:length(data)) {
      # only tag name
      tagList <- unique(data[[i]]$name)
      minor <- c(minor, tagList)
    }

    if(verbose) setTxtProgressBar(pb = pb, value = k)
  }
  if(verbose) close(pb)
  return(unique(minor))
}

# Count list is Data frame...
getHdrcountFrame <- function(path, verbose = FALSE) {
  tagName <- getCommonHdr(path, verbose = verbose)
  tagCount <- rep(1, length(tagName))

  # Common TagList
  frame <- data.frame(tagName, tagCount)
  fileList <- list.files(path = path, recursive = TRUE, full.names = TRUE)

  print(Reduce(pasteNormal, c("Common Header Counting: ", path)))
  pb <- if(verbose) txtProgressBar(min = 0, max = length(fileList), style = 3, width = 100)

  # Dicom total Count
  dCount <- 0

  for(k in 1:length(fileList)) {
    # One RDS File Read... (in multi dicom files)
    data <- readRDS(file = fileList[k])
    dCount <- dCount + length(data)

    # File Loop,,
    for(d in 1:length(data)) {
      if(!is.null(data[[d]])) {
        name <- data[[d]]$name
        value <- data[[d]]$value

        # Only one dicom file tag list
        for(f in 1:length(frame$tagName)) {
          tName <- frame$tagName[f]
          tCount <- frame$tagCount[f]

          ifelse(name %in% tName, frame$tagCount[f] <- tCount + 1, NA)
        }
      }
    }
    if(verbose) setTxtProgressBar(pb = pb, value = k)
  }
  if(verbose) close(pb)
  print(Reduce(pasteNormal, c("DICOM file total Count: ", dCount)))
  return(frame)
}
