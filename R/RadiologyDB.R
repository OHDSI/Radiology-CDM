library(rapportools)
library(foreach)

###################################### RadDB Class #############################################
#' RadDB Class
#'
#' This is a class that creates an RCDM using an extracted RDS file.
#' Parallel processing is supported only in the current Occurrence table,
#' and it is recommended that the image table not be processed in parallel
#' because of the computer science problem that the number of images in the image table does not match.
#' Even if you set Pararell through the constructor, it is not supported by the Image method.
#'
#' @param core Number of cores to use
#' @seealso https://github.com/NEONKID/RCDM-ETL/wiki
#' @usage RadDB$new(core)
#' @author Neon K.I.D
#' @example Examples/RadDB_Ex.R
#' @export
RadDB <- R6::R6Class(classname = "RadDB",
  private = list(
    mergeDfList = function(x, y) merge(x, y, all = TRUE)
  ),

  public = list(
    initialize = function(core) {
      # Parallel Processing
      doParallel::registerDoParallel(core)
    },

    createRadiologyOccurrence = function(path) {
      fileList <- list.files(path = path, recursive = TRUE, full.names = TRUE, pattern = "\\.rds$")

      radiology_occurrence_ID <- c()
      radiology_occurrence_date <- c(as.Date(NA))
      radiology_occurrence_datetime <- c(as.POSIXct(rep(NA)))
      Person_ID <- c()
      Condition_occurrence_id <- c()
      Device_concept_id <- c()
      radiology_modality_concept_ID <- c()
      Person_orientation_concept <- c()
      Person_position_concept <- c()
      radiology_protocol_concept_id <- c()
      Image_total_count <- c()
      Anatomic_site_concept_id <- c()
      radiology_Comment <- c()
      Image_dosage_unit_concept <- c()
      Dosage_value_as_number <- c()
      Image_exposure_time_unit_concept <- c()
      Image_exposure_time <- c()
      Radiology_dirpath <- c()
      Visit_occurrence_id <- c()

      ro <- foreach(f = 1:length(fileList)) %dopar% {
        data <- readRDS(file = fileList[f])
        for(i in 1:length(data)) {
          if(is.null(data[[i]]))
            stop("ERROR: There is an empty value in the data frame.")
          else {
            dcmRDS <- DicomRDS$new(data[[i]])

            # Dirpath Settings
            # Temp code, if source code open, please modify...
            sp <- strsplit(as.character(data[[i]]$path[1]), '/')
            rDirPath <- head(unlist(sp), -1)
            rDirPath <- Reduce(pastePath, rDirPath)

            roID <- as.integer(dcmRDS$createOccurrenceID())
            studyDatetime <- dcmRDS$getStudyDateTime()

            # Searching AcquisitionDateTime...
            duringTime <- ''
            for(k in length(data):i) {
              dcmRDSk <- DicomRDS$new(data[[k]])
              duringTime <- dcmRDSk$getDuringTime(studyDateTime = studyDatetime)
              if(!is.empty(duringTime)) break else duringTime <- NA
              dcmRDSk$finalize()
            }

            #pID <- dcmRDS$getPatientID()
            pID <- dcmRDS$getDirectoryID()
            coID <- 0
            dcID <- dcmRDS$getDeviceID()
            modality <- dcmRDS$getModality()
            pocID <- dcmRDS$getPosition()
            oriID <- dcmRDS$getOrientation()

            # Contrast Information,,
            # TRUE = Post image
            # FALSE = Pre image
            rpcID <- "Pre Contrast"
            for(j in i:length(data)) {
              dcmRDSj <- DicomRDS$new(data[[j]])
              if(dcmRDSj$isPost4BrainCT()) {
                rpcID <- "Post Contrast"
                break
              }
              dcmRDSj$finalize()
            }

            tCount <- length(data)
            ascID <- 4119359
            imgComment <- dcmRDS$getComment()
            dosage <- dcmRDS$getDosageunit(modality = modality)
            dosageNum <- dcmRDS$getDosage(dosageUnit = dosage)
            timeUnit <- "sec"
            voID <- 0
            break
          }
        }

        radiology_occurrence_ID <- roID
        radiology_occurrence_date <- getDate(dcmRDS$getStudyDate())
        radiology_occurrence_datetime <- studyDatetime

        Person_ID <- as.integer(pID)
        Condition_occurrence_id <- as.integer(coID)
        Device_concept_id <- dcID

        radiology_modality_concept_ID <- modality  # VARCHAR
        Person_position_concept <- pocID           # VARCHAR
        Person_orientation_concept <- oriID        # VARCHAR
        radiology_protocol_concept_id <- rpcID     # This is ID but varchar now

        Image_total_count <- as.integer(tCount)
        Anatomic_site_concept_id <- as.integer(ascID)
        radiology_Comment <- imgComment

        Image_dosage_unit_concept <- dosage
        Dosage_value_as_number <- as.numeric(dosageNum)
        Image_exposure_time_unit_concept <- timeUnit

        Image_exposure_time <- duringTime
        Radiology_dirpath <- rDirPath
        Visit_occurrence_id <- as.integer(voID)

        data.frame(
          radiology_occurrence_ID,
          radiology_occurrence_date,
          radiology_occurrence_datetime,
          Person_ID,
          Condition_occurrence_id,
          Device_concept_id,
          radiology_modality_concept_ID,
          Person_orientation_concept,
          Person_position_concept,
          radiology_protocol_concept_id,
          Image_total_count,
          Anatomic_site_concept_id,
          radiology_Comment,
          Image_dosage_unit_concept,
          Dosage_value_as_number,
          Image_exposure_time_unit_concept,
          Image_exposure_time,
          Radiology_dirpath,
          Visit_occurrence_id,
          stringsAsFactors = FALSE
        )
      }
      return(Reduce(private$mergeDfList, ro))
    },

    createRadiologyImage = function(data) {
      Radiology_occurrence_ID <- c()
      Person_ID <- c()
      Person_orientation_concept <- c()
      Image_type <- c()
      radiology_phase_concept <- c()
      Image_no <- c()
      Phase_total_no <- c()
      image_resolution_Rows <- c()
      image_Resolution_Columns <- c()
      Image_Window_Level_Center <- c()
      Image_Window_Level_Width <- c()
      Image_slice_thickness <- c()
      image_filepath <- c()

      num <- 1
      pNum <- 1
      rID <- NA
      reNum <- 1

      # Current imageType, radiology_phase_concept
      curimType <- NA
      curPCID <- NA

      for(i in 1:length(data)) {
        if(!is.null(data[[i]])) {
          dcmRDS <- DicomRDS$new(data[[i]])
          Person_ID[num] <- dcmRDS$getDirectoryID()

          # PatientPosition is null .... blank
          pocID <- dcmRDS$getOrientation()

          # Get ImageType, ORIGINAL is PRIMARY, DERIVED is SECONDARY
          imType <- dcmRDS$getImageType()
          modality <- dcmRDS$getModality()

          # Contrast Information,,
          # TRUE = Post. Additional, Color format is RGB that 3D IMAGE FORMAT..
          # FALSE = Pre
          rpcID <- "Pre Contrast"
          if(pmatch(x = imType, "SECONDARY", nomatch = FALSE) == 1) rpcID <- "DERIVED Image"
          else if(dcmRDS$isPost4BrainCT()) rpcID <- "Post Contrast"

          radiology_phase_concept[num] <- rpcID
          thickness <- dcmRDS$getThickness()
          Phase_total_no[num] <- 0

          rows <- dcmRDS$getImgRows()
          columns <- dcmRDS$getImgCols()

          # Checking Phase number..
          if(num == 1) {
            rID <- as.integer(dcmRDS$createOccurrenceID())
            curimType <- imType
            curPCID <- rpcID
          } else if(is.na(pmatch(x = rpcID, curPCID, nomatch = NA_character_))
                    || is.na(pmatch(x = imType, curimType, nomatch = NA_character_))) {
            for(k in reNum:num)
              Phase_total_no[k] <- pNum - 1

            curimType <- imType
            curPCID <- rpcID

            reNum <- num
            pNum <- 1
          } else if(i == length(data)) {
            for(k in reNum:num)
              Phase_total_no[k] <- pNum
          }

          Image_no[num] <- as.integer(pNum)
          Radiology_occurrence_ID[num] <- as.integer(rID)
          image_resolution_Rows[num] <- as.integer(rows)
          image_Resolution_Columns[num] <- as.integer(columns)
          Image_Window_Level_Center[num] <- dcmRDS$getWindowCenter()
          Image_Window_Level_Width[num] <- dcmRDS$getWindowWidth()
          Image_slice_thickness[num] <- if(is.empty(thickness)) '' else as.numeric(thickness)
          Image_type[num] <- imType
          Person_orientation_concept[num] <- pocID
          image_filepath[num] <- as.character(data[[i]]$path[1])

          num <- num + 1
          pNum <- pNum + 1
        }
      }

      Radiology_Image <- data.frame(
        Radiology_occurrence_ID,
        Person_ID,
        Person_orientation_concept,
        Image_type,
        radiology_phase_concept,
        Image_no,
        Phase_total_no,
        image_resolution_Rows,
        image_Resolution_Columns,
        Image_Window_Level_Center,
        Image_Window_Level_Width,
        Image_slice_thickness,
        image_filepath,
        stringsAsFactors = FALSE
      )
      return(Radiology_Image)
    },

    finalize = function() {}
  )
)

