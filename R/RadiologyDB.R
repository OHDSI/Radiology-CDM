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
#' @seealso https://github.com/OHDSI/Radiology-CDM/wiki
#' @author Neon K.I.D
#' @example Examples/RadDB_Ex.R
#' @export
RadDB <- R6::R6Class(classname = "RadDB",
  private = list(
    cl = NULL,
    needFunc = c('as.bigint', 'as.float', 'getDate', 'pastePath'),
    mergeDfList = function(x, y) merge(x, y, all = TRUE)
  ),

  public = list(
    initialize = function(core) {
      # library(foreach)
      # library(rapportools)

      # Parallel Processing
      private$cl <- parallel::makePSOCKcluster(core)
      doSNOW::registerDoSNOW(private$cl)
    },

    createRadiologyOccurrence = function(path) {
      fileList <- list.files(path = path, recursive = TRUE, full.names = TRUE, pattern = "\\.rds$")
      writeLines('Create Radiology_Occurrence Data frame....')
      pb <- txtProgressBar(min = 0, max = length(fileList), style = 3)

      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress=progress)

      ro <- foreach(f = 1:length(fileList), .options.snow = opts, .packages = 'rapportools', .export = private$needFunc) %dopar% {
        data <- readRDS(file = fileList[f])
        Sys.sleep(0.01)
        for(i in 1:length(data)) {
          if(is.empty(data[[i]]))
            stop("ERROR: There is an empty value in the data frame.")
          else {
            dcmRDS <- DicomRDS$new(data[[i]])

            # Dirpath Settings
            # Temp code, if source code open, please modify...
            sp <- strsplit(as.character(data[[i]]$path[1]), '/')
            rDirPath <- head(unlist(sp), -1)
            rDirPath <- Reduce(pastePath, rDirPath)
            #rDirPath <- file.path(rDirPath)

            roID <- dcmRDS$createOccurrenceID()
            studyDatetime <- dcmRDS$getStudyDateTime()

            # Searching AcquisitionDateTime...
            duringTime <- ''
            for(k in length(data):i) {
              dcmRDSk <- DicomRDS$new(data[[k]])
              duringTime <- dcmRDSk$getDuringTime(studyDateTime = studyDatetime)
              if(!is.empty(duringTime)) break else duringTime <- NA
              dcmRDSk$finalize()
            }

            pID <- dcmRDS$getPatientID()
            if(is.na(pID)) pID <- dcmRDS$getDirectoryID()
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

        radiology_occurrence_ID <- as.bigint(roID, 4)
        radiology_occurrence_date <- as.Date(getDate(dcmRDS$getStudyDate()))
        radiology_occurrence_datetime <- as.POSIXct(studyDatetime)

        Person_ID <- as.bigint(pID, 4)
        Condition_occurrence_id <- as.integer(coID)
        Device_concept_id <- as.bigint(dcID, 4)

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
        Image_exposure_time <- as.float(x = duringTime, digits = 5)

        Radiology_dirpath <- rDirPath
        Visit_occurrence_id <- as.bigint(voID, 4)

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

    createRadiologyImage = function(data, validpixelonly = FALSE) {
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
          if(validpixelonly) {
            if(!dcmRDS$isPixelData())
              next
          }
          Person_ID[num] <- as.bigint(dcmRDS$getDirectoryID(), 4)

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
          Radiology_occurrence_ID[num] <- as.bigint(rID, 4)
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

    finalize = function() {
      parallel::stopCluster(cl = private$cl)  # Requirement
    }
  )
)

