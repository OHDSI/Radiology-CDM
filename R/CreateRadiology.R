################################ Function Zone #############################################

# Finish Schema,,
# but foriegn key is temporary dicom image metadata,,
createRadiologyOccurrence <- function(path) {
  fileList <- list.files(path = path, recursive = TRUE, full.names = TRUE)

  # Unique identifier of the Radiology Process
  radiology_occurrence_ID <- c()

  # Date of recording image data
  radiology_occurrence_date <- c(as.Date(NA))

  # Date and time of shooting of image data
  radiology_occurrence_datetime <- c(as.POSIXct(rep(NA)))

  # The anonymized patient ID (FK) defined in the CDM
  Person_ID <- c()

  # Condition_occurrence table의 FK
  Condition_occurrence_id <- c()

  # The ID (FK) of the image capturing device defined in the CDM
  Device_concept_id <- c()

  # image modality (CT, X-ray, MRI, PET-CT) 정보
  radiology_modality_concept_ID <- c()

  # Information about person position such as PA, AP, supine
  # http://dicom.nema.org/medical/dicom/current/output/html/figures/PS3.3_C.7.3.1.1.2-1.svg)
  # Patient ORientation of Dicom format
  # https://www.dabsoft.ch/dicom/3/C.7.6.1.1.1/)
  Person_orientation_concept_id <- c()

  # Image process (Brain Angio CT? Chest PA ? non-contrast brain CT?) 등image process에대한정보
  radiology_protocol_concept_id <- c()

  # Total number of images collected from the patient
  Image_total_count <- c()

  # 환자 촬영 부위
  Anatomic_site_concept_id <- c()

  # User-defined comments on images
  radiology_Comment <- c()

  # image dosage 의unit (eg CT, X-ray는KVp, MRI의경우Tesla)
  Image_dosage_unit_concept_id <- c()

  # Dosage (Peak Kilo Voltage Output of x-ray generator used / 1.5? 3T in case of MRI?)
  Dosage_value_as_number <- c()

  # Unit information for exposure time (eg msec, min)
  Image_exposure_time_unit_concept_id <- c()

  # X-ray, CT, MRI exposure time
  Image_exposure_time <- c()

  # Id for the "folder" path where the image data is stored (keep the file path by this ID separately)
  Radiology_dirpath <- c()

  # visit_occurrence_id (FK)
  Visit_occurrence_id <- c()

  # Explore FileList
  for(f in 1:length(fileList)) {
    data <- readRDS(file = fileList[f])

    for(i in 1:length(data)) {
      num <- 1
      if(!is.null(data[[i]])) {
        # Dirpath Settings
        sp <- strsplit(as.character(data[[i]]$path[i]), '/')
        headPath <- head(unlist(sp), -1)

        # sp <- strsplit(as.character(data[[i]]$path[length(data[[i]]$path)]), '/')
        shortPath <- tail(headPath, -4)
        rDirPath <- Reduce(pastePath, shortPath)

        # roID <- getTagValue(data[[i]], "StudyInstanceUID")
        roID <- Reduce(pasteSQL, c(getTagValue(data[[i]], "SeriesInstanceUID"), getTagValue(data[[i]], "StudyID")))
        if(num == 1) {
          studyDate <- getTagValue(data[[i]], "StudyDate")
          studyTime <- getTagValue(data[[i]], "StudyTime")
          studyDatetime <- getDateTime(studyDate, studyTime)

          duringTime <- 0

          # Searching AcquisitionDateTime...
          for(k in length(data):i) {
            tryCatch({
              if(isValidTag(data[[k]], "AcquisitionDate")) {
                acqDate <- getTagValue(data[[k]], "AcquisitionDate")
                acqTime <- getTagValue(data[[k]], "AcquisitionTime")
                acqDatetime <- getDateTime(acqDate, acqTime)
                duringTime <- getDuringTime(studyDatetime, acqDatetime)
                break
              }
            }, error = function(e) {})
          }
        }

        pID <- getTagValue(data[[i]], "PatientID")
        coID <- 0

        dcID <- getTagValue(data[[i]], "DeviceSerialNumber")
        modality <- getTagValue(data[[i]], "Modality")

        # PatientPosition is null .... NA
        pocID <- getTagValue(data[[i]], "PatientPosition")
        if(is.na(pocID))
          pocID <- getTagValue(data[[i]], "PatientOrientation")

        # Contrast Information,,
        # TRUE = C-T Brain Angio c Contrast
        # FALSE = C-T Brain Angio c non-Contrast
        rpcID <- "Pre"
        for(j in i:length(data)) {
          tryCatch({
            if(isValidTag(data[[j]], "ContrastBolusAgent")) {
              rpcID <- "Post"
              break
            } else if(isValidTag(data[[j]], "PhotometricInterpretation")) {
              colorVal <- getTagValue(data[[j]], "PhotometricInterpretation")
              if(pmatch(x = colorVal, "RGB", nomatch = FALSE) == 1)
                rpcID <- "Post"
              break
            }
          }, error = function(e) {})
        }

        tCount <- length(data)
        ascID <- 4119359

        # Image Comments
        imgComment <- getTagValue(data[[i]], "ImageComments")

        dosage <- if(equals(modality, "MRI")) "Tesla" else "kVp"
        dosageNum <- getTagValue(data[[i]], dosage)
        dvaNum <- if(is.na(dosageNum)) 0 else dosageNum
        timeUnit <- "sec"

        voID <- 0
        break
      }
    }

    radiology_occurrence_ID[f] <- roID

    radiology_occurrence_date[f] <- getDate(studyDate)
    radiology_occurrence_datetime[f] <- studyDatetime

    Person_ID[f] <- pID
    Condition_occurrence_id[f] <- coID
    Device_concept_id[f] <- dcID

    radiology_modality_concept_ID[f] <- modality
    Person_orientation_concept_id[f] <- pocID
    radiology_protocol_concept_id[f] <- rpcID

    Image_total_count[f] <- tCount
    Anatomic_site_concept_id[f] <- ascID
    radiology_Comment[f] <- imgComment

    Image_dosage_unit_concept_id[f] <- dosage
    Dosage_value_as_number[f] <- dvaNum
    Image_exposure_time_unit_concept_id[f] <- timeUnit

    Image_exposure_time[f] <- duringTime
    Radiology_dirpath[f] <- rDirPath
    Visit_occurrence_id[f] <- voID
  }

  # Final Data Model
  Radiology_occurrence <- data.frame(
    radiology_occurrence_ID,
    radiology_occurrence_date,
    radiology_occurrence_datetime,
    Person_ID,
    Condition_occurrence_id,
    Device_concept_id,
    radiology_modality_concept_ID,
    Person_orientation_concept_id,
    radiology_protocol_concept_id,
    Image_total_count,
    Anatomic_site_concept_id,
    radiology_Comment,
    Image_dosage_unit_concept_id,
    Dosage_value_as_number,
    Image_exposure_time_unit_concept_id,
    Image_exposure_time,
    Radiology_dirpath,
    Visit_occurrence_id
  )

  return(Radiology_occurrence)
}

createRadiologyImage <- function(data) {
  Source_ID <- c()
  Radiology_occurrence_ID <- c()
  Person_ID <- c()
  Person_orientation_concept_id <- c()
  Image_type <- c()
  radiology_phase_concept_id <- c()
  Image_no <- c()
  Phase_total_no <- c()
  image_resolution_Rows <- c()
  image_Resolution_Columns <- c()
  Image_Window_Level_Center <- c()
  Image_Window_Level_Width <- c()
  Image_slice_thickness <- c()
  image_filepath <- c()

  num <- 1
  rID <- ""

  for(i in 1:length(data)) {
    if(!is.null(data[[i]])) {
      if(num == 1) rID <- Reduce(pasteSQL, c(getTagValue(data[[i]], "SeriesInstanceUID"), getTagValue(data[[i]], "StudyID")))

      Source_ID[num] <- getTagValue(data[[i]], "SOPInstanceUID")
      Radiology_occurrence_ID[num] <- rID
      Person_ID[num] <- getTagValue(data[[i]], "PatientID")

      # PatientPosition is null .... NA
      pocID <- getTagValue(data[[i]], "PatientPosition")
      if(is.na(pocID))
        pocID <- getTagValue(data[[i]], "PatientOrientation")
      Person_orientation_concept_id[num] <- pocID

      # Get ImageType, ORIGINAL is PRIMARY, DERIVED is SECONDARY
      exType <- getTagValue(data[[i]], "ImageType")
      if(is.na(exType))
        imType <- exType
      else if(str_detect(string = exType, pattern = "ORIGINAL"))
        imType <- "PRIMARY"
      else if(is.boolean(str_detect(exType, "DERIVED")))
        imType <- "SECONDARY"
      else
        imType <- exType

      # Contrast Information,,
      # TRUE = C-T Brain Angio c Contrast. Additional, Color format is RGB that 3D IMAGE FORMAT..
      # FALSE = C-T Brain Angio c non-Contrast
      modality <- getTagValue(data[[i]], "Modality")
      rpcID <- "Pre"
      tryCatch({
        if(isValidTag(data = data[[i]], name = "ContrastBolusAgent"))
          rpcID <- "Post"
        else if(isValidTag(data[[i]], "PhotometricInterpretation")) {
          colorVal <- getTagValue(data[[i]], "PhotometricInterpretation")
          if(pmatch(x = colorVal, "RGB", nomatch = FALSE) == 1)
            rpcID <- "Post"
        }
      }, error = function(e) {})
      radiology_phase_concept_id[num] <- rpcID

      thickness <- getTagValue(data[[i]], "SliceThickness")

      Image_no[num] <- num
      Phase_total_no[num] <- length(data)

      rows <- getTagValue(data[[i]], "Rows")
      if(is.na(rows)) rows <- 0

      columns <- getTagValue(data[[i]], "Columns")
      if(is.na(columns)) columns <- 0

      image_resolution_Rows[num] <- rows
      image_Resolution_Columns[num] <- columns
      Image_Window_Level_Center[num] <- getTagValue(data[[i]], "WindowCenter")
      Image_Window_Level_Width[num] <- getTagValue(data[[i]], "WindowWidth")
      Image_slice_thickness[num] <- if(is.na(thickness) || num == 1) 0 else thickness
      Image_type[num] <- imType

      # Dirpath Settings
      sp <- strsplit(as.character(data[[i]]$path[length(data[[i]]$path)]), '/')
      shortPath <- tail(x = unlist(sp), -4)
      filePath <- Reduce(pastePath, shortPath)

      image_filepath[num] <- filePath
      num <- num + 1
    }
  }

  Radiology_Image <- data.frame(
    Source_ID,
    Radiology_occurrence_ID,
    Person_ID,
    Person_orientation_concept_id,
    Image_type,
    radiology_phase_concept_id,
    Image_no,
    Phase_total_no,
    image_resolution_Rows,
    image_Resolution_Columns,
    Image_Window_Level_Center,
    Image_Window_Level_Width,
    Image_slice_thickness,
    image_filepath
  )

  return(Radiology_Image)
}

# Finish Schema,,
createDeviceSchema(data) %as% {
  Device_ID <- c()
  Device_Modality <- c()
  Device_Name <- c()
  Device_Company_Name <- c()

  num <- 1

  for(i in 1:length(data)) {
    if(!is.null(data[[i]])) {
      Device_ID[num] <- getTagValue(data[[i]], "DeviceSerialNumber")
      Device_Modality[num] <- getTagValue(data[[i]], "Modality")
      Device_Name[num] <- getTagValue(data[[i]], "ManufacturersModelName")
      Device_Company_Name[num] <- getTagValue(data[[i]], "Manufacturer")

      num <- num + 1
    }
  }

  DeviceSchema <- data.frame(
    Device_ID,
    Device_Modality,
    Device_Name,
    Device_Company_Name
  )
  return(DeviceSchema)
}
