################################ Function Zone #############################################

# Finish Schema,,
# but foriegn key is temporary dicom image metadata,,
createRadiologyOccurrence <- function(path) {
  fileList <- list.files(path = path, recursive = TRUE, full.names = TRUE)

  # radiology process 의 고유식별자(기본키)
  radiology_occurrence_ID <- c()

  # 이미지데이터의촬영날짜
  radiology_occurrence_date <- c(as.Date(NA))

  # 이미지데이터의촬영날짜및시각
  radiology_occurrence_datetime <- c(as.POSIXct(rep(NA)))

  # CDM내에정의된익명화된환자의ID(FK)
  Person_ID <- c()

  # Condition_occurrence table의 FK
  Condition_occurrence_id <- c()

  # CDM 내에서정의된이미지촬영장치의ID(FK)
  Device_concept_id <- c()

  # image modality (CT, X-ray, MRI, PET-CT) 정보
  radiology_modality_concept_ID <- c()

  # PA, AP, supine 등 person position 에 대한 정보
  # http://dicom.nema.org/medical/dicom/current/output/html/figures/PS3.3_C.7.3.1.1.2-1.svg)
  # DICOM file 의 patient orientation으로 부터
  # https://www.dabsoft.ch/dicom/3/C.7.6.1.1.1/)
  Person_orientation_concept_id <- c()

  # Image process (Brain Angio CT? Chest PA ? non-contrast brain CT?) 등image process에대한정보
  radiology_protocol_concept_id <- c()

  # 환자에게서수집되어진이미지수 전체
  Image_total_count <- c()

  # 환자 촬영 부위
  Anatomic_site_concept_id <- c()

  # 이미지에대한사용자정의주석
  radiology_Comment <- c()

  # image dosage 의unit (eg CT, X-ray는KVp, MRI의경우Tesla)
  Image_dosage_unit_concept_id <- c()

  # 도즈량(사용된x선발생기의Peak Kilo전압출력량 / MRI의 경우 1.5? 3T?)
  Dosage_value_as_number <- c()

  # 노출시간(eg msec, min)에대한단위정보
  Image_exposure_time_unit_concept_id <- c()

  # X 선, CT, MRI노출시간
  Image_exposure_time <- c()

  # 해당이미지데이터가저장된'폴더'경로에대한id (이ID별로의파일경로는따로보관)
  Radiology_dirpath <- c()

  # visit_occurrence_id (FK)
  Visit_occurrence_id <- c()

  # Explore FileList
  for(f in 1:length(fileList)) {
    data <- readRDS(file = fileList[f])

    for(i in 1:length(data)) {
      num <- 1
      if(!is.null(data[[i]])) {
        dcmRDS <- DicomRDS$new(data[[i]])

        # Dirpath Settings
        sp <- strsplit(as.character(data[[i]]$path[i]), '/')
        headPath <- head(unlist(sp), -1)

        # sp <- strsplit(as.character(data[[i]]$path[length(data[[i]]$path)]), '/')
        shortPath <- tail(headPath, -4)
        rDirPath <- Reduce(pastePath, shortPath)

        roID <- dcmRDS$createOccurrenceID()
        if(num == 1) {
          studyDatetime <- dcmRDS$getStudyDateTime()

          # Searching AcquisitionDateTime...
          duringTime <- ''
          for(k in length(data):i) {
            dcmRDSk <- DicomRDS$new(data[[k]])
            duringTime <- dcmRDSk$getDuringTime(studyDateTime = studyDatetime)
            if(!is.empty(duringTime)) break else duringTime <- ''
            dcmRDSk$finalize()
          }
        }

        pID <- dcmRDS$getPatientID()
        coID <- 0
        dcID <- dcmRDS$getDeviceID()
        modality <- dcmRDS$getModality()
        pocID <- dcmRDS$getOrientation()

        # Contrast Information,,
        # TRUE = Post image
        # FALSE = Pre image
        rpcID <- "Pre"
        for(j in i:length(data)) {
          dcmRDSj <- DicomRDS$new(data[[j]])
          if(dcmRDSj$isPost4BrainCT()) {
            rpcID <- "Post"
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

    radiology_occurrence_ID[f] <- roID
    radiology_occurrence_date[f] <- getDate(dcmRDS$getStudyDate())
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
    Dosage_value_as_number[f] <- dosageNum
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
    Visit_occurrence_id,
    stringsAsFactors = FALSE
  )
  return(Radiology_occurrence)
}

createRadiologyImage <- function(data) {
  # Only One RDS File
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
  pNum <- 1
  rID <- ''
  reNum <- 1

  # Current imageType, radiology_phase_concept_id
  curimType <- ''
  curPCID <- ''

  for(i in 1:length(data)) {
    if(!is.null(data[[i]])) {
      dcmRDS <- DicomRDS$new(data[[i]])

      Source_ID[num] <- dcmRDS$getSourceID()
      Person_ID[num] <- dcmRDS$getPatientID()

      # PatientPosition is null .... blank
      pocID <- dcmRDS$getOrientation()

      # Get ImageType, ORIGINAL is PRIMARY, DERIVED is SECONDARY
      imType <- dcmRDS$getImageType()
      modality <- dcmRDS$getModality()

      # Contrast Information,,
      # TRUE = Post. Additional, Color format is RGB that 3D IMAGE FORMAT..
      # FALSE = Pre
      rpcID <- "Pre"
      if(dcmRDS$isPost4BrainCT()) {
        if(pmatch(x = imType, "SECONDARY", nomatch = FALSE) == 1) rpcID <- "DERIVED"
        else rpcID <- "Post"
      }

      radiology_phase_concept_id[num] <- rpcID
      thickness <- dcmRDS$getThickness()
      Phase_total_no[num] <- 0

      rows <- dcmRDS$getImgRows()
      columns <- dcmRDS$getImgCols()

      # Checking Phase number..
      if(num == 1) {
        rID <- dcmRDS$createOccurrenceID()
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

      Image_no[num] <- pNum
      Radiology_occurrence_ID[num] <- rID
      image_resolution_Rows[num] <- rows
      image_Resolution_Columns[num] <- columns
      Image_Window_Level_Center[num] <- dcmRDS$getWindowCenter()
      Image_Window_Level_Width[num] <- dcmRDS$getWindowWidth()
      Image_slice_thickness[num] <- if(is.empty(thickness) || num == 1) '' else thickness
      Image_type[num] <- imType
      Person_orientation_concept_id[num] <- pocID

      # Dirpath Settings
      sp <- strsplit(as.character(data[[i]]$path[length(data[[i]]$path)]), '/')
      shortPath <- tail(x = unlist(sp), -4)
      filePath <- Reduce(pastePath, shortPath)

      image_filepath[num] <- filePath
      num <- num + 1
      pNum <- pNum + 1
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
