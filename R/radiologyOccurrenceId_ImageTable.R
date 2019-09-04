#' 'radiologyOccurrenceId'
#'
#' radiologyOccurrenceId function will read StudyInstanceUID metadata of all DICOM files read by DICOMHeaderList function
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import digest
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A list containing anonymized StudyInstanceUID of DICOM
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' radiologyOccurrenceId(DICOMList)
#' @export

radiologyOccurrenceId<-function(DICOMList){lapply(DICOMList, function(x){
    Radiology_Occurrence_ID_DF<-x[[1]] %>% dplyr::filter(name=='StudyInstanceUID') %>% dplyr::select(value)
    colnames(Radiology_Occurrence_ID_DF)<-'radiologyOccurrenceId'
    Radiology_Occurrence_ID_DF<-Radiology_Occurrence_ID_DF %>% dplyr::mutate(radiologyOccurrenceId=sapply(Radiology_Occurrence_ID_DF$radiologyOccurrenceId, digest, algo='md5'))
    return(Radiology_Occurrence_ID_DF)
})}
