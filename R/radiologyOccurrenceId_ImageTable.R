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

radiologyOccurrenceId<-function(DICOMList){
    radiologyOccurrenceId<-lapply(DICOMList, function(x){
        radiologyOccurrenceId<-as.character(data.frame(x[[1]] %>% dplyr::filter(name=='StudyInstanceUID') %>% dplyr::select(value))[c(1),])
        if(radiologyOccurrenceId=="character(0)" | radiologyOccurrenceId=="" | radiologyOccurrenceId=="integer(0)" | is.na(radiologyOccurrenceId)==T){
            radiologyOccurrenceId='NA'
        }
        return(radiologyOccurrenceId)})
    radiologyOccurrenceId<-as.data.frame(do.call(rbind, radiologyOccurrenceId))
    colnames(radiologyOccurrenceId)<-'radiologyOccurrenceId'
    return(radiologyOccurrenceId)}
