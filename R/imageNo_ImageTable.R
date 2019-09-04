#' 'imageNo'
#'
#' imageNo function will indicate order of DICOM images in specific radiologyOccurrenceId, and radiologyPhaseConceptId
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A list indicating order of DICOM images in specific radiologyOccurrenceId, and radiologyPhaseConceptId
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' imageNo(DICOMList)
#' @export

imageNo<-function(DICOMList){
    imageNo<-do.call(rbind,mapply(FUN = cbind,imageId(DICOMList),radiologyOccurrenceId(DICOMList), radiologyPhaseConceptId(DICOMList), SIMPLIFY = FALSE))
    imageNo<-imageNo%>%dplyr::group_by(radiologyOccurrenceId, radiologyPhaseConceptId)%>%dplyr::mutate(imageNo=row_number())
    imageNo<-imageNo%>%dplyr::group_by(radiologyOccurrenceId, radiologyPhaseConceptId)%>%dplyr::mutate(phaseTotalNo=n())
    return(imageNo)
}
