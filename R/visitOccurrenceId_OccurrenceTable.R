#' 'visitOccurrenceId'
#'
#' visitOccurrenceId function indicates visitOccurrenceId which needs to be joined with OMOP-CDM
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A list indicating visitOccurrenceId
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' visitOccurrenceId(DICOMList)
#' @export

#visitOccurrenceId
visitOccurrenceId<-function(DICOMList){
    data.frame(visitOccurrenceId=numeric(nrow(radiologyOccurrenceId2(DICOMList))))}
