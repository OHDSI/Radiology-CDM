#' 'conditionOccurrenceId'
#'
#' conditionOccurrenceId function indicates conditionOccurrenceId which needs to be joined with OMOP-CDM
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A dataframe indicating conditionOccurrenceId
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' conditionOccurrenceId(DICOMList)
#' @export

#conditionOccurrenceId
conditionOccurrenceId<-function(DICOMList){
    data.frame(conditionOccurrenceId=numeric(nrow(radiologyOccurrenceId2(DICOMList))))}
