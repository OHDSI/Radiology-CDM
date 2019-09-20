#' 'radiologyOccurrenceTable'
#'
#' radiologyOccurrenceTable function will show you radiologyOccurrenceTable in a dataframe format
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return radiologyOccurrenceTable in a dataframe format
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' radiologyOccurrenceTable(DICOMList)
#' @export

radiologyOccurrenceTable<-function(DICOMList){
    radiologyOccurrenceTable<-merge(radiologyOccurrenceDateTime(DICOMList), device(DICOMList), by='radiologyOccurrenceId', all=T)
    radiologyOccurrenceTable<-merge(radiologyOccurrenceTable, radiologyProtocolConceptId(DICOMList), by='radiologyOccurrenceId', all=T)
    radiologyOccurrenceTable<-merge(radiologyOccurrenceTable, imageTotalCount(DICOMList), by='radiologyOccurrenceId', all=T)
    radiologyOccurrenceTable<-merge(radiologyOccurrenceTable, radiologyDirpath(DICOMList), by='radiologyOccurrenceId', all=T)
    elsetable<-cbind(anatomicRegion(DICOMList), personId(DICOMList), modality(DICOMList), radiologyOccurrenceId(DICOMList))
    radiologyOccurrenceTable<-merge(radiologyOccurrenceTable, unique(elsetable), by='radiologyOccurrenceId', all=T)
    return(radiologyOccurrenceTable)
}

