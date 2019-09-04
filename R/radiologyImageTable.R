#' 'radiologyImageTable'
#'
#' radiologyImageTable function shows you radiologyImageTable in a dataframe format
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#'
#'
#' @return radiologyImageTable in a dataframe format
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' radiologyImageTable(DICOMList)
#' @export
#'
radiologyImageTable<-function(DICOMList){
    radiologyImageTable<-do.call(rbind,mapply(FUN = cbind,imageId(DICOMList),radiologyOccurrenceId(DICOMList), personId(DICOMList), imageOrientationConceptId(DICOMList), imageType(DICOMList), radiologyPhaseConceptId(DICOMList), imageResolutionRows(DICOMList), imageResolutionColumns(DICOMList), imageSliceThickness(DICOMList), DicomPath(DICOMList), SIMPLIFY = FALSE))}
