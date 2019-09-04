#' 'DicomPath'
#'
#' DicomPath function indicates path of each DICOM files
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A list indicating path of each DICOM files
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' lapply(DICOMList)
#' @export

DicomPath<-function(DICOMList){
    lapply(DICOMList, function(x){
        data.frame(Dicompath=names(x))
    })}
