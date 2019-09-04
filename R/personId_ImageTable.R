#' 'personId'
#'
#' personId function will read PatientID metadata of all DICOM files read by DICOMHeaderList function
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import digest
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A list containing anonymized PatientID of DICOM
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' personId(DICOMList)
#' @export

personId<-function(DICOMList){lapply(DICOMList, function(x){
    personIdDf<-x[[1]] %>% dplyr::filter(name=='PatientID') %>% dplyr::select(value)
    colnames(personIdDf)<-'personId'
    return(personIdDf)
})}
