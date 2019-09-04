#' 'imageId'
#'
#' imageId function will read SOPInstanceUID metadata of all DICOM files read by DICOMHeaderList function
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import digest
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A list containing anonymized SOPInstanceUID of DICOM
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' imageId(DICOMList)
#' @export


imageId<-function(DICOMList){lapply(DICOMList, function(x){
    imageIdDf<-x[[1]] %>% dplyr::filter(name=='SOPInstanceUID') %>% dplyr::select(value)
    colnames(imageIdDf)<-'imageId'
    imageIdDf<-imageIdDf %>% dplyr::mutate(imageId=sapply(imageIdDf$imageId, digest::digest, algo='md5'))
    return(imageIdDf)
})}
