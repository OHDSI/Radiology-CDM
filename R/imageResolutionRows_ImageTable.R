#' 'imageResolutionRows'
#'
#' imageResolutionRows function will resolution(especially rows) of each images
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A list containing resolution(especially rows) of each DICOM images
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' imageResolutionRows(DICOMList)
#' @export

imageResolutionRows<-function(DICOMList){lapply(DICOMList, function(x){
    imageResolutionRowsDf<-x[[1]] %>% dplyr::filter(name=='Rows') %>% dplyr::select(value)
    colnames(imageResolutionRowsDf)<-'imageResolutionRows'
    return(imageResolutionRowsDf)
})}
