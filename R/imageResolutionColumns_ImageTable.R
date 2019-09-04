#' 'imageResolutionColumns'
#'
#' imageResolutionColumns function will resolution(especially columns) of each images
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A list containing resolution(especially columns) of each DICOM images
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' imageResolutionColumns(DICOMList)
#' @export

imageResolutionColumns<-function(DICOMList){lapply(DICOMList, function(x){
    imageResolutionColumnsDf<-x[[1]] %>% dplyr::filter(name=='Columns') %>% dplyr::select(value)
    colnames(imageResolutionColumnsDf)<-'imageResolutionColumns'
    return(imageResolutionColumnsDf)
})}
