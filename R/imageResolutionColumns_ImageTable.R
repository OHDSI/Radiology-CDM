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

imageResolutionColumns<-function(DICOMList){
    imageResolutionColumns<-lapply(DICOMList, function(x){
        imageResolutionColumns<-as.character(x[[1]] %>% dplyr::filter(name=='Columns') %>% dplyr::select(value))
        if(imageResolutionColumns=="character(0)" | imageResolutionColumns=="" | imageResolutionColumns=="integer(0)"){
            imageResolutionColumns='NA'
        }
        return(imageResolutionColumns)
    })
    imageResolutionColumns<-as.data.frame(do.call(rbind, imageResolutionColumns))
    colnames(imageResolutionColumns)<-'imageResolutionColumns'
    return(imageResolutionColumns)
}
