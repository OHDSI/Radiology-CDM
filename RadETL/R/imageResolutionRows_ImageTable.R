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

imageResolutionRows<-function(DICOMList){
    imageResolutionRows<-lapply(DICOMList, function(x){
        imageResolutionRows<-as.character(x[[1]] %>% dplyr::filter(name=='Rows') %>% dplyr::select(value))
        if(imageResolutionRows=="character(0)" | imageResolutionRows=="" | imageResolutionRows=="integer(0)"){
            imageResolutionRows='NA'
        }
        return(imageResolutionRows)
    })
    imageResolutionRows<-as.data.frame(do.call(rbind, imageResolutionRows))
    colnames(imageResolutionRows)<-'imageResolutionRows'
    return(imageResolutionRows)
}

