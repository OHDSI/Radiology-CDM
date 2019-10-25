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


imageId<-function(DICOMList){
    imageId<-lapply(DICOMList, function(x){
        imageId<-as.character(x[[1]] %>% dplyr::filter(name=='SOPInstanceUID') %>% dplyr::select(value))
        if(imageId=="character(0)" | imageId=="" | imageId=="integer(0)"){
            imageId='NA'
        }
        return(imageId)
    })
    imageId<-as.data.frame(do.call(rbind, imageId))
    colnames(imageId)<-'imageId'
    return(imageId)
}
