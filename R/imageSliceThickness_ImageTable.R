#' 'imageSliceThickness'
#'
#' imageSliceThickness function indicates slice thickness(distance) between each images taken by CT
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A list indicating slice thickness(distance) between each images taken by CT
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' imageSliceThickness(DICOMList)
#' @export

imageSliceThickness<-function(DICOMList){
    imageSliceThickness<-lapply(DICOMList, function(x){
        imageSliceThickness<-as.character(x[[1]] %>% dplyr::filter(name =='SliceThickness') %>% dplyr::select(value))
        if(imageSliceThickness=="character(0)" | imageSliceThickness=="" | imageSliceThickness=="integer(0)"){
            imageSliceThickness='NA'
        }
        return(imageSliceThickness)})
    imageSliceThickness<-as.data.frame(do.call(rbind, imageSliceThickness))
    colnames(imageSliceThickness)<-'imageSliceThickness'
    return(imageSliceThickness)
}
