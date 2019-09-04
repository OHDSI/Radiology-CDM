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
        imageSliceThicknessDf<-x[[1]] %>% dplyr::filter(name =='SliceThickness') %>% dplyr::select(value)
        modalityDf<-x[[1]] %>% dplyr::filter(name =='Modality') %>% dplyr::select(value)
        imageSliceThicknessDf<-rbind(modalityDf,imageSliceThicknessDf)
        imageSliceThicknessDf<-if(grepl('CT', imageSliceThicknessDf[1,])==F){
            return(NA)
        } else {
            return(imageSliceThicknessDf[2,])
        }
        return(imageSliceThicknessDf)
    })
    imageSliceThicknessDf<-lapply(imageSliceThickness, function(x){
        imageSliceThicknessDf<-as.data.frame(x)
        colnames(imageSliceThicknessDf)<-'imageSliceThickness'
        return(imageSliceThicknessDf)
    })
    return(imageSliceThicknessDf)}
