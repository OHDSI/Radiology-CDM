#' 'modalityImage'
#'
#' modalityImage function indicates modality of each image
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A dataframe indicating modality of each image
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' modalityImage(DICOMList)
#' @export

#modalityImage
modalityImage<-function(DICOMList){
    modalityImage<-lapply(DICOMList, function(x){
        modalityImage<-as.character(x[[1]] %>% filter(name=='Modality') %>% select(value))
        if(modalityImage=="CR" | modalityImage=="DX"){
            modalityImage='XR'
        }
        else if(modalityImage=="character(0)" | modalityImage=="" | modalityImage=="integer(0)"){
            modalityImage='NA'
        }
        return(modalityImage)})
    modalityImage<-as.data.frame(do.call(rbind, modalityImage))
    colnames(modalityImage)<-'modality'
    return(modalityImage)
}
