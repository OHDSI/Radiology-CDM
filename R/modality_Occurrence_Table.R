#' 'modality'
#'
#' modality function indicates modality of each shoot
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A dataframe indicating modality of each shoot
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' modality(DICOMList)
#' @export

#modality
modality<-function(DICOMList){
    modality<-lapply(DICOMList, function(x){
        modality<-as.character(x[[1]] %>% filter(name=='Modality') %>% select(value))})
    modality<-as.data.frame(do.call(rbind, modality))
    colnames(modality)<-'modality'
    return(modality)
}
