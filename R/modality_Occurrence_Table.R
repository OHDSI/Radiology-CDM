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
        modality<-as.character(x[[1]] %>% filter(name=='Modality') %>% select(value))
        if(modality=="CR" | modality=="DX"){
            modality='XR'
        }
        else if(modality=="character(0)" | modality=="" | modality=="integer(0)"){
            modality='NA'
        }
        return(modality)})
    modality<-as.data.frame(do.call(rbind, modality))
    colnames(modality)<-'modality'
    modality<-cbind(modality, radiologyOccurrenceId(DICOMList))
    modality<-as.data.frame(modality %>% group_by(radiologyOccurrenceId) %>% distinct(modality))
    modality<-split(modality, modality$radiologyOccurrenceId)
    modality<-sapply(modality, function(x){
        paste0(x$modality, collapse=', ')})
    modality<-data.frame(radiologyOccurrenceId=names(modality), modality=modality, row.names = NULL)
    return(modality)
}
