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
        modalityDf<-x[[1]] %>% filter(name=='Modality') %>% select(value)
        colnames(modalityDf)<-'modality'
        return(modalityDf)})
    modality<-mapply(function(x, y) merge(x, y, all = T), x = radiologyOccurrenceId(DICOMList), y = modality, SIMPLIFY = F)
    modality<-do.call(rbind, modality)
    modality<-as.data.frame(modality %>% group_by(radiologyOccurrenceId) %>% distinct(modality))
    return(modality)
}
