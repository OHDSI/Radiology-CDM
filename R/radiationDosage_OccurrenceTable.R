#' 'radiationDosage'
#'
#' radiationDosage function indicates intensity of radiation dosage exposed by each shoot
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A dataframe indicating exposed dosage of radiation by each shoot
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' radiationDosage(DICOMList)
#' @export

#radiationDosage
radiationDosage<-function(DICOMList){
    radiationDosage<-lapply(DICOMList, function(x){
        radiationDosage<-x[[1]] %>% filter(name=='kVp') %>% select(value)
        colnames(radiationDosage)<-'radiationDosage'
        return(radiationDosage)
    })
    radiationDosage<-mapply(function(x, y) merge(x, y, all = T), x = radiologyOccurrenceId(DICOMList), y = radiationDosage, SIMPLIFY = F)
    radiationDosage<-do.call(rbind, radiationDosage)
    radiationDosage<-as.data.frame(radiationDosage %>% group_by(radiologyOccurrenceId) %>% distinct(radiationDosage))
    radiationDosage<-radiationDosage %>% mutate(radiationDosage=paste(radiationDosage, 'kVp', sep=' '))
    return(radiationDosage)
}
