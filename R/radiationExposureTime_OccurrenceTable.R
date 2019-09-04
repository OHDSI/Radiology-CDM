#' 'radiationExposureTime'
#'
#' radiationExposureTime function indicates duration of radiation exposed by each shoot
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A list indicating duration of radiation exposed by each shoot
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' radiationExposureTime(DICOMList)
#' @export

#radiationExposureTime (Priority on Localizer in AUSOM)
radiationExposureTime<-function(DICOMList){
    radiationExposureTime<-lapply(DICOMList, function(x){
        radiationExposureTime<-x[[1]] %>% filter(name=='ExposureTime') %>% select(value)
        colnames(radiationExposureTime)<-'radiationExposureTime'
        return(radiationExposureTime)
    })
    radiationExposureTime<-mapply(function(x, y) merge(x, y, all = T), x = imageType(DICOMList), y = radiationExposureTime, SIMPLIFY = F)
    radiationExposureTime<-mapply(function(x, y) merge(x, y, all = T), x = radiologyOccurrenceId(DICOMList), y = radiationExposureTime, SIMPLIFY = F)
    radiationExposureTime<-do.call(rbind, radiationExposureTime)
    radiationExposureTime<-as.data.frame(radiationExposureTime %>% group_by(radiologyOccurrenceId, imageType) %>% distinct(radiationExposureTime))
    radiationExposureTime<-radiationExposureTime %>% mutate(radiationExposureTime=paste(radiationExposureTime, 'sec', sep=' '))
    radiationExposureTime<-split(radiationExposureTime, radiationExposureTime$radiologyOccurrenceId)
    radiationExposureTime<-lapply(radiationExposureTime, function(x){
        if (grepl('Localizer', x$imageType)==T) {
            radiationExposureTime<-x %>% filter(imageType=='Localizer')
        } else {radiationExposureTime<-x %>% filter(imageType=='PRIMARY')}
        return(radiationExposureTime)
    })
    radiationExposureTime<-do.call(rbind, radiationExposureTime)
    rownames(radiationExposureTime)<-NULL
    return(radiationExposureTime[,c(1,2)])
}
