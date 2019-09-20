#' 'radiologyOccurrenceDateTime'
#'
#' radiologyOccurrenceDateTime function indicates date and time of each occurrences are taken
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A dataframe indicating date and time of each occurrences are taken
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' radiologyOccurrenceDateTime(DICOMList)
#' @export

#radiologyOccurrenceDateTime

radiologyOccurrenceDateTime<-function(DICOMList){
    radiologyOccurrenceDateTime<-lapply(DICOMList, function(x){
        studyDate<-as.character(x[[1]] %>% filter(name %in% c('StudyDate')) %>% select(value))
        studyTime<-as.character(x[[1]] %>% filter(name %in% c('StudyTime')) %>% select(value))
        if(studyDate=="character(0)" | studyDate==""){
            studyDate='NA'
        }
        if(studyTime=="character(0)" | studyTime==""){
            studyTime='NA'
        }
        studyDateTime<-paste(studyDate, studyTime, sep = '')
        studyDateTime<-gsub("NA", '', studyDateTime)
        studyDateTime<-ifelse(is.na(as.POSIXct(studyDateTime, format = '%Y%m%d%H%M%S',origin = "1970-01-01",tz ="UTC"))==F, as.character(as.POSIXct(studyDateTime, format = '%Y%m%d%H%M%S',origin = "1970-01-01",tz ="UTC")), ifelse(is.na(as.POSIXct(studyDateTime, format = '%Y%m%d',origin = "1970-01-01",tz ="UTC"))==F, as.character(as.POSIXct(studyDate, format = '%Y%m%d',origin = "1970-01-01",tz ="UTC")), 'NA'))
        return(studyDateTime)
    })
    radiologyOccurrenceDateTime<-do.call(rbind, radiologyOccurrenceDateTime)
    radiologyOccurrenceDateTime<-as.data.frame(radiologyOccurrenceDateTime)
    colnames(radiologyOccurrenceDateTime)<-'studyDateTime'
    radiologyOccurrenceDateTime<-cbind(radiologyOccurrenceId(DICOMList), radiologyOccurrenceDateTime)
    radiologyOccurrenceDateTime<-as.data.frame(radiologyOccurrenceDateTime %>% group_by(radiologyOccurrenceId) %>% distinct(studyDateTime))
    radiologyOccurrenceDateTime<-split(radiologyOccurrenceDateTime, radiologyOccurrenceDateTime$radiologyOccurrenceId)
    radiologyOccurrenceDateTime<-lapply(radiologyOccurrenceDateTime, function(x){
        as.data.frame(x[c(1),])
    })
    radiologyOccurrenceDateTime<-data.frame(do.call(rbind, radiologyOccurrenceDateTime), row.names = NULL)
    return(radiologyOccurrenceDateTime)
}
