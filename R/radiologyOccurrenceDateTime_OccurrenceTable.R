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
        studyDateDf<-x[[1]] %>% filter(name %in% c('StudyDate')) %>% select(value)
        colnames(studyDateDf)<-'studyDate'
        studyTimeDf<-x[[1]] %>% filter(name %in% c('StudyTime')) %>% select(value)
        colnames(studyTimeDf)<-'studyTime'
        studyDateTimeDf<-merge(studyDateDf, studyTimeDf, all=T)
        studyDateTimeDf<-studyDateTimeDf %>% mutate(studyDateTime=paste(studyDate,studyTime, sep = ''))
        studyDateTimeDf<-studyDateTimeDf %>% mutate(studyDateTime=ifelse(is.na(as.POSIXct(studyDateTime, format = '%Y%m%d%H%M%S',origin = "1970-01-01",tz ="UTC"))==F, as.character(as.POSIXct(studyDateTime, format = '%Y%m%d%H%M%S',origin = "1970-01-01",tz ="UTC")), as.character(as.POSIXct(date, format = '%Y%m%d',origin = "1970-01-01",tz ="UTC"))))
        studyDateTimeDf<-studyDateTimeDf[,c(3)]
        studyDateTimeDf<-data.frame(studyDateTimeDf)
        colnames(studyDateTimeDf)<-'studyDateTime'
        return(studyDateTimeDf)
    })
    radiologyOccurrenceDateTime<-mapply(function(x, y) merge(x, y, all = T), x = radiologyOccurrenceId(DICOMList), y = radiologyOccurrenceDateTime, SIMPLIFY = F)
    radiologyOccurrenceDateTime<-do.call(rbind, radiologyOccurrenceDateTime)
    radiologyOccurrenceDateTime<-as.data.frame(radiologyOccurrenceDateTime %>% group_by(radiologyOccurrenceId) %>% distinct(studyDateTime))
    return(radiologyOccurrenceDateTime)
}
