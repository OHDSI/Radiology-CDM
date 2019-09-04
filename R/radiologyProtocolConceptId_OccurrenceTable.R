#' 'radiologyProtocolConceptId'
#'
#' radiologyProtocolConceptId function indicates protocol of each shoot
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A dataframe indicating path protocol of each shoot
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' radiologyProtocolConceptId(DICOMList)
#' @export

##radiologyProtocolConceptId(BodyPartExamined, StudyDescription, radiologyPhaseConceptId, modality)
radiologyProtocolConceptId<-function(DICOMList){
    anatomicSite<-lapply(DICOMList, function(x){
        anatomicSite<-x[[1]]%>%filter(name %in% c('BodyPartExamined', 'StudyDescription')) %>% select(name, value)
        anatomicSite<-anatomicSite %>% filter(value!='')
        anatomicSite<-ifelse(any(anatomicSite$name=='BodyPartExamined')==T, anatomicSite %>% filter(name=='BodyPartExamined') %>% select(value), anatomicSite %>% filter(name=='StudyDescription') %>% select(value))
        anatomicSite<-as.data.frame(anatomicSite)
        colnames(anatomicSite)<-'anatomicSite'
        anatomicSite$anatomicSite<-ifelse(tolower(anatomicSite$anatomicSite)=='head', 'head', ifelse(tolower(anatomicSite$anatomicSite)=='neck', 'neck', anatomicSite$anatomicSite))
        return(anatomicSite)
    })
    radiologyProtocolConceptId<-mapply(function(x, y) merge(x, y, by=0, all = T), x = anatomicSite, y = radiologyOccurrenceId(DICOMList), SIMPLIFY = F)
    radiologyProtocolConceptId<-mapply(function(x, y) merge(x, y, all = T), x = radiologyProtocolConceptId, y = radiologyPhaseConceptId(DICOMList), SIMPLIFY = F)
    radiologyProtocolConceptId<-do.call(rbind, radiologyProtocolConceptId)
    radiologyProtocolConceptId<-radiologyProtocolConceptId[,c(2:4)]
    modalityElement<-modality(DICOMList)
    modalityElement<-split(modalityElement, modalityElement$radiologyOccurrenceId)
    radiologyProtocolConceptId<-split(radiologyProtocolConceptId, radiologyProtocolConceptId$radiologyOccurrenceId)
    modality<-lapply(modalityElement, function(x){
        as.character(x$modality)
    })

    anatomicSite<-lapply(radiologyProtocolConceptId, function(x){
        anatomicSite<-unique(x$anatomicSite)
        anatomicSite<-paste0(anatomicSite, collapse=', ')
    })

    radiologyPhaseConceptId<-lapply(radiologyProtocolConceptId, function(x){
        radiologyPhaseConceptId<-paste(unique(x$radiologyPhaseConceptId), sep=',')
        paste0(radiologyPhaseConceptId, collapse=', ')
    })

    radiologyProtocolConceptId<-mapply(FUN = c, modality, anatomicSite, radiologyPhaseConceptId, SIMPLIFY = F)
    radiologyProtocolConceptId<-lapply(radiologyProtocolConceptId, function(x){
        paste0(x, collapse=', ')
    })
    radiologyProtocolConceptId<-lapply(radiologyProtocolConceptId, function(x){
        if (grepl('CT', x)==T & grepl('head', x)==T & grepl('5901', x)==T) {return('3002086')}
        else if (grepl('CT', x)==T & grepl('head', x)==T & grepl('28694', x)==T) {return('3002086')}
        else if (grepl('CT', x)==T & grepl('head', x)==T & grepl('28833', x)==T) {return('3025779')}
        else if(grepl('CT', x)==T & grepl('neck', x)==T & grepl('28833', x)==T) {return('3050247')}
        else {return('needMapping')}
    }
    )
    data.frame(radiologyOccurrenceId=names(radiologyProtocolConceptId), radiologyProtocolConceptId=as.data.frame(t(do.call(cbind, radiologyProtocolConceptId)))$V1, row.names = NULL)
}
