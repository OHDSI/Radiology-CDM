#' 'radiologyProtocolConceptId'
#'
#' radiologyProtocolConceptId function indicates protocol&modality of each shoot
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A dataframe indicating path protocol&modality of each shoot
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' radiologyProtocolConceptId(DICOMList)
#' @export

##radiologyProtocolConceptId(BodyPartExamined, StudyDescription, radiologyPhaseConceptId, modality)

radiologyProtocolConceptId<-function(DICOMList){
    protocolName<-lapply(DICOMList, function(x){
        protocolName<-as.character(x[[1]] %>% dplyr::filter(name %in% c('ProtocolName')) %>% dplyr::select(value))
        if(protocolName=="character(0)" | protocolName==""){
            protocolName='NA'
        }
        protocolName<-as.data.frame(protocolName)
        colnames(protocolName)<-'protocolName'
        return(protocolName)})
    protocolName<-do.call(rbind,protocolName)
    information<-cbind(anatomicRegion(DICOMList), radiologyOccurrenceId(DICOMList), modality(DICOMList), radiologyPhaseConceptId(DICOMList), imageOrientationConceptId(DICOMList), protocolName)
    information<-unique(information)
    information<-split(information, information$radiologyOccurrenceId)
    information<-lapply(information, function(x){
        anatomicRegion<-data.frame(paste(unique(x$anatomicRegion), collapse=', '), row.names = NULL)
        colnames(anatomicRegion)<-'anatomicRegion'
        radiologyOccurrenceId<-data.frame(paste(unique(x$radiologyOccurrenceId), collapse=', '), row.names = NULL)
        colnames(radiologyOccurrenceId)<-'radiologyOccurrenceId'
        radiologyPhaseConceptId<-data.frame(paste(unique(x$radiologyPhaseConceptId), collapse=', '), row.names = NULL)
        colnames(radiologyPhaseConceptId)<-'radiologyPhaseConceptId'
        modality<-data.frame(paste(unique(x$modality), collapse=', '), row.names = NULL)
        colnames(modality)<-'modality'
        imageOrientationConceptId<-data.frame(paste(unique(x$imageOrientationConceptId), collapse=', '), row.names = NULL)
        colnames(imageOrientationConceptId)<-'imageOrientationConceptId'
        protocolName<-data.frame(paste(unique(x$protocolName), collapse=', '), row.names = NULL)
        colnames(protocolName)<-'protocolName'
        return(cbind(anatomicRegion, radiologyOccurrenceId, radiologyPhaseConceptId, modality, imageOrientationConceptId, protocolName))})
    radiologyProtocolConceptId<-sapply(information, function(x){
        if(grepl('chest', x$anatomicRegion)==T & grepl('DX', x$modality)==T & grepl('43591', x$imageOrientationConceptId)==T & grepl('43594', x$imageOrientationConceptId)==T){
            return('3031526')
        }
        else if(grepl('chest', x$anatomicRegion)==T & grepl('CR', x$modality)==T & grepl('43591', x$imageOrientationConceptId)==T & grepl('43594', x$imageOrientationConceptId)==T){
            return('3031526')
        }
        else if(grepl('chest', x$anatomicRegion)==T & grepl('DX', x$modality)==T & grepl('43594', x$imageOrientationConceptId)==T){
            return('3002676')
        }
        else if(grepl('chest', x$anatomicRegion)==T & grepl('CR', x$modality)==T & grepl('43594', x$imageOrientationConceptId)==T){
            return('3002676')
        }
        else if(grepl('head', x$anatomicRegion)==T & grepl('CT', x$modality)==T & grepl('5901', x$radiologyPhaseConceptId)==T){
            return('36305291')
        }
        else if(grepl('head', x$anatomicRegion)==T & grepl('CT', x$modality)==T & grepl('28694', x$radiologyPhaseConceptId)==T){
            return('3002086')
        }
        else if(grepl('head', x$anatomicRegion)==T & grepl('CT', x$modality)==T & grepl('28833', x$radiologyPhaseConceptId)==T){
            return('3025779')
        }
        else if(grepl('neck', x$anatomicRegion)==T & grepl('CT', x$modality)==T & grepl('28694', x$radiologyPhaseConceptId)==T & grepl('angio', tolower(x$protocolName))==T){
            return('36304600')
        }
        else if(grepl('abdomen', x$anatomicRegion)==T & grepl('CT', x$modality)==T & grepl('28833', x$radiologyPhaseConceptId)==T & grepl('11080', x$radiologyPhaseConceptId)==T & grepl('11085', x$radiologyPhaseConceptId)==T & grepl('11081', x$radiologyPhaseConceptId)==T){
            return('21492176')
        }
        else if(grepl('abdomen', x$anatomicRegion)==T & grepl('CT', x$modality)==T & grepl('28694', x$radiologyPhaseConceptId)==T){
            return('21492176')
        }
        else if(grepl('abdomen', x$anatomicRegion)==T & grepl('CR', x$modality)==T){
            return('3053099')
        }
        else if(grepl('abdomen', x$anatomicRegion)==T & grepl('DX', x$modality)==T){
            return('3053099')
        }
        else if(grepl('head', x$anatomicRegion)==T & grepl('MR', x$modality)==T){
            return('3023655')
        }
        else {
            return('others')
        }
    })
    radiologyProtocolConceptId<-data.frame(radiologyOccurrenceId=names(radiologyProtocolConceptId), radiologyProtocolConceptId=radiologyProtocolConceptId, row.names = NULL)
    return(radiologyProtocolConceptId)
}
