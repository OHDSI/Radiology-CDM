#' 'radiologyPhaseConceptId'
#'
#' radiologyPhaseConceptId function will represent phase of each image
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A list containing phase of each DICOM image
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' radiologyPhaseConceptId(DICOMList)
#' @export

radiologyPhaseConceptId<-function(DICOMList){
    contastBolusAgent<-lapply(DICOMList, function(x){
        contastBolusAgent<-as.character(x[[1]] %>% dplyr::filter(name %in% c('ContrastBolusAgent')) %>% dplyr::select(value))
        if(contastBolusAgent=="character(0)" | contastBolusAgent==""){
            contastBolusAgent='NA'
        } else {
            contastBolusAgent='haveValue'
        }
        contastBolusAgent<-as.data.frame(contastBolusAgent)
        colnames(contastBolusAgent)<-'contastBolusAgent'
        return(contastBolusAgent)})
    contastBolusAgent<-do.call(rbind,contastBolusAgent)

    seriesDescription<-lapply(DICOMList, function(x){
        seriesDescription<-as.character(x[[1]] %>% dplyr::filter(name %in% c('SeriesDescription')) %>% dplyr::select(value))
        if(seriesDescription=="character(0)" | seriesDescription==""){
            seriesDescription='NA'
        }
        seriesDescription<-as.data.frame(seriesDescription)
        colnames(seriesDescription)<-'seriesDescription'
        return(seriesDescription)})
    seriesDescription<-do.call(rbind,seriesDescription)

    information<-cbind(modality(DICOMList), imageType(DICOMList), contastBolusAgent, seriesDescription)
    information<-split(information, seq(nrow(information)))
    radiologyPhaseConceptId<-sapply(information, function(x){
        if(grepl('CT', x$modality)==F & grepl('MR', x$modality)==F){
            return('NA')
        }
        else if(grepl('Localizer', x$imageType)==T){
            return('28664')
        }
        else if(grepl('SECONDARY', x$imageType)==T){
            return('5901')
        }
        else if(grepl('PRIMARY', x$imageType)==T & grepl('pre', tolower(x$seriesDescription))==T){
            return('28833')
        }
        else if(grepl('PRIMARY', x$imageType)==T & grepl('post', tolower(x$seriesDescription))==T){
            return('28694')
        }
        else if(grepl('PRIMARY', x$imageType)==T & grepl('art', tolower(x$seriesDescription))==T){
            return('11080')
        }
        else if(grepl('PRIMARY', x$imageType)==T & grepl('por', tolower(x$seriesDescription))==T){
            return('11085')
        }
        else if(grepl('PRIMARY', x$imageType)==T & grepl('del', tolower(x$seriesDescription))==T){
            return('11081')
        }
        else if(grepl('PRIMARY', x$imageType)==T & grepl('T1', x$seriesDescription)==T){
            return('T1')
        }
        else if(grepl('PRIMARY', x$imageType)==T & grepl('T2', x$seriesDescription)==T){
            return('T2')
        }
        else if(grepl('PRIMARY', x$imageType)==T & grepl('DW', x$seriesDescription)==T){
            return('DWI')
        }
        else if(grepl('PRIMARY', x$imageType)==T & grepl('FLAIR', x$seriesDescription)==T){
            return('FLAIR')
        }
        else if(grepl('PRIMARY', x$imageType)==T & grepl('Apparent', x$seriesDescription)==T){
            return('Apparent Diffusion Coefficient')
        }
        else if(grepl('PRIMARY', x$imageType)==T & grepl('NA', x$contastBolusAgent)==T){
            return('28833')
        }
        else if(grepl('PRIMARY', x$imageType)==T & grepl('haveValue', x$contastBolusAgent)==T){
            return('28694')
        }
        else {
            return('others')
        }
    })
    radiologyPhaseConceptId<-data.frame(radiologyPhaseConceptId, row.names = NULL)
    return(radiologyPhaseConceptId)
}
