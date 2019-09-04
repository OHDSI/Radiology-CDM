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
    contastBolusAgentList<-lapply(DICOMList, function(x){
        contastBolusAgentDf<-x[[1]] %>% dplyr::filter(name %in% c('ContrastBolusAgent')) %>% dplyr::select(value)
        colnames(contastBolusAgentDf)<-'contastBolusAgent'
        return(contastBolusAgentDf)})
    imageTypeDfList<-imageType(DICOMList)
    radiologyPhaseConceptIdDf <- mapply(function(x, y) merge(x, y, by=0, all = T), x = contastBolusAgentList, y = imageTypeDfList, SIMPLIFY = F)
    radiologyPhaseConceptIdDf<-lapply(radiologyPhaseConceptIdDf, function(x){
        if(grepl('localizer', tolower(x$imageType))==T){
            return('28664')} else if(grepl('secondary', tolower(x$imageType))==T){
                return('5901')} else if(grepl('primary', tolower(x$imageType))==T & is.na(x$contastBolusAgent)==T){
                    return('28833')
                } else if(grepl('primary', tolower(x$imageType))==T & is.na(x$contastBolusAgent)==F) {
                    return('28694')
                } else {
                    return('others')
                }
    })
    radiologyPhaseConceptIdDf<-lapply(radiologyPhaseConceptIdDf, function(x){
        radiologyPhaseConceptIdDf<-as.data.frame(x)
        colnames(radiologyPhaseConceptIdDf)<-'radiologyPhaseConceptId'
        return(radiologyPhaseConceptIdDf)
    })
    return(radiologyPhaseConceptIdDf)
}
