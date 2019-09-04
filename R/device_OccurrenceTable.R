#' 'device'
#'
#' device function indicates manufacturer of device which took a shoot
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A dataframe indicating manufacturer of device which took a shoot
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' device(DICOMList)
#' @export

#device
device<-function(DICOMList){
    device<-lapply(DICOMList, function(x){
        device<-x[[1]] %>% filter(name=='Manufacturer') %>% select(value)
        colnames(device)<-'Manufacturer'
        return(device)
    })
    device<-mapply(function(x, y) merge(x, y, all = T), x = radiologyOccurrenceId(DICOMList), y = device, SIMPLIFY = F)
    device<-do.call(rbind, device)
    device<-as.data.frame(device %>% group_by(radiologyOccurrenceId) %>% distinct(Manufacturer))
    device<-split(device, device$radiologyOccurrenceId)
    device<-sapply(device, function(x){
        paste0(x$Manufacturer, collapse=', ')})
    device<-data.frame(radiologyOccurrenceId=names(device), manufacturer=device, row.names = NULL)
    return(device)
}
