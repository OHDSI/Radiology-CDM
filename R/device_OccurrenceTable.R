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
        device<-as.character(x[[1]] %>% filter(name=='Manufacturer') %>% select(value))
        if(device=="character(0)" | device==""){
            device='NA'
        }
        return(device)
    })
    device<-as.data.frame(do.call(rbind, device))
    colnames(device)<-'device'
    device<-cbind(device, radiologyOccurrenceId(DICOMList))
    device<-as.data.frame(device %>% group_by(radiologyOccurrenceId) %>% distinct(device))
    device<-split(device, device$radiologyOccurrenceId)
    device<-sapply(device, function(x){
        paste0(x$device, collapse=', ')})
    device<-data.frame(radiologyOccurrenceId=names(device), manufacturer=device, row.names = NULL)
    return(device)
}
