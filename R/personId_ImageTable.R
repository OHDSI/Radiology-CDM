#' 'personId'
#'
#' personId function will read PatientID metadata of all DICOM files read by DICOMHeaderList function
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import digest
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A list containing anonymized PatientID of DICOM
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' personId(DICOMList)
#' @export

personId<-function(DICOMList){
    personId<-lapply(DICOMList, function(x){
        personId<-as.character(x[[1]] %>% filter(name=='PatientID') %>% select(value))
        if(personId=="character(0)" | personId=="" | personId=="integer(0)"){
            personId='NA'
        }
        return(personId)})
    personId<-as.data.frame(do.call(rbind, personId))
    colnames(personId)<-'personId'
    return(personId)
}
