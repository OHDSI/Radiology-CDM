#' 'radiologyOccurrenceId2'
#'
#' radiologyOccurrenceId2 function indicates occurrence ID of each shoot
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A dataframe indicating occurrence ID of each shoot
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' radiologyOccurrenceId2(DICOMList)
#' @export

#radiologyOccurrenceId2
radiologyOccurrenceId2<-function(DICOMList){
    radiologyOccurrenceId2<-do.call(rbind, radiologyOccurrenceId(DICOMList))
    radiologyOccurrenceId2<-as.data.frame(radiologyOccurrenceId2 %>% group_by(radiologyOccurrenceId) %>% distinct())
    return(radiologyOccurrenceId2)
}
