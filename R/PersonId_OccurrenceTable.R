#' 'PersonId'
#'
#' PersonId function indicates patient ID of each occurrences
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A dataframe indicating patient ID and Occurrence ID of each occurrences
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' PersonId(DICOMList)
#' @export

#PersonId
PersonId<-function(DICOMList){
    PersonId<-mapply(function(x, y) merge(x, y, all = T), x = radiologyOccurrenceId(DICOMList), y = personId(DICOMList), SIMPLIFY = F)
    PersonId<-do.call(rbind, PersonId)
    PersonId<-as.data.frame(PersonId %>% group_by(radiologyOccurrenceId) %>% distinct(personId))
    return(PersonId)
}
