#' 'imageTotalCount'
#'
#' imageTotalCount function indicates total count of each DICOM files in each occurrences
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A dataframe indicating total count of each DICOM files in each occurrences
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' imageTotalCount(DICOMList)
#' @export

#imageTotalCount
imageTotalCount<-function(DICOMList){
    imageTotalCount<-do.call(rbind, radiologyOccurrenceId(DICOMList)) %>% group_by(radiologyOccurrenceId) %>% count(imageTotalCount=n())
    imageTotalCount[,c(1:2)]
}
