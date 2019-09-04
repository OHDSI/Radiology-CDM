#' 'imageOrientationConceptId'
#'
#' imageOrientationConceptId function will represent direction of each image
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A list containing direction of each DICOM image
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' imageOrientationConceptId(DICOMList)
#' @export

imageOrientationConceptId<-function(DICOMList){lapply(DICOMList, function(x){
    imageOrientationConceptIdDf<-x[[1]] %>% dplyr::filter(name %in% c('PatientOrientation', 'ImageType', 'SeriesDescription', 'StudyDescription', 'BodyPartExamined')) %>% dplyr::select(value)
    colnames(imageOrientationConceptIdDf)<-'imageOrientationConceptId'
    imageOrientationConceptIdDf<-sapply(imageOrientationConceptIdDf, function(x){
        if(grepl('chest', tolower(imageOrientationConceptIdDf))==T&grepl('P F', imageOrientationConceptIdDf)==T){
            return('43591')
        }
        else if(grepl('chest', tolower(imageOrientationConceptIdDf))==T&grepl('L F', imageOrientationConceptIdDf)==T){
            return('43594')
        }
        else if(grepl('axial', tolower(imageOrientationConceptIdDf))==T){
            return('10514')
        }
        else{
            return('others')
        }
    })
    imageOrientationConceptIdDf<-as.data.frame(imageOrientationConceptIdDf)
    rownames(imageOrientationConceptIdDf)<-c()
    colnames(imageOrientationConceptIdDf)<-c('imageOrientationConceptId')
    return(imageOrientationConceptIdDf)
})}
