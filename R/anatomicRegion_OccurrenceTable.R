#' 'anatomicRegion'
#'
#' anatomicRegion function represents anatomic region of each shoot
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-anatomicRegion(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A dataframe representing anatomic region of each shoot
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' anatomicRegion(DICOMList)
#' @export

anatomicRegion<-function(DICOMList){
    anatomicRegion<-lapply(DICOMList, function(x){
        anatomicRegion<-x[[1]]%>%filter(name %in% c('BodyPartExamined', 'StudyDescription', 'SeriesDescription')) %>% select(value)
        colnames(anatomicRegion)<-'anatomicRegion'
        anatomicRegion<-sapply(anatomicRegion, function(x){
            if(grepl('chest', tolower(anatomicRegion))==T){
                return('chest')
            }
            else if(grepl('head', tolower(anatomicRegion))==T){
                return('head')
            }
            else if(grepl('brain', tolower(anatomicRegion))==T){
                return('head')
            }
            else if(grepl('neck', tolower(anatomicRegion))==T){
                return('neck')
            }
            else if(grepl('abdomen', tolower(anatomicRegion))==T){
                return('abdomen')
            }
            else{
                return('others')
            }
        })
        anatomicRegion<-as.data.frame(anatomicRegion)
        rownames(anatomicRegion)<-c()
        colnames(anatomicRegion)<-c('anatomicRegion')
        return(anatomicRegion)})
    anatomicRegion<-do.call(rbind, anatomicRegion)
    return(anatomicRegion)
}

