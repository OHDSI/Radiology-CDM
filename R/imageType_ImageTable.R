#' 'imageType'
#'
#' imageType function will represent type of each image (ex. Primary or Secondary)
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A list containing type of each DICOM image
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' imageType(DICOMList)
#' @export

imageType<-function(DICOMList){
    imageTypeDf<-lapply(DICOMList, function(x){
        imageTypeDf<-x[[1]] %>% dplyr::filter(name =='ImageType') %>% dplyr::select(value)
        imageTypeDf<-sapply(imageTypeDf, function(x){
            if(grepl('derived secondary', tolower(imageTypeDf))==T){
                return('SECONDARY')
            } else if(grepl('localizer', tolower(imageTypeDf))==T){
                return('Localizer')
            }
            else{
                return('PRIMARY')
            }
        })
        imageTypeDf<-as.data.frame(imageTypeDf)
        rownames(imageTypeDf)<-c()
        colnames(imageTypeDf)<-c('imageType')
        return(imageTypeDf)
    })
    return(do.call(rbind, imageTypeDf))
}
