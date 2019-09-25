#' 'DicomPath'
#'
#' DicomPath function indicates path of each DICOM files
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A list indicating path of each DICOM files
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' lapply(DICOMList)
#' @export

dicomPath<-function(DICOMList){
    dicomPath<-lapply(DICOMList, function(x){
        dicomPath<-names(x)
        if(is.null(dicomPath)==T){
            return('NA')
        }
        return(dicomPath)
    })
    dicomPath<-as.data.frame(do.call(rbind, dicomPath))
    colnames(dicomPath)<-'dicomPath'
    return(dicomPath)
}

