#' 'radiologyDirpath'
#'
#' radiologyDirpath function indicates path which includes a single occurrence
#'
#'
#' @param DICOMList you can put it like this and then run the function : DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' @return A dataframe indicating path which includes a single occurrence
#' @examples
#' DICOMList<-DICOMHeaderList(DICOMFolderPath)
#' radiologyDirpath(DICOMList)
#' @export

#radiologyDirpath
radiologyDirpath<-function(DICOMList){
    radiologyDirpath<-cbind(dicomPath(DICOMList), radiologyOccurrenceId(DICOMList))
    radiologyDirpath<-split(radiologyDirpath, radiologyDirpath$radiologyOccurrenceId)
    radiologyDirpath<-sapply(radiologyDirpath, function(x){
        Radiology_Dirpath<-strsplit(as.character(x$dicomPath), '/')
        Radiology_Dirpath<-Reduce(intersect, Radiology_Dirpath)
        if(grepl(Radiology_Dirpath[length(Radiology_Dirpath)], '.dcm')==T){
            Radiology_Dirpath<-Radiology_Dirpath[-length(Radiology_Dirpath)]
        }
        Radiology_Dirpath<-paste(Radiology_Dirpath, collapse='/')
        return(Radiology_Dirpath)
    })
    return(data.frame('radiologyOccurrenceId'=names(radiologyDirpath), 'radiologyDirpath'=radiologyDirpath, row.names = NULL))
}
