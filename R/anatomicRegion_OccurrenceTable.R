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

