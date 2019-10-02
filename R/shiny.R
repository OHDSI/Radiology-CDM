#' 'RCDMShinyViewer'
#'
#' RCDMShinyViewer function visualizes result of database analysis
#'
#'
#' @param Radiology_Occurrence_Table is a result of R-CDM : Radiology_Occurrence_Table<-radiologyOccurrenceTable(DICOMList)
#' @param Radiology_Image_Table is a result of R-CDM : Radiology_Image_Table<-radiologyImageTable(DICOMList)
#' @import dplyr
#' @import shiny
#' @import ggplot2
#' @import DT
#' @importFrom magrittr "%>%"
#'
#'
#' @return result of database analysis
#' @examples
#' Radiology_Occurrence_Table<-radiologyOccurrenceTable(DICOMList)
#' Radiology_Image_Table<-radiologyImageTable(DICOMList)
#' RCDMShinyViewer(Radiology_Occurrence_Table, Radiology_Image_Table)
#' @export
RCDMShinyViewer<-function(Radiology_Occurrence_Table, Radiology_Image_Table){
    ui <- fluidPage(
        titlePanel('Radiology-CDM'),
        sidebarLayout(
            sidebarPanel(
                helpText('You can see reactive result of your database analysis by selecting Occurrence date, and modality'),
                sliderInput(inputId = "dateTime",
                            "Occurrence date",
                            min = min(as.Date(Radiology_Occurrence_Table$studyDateTime)),
                            max = max(as.Date(Radiology_Occurrence_Table$studyDateTime)+1),
                            value = c(as.Date('2000-01-01'), as.Date('2015-01-01'))),
                selectInput('mod', 'modality', choices = unique(Radiology_Occurrence_Table$modality), multiple = T),
                helpText('After you select the occurrence date, and modality, you have to select protocol concept ID, and phase additionally. After that extract list of DICOM images files corresponding to the selected conditions by clicking extract button.'),
                selectInput('Pro', 'select Radiology Protocol Concept ID', choices = unique(Radiology_Occurrence_Table$radiologyProtocolConceptId), multiple = T),
                selectInput('Pha', 'select Radiology Phase Concept ID', choices = unique(Radiology_Image_Table$radiologyPhaseConceptId), multiple = T),
                downloadButton('downloadData', 'Extract'),

                width=3),
            mainPanel(
                fluidRow(
                    h2('Database Analysis'),
                    column(6,
                           h4('Count of occurrences'),
                           plotOutput(outputId = "modalityCount")),
                    column(6,
                           h4('Count of images'),
                           plotOutput(outputId = "imageCount")),

                    h2('Find images you want!'),
                    column(12,
                           h4('OMOP concept IDs in your database'),
                           DTOutput(outputId = "protocolConceptId"),
                           h3(textOutput('txt')))
                ),
                width=9)
        )
    )

    server <- function(input, output){
        output$modalityCount <- renderPlot({
            modality1<-Radiology_Occurrence_Table %>% select(modality)
            barplot(table(factor(modality1$modality, levels=unique(Radiology_Occurrence_Table$modality))), border="#69b3a2", col="white", ylim=c(0, max(table(modality1))))
            par(new=TRUE)
            modality2<-Radiology_Occurrence_Table %>% filter(as.Date(studyDateTime) >= min(input$dateTime) & as.Date(studyDateTime) <= max(input$dateTime)) %>% select(modality)
            barplot(table(factor(modality2$modality, levels=unique(Radiology_Occurrence_Table$modality))), col=rgb(0.2,0.4,0.6,0.6), border="#69b3a2", ylim=c(0, max(table(modality1))))
            par(new=TRUE)
            modality3<-Radiology_Occurrence_Table %>% filter(as.Date(studyDateTime) >= min(input$dateTime) & as.Date(studyDateTime) <= max(input$dateTime) & modality %in% c(input$mod)) %>% select(modality)
            barplot(table(factor(modality3$modality, levels=unique(Radiology_Occurrence_Table$modality))), col=7, border="#69b3a2", ylim=c(0, max(table(modality1))))
            par(new=TRUE)
        })
        output$imageCount <- renderPlot({
            image1<-Radiology_Occurrence_Table %>% select(modality)
            image2<-Radiology_Occurrence_Table %>% select(imageTotalCount)
            barplot(table(factor(rep(image1$modality, image2$imageTotalCount), levels=unique(Radiology_Occurrence_Table$modality))), border="#69b3a2", col="white", ylim=c(0, max(table(rep(image1$modality, image2$imageTotalCount)))))
            par(new=TRUE)
            image3<-Radiology_Occurrence_Table %>% filter(as.Date(studyDateTime) >= min(input$dateTime) & as.Date(studyDateTime) <= max(input$dateTime)) %>% select(modality)
            image4<-Radiology_Occurrence_Table %>% filter(as.Date(studyDateTime) >= min(input$dateTime) & as.Date(studyDateTime) <= max(input$dateTime)) %>% select(imageTotalCount)
            barplot(table(factor(rep(image3$modality, image4$imageTotalCount), levels=unique(Radiology_Occurrence_Table$modality))), col=rgb(0.2,0.4,0.6,0.6), border="#69b3a2", ylim=c(0, max(table(rep(image1$modality, image2$imageTotalCount)))))
            par(new=TRUE)
            image5<-Radiology_Occurrence_Table %>% filter(as.Date(studyDateTime) >= min(input$dateTime) & as.Date(studyDateTime) <= max(input$dateTime) & modality %in% c(input$mod)) %>% select(modality)
            image6<-Radiology_Occurrence_Table %>% filter(as.Date(studyDateTime) >= min(input$dateTime) & as.Date(studyDateTime) <= max(input$dateTime) & modality %in% c(input$mod)) %>% select(imageTotalCount)
            barplot(table(factor(rep(image5$modality, image6$imageTotalCount), levels=unique(Radiology_Occurrence_Table$modality))), col=7, border="#69b3a2", ylim=c(0, max(table(rep(image1$modality, image2$imageTotalCount)))))
        })

        output$protocolConceptId <-renderDT({
            RadiologyPlaybook<-unique(LoincRsnaRadiologyPlaybook[,c(2,3)])
            RadiologyPlaybook<-data.frame(RadiologyPlaybook, row.names = NULL)
            if(is.null(input$mod)==T){
                dataframe1<-RadiologyPlaybook %>% filter(radiologyProtocolConceptId %in% Radiology_Occurrence_Table$radiologyProtocolConceptId)
                dataframe5<-Radiology_Occurrence_Table %>% filter(radiologyProtocolConceptId %in% RadiologyPlaybook$radiologyProtocolConceptId & as.Date(studyDateTime) >= min(input$dateTime) & as.Date(studyDateTime) <= max(input$dateTime)) %>% select(radiologyProtocolConceptId, imageTotalCount, radiologyOccurrenceId)
                dataframe2<-split(dataframe5, as.character(dataframe5$radiologyProtocolConceptId))
                dataframe2<-lapply(dataframe2, function(x){
                    return(data.frame(radiologyProtocolConceptId=unique(x$radiologyProtocolConceptId), Count_of_occurrences=nrow(x), Count_of_images=sum(x$imageTotalCount)))}
                )
                dataframe2<-do.call(rbind, dataframe2)
                dataframe2<-data.frame(dataframe2, row.names = NULL)
                answerDF<-merge(dataframe1, dataframe2, by='radiologyProtocolConceptId')

                dataframe4<-Radiology_Occurrence_Table %>% filter(radiologyOccurrenceId %in% as.character(dataframe5$radiologyOccurrenceId)) %>% select(radiologyOccurrenceId, radiologyProtocolConceptId)
                dataframe3<-Radiology_Image_Table %>% filter(radiologyOccurrenceId %in% as.character(dataframe4$radiologyOccurrenceId)) %>% select(radiologyOccurrenceId, radiologyPhaseConceptId) %>% group_by(radiologyOccurrenceId, radiologyPhaseConceptId) %>% count()
                answer<-merge(dataframe4, dataframe3, by='radiologyOccurrenceId')
                answer<-split(answer, as.character(answer$radiologyProtocolConceptId))
                answer<-lapply(answer, function(x){
                    answer<-split(x, as.character(x$radiologyPhaseConceptId))
                    answer<-sapply(answer, function(y){
                        sum(y$n)
                    })
                    ImageCount<-c()
                    for (i in 1:length(answer)){
                        ImageCount<-c(ImageCount, sprintf('%s : %d', names(answer)[i], answer[i]))
                    }
                    ImageCount<-paste(ImageCount, collapse = '  /  ')
                    return(data.frame(radiologyProtocolConceptId=unique(x$radiologyProtocolConceptId), counts=ImageCount))
                })
                answer<-do.call(rbind, answer)
                answer<-merge(answerDF, answer, by='radiologyProtocolConceptId')
                my_vals = answer$radiologyProtocolConceptId
                my_colors = ifelse(my_vals %in% input$Pro,'orange','white')
                datatable(answer) %>% formatStyle(
                    'radiologyProtocolConceptId',
                    target = 'row',
                    backgroundColor = styleEqual(my_vals, my_colors))
            } else if (is.null(input$mod)==F) {
                mods<-paste(input$mod, collapse ='|')
                dataframe1<-RadiologyPlaybook %>% filter(radiologyProtocolConceptId %in% Radiology_Occurrence_Table$radiologyProtocolConceptId & grepl(mods, LongCommonName)==T)
                dataframe5<-Radiology_Occurrence_Table %>% filter(radiologyProtocolConceptId %in% RadiologyPlaybook$radiologyProtocolConceptId & as.Date(studyDateTime) >= min(input$dateTime) & as.Date(studyDateTime) <= max(input$dateTime)) %>% select(radiologyProtocolConceptId, imageTotalCount, radiologyOccurrenceId)
                dataframe2<-split(dataframe5, as.character(dataframe5$radiologyProtocolConceptId))
                dataframe2<-lapply(dataframe2, function(x){
                    return(data.frame(radiologyProtocolConceptId=unique(x$radiologyProtocolConceptId), Count_of_occurrences=nrow(x), Count_of_images=sum(x$imageTotalCount)))}
                )
                dataframe2<-do.call(rbind, dataframe2)
                dataframe2<-data.frame(dataframe2, row.names = NULL)
                answerDF<-merge(dataframe1, dataframe2, by='radiologyProtocolConceptId')

                dataframe4<-Radiology_Occurrence_Table %>% filter(radiologyOccurrenceId %in% as.character(dataframe5$radiologyOccurrenceId)) %>% select(radiologyOccurrenceId, radiologyProtocolConceptId)
                dataframe3<-Radiology_Image_Table %>% filter(radiologyOccurrenceId %in% as.character(dataframe4$radiologyOccurrenceId)) %>% select(radiologyOccurrenceId, radiologyPhaseConceptId) %>% group_by(radiologyOccurrenceId, radiologyPhaseConceptId) %>% count()
                answer<-merge(dataframe4, dataframe3, by='radiologyOccurrenceId')
                answer<-split(answer, as.character(answer$radiologyProtocolConceptId))
                answer<-lapply(answer, function(x){
                    answer<-split(x, as.character(x$radiologyPhaseConceptId))
                    answer<-sapply(answer, function(y){
                        sum(y$n)
                    })
                    ImageCount<-c()
                    for (i in 1:length(answer)){
                        ImageCount<-c(ImageCount, sprintf('%s : %d', names(answer)[i], answer[i]))
                    }
                    ImageCount<-paste(ImageCount, collapse = '  /  ')
                    return(data.frame(radiologyProtocolConceptId=unique(x$radiologyProtocolConceptId), Count_of_Images_of_each_Phase=ImageCount))
                })
                answer<-do.call(rbind, answer)
                answer<-merge(answerDF, answer, by='radiologyProtocolConceptId')
                my_vals = answer$radiologyProtocolConceptId
                my_colors = ifelse(my_vals %in% input$Pro,'orange','white')
                datatable(answer) %>% formatStyle(
                    'radiologyProtocolConceptId',
                    target = 'row',
                    backgroundColor = styleEqual(my_vals, my_colors)


                )
            }
        })
        output$txt <-renderText({
            answer1<-Radiology_Image_Table[, c('radiologyOccurrenceId', 'radiologyPhaseConceptId', 'dicomPath')]
            answer1<-answer1 %>% filter(radiologyOccurrenceId %in% Radiology_Occurrence_Table$radiologyOccurrenceId)
            answer2<-RadiologyPlaybook %>% filter(radiologyProtocolConceptId %in% Radiology_Occurrence_Table$radiologyProtocolConceptId)
            answer2<-merge(Radiology_Occurrence_Table[, c('radiologyOccurrenceId', 'studyDateTime', 'modality', 'radiologyProtocolConceptId')], answer2, by='radiologyProtocolConceptId')
            answer<-merge(answer1, answer2, by='radiologyOccurrenceId')
            answer<-answer %>% filter(as.Date(studyDateTime) >= min(input$dateTime) & as.Date(studyDateTime) <= max(input$dateTime) & grepl(paste(input$mod, collapse ='|'), LongCommonName)==T & radiologyProtocolConceptId %in% input$Pro & radiologyPhaseConceptId %in% input$Pha)
            Counts<-nrow(answer)
            sprintf("You have selected %d images! Now extract list of DICOM files by clicking Export button.", Counts)
        })
        output$downloadData <- downloadHandler(
            filename = function() {
                paste('List_of_DICOM_files.csv')
            },
            content = function(file) {
                answer1<-Radiology_Image_Table[, c('radiologyOccurrenceId', 'radiologyPhaseConceptId', 'dicomPath')]
                answer1<-answer1 %>% filter(radiologyOccurrenceId %in% Radiology_Occurrence_Table$radiologyOccurrenceId)
                answer2<-RadiologyPlaybook %>% filter(radiologyProtocolConceptId %in% Radiology_Occurrence_Table$radiologyProtocolConceptId)
                answer2<-merge(Radiology_Occurrence_Table[, c('radiologyOccurrenceId', 'studyDateTime', 'modality', 'radiologyProtocolConceptId')], answer2, by='radiologyProtocolConceptId')
                answer<-merge(answer1, answer2, by='radiologyOccurrenceId')
                answer<-answer %>% filter(as.Date(studyDateTime) >= min(input$dateTime) & as.Date(studyDateTime) <= max(input$dateTime) & grepl(paste(input$mod, collapse ='|'), LongCommonName)==T & radiologyProtocolConceptId %in% input$Pro & radiologyPhaseConceptId %in% input$Pha)
                write.csv(data.frame(answer$dicomPath, row.names = NULL), file)
            })
    }

    shinyApp(ui=ui,server=server)
}

