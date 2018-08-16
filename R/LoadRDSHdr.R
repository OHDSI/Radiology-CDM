########################### Function Zone ##############################

# Parameter name is TagName
# Parameter data is RDS Data...
getTagValue(data, name) %as% {
  tryCatch({
    res <- data$value[which(ifelse(data$name %in% name, TRUE, FALSE))]
    if(is.empty(res))
      res <- NA
  }, error = function(e) {
    res <- NA
    assign("res", res, envir = .GlobalEnv)
  })
  return(res)
}

# Parameter name valid in RDS data...
isValidTag(data, name) %as% if(is.boolean(which(ifelse(data$name %in% name, TRUE, FALSE)) == 0)) TRUE else FALSE

# Available multiple RDS file
# maybe seems to parallel processing...
getCommonHdr <- function(path, verbose = FALSE) {
  fileList <- list.files(path = path, recursive = TRUE, full.names = TRUE)
  print(Reduce(pasteNormal, c("Common Header Extracting: ", path)))
  pb <- if(verbose) txtProgressBar(min = 0, max = length(fileList), style = 3, width = 100)

  for(k in 1:length(fileList)) {
    data <- readRDS(file = fileList[k])
    minor <- c()

    for(i in 1:length(data)) {
      # only tag name
      tagList <- unique(data[[i]]$name)
      minor <- c(minor, tagList)
    }

    if(verbose) setTxtProgressBar(pb = pb, value = k)
  }
  if(verbose) close(pb)
  return(unique(minor))
}

# Count list is Data frame...
getHdrcountFrame <- function(path, verbose = FALSE) {
  tagName <- getCommonHdr(path, verbose = verbose)
  tagCount <- rep(1, length(tagName))

  # Common TagList
  frame <- data.frame(tagName, tagCount)
  fileList <- list.files(path = path, recursive = TRUE, full.names = TRUE)

  print(Reduce(pasteNormal, c("Common Header Counting: ", path)))
  pb <- if(verbose) txtProgressBar(min = 0, max = length(fileList), style = 3, width = 100)

  # Dicom total Count
  dCount <- 0

  for(k in 1:length(fileList)) {
    # One RDS File Read... (in multi dicom files)
    data <- readRDS(file = fileList[k])
    dCount <- dCount + length(data)

    # File Loop,,
    for(d in 1:length(data)) {
      if(!is.null(data[[d]])) {
        name <- data[[d]]$name
        value <- data[[d]]$value

        # Only one dicom file tag list
        for(f in 1:length(frame$tagName)) {
          tName <- frame$tagName[f]
          tCount <- frame$tagCount[f]

          ifelse(name %in% tName, frame$tagCount[f] <- tCount + 1, NA)
        }
      }
    }
    if(verbose) setTxtProgressBar(pb = pb, value = k)
  }
  if(verbose) close(pb)
  print(Reduce(pasteNormal, c("DICOM file total Count: ", dCount)))
  return(frame)
}

############################### END ####################################