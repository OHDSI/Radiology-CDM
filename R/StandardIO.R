# Paste Functions..
pasteSep <- function(sep) {
  function(...) {
    paste(..., sep = sep)
  }
}

# Paste String Lists...
pasteNormal = pasteSep("")
pasteSpacing = pasteSep(" ")
pastePath = pasteSep("/")
pasteSQL = pasteSep(".")

# Create directory...
createDir <- function(path) {
  dirPath <- Reduce(pastePath, c(savePathRoot, path))
  if(!dir.exists(dirPath)) {
    dir.create(path = dirPath, recursive = TRUE, mode = "0755")
    msg <- c(Sys.time(), " -> Created Dir: ", dirPath)
    print(Reduce(pasteNormal, msg))
  }
}

# Save RDS..
objectToRDS <- function(data, savePath) {
  saveRDS(object = data, file = savePath)
  msg <- c(Sys.time(), " -> Saved: ", savePath)
  print(Reduce(pasteNormal, msg))
}

# String equals Method,,
equals <- function(str1, str2) {
  if(is.null(str1))
    stop("NPE...")
  else if(pmatch(str1, str2, nomatch = FALSE) == 1) TRUE else FALSE
}

# Covert DateTime format
getDateTime <- function(date, time) strptime(Reduce(pasteNormal, c(date, time)), format="%Y%m%d%H%M%OS")

# Convert Date format
getDate <- function(date) as.Date(date, format="%Y%m%d")

# Caculate during time..
getDiffTime <- function(before, after, units = "secs") {
  options(digits = 3)
  difftime(after, before, units = units)
}

# Prefix + filePath
combinePath <- function(prefix, path) {
  if(is.vector(path)) {
    newList <- list()
    for(i in 1:length(path))
      newList[i] <- Reduce(pastePath, c(prefix, path[i]))
    return(newList)
  } else return(Reduce(pastePath, c(prefix, list)))
}
