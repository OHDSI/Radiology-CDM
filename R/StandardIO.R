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

# Data Frame searching na Data,
# and deleta na Data..
trimData <- function(df) {
  tmp <- na.omit(df)
  fin <- tryCatch({
    tmp[tmp$length > 0,]
  },
  error = function(e){
    print(e)
    fin <- tmp
  })
  return(fin)
}

# Covert DateTime format
getDateTime(date, time) %as% strptime(Reduce(pasteNormal, c(date, time)), format="%Y%m%d%H%M%OS")

# Convert Date format
getDate(date) %as% as.Date(date, format="%Y%m%d")

# Caculate during time..
getDuringTime(before, after, units = "secs") %as% difftime(after, before, units = units)
