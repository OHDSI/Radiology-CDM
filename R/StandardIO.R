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
createDir <- function(savePathRoot, path) {
  dirPath <- Reduce(pastePath, c(savePathRoot, path))
  if(!dir.exists(dirPath)) {
    dir.create(path = dirPath, recursive = TRUE, mode = "0755")
    msg <- c(Sys.time(), " -> Created Dir: ", dirPath)
    print(Reduce(pasteNormal, msg))
  }
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

as.float <- function(x, digits) {
  options(digits = digits)
  ret <- as.numeric(x)
  options(digits = 1)
  return(ret)
}

as.bigint <- function(x, scipen) {
  options(scipen = scipen)
  ret <- as.numeric(x)
  return(ret)
}

getOS <- function() {
  sysinf <- Sys.info()
  if(!is.null(sysinf)) {
    os <- sysinf['sysname']
    os <- switch(os, Darwin = 'osx', Linux = 'Linux', 'cpm')
  } else {
    os <- .Platform$OS.type
    if(grepl("^darwin", R.version$os)) os <- 'osx'
    else if(grepl("^linux-gnu", R.version$os)) os <- 'Linux'
    else os <- 'cpm'
  }
  return(os)
}
