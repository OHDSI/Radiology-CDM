# Using DatabaseConnector for OHDSI (included JDBC)
connectDBMS <- function(server, user, pw, dbms, port = 0) {
  if(port != 0)
    sql <- createConnectionDetails(dbms = dbms, user = user, password = pw, server = server, port = port)
  else
    sql <- createConnectionDetails(dbms = dbms, user = user, password = pw, server = server)
  con <- connect(connectionDetails = sql)
  return(con)
}

# [NOTICE]
# Before call this function, create database and table,,
insertDB <- function(con, dbS, tbS, df) {
  dbSchema <- c(dbS, tbS)
  tb <- Reduce(pasteSQL, dbSchema)
  val <- paste0(apply(df, 1, function(x) paste0("('", paste0(x, collapse = "', '"), "')")), collapse = ", ")
  dbSendStatement(con, paste0("INSERT INTO ",tb," VALUES ", val))
}

# Using SQL for RDBMS...
searchUseSQL <- function(con, dbS, tbS, condition = NULL) {
  dbSchema <- c(dbS, tbS)
  tb <- Reduce(pasteSQL, dbSchema)
  if(is.null(condition))
    return(querySql(connection = con, sql = paste0("SELECT * FROM ", tb)))
  else
    return(querySql(connection = con, sql = paste0("SELECT * FROM ", tb, " WHERE ", condition)))
}

# 4th val is concept_id, 5th image_Type
searchForImg <- function(con, occur_id){
  dbSchema <- 'Radiology_CDM.dbo'
  tbSchema <- 'Radiology_Image'

  tb <- Reduce(pasteSQL, c(dbSchema, tbSchema))
  defSql <- paste0("SELECT * FROM ", tb)
  df <- querySql(connection = con, sql = paste0(defSql, " WHERE Radiology_occurrence_ID = ", occur_id))
}

# 4th val is concept_id, 5th Directory Path
searchForOcur <- function(con, occur_id = NULL) {
  dbSchema <- 'Radiology_CDM.dbo'
  tbSchema <- 'Radiology_Occurrence'

  tb <- Reduce(pasteSQL, c(dbSchema, tbSchema))
  defSql <- paste0("SELECT * FROM ", tb)
  if(!is.null(occur_id))
    df <- querySql(connection = con, sql = paste0(defSql, " WHERE Radiology_occurrence_ID = ", occur_id))
  else
    df <- querySql(connection = con, sql = paste0(defSql))
}
