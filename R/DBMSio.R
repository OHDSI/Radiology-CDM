################################ DBMSIO (include DatabaseConnector) Class #######################################
if(!require(R6))
  install.packages("R6")
library(R6)

#' @export
DBMSIO <- R6Class(classname = "DBMSIO",
  private = list(
    con = NULL,

    # Using DatabaseConnector for OHDSI (included JDBC)
    connectDBMS = function(server, user, pw, dbms, port) {
      if(port != 0)
        sql <- createConnectionDetails(dbms = dbms, user = user, password = pw, server = server, port = port)
      else
        sql <- createConnectionDetails(dbms = dbms, user = user, password = pw, server = server)
      con <- connect(connectionDetails = sql)
      return(con)
    }
  ),

  public = list(
    initialize = function(server, user, pw, dbms, port = 0) {
      # Using DatabaseConnector for OHDSI Package
      if(!require(DatabaseConnector))
        install.packages("DatabaseConnector")
      library(DatabaseConnector)
      private$con <- private$connectDBMS(server, user, pw, dbms, port)
    },

    # [NOTICE]
    # Before call this function, create database and table,,
    insertDB = function(dbS, tbS, df) {
      dbSchema <- c(dbS, tbS)
      tb <- Reduce(pasteSQL, dbSchema)
      val <- paste0(apply(df, 1, function(x) paste0("('", paste0(x, collapse = "', '"), "')")), collapse = ", ")
      dbSendStatement(private$con, paste0("INSERT INTO ",tb," VALUES ", val))
    },

    # Using SQL for RDBMS...
    searchUseSQL = function(dbS, tbS, condition = NULL) {
      dbSchema <- c(dbS, tbS)
      tb <- Reduce(pasteSQL, dbSchema)
      if(is.null(condition))
        return(querySql(connection = private$con, sql = paste0("SELECT * FROM ", tb)))
      else
        return(querySql(connection = private$con, sql = paste0("SELECT * FROM ", tb, " WHERE ", condition)))
    },

    searchUseSQLAdvanced = function(dbS, tbS, condition, count) {
      dbSchema <- c(dbS, tbS)
      tb <- Reduce(pasteSQL, dbSchema)
      return(querySql(connection = private$con, sql = paste0("SELECT TOP(", count, ") * FROM ", tb, " WHERE ", condition, ";")))
    },

    # 4th val is concept_id, 5th image_Type
    searchForImg = function(occur_id){
      dbSchema <- 'Radiology_CDM.dbo'
      tbSchema <- 'Radiology_Image'

      tb <- Reduce(pasteSQL, c(dbSchema, tbSchema))
      defSql <- paste0("SELECT * FROM ", tb)
      return(querySql(connection = private$con, sql = paste0(defSql, " WHERE Radiology_occurrence_ID = ", occur_id)))
    },

    # 4th val is concept_id, 5th Directory Path
    searchForOcur = function(occur_id = NULL) {
      dbSchema <- 'Radiology_CDM.dbo'
      tbSchema <- 'Radiology_Occurrence'

      tb <- Reduce(pasteSQL, c(dbSchema, tbSchema))
      defSql <- paste0("SELECT * FROM ", tb)
      if(!is.null(occur_id))
        df <- querySql(connection = private$con, sql = paste0(defSql, " WHERE Radiology_occurrence_ID = ", occur_id))
      else
        df <- querySql(connection = private$con, sql = paste0(defSql))
      return(df)
    },

    finalize = function() disconnect(private$con)
  )
)
