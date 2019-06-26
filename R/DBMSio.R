################################ DBMSIO (include DatabaseConnector) Class #######################################
#' DBMSIO Class
#'
#' This class is a DBMS I/O class created using the DatabaseConnector package.
#'
#' @param server Enter the server address to access (See http://ohdsi.github.io/DatabaseConnector/reference/connect.html)
#' @param user Enter the user ID of the DBMS you want to access
#' @param pw Enter the password for the DBMS you want to access.
#' @param dbms Enter the DBMS type. ex: sql server, oracle
#' @example Examples/DBMSio_Ex.R
#' @author Neon K.I.D
#' @export
DBMSIO <- R6::R6Class(classname = "DBMSIO",
  private = list(
    con = NULL,
    dbms = NULL,

    # Using DatabaseConnector for OHDSI (included JDBC)
    connectDBMS = function(server, user, pw, dbms, dbS, port) {
      if(port != 0)
        sql <- createConnectionDetails(dbms = dbms, user = user, password = pw, server = server, schema = dbS, port = port)
      else
        sql <- createConnectionDetails(dbms = dbms, user = user, password = pw, server = server, schema = dbS)
      con <- connect(connectionDetails = sql)
      return(con)
    },

    convertSql = function(query, ohdsiSchema) {
      sql <- render(sql = query, ohdsiSchema = ohdsiSchema)
      sql <- translate(sql = sql, targetDialect = private$dbms)
      return(sql)
    },

    setlastOccurID = function(tbS, progressBar = F) {
      sql <- readSql(sourceFile = file.path(system.file('extdata/migration', package = 'RadETL', mustWork = T),
                                            private$dbms, 'getMaxOccurID.sql'))
      last_id <- querySql(connection = private$con, sql = sql)

      writeLines('Refresh Occurrence ID...')
      sql <- readSql(sourceFile = file.path(system.file('extdata/migration', package = 'RadETL', mustWork = T),
                                            private$dbms, 'setNewOccurID.sql'))
      rsql <- render(sql, ohdsiSchema = tbS, cur_id = last_id[1,,] + 1)
      executeSql(connection = private$con, sql = rsql, progressBar = progressBar)
    }
  ),

  public = list(
    initialize = function(server, user, pw, dbms, dbS, port = 0) {
      # Using DatabaseConnector for OHDSI Package
      if(!require(DatabaseConnector))
        install.packages("DatabaseConnector")
      library(DatabaseConnector)

      if(!require(SqlRender))
        install.packages("SqlRender")
      library(SqlRender)

      private$dbms <- dbms
      private$con <- private$connectDBMS(server, user, pw, dbms, dbS, port)
    },

    # [NOTICE]
    # Before call this function, create database and table,,
    # using DatabaseConnector, but use connection DBMSIO object.

    # Read Radiology Database table columns,,
    # occur_rows: Radiology_Occurrence.rda
    # img_rows: Radiology_Image.rda
    insertDB = function(tbS = 'dbo', data, createTable = FALSE, tempTable = FALSE, useMppBulkLoad = FALSE, progressBar = FALSE) {
      if(all(colnames(data) == occur_cols)) {
        tableName <- Reduce(pasteSQL, c(tbS, 'Radiology_Occurrence'))
        writeLines(text = sprintf('Execute DDL Query for %s', tableName))
        if(createTable) {
          osql <- readSql(sourceFile = file.path(system.file('extdata/ddl', package = 'RadETL', mustWork = T),
                                                 private$dbms, 'Radiology_Occurrence.sql'))
          executeSql(connection = private$con, sql = private$convertSql(osql, ohdsiSchema = tbS))
        }
      } else if(all(colnames(data) == img_cols)) {
        tableName <- Reduce(pasteSQL, c(tbS, 'Radiology_Image'))
        writeLines(text = sprintf('Execute DDL Query for %s', tableName))
        if(createTable) {
          osql <- readSql(sourceFile = file.path(system.file('extdata/ddl', package = 'RadETL', mustWork = T),
                                                 private$dbms, 'Radiology_Image.sql'))
          executeSql(connection = private$con, sql = private$convertSql(osql, ohdsiSchema = tbS))
        }
      } else stop("This data is not Radiology CDM \n Please check data and retry...")

      writeLines(text = sprintf('Insert the %s into the database', tableName))
      val <- paste0(apply(data, 1, function(x) paste0("('", paste0(x, collapse = "', '"), "')")), collapse = ", ")
      val <- gsub(x = gsub(pattern = "NA|'\\'", replacement = "NULL", val), pattern = "\\'NULL'", replacement = "NULL", val)
      sql <- paste0("INSERT INTO ", tableName, " VALUES ", val)
      executeSql(private$con, sql)
      private$setlastOccurID(tbS, progressBar)
    },

    # [WARNING]
    # This function DROPs all RCDMs, including Occurrence and Image.
    dropDB = function(tbS) {
      writeLines(text = "[WARNING]\nThis function erases all data in the RCDM ! \nThis action can not be undone.")
      ch <- readline("Would you like to continue? [y/N]: ")
      switch(tolower(ch), y = {
        osql <- readSql(sourceFile = file.path(system.file('extdata/ddl', package = 'RadETL', mustWork = T),
                                               private$dbms, 'rollback', 'Drop_RCDM.sql'))
        rsql <- render(sql = osql, ohdsiSchema = tbS)
        writeLines(text = "Drop RCDM tables...")
        executeSql(connection = private$con, sql = translate(rsql, private$dbms), progressBar = T)
      })
    },

    # Using SQL for RDBMS...
    dbGetdtS = function(dbS, tbS, condition = NULL) {
      dbSchema <- c(dbS, tbS)
      tb <- Reduce(pasteSQL, dbSchema)
      if(is.null(condition))
        sql <- paste0("SELECT * FROM ", tb)
      else
        sql <- paste0("SELECT * FROM ", tb, " WHERE ", condition)
      return(querySql(connection = private$con, sql = private$convertSql(query = sql)))
    },

    executeSql = function(sql) {
      executeSql(connection = private$con, sql = private$convertSql(query = sql))
    },

    querySql = function(sql) {
      querySql(connection = private$con, sql = private$convertSql(query = sql))
    },

    finalize = function() disconnect(private$con)
  )
)
