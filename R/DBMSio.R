################################ DBMSIO (include DatabaseConnector) Class #######################################
#' DBMSIO Class
#'
#' This class is a DBMS I/O class created using the DatabaseConnector package.
#'
#' @param server Enter the server address to access
#' @param user Enter the user ID of the DBMS you want to access
#' @param pw Enter the password for the DBMS you want to access.
#' @param dbms Enter the DBMS type. ex: sql server, oracle
#'
#' @usage DBMSIO$new(server, user, pw, dbms, port)
#' @example Examples/DBMSio_Ex.R
#' @author Neon K.I.D
#' @export
DBMSIO <- R6::R6Class(classname = "DBMSIO",
  private = list(
    con = NULL,
    dbms = NULL,

    # Using DatabaseConnector for OHDSI (included JDBC)
    connectDBMS = function(server, user, pw, dbms, port) {
      if(port != 0)
        sql <- createConnectionDetails(dbms = dbms, user = user, password = pw, server = server, port = port)
      else
        sql <- createConnectionDetails(dbms = dbms, user = user, password = pw, server = server)
      con <- connect(connectionDetails = sql)
      return(con)
    },

    convertSql = function(query) {
      sql <- renderSql(sql = query)$sql
      sql <- translateSql(sql = sql, targetDialect = private$dbms)$sql
      return(sql)
    }
  ),

  public = list(
    initialize = function(server, user, pw, dbms, port = 0) {
      # Using DatabaseConnector for OHDSI Package
      if(!require(DatabaseConnector))
        install.packages("DatabaseConnector")
      library(DatabaseConnector)

      if(!require(SqlRender))
        install.packages("SqlRender")
      library(SqlRender)

      private$dbms <- dbms
      private$con <- private$connectDBMS(server, user, pw, dbms, port)
    },

    # [NOTICE]
    # Before call this function, create database and table,,
    # using DatabaseConnector, but use connection DBMSIO object.
    insertDB = function(dbS, data, dropTableIfExists = FALSE, createTable = TRUE, tempTable = FALSE, useMppBulkLoad = FALSE) {
      occur_rows <- c(radiology_occurrence_ID, radiology_occurrence_date, radiology_occurrence_datetime,
                      Person_ID, Condition_occurrence_id, Device_concept_id, radiology_modality_concept_ID,
                      Person_orientation_concept, Person_position_concept, radiology_protocol_concept_id,
                      Image_total_count, Anatomic_site_concept_id, radiology_Comment, Image_dosage_unit_concept,
                      Dosage_value_as_number, Image_exposure_time_unit_concept, Image_exposure_time, Radiology_dirpath, Visit_occurrence_id)
      img_rows <- c(Radiology_occurrence_ID, Person_ID, Person_orientation_concept, Image_type, radiology_phase_concept_id,
                    Image_no, Phase_total_no, image_resolution_Rows, image_Resolution_Columns, Image_Window_Level_Center,
                    Image_Window_Level_Width, Image_slice_thickness, image_filepath)

      # Using dbms is Microsoft SQL server
      if(private$dbms == "sql server")
        tableName <- "dbo"

      if(all(colnames(data) == occur_rows))
        tableName <-Reduce(pasteSQL, c(dbS, tableName, 'Radiology_Occurrence'))
      else if(all(colnames(data) == img_rows))
        tableName <- Reduce(pasteSQL, c(dbS, tableName, 'Radiology_Image'))
      else
        stop("This data is not Radiology CDM \n Please check data and retry...")

      insertTable(connection = private$con,
                  tableName = tableName,
                  data = data,
                  dropTableIfExists = dropTableIfExists,
                  createTable = createTable,
                  tempTable = tempTable,
                  useMppBulkLoad = useMppBulkLoad)
    },

    # Using SQL for RDBMS...
    searchUseSQL = function(dbS, tbS, condition = NULL) {
      dbSchema <- c(dbS, tbS)
      tb <- Reduce(pasteSQL, dbSchema)
      if(is.null(condition))
        sql <- paste0("SELECT * FROM ", tb)
      else
        sql <- paste0("SELECT * FROM ", tb, " WHERE ", condition)
      return(querySql(connection = private$con, sql = private$convertSql(query = sql)))
    },

    finalize = function() disconnect(private$con)
  )
)
