############################# Example Code: Data frame transfer RDBMS.. #############################

# First, connect RDBMS
# I'm not recommend to input server information in programming code (Security issues)
# So, I recommend to use environment variables in operating system
dbms <- "sql server"
user <- Sys.getenv("user")
pw <- Sys.getenv("pw")
server <- Sys.getenv("dbServer")

# input tableName, databaseName,,
databaseSchema <- 'Radiology_CDM.dbo'
tbSchema <- 'Radiology_Image'

### Using DatabaseConnector package

# Write connect information
conDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms, user = user, password = pw, server = server)

# Connect DBMS
con <- DatabaseConnector::connect(connectionDetails = conDetails)

# insert tables...
DatabaseConnector::insertTable(connection = con, tableName = paste0(databaseSchema, tbSchema), data = df)

# if want disconnect
DatabaseConnector::disconnect(connection = con)

### Using DBMSIO class in RadETL package

# Connect DBMS...
db <- DBMSIO$new(server = server, user = user, pw = pw, dbms = dbms)

# df is radiology Data frame,,
# The insertDB method checks the row in the data frame and automatically inserts the table name.
# If the data frame does not fit the R-CDM, an error occurs.
# You can also inherit the insertTable from the DatabaseConnector package to accept the options for that function.
# e.q dropTableIfExists etc.
db$insertDB(dbS = databaseSchema, df = df)

# If InsertDB upper 1000 rows, divide data frame,,
# It is only sample...
df <- createRadiologyImage(readRDS(path))
if(nrow(df) > 1000) {
  div <- nrow(df) / 2
  db$insertDB(dbS = databaseSchema, head(df, div))
  db$insertDB(dbS = databaseSchema, tail(df, div))
} else {
  db$insertDB(dbS = databaseSchema, df = df)
}

# if want disconnect (required)
db$finalize()

################################## Example Code: Get data for RDBMS.... #############################

### Using SqlRender/DatabaseConnector package
query <- 'SELECT * from Radiology_Image'
sql <- SqlRender::renderSql(sql = query)$sql
sql <- SqlRender::translateSql(sql = sql, targetDialect = dbms)$sql

DatabaseConnector::querySql(connection = con, sql = sql)

### Using DBMSIO class in RadETL package
df <- db$searchUseSQL(dbS = databaseSchema, tbS = tbSchema)

# include conditions....
df <- db$searchUseSQL(dbS = databaseSchema, tbS = tbSchema, condition = "${Column_ID} condition")

######################################## Example Code: END ##########################################
