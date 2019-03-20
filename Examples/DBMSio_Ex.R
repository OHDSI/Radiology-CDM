############################# Example Code: Data frame transfer RDBMS.. #############################

# First, connect RDBMS
# I'm not recommend to input server information in programming code (Security issues)
# So, I recommend to use environment variables in operating system
dbms <- "sql server"
user <- Sys.getenv("user")
pw <- Sys.getenv("pw")
server <- Sys.getenv("dbServer")

# Choose Database name
databaseSchema <- 'Radiology_CDM.dbo'

# Using DatabaseConnector package
io <- DBMSIO$new(server = server, user = user, pw = pw, dbms = dbms)

# df is radiology Data frame,,
# The insertDB method checks the row in the data frame and automatically inserts the table name.
# If the data frame does not fit the R-CDM, an error occurs.
# You can also inherit the insertTable from the DatabaseConnector package to accept the options for that function.
# e.q dropTableIfExists etc.
io$insertDB(dbS = databaseSchema, df = df)

################################## Example Code: Get data for RDBMS.... #############################

# Using DBMSIO class in RadETL package
# table schema names are Radiology_Image and Radiology_Occurrence
tbSchema <- "Radiology_Occurrence"
df <- io$dbGetdtS(dbS = databaseSchema, tbS = tbSchema)

# include conditions....
df <- io$dbGetdtS(dbS = databaseSchema, tbS = tbSchema, condition = "Image_total_count < 20")
df$RADIOLOGY_OCCURRENCE_ID

# if want disconnect (required)
io$finalize()

######################################## Example Code: END ##########################################
