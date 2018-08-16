############################# Example Code: DICOM file to RDS File...   #############################

# Image FilePath
path <- "FILE_PATH"

# Require savePathRoot
savePathRoot <- "SAVEROOTPATH"
dcmToRDS(path, savePathRoot, verbose = TRUE)

############################# Example Code: Create Radiology from RDS.. #############################

# Extract RDS...
df <- createRadiologyOccurrence("DIRECTORY_PATH")
fi <- createRadiologyImage(readRDS("RDS_FILE_PATH"))

# View data...
View(df)
View(fi)

######################################## Example Code: END ##########################################

############################# Example Code: Data frame transfer RDBMS.. #############################

# First, connect RDBMS
# I'm not recommend to input server information in programming code (Security issues)
# So, I recommend to use environment variables in operating system
dbms <- "sql server"
user <- Sys.getenv("user")
pw <- Sys.getenv("pw")
server <- Sys.getenv("dbServer")

# Using DatabaseConnector for OHDSI Package
if(!require(DatabaseConnector))
  install.packages("DatabaseConnector")
library(DatabaseConnector)
con <- connectDBMS(server = server, user = user, pw = pw, dbms = dbms)

# If success connection, input tableName, databaseName,,
databaseSchema <- 'Radiology_CDM.dbo'
tbSchema <- 'Radiology_Image'

# df is radiology Data frame,,
insertDB(con = con, dbS = databaseSchema, tbS = tbSchema, df = df)

# If InsertDB upper 1000 rows, divide data frame,,
# It is only sample...
df <- createRadiologyImage(readRDS(path))
if(nrow(df) > 1000) {
  div <- nrow(df) / 2
  insertDB(con = con, dbS = databaseSchema, tbS = tbSchema, head(df, div))
  insertDB(con = con, dbS = databaseSchema, tbS = tbSchema, tail(df, div))
} else {
  insertDB(con = con, dbS = databaseSchema, tbS = tbSchema, df = df)
}

######################################## Example Code: END ##########################################

################################## Example Code: Get data for RDBMS.... #############################

# Using SQL query
# No condition..
df <- searchUseSQL(con = con, dbS = databaseSchema, tbS = tbSchema)

# include conditions....
df <- searchUseSQL(con = con, dbS = databaseSchema, tbS = tbSchema, condition = "${Column_ID} condition")

# Using only ABMI...
# Search from Radiology_Image table...
df <- searchForImg(con = con, occur_id = "Want to occurence_ID (require)")

# Search from Radiology_Occurrence table...
df <- searchForOcur(con = con, occur_id = "Want to occurence_ID (not require)")

######################################## Example Code: END ##########################################

############################# Example Code: Image processing for data.. #############################

# Show Images for Papayar
showImages("IMAGE_PATH")

######################################## Example Code: END ##########################################

