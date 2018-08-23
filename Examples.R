############################# Example Code: DICOM file to RDS File...   #############################

# Image FilePath
path <- "FILE_PATH"

# Require savePathRoot
savePathRoot <- "SAVEROOTPATH"

# Create DcmFileModule
DcmFm <- DcmFileModule$new(path, savePathRoot)

# rootPathCount is A number from the beginning of the path 
# to determine how many intervals to trim with delimiters.
# verbose shows progress in detail.
DcmFm$dcmToRDS(rootPathCount = 4, verbose = TRUE)

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

# Connect DBMS...
db <- DBMSIO$new(server = server, user = user, pw = pw, dbms = dbms)

# If success connection, input tableName, databaseName,,
databaseSchema <- 'Radiology_CDM.dbo'
tbSchema <- 'Radiology_Image'

# df is radiology Data frame,,
db$insertDB(dbS = databaseSchema, tbS = tbSchema, df = df)

# If InsertDB upper 1000 rows, divide data frame,,
# It is only sample...
df <- createRadiologyImage(readRDS(path))
if(nrow(df) > 1000) {
  div <- nrow(df) / 2
  db$insertDB(dbS = databaseSchema, tbS = tbSchema, head(df, div))
  db$insertDB(dbS = databaseSchema, tbS = tbSchema, tail(df, div))
} else {
  db$insertDB(dbS = databaseSchema, tbS = tbSchema, df = df)
}

######################################## Example Code: END ##########################################

################################## Example Code: Get data for RDBMS.... #############################

# Using SQL query
# No condition..
df <- db$searchUseSQL(dbS = databaseSchema, tbS = tbSchema)

# include conditions....
df <- db$searchUseSQL(dbS = databaseSchema, tbS = tbSchema, condition = "${Column_ID} condition")

# Using only ABMI...
# Search from Radiology_Image table...
df <- db$searchForImg(occur_id = "Want to occurence_ID (require)")

# Search from Radiology_Occurrence table...
df <- db$searchForOcur(occur_id = "Want to occurence_ID (not require)")

######################################## Example Code: END ##########################################

############################# Example Code: Image processing for data.. #############################

# Show Images for Papayar
showImages("IMAGE_PATH")

######################################## Example Code: END ##########################################

