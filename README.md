# Radiology CDM ETL Module

[![Build Status](https://travis-ci.com/NEONKID/DicomHeaderExtractionModule.svg?token=KX1jEf9MwMRzGiDnba2h&branch=master)](https://travis-ci.com/NEONKID/DicomHeaderExtractionModule)



## Overview

Metadata extraction module of DICOM image format.

The main functions include the Pre and Post identification algorithms of CT and the ability to load them into the DBMS.

 

## Require Package

This package uses OHDSI's DatabaseConnector, oro.dicom package, and so on. In addition, some functions are implemented in Lambda, so you will also need the lambda.r package. In addition, You should also include the R6 package using object-oriented programming techniques such as Java and the C # language.

```
devtools, dplyr, oro.dicom, lambda.r, rapportools, papayar, oro.nifti, DatabaseConnector, R6
```



## Extraction sequence

1. Use the DcmFileModule class to extract metadata from a DICOM image file into an RDS file

   ```R
   # Example
   DFM <- DcmFileModule$new(path, savePathRoot)
   DFM$dcmToRDS(rootPathCount = 4, verbose = FALSE)
   ```

2. Read the extracted metadata RDS file using the Radiology CDM generation function. 
   (Using createRadiology family function)

   ```R
   # Example 
   path <- "/home/ohdsi/dicom.rds"
   
   # Create Data frame for Radiology Occurrence Table (based on Radiology CDM)
   df <- createRadiologyOccurrence(path)
   
   # Create Data frame Radiology Image Table (based on Radiology CDM)
   df <- createRadiologyImage(data = readRDS(path))
   ```

3. Loads data from the RDS file into the RDBMS. 
   (Using DBMSIO Class)

   ```R
   # Example
   dbms <- "sql server"
   user <- Sys.getenv("user")
   pw <- Sys.getenv("pw")
   server <- Sys.getenv("dbServer")
   
   # Connect DBMS...
   db <- DBMSIO$new(server = server, user = user, pw = pw, dbms = dbms)
   
   # df is radiology Data frame,,
   db$insertDB(dbS = databaseSchema, tbS = tbSchema, df = df)
   ```



## Example / Documentation

Documents and examples in the DicomHeaderExtractionModule package can be loaded into the class and function names you want to provide the document using the question mark keyword after package installation.

```R
?DBMSIO
```

