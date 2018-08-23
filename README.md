# DicomHeaderExtractionModule

[![Build Status](https://travis-ci.com/NEONKID/DicomHeaderExtractionModule.svg?token=KX1jEf9MwMRzGiDnba2h&branch=master)](https://travis-ci.com/NEONKID/DicomHeaderExtractionModule)



## Overview

Metadata extraction module of DICOM image format.

The main functions include the Pre and Post identification algorithms of CT and the ability to load them into the DBMS.

 

## Require Package

This package uses OHDSI's DatabaseConnector, oro.dicom package, and so on. In addition, some functions are implemented in Lambda, so you will also need the lambda.r package. In addition, You should also include the R6 package using object-oriented programming techniques such as Java and the C # language.

```
devtools, dplyr, oro.dicom, lambda.r, rapportools, papayar, oro.nifti, DatabaseConnector, R6
```



## Example

Function to convert dcm file to RDS

```R
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
```

Multiple folders are supported reliably.



```R
# RDS filePath
path <- "FILE_PATH"

# Read for data frame...
df <- readRDS("FILE_PATH")
```

The stored RDS file can be easily moved to the data frame using the readRDS function.

See the Example.R file for a more detailed example.