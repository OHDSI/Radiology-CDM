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



## Extraction sequence

1. Use the DcmFileModule class to extract metadata from a DICOM image file into an RDS file
2. Read the extracted metadata RDS file and load it into the DBMS using the Radiology CDM generation function. (Using the DBMSIO class and the createRadiology family function)



## Example / Documentation

Documents and examples in the DicomHeaderExtractionModule package can be loaded into the class and function names you want to provide the document using the question mark keyword after package installation.

```R
?DBMSIO
```

