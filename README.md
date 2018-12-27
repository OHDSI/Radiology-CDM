# Radiology CDM ETL Module

[![Build Status](https://travis-ci.com/OHDSI/Radiology-CDM.svg?branch=master)](https://travis-ci.com/OHDSI/Radiology-CDM)



## Overview

Metadata extraction module of DICOM image format.

The main functions include the Pre and Post identification algorithms of CT and the ability to load them into the DBMS.



## How to work

![RCDM-ETL_Process](images/RCDM-ETL_Process.png)

Radiology-CDM extracts Metadata and Pixeldata from DICOM file, which is the original radiology image, and combines the data required for RCDM and stores it in DB format and converts it into CDM. 



## Extraction sequence

See the [wiki](https://github.com/OHDSI/Radiology-CDM/wiki/How-to-extract) for details on ETL methods.




## Example / Documentation

Documents and examples in the RadETL package can be loaded into the class and function names you want to provide the document using the question mark keyword after package installation.

```R
?DBMSIO
```

