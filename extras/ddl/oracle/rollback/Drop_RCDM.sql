IF OBJECT_ID('@ohdsiSchema.Radiology_Image', 'U') IS NOT NULL
DROP TABLE @ohdsiSchema.Radiology_Image;

IF OBJECT_ID('@ohdsiSchema.Radiology_img_seq', 'SO') IS NOT NULL
DROP SEQUENCE @ohdsiSchema.Radiology_img_seq;

IF OBJECT_ID('@ohdsiSchema.Radiology_Occurrence', 'U') IS NOT NULL
DROP TABLE @ohdsiSchema.Radiology_Occurrence;

IF OBJECT_ID('@ohdsiSchema.Radiology_occur_seq', 'SO') IS NOT NULL
DROP SEQUENCE @ohdsiSchema.Radiology_occur_seq;
