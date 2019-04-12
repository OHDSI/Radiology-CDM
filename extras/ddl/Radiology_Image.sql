CREATE TABLE @ohdsiSchema.Radiology_Image (
  Image_ID                    BIGINT              NOT NULL IDENTITY(1,1) CONSTRAINT RAD_IMG_PK PRIMARY KEY,
  Radiology_Occurrence_ID     BIGINT              NOT NULL,
  Person_ID                   BIGINT              NOT NULL,
  Image_type                  VARCHAR(255)        NOT NULL,
  Radiology_Phase_Concept_ID  INT                 NOT NULL,
  Image_No                    INT                 NOT NULL,
  Phase_Total_No              INT                 NOT NULL,
  Image_Resolution_Rows       INT                 NOT NULL,
  Image_Resolution_Columns    INT                 NOT NULL,
  Image_Window_Level_Center   VARCHAR(255)        NULL,
  Image_Window_Level_Width    VARCHAR(255)        NULL,
  Image_Slice_Thickness       FLOAT               NULL,
  Image_filepath              VARCHAR(255)        NOT NULL
);
