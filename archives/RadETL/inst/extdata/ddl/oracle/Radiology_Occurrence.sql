CREATE TABLE @ohdsiSchema.Radiology_Occurrence (
  Radiology_Occurrence_ID             NUMBER(19)    NOT NULL,
  Radiology_Occurrence_date           DATE          NOT NULL,
  Radiology_Occurrence_datetime       DATETIME      NULL,
  Person_ID                           NUMBER(19)    NOT NULL,
  Condition_Occurrence_ID             INTEGER       NULL,
  Device_Concept_ID                   NUMBER(19)    NOT NULL,
  Radiology_Modality_Concept_ID       INTEGER       NOT NULL,
  Radiology_Protocol_Concept_ID       INTEGER       NOT NULL,
  Person_position_Concept_ID          INTEGER       NULL,
  Image_Total_Count                   INTEGER       NOT NULL,
  Anatomic_site_Concept_ID            INTEGER       NULL,
  Radiology_Comment                   VARCHAR(2048) NULL,
  Image_Dosage_Unit_Concept           VARCHAR(4)    NULL,
  Dosage_value_as_number              INTEGER       NULL,
  Image_exposure_time_unit_Concept    VARCHAR(4)    NULL,
  Image_exposure_time                 FLOAT         NULL,
  Visit_Occurrence_ID                 NUMBER(19)    NULL,
  Radiology_dirpath                   VARCHAR(255)  NOT NULL,
  CONSTRAINT RAD_OCCUR_PK PRIMARY KEY (Radiology_Occurrence_ID)
);

CREATE SEQUENCE ${ohdsiSchema}.Radiology_occur_seq START WITH 1 MINVALUE 1 MAXVALUE 9223372036854775807 NOCYCLE;
