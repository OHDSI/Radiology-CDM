CREATE SEQUENCE @ohdsiSchema.Radiology_occur_seq START WITH 1 INCREMENT BY 1 MAXVALUE 9223372036854775807 NO CYCLE;

CREATE TABLE @ohdsiSchema.Radiology_Occurrence (
  Radiology_Occurrence_ID             BIGINT        NOT NULL CONSTRAINT RAD_OCCUR_PK PRIMARY KEY CONSTRAINT RAD_OCCUR_DF DEFAULT nextval('Radiology_occur_seq'),
  Radiology_Occurrence_date           DATE          NOT NULL,
  Radiology_Occurrence_datetime       DATETIME      NULL,
  Person_ID                           BIGINT        NOT NULL,
  Condition_Occurrence_ID             INT           NULL,
  Device_Concept_ID                   BIGINT        NOT NULL,
  Radiology_Modality_Concept_ID       INT           NOT NULL,
  Radiology_Protocol_Concept_ID       INT           NOT NULL,
  Person_position_Concept_ID          INT           NULL,
  Image_Total_Count                   INT           NOT NULL,
  Anatomic_site_Concept_ID            INT           NULL,
  Radiology_Comment                   VARCHAR(2048) NULL,
  Image_Dosage_Unit_Concept           VARCHAR(4)    NULL,
  Dosage_value_as_number              INT           NULL,
  Image_exposure_time_unit_Concept    VARCHAR(4)    NULL,
  Image_exposure_time                 FLOAT         NULL,
  Visit_Occurrence_ID                 BIGINT        NULL,
  Radiology_dirpath                   VARCHAR(255)  NOT NULL
);
