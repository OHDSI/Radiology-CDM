SELECT nvl2(max(Radiology_Occurrence_ID), max(Radiology_Occurrence_ID) + 1, 1) FROM ${ohdsiSchema}.Radiology_Occurrence;
