SELECT setval('@ohdsiSchema.Radiology_occur_seq', coalesce(max(Radiology_Occurrence_ID), 1)) FROM @ohdsiSchema.Radiology_Occurrence;
