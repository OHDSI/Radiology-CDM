SELECT coalesce(MAX(Radiology_Occurrence_ID), 1) FROM @ohdsiSchema.Radiology_Occurrence
