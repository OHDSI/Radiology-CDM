# Resizing DICOM image,,
# Using OpenRImage package,,
resizeDICOM(imData, w, h) %as% resizeImage(image = dcmImg, width = w, height = h)

# Package for Papayar Widgets
# Connecting SQL Server,,
# After connection, input want informations..
showImages(con, Occurrence_ID, protocol_concept = NULL, Image_Type = NULL, phase_concept = NULL) %as% {
  # Required PixelData
  imgs <- readDCM(path = pathList, debug = TRUE, view = TRUE)
  nif <- dicom2nifti(imgs)
  papaya(nif)
}
