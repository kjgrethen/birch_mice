# Script wrangle data from birch mouse project 

library(data.table)

#set path
path = "D:/julie_recovery"

#extract all existing filepaths
filepaths = data.table(path = list.files(path, full.names = TRUE, pattern = "\\.JPG$", recursive = TRUE))

#extract metadata from paths
filepaths[, c("device","base_folder", "camera", "SD", "image") := tstrsplit(path, "/", fixed = TRUE)]
#some cameras are split into SD-card folders, some are not 
filepaths[is.na(image), image := SD]
filepaths[image== SD, SD := NA]

#check unique cameras and SDs
filepaths[, unique(.SD), .SDcols = c("device", "base_folder", "camera", "SD")]
