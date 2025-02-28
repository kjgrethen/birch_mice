# Script wrangle data from birch mouse project 

#clear workspace
rm(list = ls())

library(data.table) # data.frames better
library(exifr) # read metadata from images
library(parallel) # to run on several cores

#set path
data_path = "D:/julie_recovery"

#extract all existing filepaths, if other extensions are necessary :"\\.(jpg|jpeg|png|tif|tiff|heic)$"
filepaths = list.files(data_path, full.names = TRUE, pattern = "\\.JPG$", recursive = TRUE)


#probably do not extract metadata for all images (costly)
#do after on specific images necessary for analysis 

#extract metadata from images
files = as.data.table(read_exif(filepaths, tags = c("DateTimeOriginal", "Sequence", 
                                                    "MoonPhase", "AmbientTemperature", "AmbientLight")))

#extract metadata from paths
files[, c("device","base_folder", "camera", "SD", "image") := tstrsplit(SourceFile, "/", fixed = TRUE)]
#some cameras are split into SD-card folders, some are not 
files[is.na(image), image := SD]
files[image== SD, SD := NA]

#check unique cameras and SDs
files[, unique(.SD), .SDcols = c("device", "base_folder", "camera", "SD")]


##
# for linux use - as speed up:
# #set path
# data_path = "D:/julie_recovery"
# 
# #extract all existing filepaths if necessary later :"\\.(jpg|jpeg|png|tif|tiff|heic)$"
# filepaths = list.files(data_path, full.names = TRUE, pattern = "\\.JPG$", recursive = TRUE)
# 
# # Define the number of cores (leave 1 free for system stability)
# num_cores = max(1, detectCores() - 1)
# 
# # Function to extract EXIF metadata (recursive scan of the root folder)
# extract_exif <- function(path) {
#   read_exif(path,tags = c("DateTimeOriginal", "Sequence", "MoonPhase", "AmbientTemperature", "AmbientLight"))
# }
# 
# # Run read_exif in parallel
# #exif_list = mclapply(filepaths, extract_exif, mc.cores = num_cores)
# 
# # Combine results into a data.table
# metaData = rbindlist(exif_list, fill = TRUE)

