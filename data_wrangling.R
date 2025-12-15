# Script wrangle data from birch mouse project 

#clear workspace
rm(list = ls())

set.seed(42)
library(data.table) # data.frames better
library(stringr) #dealing with strings
#library(exifr) # read metadata from images
#library(parallel) # to run on several cores

#set path
data_path = "Z:/"

#read in file containing all paths to images in directory
files = fread(file = file.path(data_path, "file_list_utf8.txt"), header = FALSE, col.names = "filepath", sep = "\n")

#read in file with species
species= fread(file = file.path(data_path,"species_helper.csv"), header = TRUE)

#extract image names from paths without extension
files[, image_name := tools::file_path_sans_ext(basename(filepath))]
files[, dir := dirname(filepath)]
files[, year := tstrsplit(filepath, "\\\\" )[[2]]]
files[, year := as.numeric(year)]

# Collapse species abbreviations into one single regex pattern (case-insensitive)
pattern <- paste(species$Abbreviation, collapse = "|")

# Use the pattern to match against image_name to find which images were annotated
files[, annot := ifelse(grepl(pattern, image_name, ignore.case = TRUE), TRUE, FALSE)]

#extract the image number to delete pictures that are not of the project and to find originals
files[, image_no := str_extract(image_name, "\\d{3,}")]
#TODO:
#throw out all images that are not from the project


# Remove rows where 'image_no' is NA
files = files[!is.na(image_no)]

#TODO: does not work properly as it misses those not in the same folder
#requires knowing the location
#extract location using function which takes pathdepths and jumps upwards in directories 
#if they are called something with REC or something with DCMI
#find the originals which the annotated images are based on if they were copied
index_orig = files[, {
  copy_ids = image_no[annot == TRUE] #get all the copies
  is_orig = image_no %in% copy_ids & annot == FALSE #find the originals
  .I[is_orig]  # return indices of matching originals
}, by = dir]$V1 # do by group and save indices of originals

# Set TRUE for those originals
files[index_orig, orig_annot := TRUE]
files[is.na(orig_annot), orig_annot := FALSE]


test = files[, tstrsplit(dir, "/" , fixed = TRUE)][,3:9]

# Vector of column names to modify
cols_to_modify = c("V5", "V6", "V7", "V8", "V9")

# Replace values containing "RECNX" = SD-card names with NA
test[, (cols_to_modify) := lapply(.SD, function(x) ifelse(grepl("RECNX", x), NA, x)), .SDcols = cols_to_modify]
# Replace values containing "DCIM" = SD-card with NA
test[, (cols_to_modify) := lapply(.SD, function(x) ifelse(grepl("DCIM", x), NA, x)), .SDcols = cols_to_modify]

#filter out annotated images
annot_files = files[annot== TRUE,1:2]

# mark images that are duplicated and create vector with new names for the training file folder 
# Add a counter by image_name
annot_files[, image_name_save := if (.N == 1) image_name else paste0(image_name, "_", seq_len(.N)), by = image_name]

#mark those that are unsure
annot_files[, unsure := ifelse(grepl("jeroen", image_name, ignore.case = TRUE), TRUE, FALSE)]
annot_files[grepl("X_|X$", image_name), unsure := TRUE]
annot_files[grepl("SPG|sp$", image_name, ignore.case = TRUE), unsure := TRUE]

#extract the species from the image name and add the group to it

# Add a column to annot_files for abbreviation matching
annot_files[, abbrev := sapply(image_name, function(img) {
  # Find all abbreviations that match the image_name
  abbrs <- species$Abbreviation[sapply(species$Abbreviation, function(abbr) grepl(abbr, img, ignore.case = TRUE))]
  
  # If there are matches, concatenate them with an underscore, else return NA
  if (length(abbrs) > 0) {
    return(paste(abbrs, collapse = "_"))  # Concatenate with underscores
  } else {
    return(NA_character_)  # No match found
  }
})]

#fix non-unique patterns 
annot_files[grepl("^Micr_Microtus", abbrev), abbrev := "Microtus"]

#create vector indicating if there are several species found for the image
annot_files[, several_matches := ifelse(grepl("_", abbrev), TRUE, FALSE)]

# Now we join the species table to get the `Group` for each abbreviation
annot_files <- merge(annot_files, species[, .(Abbreviation, Group)], by.x = "abbrev", by.y = "Abbreviation", all.x = TRUE)

#some summary stats on the annotated data
annot_files[, .(Instances = .N, Unsure = sum(unsure)), by = abbrev]
annot_files[, .(Instances = .N, Unsure = sum(unsure)), by = Group]

# Add group to image_name
annot_files[, image_name_save := paste0(image_name_save,"_", Group,".", tools::file_ext(filepath))]


#save table of annotated images
fwrite(
  annot_files[unsure == FALSE,c("filepath", "image_name_save")],
  file = file.path(data_path, "annot_files.csv"),
  sep = ",",
  quote = TRUE,      # ensures special characters are preserved safely
  bom = TRUE         # adds UTF-8 BOM for better compatibility (especially with Excel)
)


#TODO: create empty files for transfer training
#find out which folders are the locations (above SD cards, something with camera)
#CAREFUL!! only use as noise files those with annot and orig_annot == FALSE 
#for now just use those that are not annotated and go through with script to find empties
random_indices = sample(files[annot == FALSE & orig_annot == FALSE, which = TRUE], 100)
files[random_indices, select := TRUE]

#save table of images to check for being empty
fwrite(
  files[select == "TRUE",],
  file = file.path(data_path, "pot_empty_files_1.csv"),
  sep = ",",
  quote = TRUE,      # ensures special characters are preserved safely
  bom = TRUE         # adds UTF-8 BOM for better compatibility (especially with Excel)
)

random_indices = sample(files[annot == FALSE & orig_annot == FALSE & is.na(select), which = TRUE], 100)
files[random_indices, select2 := TRUE]

#save table of images to check for being empty
fwrite(
  files[select2 == "TRUE",],
  file = file.path(data_path, "pot_empty_files_2.csv"),
  sep = ",",
  quote = TRUE,      # ensures special characters are preserved safely
  bom = TRUE         # adds UTF-8 BOM for better compatibility (especially with Excel)
)

check = fread(file.path(data_path,"pot_empty_files_old.csv"))

random_indices = sample(files[annot == FALSE & orig_annot == FALSE & !(filepath %in% check$filepath), which = TRUE], 500)
files[random_indices, select3 := TRUE]

#save table of images to check for being empty
fwrite(
  files[select3 == "TRUE",],
  file = file.path(data_path, "pot_empty_files.csv"),
  sep = ",",
  quote = TRUE,      # ensures special characters are preserved safely
  bom = TRUE         # adds UTF-8 BOM for better compatibility (especially with Excel)
)


#check Z:\2021\Energinet\Kameraer_Gesten_Egholtvej16\Kamera07_OK -> images not in folder below also again in 08 vvul as fcat


# extract 1200 images for detection boxes + 100 extra (25 per group)
# for now: 4 groups - mouse, vole, shrew, other
set.seed(42)
annot_files[, training := NA_character_]
# Sample 325 random indices from the mouse group
random_indices = sample(annot_files[unsure == FALSE & Group == "mouse", which = TRUE], 325)
annot_files[random_indices, training := "mouse"]
random_indices = sample(annot_files[unsure == FALSE & Group == "vole", which = TRUE], 325)
annot_files[random_indices, training := "vole"]
random_indices = sample(annot_files[unsure == FALSE & Group == "shrew", which = TRUE], 325)
annot_files[random_indices, training := "shrew"]
random_indices = sample(annot_files[unsure == FALSE & Group != "mouse" & Group != "vole" & Group != "shrew", which = TRUE], 325)
annot_files[random_indices, training := "other"]

training_files = annot_files[!is.na(training), c("filepath", "image_name_save", "training")]


#save table of training images
fwrite(
  training_files,
  file = file.path(data_path, "training_files.csv"),
  sep = ",",
  quote = TRUE,      # ensures special characters are preserved safely
  bom = TRUE         # adds UTF-8 BOM for better compatibility (especially with Excel)
)

# Sample 325 random indices from the mouse group
random_indices = sample(annot_files[is.na(training) & unsure == FALSE & Group == "mouse", which = TRUE], 50)
annot_files[random_indices, training := "mouse2"]
random_indices = sample(annot_files[is.na(training) & unsure == FALSE & Group == "vole", which = TRUE], 50)
annot_files[random_indices, training := "vole2"]
random_indices = sample(annot_files[is.na(training) & unsure == FALSE & Group == "shrew", which = TRUE], 50)
annot_files[random_indices, training := "shrew2"]

training_files = annot_files[!is.na(training), c("filepath", "image_name_save", "training")]

#save table of additional training images
fwrite(
  training_files[training == "mouse2" | training == "shrew2",],
  file = file.path(data_path, "training_files_shrew.csv"),
  sep = ",",
  quote = TRUE,      # ensures special characters are preserved safely
  bom = TRUE         # adds UTF-8 BOM for better compatibility (especially with Excel)
)



#---------------------------------------------------------------------------------------

#OLD:
#extract all existing filepaths, if other extensions are necessary :"\\.(jpg|jpeg|png|tif|tiff|heic)$"
#filepaths = list.files(data_path, full.names = TRUE, pattern = "\\.(jpg|jpeg|JPG)$", recursive = TRUE)

#probably do not extract metadata for all images (costly)
#do after on specific images necessary for analysis 

#extract metadata from images
#files = as.data.table(read_exif(filepaths, tags = c("DateTimeOriginal", "Sequence", 
#                                                    "MoonPhase", "AmbientTemperature", "AmbientLight")))

#extract metadata from paths c("network","year", "location","loc_subfolder", "camera", "SD", "image") := 
#files[, tstrsplit(filepath, "\\" , fixed = TRUE)]

# # Dynamically split into columns
# files[, c(paste0("var", 1:12)) := tstrsplit(image_name, "_", fixed = TRUE, fill = NA, type.convert = TRUE)]
# 
# #some cameras are split into SD-card folders, some are not 
# files[is.na(image), image := SD]
# files[image== SD, SD := NA]
# 
# #check unique cameras and SDs
# files[, unique(.SD), .SDcols = c("device", "base_folder", "camera", "SD")]


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

