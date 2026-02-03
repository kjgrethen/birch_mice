# Script to extract exif data from birch mouse project 

#clear workspace
rm(list = ls())

set.seed(42)
library(data.table) # data.frames better
library(stringr) #dealing with strings
library(progress) #for progress bar for lengthy procedures
#library(exifr) # read metadata from images
#library(parallel) # to run on several cores

#set path
data_path = "//sshfs/idQyuCd7MH@io.erda.au.dk!2222"

#read in file containing all paths to images in directory
files = fread(file = file.path(data_path, "file_list_utf8.txt"), header = FALSE, col.names = "filepath", sep = "\n")

#extract image names from paths without extension
files[, image_name := tools::file_path_sans_ext(basename(filepath))]
files[, dir := dirname(filepath)]
files[, year := tstrsplit(filepath, "\\\\" )[[2]]]
files[, year := as.numeric(year)]
files[, image_no := str_extract(image_name, "\\d{3,}")]
# Remove rows where 'image_no' is NA
files = files[!is.na(image_no)]
#switch first letter to network location
files[, filepath := sub("^Z", "Y", filepath)]

#run with external exif tool because exif r is too slow
exiftool_path = file.path("C:/Users/au784040/Documents_C/exiftool-13.32_64/exiftool.exe")

filepaths <- files$filepath
batch_size <- 1000
n <- length(filepaths)
batches <- split(filepaths, ceiling(seq_along(filepaths)/batch_size))


# Progress bar setup
pb <- progress_bar$new(
  format = "[:bar] :current/:total (:percent) eta: :eta",
  total = length(batches),
  width = 60
)

results <- list()

for (i in seq_along(batches)) {
  batch <- batches[[i]]
  tmpfile <- tempfile(fileext = ".txt")
  #write all paths from the batch to txt file
  writeLines(batch, tmpfile)
  
  #create cmd to run exiftool on batch and return csv output
  cmd <- sprintf('"%s" -@ "%s" -csv', exiftool_path, tmpfile)
  #csv output is read by data.table and saved
  result <- tryCatch(fread(cmd = cmd), error = function(e) NULL)
  
  if (!is.null(result)) {
    results[[i]] <- result
  }
  pb$tick()
}

# Combine all results
all_results <- rbindlist(results, use.names = TRUE, fill = TRUE)

# Replace tabs with single spaces in all character columns
char_cols <- names(all_results)[vapply(all_results, is.character, logical(1))]
all_results[, (char_cols) := lapply(.SD, function(x) gsub("\t", " ", x)), .SDcols = char_cols]


#save table of exif_data
fwrite(
  all_results,
  file = file.path("C:/Users/au784040/Documents_C/BirchMice", "exif_exp.csv"),
  #sep = ";",
  #dec = ".",
  #na = "",
  #quote = "auto",      # ensures special characters are preserved safely
  #bom = TRUE         # adds UTF-8 BOM for better compatibility (especially with Excel)
)
