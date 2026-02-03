# Script to prepare tarining data from birch mouse project 
library(data.table) # data.frames better
library(jsonlite)

#clear workspace
rm(list = ls())

set.seed(42)

#set path
data_path = file.path("C:/Users/au784040/Documents_C/birch_mice_project/training")

training_data = data.table(image = character(1000), annot = character(1000))

#read in all training files
mouse_files = list.files(path = file.path(data_path, "sorted/mouse"), full.names = TRUE)
mouse_annot = list.files(path = file.path(data_path, "sorted/annotations_mouse"), full.names = TRUE)

rand_index = sample(1:length(mouse_files), 200)
training_data[1:200, image := mouse_files[rand_index]]
training_data[1:200, annot := mouse_annot[rand_index]]

shrew_files = list.files(path = file.path(data_path, "sorted/shrew"), full.names = TRUE)
shrew_annot = list.files(path = file.path(data_path, "sorted/annotations_shrew"), full.names = TRUE)

rand_index = sample(1:length(shrew_files), 200)
training_data[201:400, image := shrew_files[rand_index]]
training_data[201:400, annot := shrew_annot[rand_index]]

vole_files = list.files(path = file.path(data_path, "sorted/vole"), full.names = TRUE)
vole_annot = list.files(path = file.path(data_path, "sorted/annotations_vole"), full.names = TRUE)

rand_index = sample(1:length(vole_files), 200)
training_data[401:600, image := vole_files[rand_index]]
training_data[401:600, annot := vole_annot[rand_index]]

other_files = list.files(path = file.path(data_path, "sorted/other"), full.names = TRUE)
other_annot = list.files(path = file.path(data_path, "sorted/annotations_other"), full.names = TRUE)

rand_index = sample(1:length(other_files), 200)
training_data[601:800, image := other_files[rand_index]]
training_data[601:800, annot := other_annot[rand_index]]

empty_files = list.files(path = file.path(data_path, "sorted/empty"), full.names = TRUE)
empty_annot = list.files(path = file.path(data_path, "sorted/annotations_empty"), full.names = TRUE)

rand_index = sample(1:length(empty_files), 200)
training_data[801:1000, image := empty_files[rand_index]]
training_data[801:1000, annot := empty_annot[rand_index]]


all_data = data.table(image = c(mouse_files, shrew_files, vole_files, other_files, empty_files),
                      annot = c(mouse_annot, shrew_annot, vole_annot, other_annot, empty_annot))

validation_data = all_data[!(image %in% training_data$image),]


#save table of training data
fwrite(
  training_data,
  file = file.path(data_path, "training_set4/training_data.csv"),
  sep = ",",
  quote = TRUE,      # ensures special characters are preserved safely
  bom = TRUE         # adds UTF-8 BOM for better compatibility (especially with Excel)
)

#save table of validation data
fwrite(
  validation_data,
  file = file.path(data_path, "training_set4/validation_data.csv"),
  sep = ",",
  quote = TRUE,      # ensures special characters are preserved safely
  bom = TRUE         # adds UTF-8 BOM for better compatibility (especially with Excel)
)



#------------extract only mice, shrews and voles ---------------------

dest_dir = "training_set5"
source_dir = "training_set4" 


#training
tr_data = list.files(file.path(data_path, source_dir, "training_images"))
tr_annot = list.files(file.path(data_path, source_dir, "training_annotations"))

#validation
val_data = list.files(file.path(data_path, source_dir, "validation_images"))
val_annot = list.files(file.path(data_path, source_dir, "validation_annotations"))


species_patterns <- paste(
  unique(na.omit(str_extract(paste(tr_data,val_data), "(?<=_)[^_\\d]+(?=\\.[Jj][Pp][Gg]$)"))),
  collapse = "|"
)


sub_tr = tr_data[grepl("mouse|vole|shrew|rat|mole", tr_data) | !grepl(species_patterns, tr_data)]
unique(str_extract(sub_tr, "(?<=_)[^_\\d]+(?=\\.[Jj][Pp][Gg]$)"))
sub_tr = sub_tr[!grepl("Mammalia", sub_tr)]

sub_val = val_data[grepl("mouse|vole|shrew|rat|mole", val_data) | !grepl(species_patterns, val_data)]
unique(str_extract(sub_val, "(?<=_)[^_\\d]+(?=\\.[Jj][Pp][Gg]$)"))



sub_tr_annot = tr_annot[tools::file_path_sans_ext(tr_annot) %in% tools::file_path_sans_ext(sub_tr)]
sub_val_annot = val_annot[tools::file_path_sans_ext(val_annot) %in% tools::file_path_sans_ext(sub_val)]


file.copy(
  from = file.path(data_path,source_dir,"training_images", sub_tr),
  to   = file.path(data_path, dest_dir,"training_images", sub_tr),
  overwrite = FALSE
)

file.copy(
  from = file.path(data_path,source_dir,"validation_images", sub_val),
  to   = file.path(data_path, dest_dir,"validation_images", sub_val),
  overwrite = FALSE
)

file.copy(
  from = file.path(data_path,source_dir,"training_annotations", sub_tr_annot),
  to   = file.path(data_path, dest_dir,"training_annotations", sub_tr_annot),
  overwrite = FALSE
)

file.copy(
  from = file.path(data_path,source_dir,"validation_annotations", sub_val_annot),
  to   = file.path(data_path, dest_dir,"validation_annotations", sub_val_annot),
  overwrite = FALSE
)


#change boxes for rats (to mice) and moles (to shrews)
replace_value <- function(x, old, new) {
  if (is.character(x) && x == old) return(new)
  if (is.list(x)) return(lapply(x, replace_value, old, new))
  x
}


json_dir <- "training_annotations"  

json_files <- list.files(file.path(data_path, dest_dir,json_dir), pattern = "\\.json$", full.names = TRUE)
json_files <- json_files[grepl("rat", basename(json_files), ignore.case = TRUE)]

for (f in json_files) {
  
  x <- fromJSON(f, simplifyVector = FALSE)
  
  x_new <- replace_value(x, "Pipistrellus sp.", "Barbastella barbastellus")
  
  write_json(x_new, f, pretty = TRUE, auto_unbox = TRUE)
}

json_files <- list.files(file.path(data_path, dest_dir,json_dir), pattern = "\\.json$", full.names = TRUE)
json_files <- json_files[grepl("mole", basename(json_files), ignore.case = TRUE)]

for (f in json_files) {
  
  x <- fromJSON(f, simplifyVector = FALSE)
  
  x_new <- replace_value(x, "Pipistrellus sp.", "Eptesicus serotinus")
  
  write_json(x_new, f, pretty = TRUE, auto_unbox = TRUE)
}

json_dir <- "validation_annotations"  

json_files <- list.files(file.path(data_path, dest_dir,json_dir), pattern = "\\.json$", full.names = TRUE)
json_files <- json_files[grepl("rat", basename(json_files), ignore.case = TRUE)]

for (f in json_files) {
  
  x <- fromJSON(f, simplifyVector = FALSE)
  
  x_new <- replace_value(x, "Pipistrellus sp.", "Barbastella barbastellus")
  
  write_json(x_new, f, pretty = TRUE, auto_unbox = TRUE)
}

json_files <- list.files(file.path(data_path, dest_dir,json_dir), pattern = "\\.json$", full.names = TRUE)
json_files <- json_files[grepl("mole", basename(json_files), ignore.case = TRUE)]

for (f in json_files) {
  
  x <- fromJSON(f, simplifyVector = FALSE)
  
  x_new <- replace_value(x, "Pipistrellus sp.", "Eptesicus serotinus")
  
  write_json(x_new, f, pretty = TRUE, auto_unbox = TRUE)
}
